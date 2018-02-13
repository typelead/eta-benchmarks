-- Courtesy of Neil Mitchell:
-- https://gist.github.com/ndmitchell/4733855
{-# LANGUAGE RecordWildCards, DeriveDataTypeable #-}

module Main (main) where

-- Standard libraries
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid((<>))
import qualified Data.Map as M
import Data.Time.Clock
import qualified System.Directory as IO
import System.Exit
import System.Info
import System.IO
import System.Process

-- CmdArgs - argument parsing
import System.Console.CmdArgs

-- Shake - build system
import Development.Shake
import Development.Shake.FilePath

---------------------------------------------------------------------
-- TEST CONFIGURATION - which tests are available to run

-- | These are directories that contain tests.
testRoots :: [String]
testRoots = words "imaginary spectral real parallel spectral/hartel shootout smp"

-- | These are tests that are under testRoots, but should be skipped (all are skipped by the Makefile system)
disabledTests :: [String]
disabledTests = []-- words "hartel ebnf2ps PolyGP rx cfd dcbm linsolv warshall"

-- | These tests are compiled by the Makefile system, but don't work for me (mostly GHC 7.4 breaks)
newlyDisabledTests :: [String]
newlyDisabledTests = []--words "lift fulsom fluid"

-- | Directories containing tests that the system can run.
allTests :: IO [FilePath]
allTests = do
  xs <- forM testRoots $ \x -> do
    ys <- IO.getDirectoryContents x
    return [ x </> y | y <- ys, '.' `notElem` y, y `notElem` disabledTests, y `notElem` newlyDisabledTests ]
  fmap sort $ flip filterM (concat xs) $ \x -> do
    b <- IO.doesDirectoryExist x
    if b
    then IO.doesFileExist $ x </> "Makefile"
    else return False

---------------------------------------------------------------------
-- ARGUMENT PARSING - mostly based on CmdArgs

data Nofib =
  Clean
  | Build
    { clean      :: Bool
    , tests      :: [String]
    , way        :: [String]
    , threads    :: Int
    , compiler   :: String
    , tag        :: String
    , output     :: String
    , run        :: Maybe Speed
    , rts        :: [String]
    , jmh        :: [String]
    , times      :: Int
    , skip_check :: Bool }
  deriving (Data,Typeable,Show)

data Speed = Fast | Norm | Slow
  deriving (Data,Typeable,Show)

nofibMode :: Mode (CmdArgs Nofib)
nofibMode = cmdArgsMode $ modes
  [ Clean &= help "Clean the build"
  , Build
    { clean      = False   &= groupname "Building"
                           &= help "Clean before building"
    , tests      = []      &= args
                           &= typ "TEST"
    , way        = []      &= help "Which way to build, defaults to -O1"
    , threads    = 1       &= name "j"
                           &= typ "NUM"
                           &= help "Number of threads, defaults to 1"
    , compiler   = "eta"   &= help "Compiler to use, defaults to ghc"
    , tag        = ""      &= help "Tag to name the compiler, defaults to compiler --version"
    , output     = ""      &= help "Where to put created files under _make, defaults to tag/way"
    , run        = Nothing &= groupname "Running"
                           &= opt "norm"
                           &= help "Run the results (Fast,Norm,Slow)"
    , times      = 1       &= help "Number of times to run each test"
    , rts        = []      &= help "Which RTS options to pass when running"
    , jmh        = []      &= help "Which JMH options to pass when running"
    , skip_check = False   &= help "Skip checking the results of the tests"
    } &= auto
      &= help "Build"
      &= help "Build and run"
  ] &= summary "Nofib benchmark suite"

-- | Create a clean set of arguments, with any defaults filled in
nofibArgs :: IO Nofib
nofibArgs = do
  args <- cmdArgsRun nofibMode
  case args of
    Clean -> return args
    Build {..} -> do
      way    <- return $ let xs = concatMap words way
                         in if null xs then ["-O1"] else xs
      tag    <- if tag == "" then compilerTag compiler else return tag
      tests  <- resolveTests tests
      output <- return $ "_make" </> (if null output
                                      then tag </> intercalate "_" way
                                      else output)
      return Build {..}

-- | Given the tests the user asked for, expand them out, e.g. real is the full real suite.
resolveTests :: [String] -> IO [String]
resolveTests []    = allTests
resolveTests finds = do
  tests <- allTests

  let whole  = filter
                 (\test ->
                    any (\find ->
                           slash2 find `isInfixOf` slash2 test) finds)
                 tests -- whole matches

      prefix = filter
                 (\test ->
                    any (\find ->
                           slash1 find `isInfixOf` slash2 test) finds)
                 tests -- prefix matches

  let res = if null whole then prefix else whole
  when (null res) $
      error $ "The targets failed to match any programs: " ++ unwords finds
  return res
  where slash1 x = "/" ++ map (\i -> if i == '\\' then '/' else i) x
        slash2 x = slash1 x ++ "/"

-- | Find the default compiler string, e.g. ghc-7.4.1
compilerTag :: String -> IO String
compilerTag compiler = do
    (_,ver',_) <- readProcessWithExitCode compiler ["--numeric-version"] ""
    let ver = head . lines $ ver'
    return $ if null ver then "unknown" else "eta-" ++ ver

---------------------------------------------------------------------
-- MAIN DRIVER

-- | Main program, just interpret the arguments and dispatch the tasks.
main :: IO ()
main = do
  args <- nofibArgs
  case args of
    Clean      -> removeDirectoryRecursive "_make"
    Build {..} -> do
      when clean $
        removeDirectoryRecursive output

      shake shakeOptions
              { shakeThreads   = threads
              , shakeFiles     = output ++ "/"
              , shakeReport    = [output ++ "/shake_report.html"]
              , shakeVerbosity = Development.Shake.Loud} $
        buildRules args
      putStrLn "Build completed"

      when (isJust run) $ do
        ns <- mapM (runTest args) tests
        let tot = length ns
            bad = length $ filter not ns
            t i = if i == 1 then "1 test" else show i ++ " tests"
        if bad == 0 then
            putStrLn $ "Ran " ++ t tot ++ " successfully"
          else do
            putStrLn $ "WARNING: " ++ t bad ++ " failed, out of " ++ t tot
            exitFailure

-- | Rules to build the given tests. For each test, there are three files
--   we care about:
--
-- * config.txt - a cleaned up version of the configuration out of Makefile,
--   created by convertConfig. Also contains "MAIN" which points at the name
--   of the Main module.
--
-- * Out.jar - the actual binary, produced by eta linking everything.
--
-- * Out.deps - the files that Out.jar depends on, eta -M.
--
-- * .hi/.jar - files produced by eta -c.
--
--   Most complication comes from modules not named Main, which still produce
--   Main.jar jar files (I think ghc -M gets these wrong).
buildRules :: Nofib -> Rules ()
buildRules Build {..} = do
  let unoutput x =
        let f x = if hasExtension x then f $ takeDirectory x else x
        in f $ takeDirectory $ drop (length output + 1) x
  want $ concat [ [s </> "Out" <.> "jar", s </> "config.txt"]
                | t <- tests, let s = output </> t ] ++ [jmhJar]

  jmhJar %> \out -> do
    need ["build.gradle", "src/jmh/java/com/typelead/EtaBenchmark.java"]
    unit $ cmd "./gradlew" "shadowJar"

  "//config.txt" %> \out -> do
    let dir = unoutput out
    src <- readFileLines $ dir </> "Makefile"
    let poss = [ "Main.hs"
               , "Main.lhs"
               , takeFileName dir <.> "hs"
               , takeFileName dir <.> "lhs" ]
    bs <- filterM (doesFileExist . (dir </>)) poss
    let mainMod = case bs of
          []  -> error $ "Could not find Main file for " ++ dir
          x:_ -> "MAIN = " ++ x
    writeFileLines out $ mainMod : convertConfig src

  ["//*.jar", "//*.hi"] &%> \[jar, _] ->
    if takeFileName jar == "Out.jar"
    then do
      let out = jar
      -- Generate dummy inteface file for Out
      writeFile' (replaceExtension out "hi") ""
      deps <- readFile' $ replaceExtension out "deps"
      let os = nub [ if isLower $ head $ takeFileName x then replaceExtension out "jar" else output </> x
                    | x <- words deps, takeExtension x == ".jar"]
      need os
      config <- readConfig' $ takeDirectory out </> "config.txt"
      let dir = unoutput out
          obj = takeDirectory out
          name = takeFileName dir
      -- TODO: Work on timing stats for the compiler
      putNormal $ "==nofib== " ++ name ++ " : time to link "
                ++ name ++ " follows..."
      unit $ cmd compiler
        $ ["-Rghc-timing", "-rtsopts", "-shared", "-o", out]
        ++ os ++ way ++ words (config "HC_OPTS")
      putNormal $ "==nofib== " ++ name ++ ": size of " ++ name ++ " follows..."
      sizeCmd [out]
    else do
      let dir = unoutput jar
          obj = output </> dir
      config <- readConfig' $ obj </> "config.txt"
      let mod = let x = dropExtension $ drop (length obj + 1) jar
                in if x == "Main" then dropExtension $ config "MAIN" else x
      src <- do b <- doesFileExist $ dir </> mod <.> "hs"
                return $ dir </> mod <.> (if b then "hs" else "lhs")
      deps <- readFileLines $ obj </> "Out.deps"
      need [ if takeExtension r `elem` [".h",".hs",".lhs"] then r else output </> r
          | lhs:":":rhs <- map words $ deps
          , dir </> mod <.> "jar" == lhs
          , r <- rhs ]
      let name = takeFileName dir
      putNormal $ "==nofib== " ++ name ++ " : time to compile "
              ++ mod ++ " follows..."
      unit $ cmd compiler $
        [ "-Rghc-timing", "-c", src, "-w", "-i" ++ obj, "-odir=" ++ obj
        , "-hidir=" ++ obj ]
        ++ way ++ words (config "HC_OPTS")
      putNormal $ "==nofib== " ++ name ++ ": size of "
              ++ takeFileName jar ++ " follows..."
      sizeCmd [jar]

  "//Out.deps" %> \out -> do
    let dir = unoutput out
    config <- readConfig' $ takeDirectory out </> "config.txt"
    unit $ cmd compiler $
      ["-w", "-M", dir </> config "MAIN", "-i" ++ dir, "-dep-makefile=" ++ out]
      ++ words (config "HC_OPTS")
    src <- liftIO $ readFile out
    need [x | x <- words src, takeExtension x `elem` [".hs",".lhs",".h"]]

-- | Run a test, checking stdout/stderr are as expected, and reporting time.
--   Return True if the test passes.
runTest :: Nofib -> String -> IO Bool
runTest nofib@Build {run = Just speed, ..} test = do
  putStrLn $ "==nofib== " ++ takeDirectory1 test ++ ": time to run "
          ++ takeFileName test ++ " follows..."
  config <- readConfig $ output </> test </> "config.txt"
  let totalArgs = (words (config "PROG_ARGS") ++ ["+RTS"] ++ concat (map words rts))
      (jvmFlags, progArgs) = stackHeapFlags totalArgs
      stackHeapFlags (x:xs)
        | "-M" `isPrefixOf` x = (("-Xmx" ++ drop 2 x) : ys, zs)
        | "-H" `isPrefixOf` x = (("-Xms" ++ drop 2 x) : ys, zs)
        | "-K" `isPrefixOf` x = (("-Xss" ++ drop 2 x) : ys, zs)
        | otherwise           = (ys, x:zs)
        where (ys, zs) = stackHeapFlags xs
      stackHeapFlags [] = ([], [])

      args = words (config $ map toUpper (show speed) ++ "_OPTS") ++ progArgs
  stdin <- let s = config "STDIN_FILE"
           in if s == "" then grabIn "stdin" else return . Just $ test </> s
  let stats = output </> test </> "stats.txt"
  -- TODO: Make this work on windows too.
  paths <- defaultLibPaths nofib config
  let classpath = intercalate ":"
                $ (output </> test </> "Out.jar")
                : jmhJar : paths
      rtsArgs = "args=" ++ intercalate " " args
      stdInArgs
        | Just path <- stdin
        = ["-p", "inputFile=" ++ path]
        | otherwise
        = []
      jmhArgs' = words "-wi 5 -i 5 -bm sample -bs 1 -tu ms -foe true -f 1"
              ++ concat (map words jmh)
      jmhArgs = concat
              $ map (\(arg, val) -> [arg, val])
              $ M.toList
              $ overrideJMH jmhArgs' M.empty
  fmap and $ replicateM times $ do
    (code, stdout', stderr') <-
      readProcessWithExitCodeAndWorkingDirectory
        "."
        "java"
        (["-classpath", classpath]
         ++ jvmFlags
         ++ ["org.openjdk.jmh.Main"]
         ++ ["-p", rtsArgs]
         ++ stdInArgs
         ++ ["-o", stats]
         ++ jmhArgs) -- -rff stats.csv
        ""
    writeFile (output </> test </> "rawstdout") stdout'
    writeFile (output </> test </> "rawstderr") stderr'
    let stdout = getOutput stdout'
        stderr = getOutput stderr'
    stdoutWant <- grab "stdout"
    stderrWant <- grab "stderr"
    writeFile (output </> test </> "stdout") stdout
    writeFile (output </> test </> "stderr") stderr
    statsStr <- readFile stats
    let statsLines = lines statsStr
        statsLen   = length statsLines
    putStr $ unlines $ dropWhile (not . isPrefixOf "Benchmark") statsLines
    err <- return $
        if not skip_check && stderr /= stderrWant then
          "FAILED STDERR\nWANTED: " ++ snip stderrWant ++ "\nGOT: " ++ snip stderr
        else if not skip_check && stdout /= stdoutWant then
          "FAILED STDOUT\nWANTED: " ++ snip stdoutWant ++ "\nGOT: " ++ snip stdout
        else if not skip_check && code /= ExitSuccess then
          "FAILED EXIT CODE " ++ show code
        else ""
    if null err then return True else putStrLn err >> return False
  where snip x = if length x > 200 then take 200 x ++ "..." else x
        grabIn ext = do
          let s = [ test </> takeFileName test <.> map toLower (show speed) ++ ext
                  , test </> takeFileName test <.> ext]
          ss <- filterM IO.doesFileExist s
          return $ listToMaybe ss
        grab ext = maybe (return "") readFile =<< grabIn ext
        getOutput str
          | "@OUT@" `isPrefixOf` str = fst . break (== '@') $ drop 5 str
          | otherwise = str
        overrideJMH args'@(arg:args)
          | head arg == '-' =
            case args of
              (x:xs) -> if head x == '-'
                        then overrideJMH args . M.insert arg ""
                        else overrideJMH xs   . M.insert arg x
              [] -> id
          | otherwise = error $ "Bad JMH arguments: " ++ show args'
        overrideJMH [] = id

---------------------------------------------------------------------
-- CONFIGURATION UTILITIES
-- The Makefile's are slurped for configuration, to produce a cleaned-up config file

-- | Given the source of a Makefile, slurp out the configuration strings.
convertConfig :: [String] -> [String]
convertConfig xs = [remap a ++ " = " ++ b | x <- xs, let (a,b) = separate x, a `elem` keep]
    where
        keep = words "PROG_ARGS SRC_HC_OPTS SRC_RUNTEST_OPTS SLOW_OPTS NORM_OPTS FAST_OPTS STDIN_FILE HC_OPTS"
        remap "SRC_RUNTEST_OPTS" = "PROG_ARGS"
        remap "SRC_HC_OPTS" = "HC_OPTS"
        remap x = x

        separate x = (name,rest)
            where (name,x2) = span (\x -> isAlpha x || x == '_') x
                  rest = dropWhile isSpace $ dropWhile (`elem` "+=") $ dropWhile isSpace x2


-- | Read a configuration file (new format) into a function supplying options.
readConfig :: FilePath -> IO (String -> String)
readConfig x = do
  src <- readFile x
  let res = [ ( reverse $ dropWhile isSpace $ reverse a
              , dropWhile isSpace $ drop 1 b )
            | y <- lines src, let (a,b) = break (== '=') y]
  return $ \x -> fromMaybe "" $ lookup x res


-- | readConfig lifted into the Action monad.
readConfig' :: FilePath -> Action (String -> String)
readConfig' x = do
  need [x]
  liftIO $ readConfig x

---------------------------------------------------------------------
-- GENERAL UTILITIES

-- -- | The executable extension on this platform.
-- exe :: String
-- exe = if os == "mingw32" then "exe" else ""

-- | Like the standard removeDirectoryRecursive, but doesn't fail if the path is missing.
removeDirectoryRecursive :: FilePath -> IO ()
removeDirectoryRecursive x = do
  b <- IO.doesDirectoryExist x
  when b $ IO.removeDirectoryRecursive x

-- | Source for readProcessWithExitCode, plus addition of cwd
readProcessWithExitCodeAndWorkingDirectory
    :: FilePath                 -- ^ directory to use
    -> FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> String                   -- ^ standard input
    -> IO (ExitCode,String,String) -- ^ exitcode, stdout, stderr
readProcessWithExitCodeAndWorkingDirectory cwd cmd args input = do
  readCreateProcessWithExitCode ((proc cmd args) { cwd = Just cwd }) input

sizeCmd :: [String] -> Action ()
sizeCmd args = unit $ cmd ("stat " <> opt) args
  where opt
          | os == "darwin" = "-f%z"
          | otherwise      = "-c%z"

basePackages :: [String]
basePackages = ["ghc-prim","integer","base", "rts"]

defaultLibPaths :: Nofib -> (String -> String) -> IO [String]
defaultLibPaths Build {..} config = do
  let filterPackageFlags (package:rest)
        | "-package" `isInfixOf` package
        = head rest : filterPackageFlags (tail rest)
        | otherwise
        = filterPackageFlags rest
      filterPackageFlags _ = []
      extraPackages = filterPackageFlags $ words (config "HC_OPTS")
      packages = nub (basePackages ++ extraPackages)
  searchForJars tag packages

searchForJars :: String -> [String] -> IO [String]
searchForJars tag packages = do
  libDir  <- fmap (\x -> x </> "lib") $ IO.getAppUserDataDirectory "etlas"
  libDir' <- fmap (head . filter (tag `isInfixOf`))
           $ IO.getDirectoryContents libDir
  let packagesDir = libDir </> libDir'
  packageDirs <- fmap (filter (\x -> any (`isInfixOf` x) packages))
               $ IO.getDirectoryContents packagesDir
  forM packageDirs $ \p -> do
    let packageDir = packagesDir </> p
    jar <- fmap (head . filter (".jar" `isSuffixOf`))
         $ IO.getDirectoryContents packageDir
    return $ packageDir </> jar

jmhJar :: FilePath
jmhJar = "build/libs/eta-benchmarks-all.jar"
