# Java Micobenchmarking Harness for Eta

To try out JMH execution, you can run the following command in this directory:

```
java -classpath "../build/libs/eta-benchmarks-all.jar" org.openjdk.jmh.Main [OPTS]
```

## Reference

For reference, here are the options:

```
Usage: java -jar ... [regexp*] [options]
 [opt] means optional argument.
 <opt> means required argument.
 "+" means comma-separated list of values.
 "time" arguments accept time suffixes, like "100ms".

Command line options usually take precedence over annotations.

  [arguments]                 Benchmarks to run (regexp+). (default: .*) 

  -bm <mode>                  Benchmark mode. Available modes are: [Throughput/thrpt, 
                              AverageTime/avgt, SampleTime/sample, SingleShotTime/ss, 
                              All/all]. (default: Throughput) 

  -bs <int>                   Batch size: number of benchmark method calls per 
                              operation. Some benchmark modes may ignore this 
                              setting, please check this separately. (default: 
                              1) 

  -e <regexp+>                Benchmarks to exclude from the run. 

  -f <int>                    How many times to fork a single benchmark. Use 0 to 
                              disable forking altogether. Warning: disabling 
                              forking may have detrimental impact on benchmark 
                              and infrastructure reliability, you might want 
                              to use different warmup mode instead. (default: 
                              10) 

  -foe <bool>                 Should JMH fail immediately if any benchmark had 
                              experienced an unrecoverable error? This helps 
                              to make quick sanity tests for benchmark suites, 
                              as well as make the automated runs with checking error 
                              codes. (default: false) 

  -gc <bool>                  Should JMH force GC between iterations? Forcing 
                              the GC may help to lower the noise in GC-heavy benchmarks, 
                              at the expense of jeopardizing GC ergonomics decisions. 
                              Use with care. (default: false) 

  -h                          Display help, and exit. 

  -i <int>                    Number of measurement iterations to do. Measurement 
                              iterations are counted towards the benchmark score. 
                              (default: 1 for SingleShotTime, and 20 for all other 
                              modes) 

  -jvm <string>               Use given JVM for runs. This option only affects forked 
                              runs. 

  -jvmArgs <string>           Use given JVM arguments. Most options are inherited 
                              from the host VM options, but in some cases you want 
                              to pass the options only to a forked VM. Either single 
                              space-separated option line, or multiple options 
                              are accepted. This option only affects forked runs. 

  -jvmArgsAppend <string>     Same as jvmArgs, but append these options before 
                              the already given JVM args. 

  -jvmArgsPrepend <string>    Same as jvmArgs, but prepend these options before 
                              the already given JVM arg. 

  -l                          List the benchmarks that match a filter, and exit. 

  -lp                         List the benchmarks that match a filter, along with 
                              parameters, and exit. 

  -lprof                      List profilers, and exit. 

  -lrf                        List machine-readable result formats, and exit. 

  -o <filename>               Redirect human-readable output to a given file. 

  -opi <int>                  Override operations per invocation, see @OperationsPerInvocation 
                              Javadoc for details. (default: 1) 

  -p <param={v,}*>            Benchmark parameters. This option is expected to 
                              be used once per parameter. Parameter name and parameter 
                              values should be separated with equals sign. Parameter 
                              values should be separated with commas. 

  -prof <profiler>            Use profilers to collect additional benchmark data. 
                              Some profilers are not available on all JVMs and/or 
                              all OSes. Please see the list of available profilers 
                              with -lprof. 

  -r <time>                   Minimum time to spend at each measurement iteration. 
                              Benchmarks may generally run longer than iteration 
                              duration. (default: 1 s) 

  -rf <type>                  Format type for machine-readable results. These 
                              results are written to a separate file (see -rff). 
                              See the list of available result formats with -lrf. 
                              (default: CSV) 

  -rff <filename>             Write machine-readable results to a given file. 
                              The file format is controlled by -rf option. Please 
                              see the list of result formats for available formats. 
                              (default: jmh-result.<result-format>) 

  -si <bool>                  Should JMH synchronize iterations? This would significantly 
                              lower the noise in multithreaded tests, by making 
                              sure the measured part happens only when all workers 
                              are running. (default: true) 

  -t <int>                    Number of worker threads to run with. 'max' means 
                              the maximum number of hardware threads available 
                              on the machine, figured out by JMH itself. (default: 
                              1) 

  -tg <int+>                  Override thread group distribution for asymmetric 
                              benchmarks. This option expects a comma-separated 
                              list of thread counts within the group. See @Group/@GroupThreads 
                              Javadoc for more information. 

  -to <time>                  Timeout for benchmark iteration. After reaching 
                              this timeout, JMH will try to interrupt the running 
                              tasks. Non-cooperating benchmarks may ignore this 
                              timeout. (default: 10 min) 

  -tu <TU>                    Override time unit in benchmark results. Available 
                              time units are: [m, s, ms, us, ns]. (default: SECONDS) 

  -v <mode>                   Verbosity mode. Available modes are: [SILENT, NORMAL, 
                              EXTRA]. (default: NORMAL) 

  -w <time>                   Minimum time to spend at each warmup iteration. Benchmarks 
                              may generally run longer than iteration duration. 
                              (default: 1 s) 

  -wbs <int>                  Warmup batch size: number of benchmark method calls 
                              per operation. Some benchmark modes may ignore this 
                              setting. (default: 1) 

  -wf <int>                   How many warmup forks to make for a single benchmark. 
                              All iterations within the warmup fork are not counted 
                              towards the benchmark score. Use 0 to disable warmup 
                              forks. (default: 0) 

  -wi <int>                   Number of warmup iterations to do. Warmup iterations 
                              are not counted towards the benchmark score. (default: 
                              0 for SingleShotTime, and 20 for all other modes) 

  -wm <mode>                  Warmup mode for warming up selected benchmarks. 
                              Warmup modes are: INDI = Warmup each benchmark individually, 
                              then measure it. BULK = Warmup all benchmarks first, 
                              then do all the measurements. BULK_INDI = Warmup 
                              all benchmarks first, then re-warmup each benchmark 
                              individually, then measure it. (default: INDI) 

  -wmb <regexp+>              Warmup benchmarks to include in the run in addition 
                              to already selected by the primary filters. Harness 
                              will not measure these benchmarks, but only use them 
                              for the warmup. 
```

## List of Profilers

This list may not be the same for your platform. Shown for illustration purposes.

```
Supported profilers:
        cl: Classloader profiling via standard MBeans 
      comp: JIT compiler profiling via standard MBeans 
        gc: GC profiling via standard MBeans 
     hs_cl: HotSpot (tm) classloader profiling via implementation-specific MBeans 
   hs_comp: HotSpot (tm) JIT compiler profiling via implementation-specific MBeans 
     hs_gc: HotSpot (tm) memory manager (GC) profiling via implementation-specific MBeans 
     hs_rt: HotSpot (tm) runtime profiling via implementation-specific MBeans 
    hs_thr: HotSpot (tm) threading subsystem via implementation-specific MBeans 
    pauses: Pauses profiler 
     stack: Simple and naive Java stack profiler 

Unsupported profilers:
      perf: <none> 
[Cannot run program "perf": error=2, No such file or directory]
   perfasm: <none> 
[Cannot run program "perf": error=2, No such file or directory]
  perfnorm: <none> 
[Cannot run program "perf": error=2, No such file or directory]
  xperfasm: <none> 
[Cannot run program "xperf": error=2, No such file or directory]
```
