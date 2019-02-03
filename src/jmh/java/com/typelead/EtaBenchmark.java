package com.typelead;

import org.openjdk.jmh.annotations.*;
import java.io.FileNotFoundException;
import java.util.Arrays;
import java.util.concurrent.TimeUnit;
import eta.main;
import eta.runtime.thunk.Thunk;


import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Paths;
import java.nio.file.Files;
import java.io.ByteArrayInputStream;

@State(Scope.Benchmark)
public class EtaBenchmark {

    @Param({""})
    String args;

    @Param({""})
    String inputFile;

    byte[] inputBytes;
    boolean setInput;

    String[] _args;

    @Setup(Level.Trial)
    public void setup() throws IOException {
        if (args.length() > 0) {
            _args = args.split(" ");
        } else {
            _args = new String[0];
        }

        if (inputFile != null && inputFile.length() > 0) {
            inputBytes = Files.readAllBytes(Paths.get(inputFile));
            setInput = true;
        }
        Thunk.setKeepCAFs();
    }

    @Benchmark
    public void benchmark() {
        System.out.print("@OUT@");
        eta.main.main(_args);
    }

    @Setup(Level.Invocation)
    public void setupIteration() throws FileNotFoundException {
        if (setInput) {
            System.setIn(new ByteArrayInputStream(inputBytes));
        }
    }

    @TearDown(Level.Invocation)
    public void tearDown() {
        Thunk.revertCAFs();
    }
}
