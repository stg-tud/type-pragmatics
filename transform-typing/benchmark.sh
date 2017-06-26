#!/bin/sh

sbt startScript

target/start benchmark.Benchmark benchmark/let.csv benchmark/let-details.csv stlc/let
target/start benchmark.Diagram benchmark/let-details.csv

target/start benchmark.Benchmark benchmark/delta.csv benchmark/delta-details.csv stlc/delta
target/start benchmark.Diagram benchmark/delta-details.csv

target/start benchmark.Benchmark benchmark/cps.csv benchmark/cps-details.csv stlc/cps
target/start benchmark.Diagram benchmark/cps-details.csv

target/start benchmark.Benchmark benchmark/matrix.csv benchmark/matrix-details.csv whilelang/matrix
target/start benchmark.Diagram benchmark/matrix-details.csv
