#!/usr/bin/env fish

cargo build
target/debug/paridae
cd ..
llc -o main.s main.ll
as -o main.o main.s
clang entry.c main.o -o main
rm main.o main.s

