#!/bin/fish

cargo run
llc -o main.s main.ll
as -o main.o main.s
clang entry.c main.o -o main

