#!/bin/bash


stack run compile test.t test

clang -c ../clib/print.c -o lib.o

clang test.o lib.o
