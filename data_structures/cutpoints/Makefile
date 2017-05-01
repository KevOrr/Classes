CXX=g++
CXXFLAGS=-g -std=c++11
SHELL=/bin/bash

main: graph.o cut_tester.o
		$(CXX) $(CXXFLAGS) -o $@ $^

graph.o: graph.cpp graph.h
		$(CXX) $(CXXFLAGS) -o $@ -c $<

cut_tester.o: cut_tester.cpp
		$(CXX) $(CXXFLAGS) -o $@ -c $<

test: main
		diff -u cut_correct <(./main <cut_input)

clean:
		rm -f *.o main

.PHONY: test clean
