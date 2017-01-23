#!/bin/bash

# `Unsynchronized` and `GetNSet` omitted because they can deadlock
for class in BetterSorry
do
  echo "testing ${class}..."
  for nthreads in 1 2 4 8 16 32
  do
    echo "  with ${nthreads} thread(s)..."
    for i in {1..100}
    do 
      java UnsafeMemory "$class" "$nthreads" 100000 6 5 6 3 0 3 | \
      grep -oP ' \d+' | \
      head -1 >> \
      "results.${class}.txt"
    done
  done
done
