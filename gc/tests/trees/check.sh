#!/bin/bash

testsize=30
valgrind_options="--leak-check=full"

if [ ! -e Makefile ] && [ ! -e main.c ] && [ ! -e runtime.c ]
then
  newt -
fi

if [ $testsize -gt 50 ]
then
  error_stream="stderr.txt"
else
  error_stream="/dev/stderr"
fi

echo "$testsize" > test_input.txt
for i in `seq $testsize`
do
  echo $RANDOM >> test_input.txt
done

tail -n $testsize test_input.txt | sort -n > right_output.txt

make -B && cat test_input.txt | valgrind $valgrind_options ./trees 2> $error_stream | tail -n +4 > output.txt
if [ $? -ne 0 ]
then
  echo "*** FAIL ***"
  exit 1
fi

diff output.txt right_output.txt -q
if [ $? -ne 0 ]
then
  echo "*** FAIL ***"
  exit 2
fi

echo "***  OK  ***"
