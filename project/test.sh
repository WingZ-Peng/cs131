#!/bin/bash

# source: https://piazza.com/class/ij4pi2k5m0d5gn?cid=366

args=("$@")
echo "Alford: ${args[0]}"
echo "Bolden: ${args[1]}"
echo "UID: ${args[2]}"
echo "Checking 'enter'..."
{
  echo ""
  sleep 2
} | telnet localhost ${args[0]} > ${args[2]}-1.txt
echo "Checking IAMAT..."
{
  CURRENT_TIMESTAMP=`date +%s`
  echo "IAMAT hello.cs.ucla.edu +34.068930-118.445127 $CURRENT_TIMESTAMP"
  sleep 2
} | telnet localhost ${args[0]} > ${args[2]}-2.txt
echo "Checking WHATSAT..."
{
  echo WHATSAT hello.cs.ucla.edu 10 5
  sleep 2
} | telnet localhost ${args[0]} > ${args[2]}-3.txt
echo "Checking WHATSAT (unknown host)"
{
  echo WHATSAT kiwi.cs.ucla.edu 10 5
  sleep 2
} | telnet localhost ${args[0]} > ${args[2]}-4.txt
echo "Checking flooding..."
{
  echo WHATSAT hello.cs.ucla.edu 10 5
  sleep 2
} | telnet localhost ${args[1]} > ${args[2]}-5.txt
echo "Checking error handling..."
{
  echo hello world
  sleep 2
} | telnet localhost ${args[0]} > ${args[2]}-6.txt
