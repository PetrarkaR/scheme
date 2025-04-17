#!/bin/bash

test=1
ghc Main.hs 
if [ "$(./Main "   test")" = "Found value" ]; then 
echo "Passed test $test"; 
  else
echo "Failed test $test"; fi
((test++))
if [ "$(./Main "   []")" = "Found value" ]; then 
echo "Passed test $test"; 
  else
echo "Failed test $test"; fi
((test++))
if [ "$(./Main "\n")" = "Found value" ]; then 
echo "Passed test $test"; 
  else
echo "Failed test $test"; fi
((test++))
if [ "$(./Main "\"")" = "Found value" ]; then 
echo "Passed test $test"; 
  else
echo "Failed test $test"; fi
((test++))
if [ "$(./Main "\r")" = "Found value" ]; then 
echo "Passed test $test"; 
  else
echo "Failed test $test"; fi
((test++))

