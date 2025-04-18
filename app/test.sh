#!/bin/bash

test=1
ghc Main.hs 
echo "   test"
if [ "$(./Main "   test")" = "Found value" ]; then 
echo "Passed test $test"; 
  else
echo "Failed test $test"; fi
((test++))
echo "   []"
if [ "$(./Main "   []")" = "Found value" ]; then 
echo "Passed test $test"; 
  else
echo "Failed test $test"; fi
((test++))
echo "\"\\r\""
if [ "$(./Main "\"\\r\"")" = "Found value" ]; then 
echo "Passed test $test"; 
  else
echo "Failed test $test"; fi
((test++))
echo "\" \r  \""
if [ "$(./Main "\" \r  \"")" = "Found value" ]; then 
echo "Passed test $test"; 
  else
echo "Failed test $test"; fi
((test++))
echo "3.0"
if [ "$(./Main "3.0")" = "Found value" ]; then 
echo "Passed test $test"; 
  else
echo "Failed test $test"; fi
((test++))
echo "3.0+3.0i"
if [ "$(./Main "3.0+3.0i")" = "Found value" ]; then 
echo "Passed test $test"; 
  else
echo "Failed test $test"; fi
((test++))


