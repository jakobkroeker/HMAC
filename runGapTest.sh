#!/bin/bash
GAPPROG=$1;
testfile=$2;

testoutput=$( cat $testfile | $GAPPROG -r -q | tee /dev/tty );
errorcount=$( echo $testoutput | egrep "[-|+]" | wc -l ); 

if (( $errorcount != 0 ));  then 
   echo "package test: detected difference to reference output!"; 
   (exit 1); 
else
   echo -e "\n Tests passed! " ; 
fi; 

