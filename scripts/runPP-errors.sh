#!/bin/bash

for i in `ls ./samples/fail/preparser/` ; do 
    echo "$i :" ;
    echo -e "\t `stack exec acif-exe -- preparse < ../samples/fail/preparser/$i`"; 
done
