#!/bin/bash
for line in $(sensors -f | grep "Core" | awk '{print "cpu"substr($2,0,1)"|"substr($3,2,5)}')
do
    core=$(echo $line | cut -f1 -d'|')
    temp=$(echo $line | cut -f2 -d'|')
    echo cputemp,core=$core,host=$(hostname) temp=$temp
done
