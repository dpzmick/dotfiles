#!/bin/bash

for f in /sys/devices/system/cpu/cpu*/cpufreq/scaling_cur_freq
do
    cpu=$(echo $f | cut -f6 -d'/')
    val=$(cat $f)
    echo cpufreq,cpu=$cpu,host=$(hostname) freq=$val
done
