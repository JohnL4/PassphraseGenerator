#!/bin/bash

# Usage: shuffle-pick -w <n> -l <m>
#
# Picks <m> lines of <n> random words each from googlebooks-eng-us-all-1gram-20120701-all-mostFrequent-17bits

while getopts "l:w:" opt; do
    case $opt in
        l) nLines=$OPTARG
            ;;
        w) nWords=$OPTARG
            ;;
    esac
done

i=$nLines
while [ $i -gt 0 ]; do
    echo `shuf -n $nWords googlebooks-eng-us-all-1gram-20120701-all-mostFrequent-17bits | awk '{print $1}'`
    i=`expr $i - 1`
done
