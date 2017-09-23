#!/bin/bash

help()
{
    cat << EOF
Usage: $me -w <n> -l <m>

Picks <m> lines of <n> random words each from googlebooks-eng-us-all-1gram-20120701-all-mostFrequent-17bits
EOF
}

me=`basename $0`

while getopts "l:w:" opt; do
    case $opt in
        l) nLines=$OPTARG
            ;;
        w) nWords=$OPTARG
            ;;
    esac
done

if [ "${l:-}" = "" -o "${w:-}" = "" ]; then
   help
   exit 1
fi

i=$nLines
while [ $i -gt 0 ]; do
    echo `shuf -n $nWords googlebooks-eng-us-all-1gram-20120701-all-mostFrequent-17bits | awk '{print $1}'`
    i=`expr $i - 1`
done
