#!/bin/bash

help()
{
    cat << EOF
Usage: $me -w <n> -l <m>

Picks <m> lines of <n> random words each from googlebooks-eng-us-all-1gram-20120701-all-mostFrequent-17bits
EOF
}

me=`basename $0`

nWords=3                        # default
nLines=10                       # default

WORDS_FILE="mostFreq-a-z-first-33000.txt"
if shuf </dev/null >/dev/null 2>&1; then
   SHUF=shuf
elif gshuf </dev/null >/dev/null 2>&1; then
   SHUF=gshuf
else
    echo "${me}: Can't find 'shuf' or equivalent" >&2
    exit 1
fi

while getopts "l:w:" opt; do
    case $opt in
        l) nLines=$OPTARG
            ;;
        w) nWords=$OPTARG
            ;;
    esac
done

if [ "${nLines:-}" = "" -o "${nWords:-}" = "" ]; then
   help
   exit 1
fi

i=$nLines
while [ $i -gt 0 ]; do
    echo `$SHUF -n $nWords $WORDS_FILE | awk '{print $1}'`
    i=`expr $i - 1`
done

exit 0
