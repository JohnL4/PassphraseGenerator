#!/bin/bash

set -eu
set -x

# cat googlebooks-eng-us-all-1gram-20120701-a | ./filterSpecialCharacters.awk | head -1000000 > googlebooks-eng-us-all-1gram-20120701-a-filtered-1000000

ghc -fforce-recomp -prof -fprof-auto -fprof-cafs mostFrequent.hs

# -hr is "retainers"
# -hd is "data constructors"
# -hy is "types"

cat googlebooks-eng-us-all-1gram-20120701-a-filtered-1000000 \
      | ./mostFrequent +RTS -p -hr >/dev/null \
    && hp2ps -c mostFrequent.hp \
    && ps2pdf mostFrequent.ps

