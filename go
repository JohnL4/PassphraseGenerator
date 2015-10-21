#!/bin/bash

# cat googlebooks-eng-us-all-1gram-20120701-a | ./filterSpecialCharacters.awk | head -100000 > googlebooks-eng-us-all-1gram-20120701-a-filtered-100000

ghc -fforce-recomp -prof -fprof-auto -fprof-cafs mostFrequent.hs

cat googlebooks-eng-us-all-1gram-20120701-a-filtered-100000 \
      | ./mostFrequent +RTS -p -hr >/dev/null \
    && hp2ps -c mostFrequent.hp \
    && ps2pdf mostFrequent.ps

