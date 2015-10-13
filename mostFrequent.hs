-- | Reads stdin and prints the n most-frequent words occurring after year y to stdout, along with
-- | their counts.

import GHC.IO.Handle (hPutStr)
import GHC.IO.Handle.FD (stderr)

main :: IO()
main = do
{-
Read line, split on tabs, take first three fields, which are: 1-gram (word), year, match-count.
If year >= y:
   Trim parts of speech (POS) from 1-gram (leading, trailing known fragments delimited by "_")
      Known fragments: NOUN, VERB, ADJ, ADV, PRON (pronouns), DET (determiners and articles), ADP
      (prepositions, postpositions), NUM, CONJ, PRT (particles), X (miscellaneous)
   Split on "_", discard expected known fragments, complain if there are more than one fragments left.
   Find word in dictionary and add match-count to that entry.
At end of input, sort dictionary entries by match-counts (descending) and take first n entries for output.

-}
  allInput <- getContents
  hPutStr stderr "Done.\n"
