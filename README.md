# Parallelelized Entropy-based Wordle Solver in Haskell

Authors: Sanjay Rajasekharan and Zac Coeur

For Project Info see [ParWordle-report.pdf](ParWordle-report.pdf) and [ParWordle-Presentation.pdf](ParWordle-Presentation.pdf)

#### Compilation:
- stack build
#### Parallel Usage:
- stack exec par-wordle-exe \<answer\> valid-wordle-words.txt -- +RTS 
-N4

#### Sequential Usage:
- stack exec seq-wordle-exe \<answer\> valid-wordle-words.txt


