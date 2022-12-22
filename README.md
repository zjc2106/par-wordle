# Parallelelized Entropy-based Wordle Solver in Haskell

Authors: Sanjay Rajasekharan and Zac Coeur

#### Compilation:
- stack build
#### Parallel Usage:
- stack exec par-wordle-exe \<answer\> valid-wordle-words.txt -- +RTS 
-N4

#### Sequential Usage:
- stack exec seq-wordle-exe \<answer\> valid-wordle-words.txt


