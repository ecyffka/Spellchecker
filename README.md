# CPSC312 Project1: Bayesian Spellchecker

Authors: Arya, Mizuki, Erin
Project wiki page: ​​https://www.cs.ubc.ca/~poole/cs312/2023/projects/project1.pdf

## Usage

To use the spellchecker:
- Clone the project
- From the root directory, run "cabal run Spellchecker"
- The Spellchecker loads the Brown corpus by default
- Check typos or add new sentences to the corpus

## Feasibility study: is functional programming a feasible option for machine learning applications?

### What is the problem?
Given a misspelled word, determine the most likely correct word to replace it. It will train a naive bayes model on a corpus to determine word probabilities. The algorithm will check all words that can be reached from the typo word up to a certain “edit distance”, eg. edit distance 3. Then, for valid reachable words, it will find the probability of that word having generated the given typo. This probability will use the naive bayes assumption:  $P(word|typo) \propto P(typo|word) P(word)$. Probability of the word is given by the word model trained on the text corpus. Probability of the typo given the word is determined by the edit distance. Eg. given a typo “acress”, it will compare the probability of omitting a ‘t’ so the correct word was “actress”, of adding an extra ‘s’ so the correct word was “acres”, of swapping two letters so the correct word was “caress”, etc.

### What is the something extra?
When the program suggests a list of likely replacements for the misspelled word, the user can choose the correct one or type in the correct word if it was missing from the suggestions. Then, the counts will be updated in the naive bayes word model so that it will be more accurate in the future.

### What did we learn from doing this?
In theory, functional programming is a reasonable option for machine learning, as ML programs can certainly benefit from the inherent simplicity and transparency that functional programming provides. In terms of Haskell, however, there are very few ML libraries available for use (as compared to, say, Python). Depending on the task at hand, this can make things considerably more challenging (eg. in situations where out-of-the-box solutions would be appropriate). 

In situations where new algorithms and libraries are being developed from scratch, though, Haskell could be particularly well-suited. It performs well with parallelized computation, and data immutability prevents unwanted state changes. Additionally, ML-based systems are more and more often safety-critical. Haskell's strong typing increases its reliability, as many errors can be caught prior to runtime.

<nowiki>https://hub.packtpub.com/what-makes-functional-programming-a-viable-choice-for-artificial-intelligence-projects/</nowiki>

<nowiki>https://mmhaskell.com/blog/2017/8/7/the-future-is-functional-haskell-and-the-ai-native-world</nowiki>