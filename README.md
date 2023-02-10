# CPSC312 Project1: Bayesian Spellchecker
Due: Feb 13, 2023
Erin, Arya, Mizuki

Project deadline: Feb 27
Project demo: Feb 28 - Mar 6
Project description: ​​https://www.cs.ubc.ca/~poole/cs312/2023/projects/project1.pdf

## Feasibility study: is functional programming a feasible option for machine learning applications?

Make a Bayesian spell checker that suggests "correct" options for misspelled words from a predefined list of words. “Something extra”: user’s chosen suggestion would trigger an update of the model probabilities.

User input: text to be checked
Program behavior: 
check input text for misspelled words
iterate through identified misspellings
for each misspelling: offer suggestions, and prompt user to either pick a suggestion or verify that the original is correct
update model probabilities to reflect new data
output corrected text

[Broad to do list, maybe?] What the program would need is 
a dictionary of correct words and 
a training set of typos, which I could find

## What is the Problem?
Given a misspelled word, determine the most likely correct word to replace it based on the probability of the typo made. It will train a naive bayes model on a set of typos and their corrections in order to determine the likelihood of particular typos being made, eg. “what is the probability of accidentally typing an ‘a’ instead of an ‘s’”. The algorithm will check all words that can be reached from the typo word up to a certain “edit distance”, eg. edit distance 3. Then, for valid reachable words, it will accumulate probabilities of those edits using a naive bayes model to determine the most likely correct words. Eg. given a typo “acress”, it will compare the probability of omitting a ‘t’ so the correct word was “actress”, of adding an extra ‘s’ so the correct word was “acres”, of swapping two letters so the correct word was “caress”, etc.

## What is the Something Extra?
When the program suggests a list of likely replacements for the misspelled word, the user can choose the correct one or type in the correct word if it was missing from the suggestions. Then, the counts will be updated in the naive bayes model so that it will be more accurate in the future.
