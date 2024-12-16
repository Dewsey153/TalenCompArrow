# Open questions

## Exercise 4
Happy uses left recursion in order to save on stack-space. Right recursion uses more stack space in proportion to the length of the parsed list. Regular parser combinators must use right recursion, as all of the parsing is dependant on what has been parsed.
## Exercise 10
This should not matter. The commands will still be processed, just in a different order. In terms of size, we should not worry, as the commands of a recursive rule are only added when the rule is at the top of the stack. Only if the recursive call is in the middle could the command stack grow too large.