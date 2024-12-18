# Open questions

## Exercise 4
Happy uses left recursion in order to save on stack-space.
Left recursion has a constant stack space.
Right recursion uses more stack space in proportion to the length of the parsed list. 
Regular parser combinators must use right recursion, as that is standard to haskell.
## Exercise 10
This only matters in terms of stack size if the recursive call is in the middle.
In all cases, the commands will still be processed, just in a different order. 
But in terms of size, it only stays consistent if the call is at the end, otherwise it grows by the amount of commands after the recursion per recursive call.