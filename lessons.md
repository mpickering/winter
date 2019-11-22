Lessons from the stage

1. Separating runtime exceptions from compile-time exceptions -- conflated ideas
in the interpreter. This leads to problems with template-haskell due to lack
of overloaded quotations so you have to use TH error handling facility to handle
compile time excpetions.

2. Monad transformers do not play well with Code because Code is not a monad
For example, reader monad does not work well.

3. Interpreter stored syntax and re-interpreted it.

* Option to either compiler the functions (the one we took) or write the
interpreter in a stage-polymorphic manner so it can be used at runtime to
interpret syntax. (Similar to black/lms)

Faster to compile functions when possible

* Also needed to compile loop bodies to avoid infinite recursion. The naive translation was entering a loop because from a break it would try and compile the loop body again and so on. The solution was to generate a recursive function in the program.

4. Pattern matching doesn't work on code values very nicely. You have to match using a case. Can't use built in syntax.

5. Converting code to work in CPS to make sure variables are in scope.

* IORefs are initialised for use as variables for globals, memory and local functions. These are generated in initialise and then we want to use their references directly in the generated code rather than doing a runtime loookup. The way to achieve this is write all these functions in CPS so the reference to the variables can
be passed to the continuation.

It's not possible to return the reference out of `Code`.

TODO: Try to write using ContT


6. Decision about whether to stage Memory/Globals/Table

The reference to the mutable variable is clearly dynamic as it is only created at
runtime but the `miMax` field is static knowledge. Therefore I chose to make
`MemoryINst` a PS-D. The effect of this is that many functions in `Memory` have
to be modified for this reality. It doesn't look like I take advantage of
knowing miMax statically anywhere yet either.

Same story for Global and Table

7. How much to inline?

Template haskell is very syntactic, what you write is what you get and this means that if you write a big case in your program is can get inlined into many different places causing big code bloat. Of course, this might be what you want if they all
collpase into a single match but in general you have to control this as otherwise
the generated program ends up being very big.

8. Generated core is not understandable at all

So much inlining of Vector and IO stuff happens that the generated program is not
readable to a mortla.

9. Bugs to do with polymorphism -- generating polymorphic values does not work well at all.

I found quite a few panics caused by polymorphism and typed quotes. Using typed
holes is a good way to generate a panic if you have some polymorphism floating around. The annoying thing is obviously that types are static knowledge and so should also be elimated at compile time.

Moral: Generate a monomorphic program

10. Usage mode is quite non-optimal, would be better if the compiler could take arguments on the command line rather than having to write them into a file.

11. Using a newtype == unwrappps

12. Cross stage persistence can lead to code blow up. CSP makes no attempt to try
to share persisted values, this leads to the same thing being inserted into many
different places in the program. Again, it's good if the value interacts with a case but not otherwise!

