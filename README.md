Code from [Write Yourself a Scheme in 48 Hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Parsing).

## TODO
- use stack for easier repl interaction
- newer Haskell functions like fmap instead of liftM
- edit original article


### Exercises
- Our strings aren't quite R5RS compliant, because they don't support escaping of internal quotes within the string. Change parseString so that \" gives a literal quote character instead of terminating the string. You may want to replace noneOf "\"" with a new parser action that accepts either a non-quote character or a backslash followed by a quote mark.

- Modify the previous exercise to support \n, \r, \t, \\, and any other desired escape characters
Change parseNumber to support the Scheme standard for different bases. You may find the readOct and readHex functions useful.
- Add a Character constructor to LispVal, and create a parser for character literals as described in R5RS.
- Add a Float constructor to LispVal, and support R5RS syntax for decimals. The Haskell function readFloat may be useful.
- Add data types and parsers to support the full numeric tower of Scheme numeric types. Haskell has built-in types to represent many of these; check the Prelude. For the others, you can define compound types that represent eg. a Rational as a numerator and denominator, or a Complex as a real and imaginary part (each itself a Real).
- Add support for the backquote syntactic sugar: the Scheme standard details what it should expand into (quasiquote/unquote).
- Add support for vectors. The Haskell representation is up to you: GHC does have an Array data type, but it can be difficult to use. Strictly speaking, a vector should have constant-time indexing and updating, but destructive update in a purely functional language is difficult. You may have a better idea how to do this after the section on set!, later in this tutorial.
- Instead of using the try combinator, left-factor the grammar so that the common subsequence is its own parser. You should end up with a parser that matches a string of expressions, and one that matches either nothing or a dot and a single expressions. Combining the return values of these into either a List or a DottedList is left as a (somewhat tricky) exercise for the reader: you may want to break it out into another helper function.
- Add primitives to perform the various type-testing functions of R5RS: symbol?, string?, number?, etc.
- Change unpackNum so that it always returns 0 if the value is not a number, even if it's a string or list that could be parsed as a number.
- Add the symbol-handling functions from R5RS. A symbol is what we've been calling an Atom in our data constructors

- Instead of treating any non-false value as true, change the definition of if so that the predicate accepts only Bool values and throws an error on any others.

- equal? has a bug in that a list of values is compared using eqv? instead of equal?. For example, (equal? '(1 "2") '(1 2)) = #f, while you'd expect it to be #t. Change equal? so that it continues to ignore types as it recurs into list structures. You can either do this explicitly, following the example in eqv?, or factor the list clause into a separate helper function that is parameterized by the equality testing function.
- Implement the cond and case expressions.
- Add the rest of the string functions. You don't yet know enough to do string-set!; this is difficult to implement in Haskell, but you'll have enough information after the next two sections
