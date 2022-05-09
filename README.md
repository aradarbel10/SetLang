# Depracated! See [spiritual successor](https://github.com/aradarbel10/ReasonableLang)

# ~~SetLang~~
An interpreted math-based recreational programming language implemented in haskell.

[\[trello board\]](https://trello.com/b/TWynyRIs)

### List of fancy terminology:
- symbolically interpreted
- math oriented
- impure functional & procedural
- somewhat lazy
- sets as types
- gradually typed (to be)

### Code Examples
all code examples can be found under the `app/examples` directory
```
-- single line comments
-/ multi-line
       comments /-
NOP -- dummy statement

-- numbers, booleans, comparisons --
writeln 1 + 2
writeln (1 = 1)
writeln (false = (true /= false))
writeln ((1 < 100) /= (20 < 6))

-- rastor sets --
writeln {}
writeln {1, 2, 3}
writeln {∅, 1, 2, 3, 2 + 1, 4, {100 < 200}, ∅}

-- assign variables --
var <- 321
writeln var
var <- 123
writeln var

-- define procedures and execute them --
proc printNum:
    favNum <- var
    writeln favNum

printNum

-- definitions with parameters --
func multiply factor := 73 * factor

writeln (multiply 1)
writeln (multiply 2)
writeln (multiply 10)

-- if else statements & expressions --
if 2 < 3:
    writeln (if true  then 40 else 50)
else:
    writeln (if false then 60 else 70)

-- recursive functions --
func fib n :=
    if n <= 1
    then n
    else fib (n - 1) + fib (n - 2)

writeln (fib 10)
```
