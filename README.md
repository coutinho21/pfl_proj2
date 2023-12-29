# Practical Assignment 2 - 2023/2024 PFL
# Haskell Compiler

> **Group**: T05_G10
> 
> **Members**:
> - Guilherme Correia da Silva Coutinho, up202108872 - 50%
> - Xavier Ribeiro Outeiro, up202108895 - 50%


## Strategy Used

**This assignment was split in two parts:**

### Low-level Assembler

The goal was to implement a low-level machine that receives a set of configurations of the form (c, (e, s)) where c is a list of instructions (or code) to be executed, e is the evaluation stack, and s is the storage. We use the evaluation stack to evaluate arithmetic (composed of integer numbers only, which can positive or negative) and boolean expressions.

The instructions of the machine are: push-n, add, mult, sub, true, false, eq, le,
and, neg, fetch-x, store-x, noop, branch(c1, c2) and loop(c1, c2).

An example of an instruction is:

`
([Push 10, Push 4, Push 3, Sub, Mult], ("-10", ""))
`

To solve this, we defined some data types:
```
data StackVal = IntVal Integer | BoolVal Bool deriving (Show)
type Stack = [StackVal]
type State = [(String, StackVal)]
```
-   **StackVal** represents either an integer or a boolean that is going to be stored temporarily in the stack
-   **Stack** is represented as a list of StackVals
-   **State** is represented as a list of tuples, where each tuple has a string and a StackVal

After defining these data types and also using the provided ones in the file template, we implemented the required functions in the template.





## Conclusions

This work has proven to be a great way to learn both a new language and a different way of thinking, which was new to us. We believe that we have reinforced our prior knowledge and also expanded it.

