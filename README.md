
## Compiler for Pascal-0

This is a project for the curricular unit of Compilers.

## Running the project

There are some custom examples in the folders named examples and original examples.
To run the examples, we simply run the command:
```shell
./run
```

The compiler creates an AST (abstract syntax tree), runs a type-checker to verify the integrity, and then converts the language to MIPS instructions that can be ran on a MIPS simulator, such as [Mars](https://courses.missouristate.edu/kenvollmar/mars/).

The compiler was written using the Haskell programming language.

----
Hugo Cardante && Marco Gonçalves
