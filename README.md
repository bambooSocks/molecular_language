# CRN++ interpreter and compiler

This project is a part of 02257 Applied Functional Programming course at DTU. The main goal of the project is to develop a tool that is capable of parsing, checking and running of CRN++ programs. Further the tool has to be able to compile the code into chemical reaction networks and simulate them. The project is based on .NET 6 and is implemented in F#. Therefore it is required to have the relevant .NET SDK installed.

## How to run it

### The main app
In order to run the interpreter navigate to `src/App` and run `dotnet run interpreter`. This should run the parser, type checker as well as interpreter, showing the result in a web browser as the graph of concentrations over time. Similarly in order to run the compiler and simulator run `dotnet run simulator`. This should ...(Christopher please update this)

### The tests
In order to run the tests navigate to `src/Tests` and run `dotnet test`. This executes all property tests for the parsers, semantic rules, etc. In addition it also runs some unit tests which mostly test the interpreter and type checker.
