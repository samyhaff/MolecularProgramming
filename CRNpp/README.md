# Project 2: Molecular Programming Language CRN++

Source code for project 2.

## Requirements and dependencies

* .NET 7.0
* FsCheck (Version 2.16.5)
* FSharp.Configuration (Version 2.0.0)
* Plotly.Net (Version 4.0.0)
* FParsec (Version 1.1.1)

## Running instructions

* The main program is located in the `src/Compiler` folder
* Running simulation for a given program file: `dotnet run <program> <?focussed species eg: a b c>`
* Example programs can be found in `src/Compiler/ProgramExamples`
* For example, simulating the GCD program from the article, showing only the `a` and `b` species: `dotnet run ProgramExamples/example.crn a b`
* Running checks: `dotnet run checks`
* Running module/error/formula examples: `dotnet run example <exampleName>`
* For example, running the pi computation example: `dotnet run example "formula:pi"`

## Available example options

```
module:add
module:subA>B
module:subA<B
module:mul
module:div
module:divBy0
module:sqrt
module:clock"
module:cmp
module:ifGt
module:rxn
reaction
formula:fact5
formula:e
formula:pi
formula:discrete_counter
formula:division
formula:integer_square_root
performance:bigCRN
errors:add
errors:sub
errors:subA>B
errors:subClose
errors:subOne
errors:mul
errors:div
```
