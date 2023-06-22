# Project 2: Molecular Programming Language CRN++

Source code for project 2.

## Requirements and dependencies

* .NET 7.0
* FsCheck (Version 2.16.5)
* FSharp.Configuration (Version 2.0.0)
* Plotly.Net (Version 4.0.0)
* FParsec (Version 1.1.1)

## Running instructions

* Main program is located in the `Compiler` folder
* Running simulation for a given program file: `dotnet run - <filename> <?focussed species eg: a b c>`
* Example programs can be found in `Compiler/ProgramExamples`
* For example, simulating the GCD program from the article, showing only the `a` and `b` species: `dotnet run - ProgramExamples/example.crn`
* Running checks: `dotnet run checks`
