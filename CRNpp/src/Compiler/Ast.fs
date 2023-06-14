module Ast

type species = string

type Command =
   | Load of species * species
   // Arithmetic
   | Add of species * species * species
   | Sub of species * species * species
   | Mul of species * species * species
   | Div of species * species * species
   | Sqrt of species * species
   // Logic
   | Cmp of species * species
   | Ifgt of Command list
   | Ifge of Command list
   | Iflt of Command list
   | Ifle of Command list
   | Ifeq of Command list
   // TODO add rxn

type Root =
    | Conc of species * float
    | Step of Command list

type Crn = Root list
