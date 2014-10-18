module Command where

import Boolean.Expression

data Command
    = PassCommand
    | QuitCommand
    | HelpCommand
    | DefineCommand   String Function
    | UndefineCommand String
    | ShowCommand ShowForm String
    | CompareCommand String String
    | ClassCommand String
    | CompleteCommand [String]
    | ListCommand
    | CleanCommand
    | ResetCommand
    | OperatorsCommand

data ShowForm
    = ShowDefault
    | ShowCNF
    | ShowDNF
    | ShowANF
    | ShowTable
