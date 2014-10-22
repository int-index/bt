module Command where

import Boolean.Expression

data Command
    = PassCommand
    | QuitCommand
    | HelpCommand
    | DefineCommand   String Function
    | UndefineCommand String
    | EvalCommand Function
    | ShowCommand ShowForm String
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
