module Command where

import Boolean.Expression

data Command
    = PassCommand
    | QuitCommand
    | HelpCommand
    | DefineCommand   Name Function
    | UndefineCommand Name
    | EvalCommand Function
    | ShowCommand ShowForm Name
    | ClassCommand Name
    | CompleteCommand [Name]
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
