namespace DSL.Teams
open System

module Team =

    module PublicTypes =

        type UnvalidatedParameter =
            {
                Name: string
                Value: string
            }

        type UnvalidatedTeam = 
            {
                Name: string
                Parameters: UnvalidatedParameter list
            }

        type UnvalidatedTeams = private UnvalidatedTeams of UnvalidatedTeam list

        module UnvalidatedTeams =
            let create (value: UnvalidatedTeam list) =
                UnvalidatedTeams value

            let value (UnvalidatedTeams teams) =
                teams

    module Types =

        type ParameterName = private ParameterName of string
    
        type ParameterValue = private ParameterValue of string

        type TeamName = private TeamName of string
    
        type Teams = private Teams of Map<TeamName, Map<ParameterName, ParameterValue>>

        module TeamName =
            let create (name: string) =
                TeamName (name.ToUpper())
        
            let parameterNameKey =
                "TeamName"

        module ParameterName =
            let create (name: string) =
                ParameterName (name.ToUpper())

        module ParameterValue =
            let create (v: string) =
                ParameterValue v

            let value (ParameterValue v) =
                v

        module Teams = 
            let create (teams: Map<TeamName, Map<ParameterName, ParameterValue>>) =
                Teams teams

            let value (Teams teams) =
                teams

    module Logs =
        open Types

        type FunctionUsedValueType = 
            | FunctionUsedParameter of (ParameterName * ParameterValue)
            | FunctionUsedKey of TeamName

        type FunctionUsedValues = FunctionUsedValueType list
        
        type FunctionFailureType =
            | UnexpectedParameterType

        type FunctionFailure = 
            | Unexpected of FunctionFailureType

    module Monads =
        open Types
        open Logs

        type OperationFunctionResult<'TSuccess, 'TFailure> = 
            | Success of 'TSuccess * FunctionUsedValues
            | Failure of 'TFailure * FunctionUsedValues
    
        let map f res =
            match res with      
                |Success (x, log) -> Success (f x, log)
                |Failure (x, log) -> Failure (x, log)
    
        let (<!>) x f = map f x

        let bind exprFunction xOpt  = 
            match xOpt with
            | Success (r, log) -> match exprFunction r with
                                    | Success (r2, log2) -> Success (r2, log @ log2)
                                    | Failure (x, log2) -> Failure (x, log @ log2)
            | Failure (x, log) -> Failure (x, log)

        let (>>=) f x = bind f x

        let (>=>) f1 f2 arg = 
            match f1 arg with
            | Success (x, log) -> match f2 x with      
                                    |Success (x2, log2) -> Success (x2, (log @ log2))
                                    |Failure (x2, log2) -> Failure (x2, (log @ log2))
            | Failure (x, log) -> Failure (x, log)

    module FunctionTypes =
        open PublicTypes
        open Types
        open Monads
        open Logs

        type ValidateTeams =
           UnvalidatedTeams // input
            -> OperationFunctionResult<Teams, FunctionFailure> // output

        type CheckTeamExists =
           TeamName // input
            -> Teams  // input
            -> OperationFunctionResult<bool, FunctionFailure> // output

        type CheckTeamExistsWorkflow =
           ValidateTeams // dependency
            -> CheckTeamExists // dependency
            -> TeamName // input
            -> UnvalidatedTeams // input
            -> OperationFunctionResult<bool, FunctionFailure> // output

