namespace DSL.Parser

open WhiteSpaceParsers
open FParsec
open AbstractSyntaxTree
open CharParsers

module IdentifierParsers =
    
    let pTeamVictoriesParameter : Parser<TeamParameterValue, unit> =
        strws "victories"
        |>> fun _ -> Victories

    let pTeamPolePositionsParameter : Parser<TeamParameterValue, unit> =
        strws "polepositions"
        |>> fun _ -> PolePositions

    let pTeamParameterValue : Parser<TeamParameterValue, unit> =
        choicews [
            pTeamVictoriesParameter;
            pTeamPolePositionsParameter;
        ]

    let pTeamParameter : Parser<IdentifierParameter, unit> =
        pipe2   (pLetterBetween "[" "]:" 
                |>> TeamKey)
                (pTeamParameterValue)
                (fun id param -> TeamParameter (id, param))

    let pTeamIdentifier : Parser<IdentifierParameter, unit> =
        pLetterBetween "[" "]"
        |>> TeamKey
        |>> TeamIdentifier

    let pIdentifierParameter : Parser<IdentifierParameter, unit> = 
        choicews [
            attempt pTeamParameter;
            pTeamIdentifier;
        ]

    let pTeam : Parser<IdentifierParameter, unit> = 
        pAfterString pIdentifierParameter "team"

    let pIdentifier : Parser<IdentifierParameter, unit> =
        choicews [
            pTeam;
        ]

    let pIdentifierExpression : Parser<Expression, unit> =
        pIdentifier
        |>> IdentifierParameter