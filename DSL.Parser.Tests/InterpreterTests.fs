namespace DSL.Parser.Tests

open Xunit.Abstractions
open DSL.Parser.ParserTypes.Monads
open DSL.Interpreter.Interpreter
open DSL.Interpreter.InterpreterTypes.Monads
open DSL.Teams.Team.PublicTypes
open DSL.Teams.Team.Monads
open DSL.Teams.Team.Logs
open DSL.Teams.Team.Types
open DSL.Interpreter.InterpreterTypes.Types

module InterpreterTests =
    open Xunit
    open DSL.Parser.Parser

    let testTeamData = UnvalidatedTeams.create [
        {
            Name = "Ferrari"
            Parameters = [
                {
                    Name = "victories"
                    Value = "239"
                };
                {
                    Name = "polepositions"
                    Value = "222"
                }
            ]
        };
        {
            Name = "Williams"
            Parameters = [
                {
                    Name = "victories"
                    Value = "114"
                };
                {
                    Name = "polepositions"
                    Value = "129"
                }
            ]
        }
    ]

    type InterpreterTests(output: ITestOutputHelper) =
        
        [<Theory>]
        [<InlineData("query EXISTS team[Williams]")>]
        member __.``Existence interpretation returns true`` data =
            let result = parse data

            match result with
            | (ParsingResult.Success ast) -> 
                match interpret ast with
                    | InterpreterResult.Success func -> 
                        match func testTeamData with
                            | OperationFunctionResult.Success (s, logs) ->  
                                Assert.Equal(true, s)
                                Assert.Equal<FunctionUsedValues>([FunctionUsedKey (TeamName.create "Williams")], logs)
                            |_ -> 
                                failwith "function operation failed"
                    | _ -> 
                        failwith "interpreting was unsuccessful"
            | _ -> 
                failwith "parsing was unsuccessful"

        [<Theory>]
        [<InlineData("query EXISTS team[McLaren]")>]
        member __.``Existence interpretation returns false`` data =
            let result = parse data

            match result with
            | (ParsingResult.Success ast) -> 
                match interpret ast with
                    | InterpreterResult.Success func -> 
                        match func testTeamData with
                            | OperationFunctionResult.Success (s, logs) ->  
                                Assert.Equal(false, s)
                                Assert.Equal<FunctionUsedValues>([FunctionUsedKey (TeamName.create "McLaren")], logs)
                            |_ -> 
                                failwith "function operation failed"
                    | _ -> 
                        failwith "interpreting was unsuccessful"
            | _ -> 
                failwith "parsing was unsuccessful"

        [<Theory>]
        [<InlineData("query EXISTS 2")>]
        member __.``Existence interpretation fails`` data =
            let result = parse data

            match result with
            | (ParsingResult.Success ast) -> 
                match interpret ast with
                    | InterpreterResult.Failure f -> Assert.Equal(InterpreterFailure.Unexpected UnsupportedExistsExpression, f)
                    | _ -> 
                        failwith "interpreting was successful"
            | _ -> 
                failwith "parsing was unsuccessful"
