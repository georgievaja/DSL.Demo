namespace DSL.Parser.Tests

open Xunit.Abstractions
open Xunit.Sdk
open DSL.Parser.ParserTypes.Monads
open DSL.Parser.ParserTypes.Types
open DSL.Parser.AbstractSyntaxTree

module ParserTests =

    open System
    open Xunit
    open FParsec
    open DSL.Parser.OperationParsers
    open DSL.Parser.OperatorParsers
    open DSL.Parser.Parser

    type ParserTests(output:ITestOutputHelper) =

        [<Theory>]
        [<InlineData("2 / 65 - 5 +6")>]
        [<InlineData("(2 / 65 - 5 +6)")>]
        member __.``Arithmetic operator passes`` data =
            let result = run pOperator data

            match result with
            | (Success _) -> Assert.True(true)
            | _ -> Assert.True(false)

        [<Theory>]
        [<InlineData("query EXISTS team[Ferrari]")>]
        member __.``Predicate operator passes`` data =
            let result = parse data

            match result with
            | (ParsingResult.Success _) -> Assert.True(true)
            | _ -> Assert.True(false)

        [<Theory>]
        [<InlineData("2 / 65 - 5 +6=25")>]
        [<InlineData("team[Ferrari]:victories=team[Ferrari]:victories")>]
        [<InlineData("team[Ferrari]:victories<>team[Ferrari]:victories")>]
        [<InlineData("team[Ferrari]:victories>team[Ferrari]:victories")>]
        [<InlineData("team[Ferrari]:victories<team[Ferrari]:victories")>]
        [<InlineData("team[Ferrari]:victories<=team[Ferrari]:victories+5")>]
        [<InlineData("team[Ferrari]:victories>=team[Ferrari]:victories%2")>]
        member __.``Comparision operator passes`` data =
            let result = run pOperator data

            match result with
            | (Success _) -> Assert.True(true)
            | _ -> Assert.True(false)

        [<Theory>]
        [<InlineData("2 * 6 + 5", "(2*6)+5")>]
        [<InlineData("2 * 6 + 5<>5", "((2*6)+5)<>5")>]
        [<InlineData("NOT 2 * 6 + 5<>5", "NOT(((2*6)+5)<>5)")>]
        [<InlineData("NOT 2 * 6 + 5<>5 AND 2-4/15>5", "(NOT(((2*6)+5)<>5)) AND (2-(4/15)>5)")>]
        [<InlineData("NOT 2 * 6 + 5<>5 AND 2-4/15>5 OR team[Ferrari]:victories%2>=team[Ferrari]:victories*6-2", "((NOT(((2*6)+5)<>5)) AND (2-(4/15)>5)) OR ((team[Ferrari]:victories%2)>=(team[Ferrari]:victories*6)-2)")>]
        [<InlineData("2 / 65 - 5 +6/3*6", "(2 / 65) - 5 +(6/3*6)")>]
        [<InlineData("2 / 65 - 5", "(2/65)-5")>]
        [<InlineData("team[Ferrari]:victories * 6 + 5", "(team[Ferrari]:victories*6)+5")>]
        [<InlineData("team[Ferrari]:victories * team[Ferrari]:victories + team[Ferrari]:polepositions", "(team[Ferrari]:victories * team[Ferrari]:victories) + team[Ferrari]:polepositions")>]
        member __.``Operator precedence equality passes`` data parenthesisData =
            let result = run pOperator data
            let parenthesis = run pOperator parenthesisData

            match (result, parenthesis) with
            | ((Success (s1, _, _)), (Success (s2, _, _))) ->   output.WriteLine(sprintf "RESULT WITHOUT PARENTHESIS: %A" s1)
                                                                output.WriteLine(sprintf "RESULT WITH PARENTHESIS: %A" s2)
                                                                Assert.Equal(s1, s2)
            | _ -> failwith "result was not successful!" 

        [<Theory>]
        [<InlineData("query NOT 2 * 6 + 5<>5 AND 2-4/15>5 OR team[Ferrari]:victories%2>=team.victories*6-2")>]
        member __.``Syntactic error customized properly`` data =
            let result = parse data
        
            match result with
            | (ParsingResult.Success _) -> Assert.True(false)
            | (ParsingResult.Failure f) ->              
                output.WriteLine(sprintf "%A" f)

                match f with
                | Syntactic _ -> Assert.True(true)
                | Semantic _ -> Assert.True(false)

        [<Theory>]
        [<InlineData("query 2 * 6")>]
        member __.``Semantic error customized properly`` data =
            let result = parse data
        
            match result with
            | (ParsingResult.Success _) -> Assert.True(false)
            | (ParsingResult.Failure f) ->              
                output.WriteLine(sprintf "%A" f)

                match f with
                | Semantic _ -> Assert.True(true)
                | Syntactic _ -> Assert.True(false)

        [<Theory>]
        [<InlineData("query 2 * 6 = 15")>]
        member __.``AST represented: query 2 * 6 = 15`` data =
            let result = parse data
        
            let expectedAst = 
                Query 
                    (ComparisionOperator
                        (EqualTo 
                            (ArithmeticOperator
                                (Multiply
                                    ((DataType 
                                        (Numeric 
                                            (Float 2.0))),
                                    (DataType 
                                        (Numeric
                                            (Float 6.0))))),
                            (DataType 
                                (Numeric
                                    (Float 15.0))))))

            match result with
            | (ParsingResult.Success s) -> Assert.Equal(expectedAst, s)
            | (ParsingResult.Failure f) ->              
                output.WriteLine(sprintf "%A" f)

                match f with
                | Semantic _ -> Assert.True(true)
                | Syntactic _ -> Assert.True(false)