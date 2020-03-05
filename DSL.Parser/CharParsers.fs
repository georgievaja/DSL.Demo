namespace DSL.Parser

open FParsec
open WhiteSpaceParsers

module CharParsers =
    let pParenthesisOf<'a> (parser : Parser<'a, _>) : Parser<'a, unit> = 
        pcharws '(' >>. parser .>> pcharws ')'

    let pNormalChar : Parser<char, unit> = 
        satisfy(fun c -> c <> '\\' && c <> '"' && c <> '\'' && c <> '%')

    let pStringAfterString s = 
        strws s >>. manyChars letter

    let pAfterString p s =
        strws s >>. p

    let pLetterBetween str1 str2 = 
        between (strws str1) (strws str2) (manyChars letter)

    let pBetween str1 str2 = 
        between (strws str1) (strws str2) (manyChars pNormalChar)

    let pStringLiteral = 
        pBetween "'" "'"

    let pTrue : Parser<bool, unit> = 
        stringReturn "true" true

    let pFalse : Parser<bool, unit> = 
        stringReturn "false" false

    let pBool : Parser<bool, unit> =
        choicews [
            pTrue;
            pFalse;
        ]
    

