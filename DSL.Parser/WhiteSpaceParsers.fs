namespace DSL.Parser

open FParsec

module WhiteSpaceParsers = 
    let strws s     = spaces >>. pstring s .>> spaces
    let pcharws ch  = spaces >>. pchar ch .>> spaces
    let choicews ps = spaces >>. choice ps .>> spaces
    let pfloatws : Parser<float, unit> = 
        spaces >>. pfloat .>> spaces

