namespace FRuby

[<AutoOpenAttribute>]
module ParseExpection = 
    exception SyntaxError of string
    exception TypeError of string

