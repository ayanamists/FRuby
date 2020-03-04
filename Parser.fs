namespace FRuby

    [<AutoOpen>]
    module Parser =
        open System
        open FSharp.Collections
        open FParsec
        open Ast

        let pFRWhite = spaces
        let pFRWhite1 = spaces1
        let pFRSpace : Parser<unit,unit> = skipMany (pstring " ")
        let pFRSpace1: Parser<unit,unit>  = skipMany1 (pstring " ")
        let pFRNewLine1: Parser<unit, unit> = skipNewline
        let pFRTerm:Parser<unit, unit> = pFRNewLine1 <|> (pstring ";" |>> ignore)
        let pFRTerms:Parser<unit, unit> = many1 pFRTerm |>> ignore

        let pFRStrNode(a): Parser<FRAstNode, unit> = pstring a |>> FRString |>> Terminal
        let pFRKeyNode(a): Parser<FRAstNode, unit> = pstring a |>> FRKeyWord |>> Terminal
    
    // Debug helper
        let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
            fun stream ->
                RealPrint "%A: Entering %s\n" stream.Position label
                let reply = p stream
                RealPrint "%A: Leaving %s (%A)\n" stream.Position label reply.Status
                reply

    // Parse Identifiers
        let ListToString (a:list<char>) =
            let str = Text.StringBuilder()
            for i in a do 
                str.Append(i) |> ignore
            str.ToString()

        let pFRIdentifierRaw: Parser<_, unit> = 
            ((pipe2 (asciiLetter <|> pchar '_') 
                (many (asciiLetter <|> pchar '_' <|> digit)) 
                (fun a b -> a.ToString() + (ListToString b)))) 

        let pFRIdentifier:Parser<_, unit> = 
            pFRIdentifierRaw |>> FRIdentifier

        let pFRIdentifierNode:Parser<_, unit> = pFRIdentifier |>> Terminal <!> "Identifier"

    // Parse Varnames
        let inline GetValue1(x)=
            match x with
            | FRIdentifier(t) -> t
            | _ -> ""
        let addCharAndString (a:char)  b =
            a.ToString() + b

        let pFRVarGlobal:Parser<_, unit> = 
            (pipe2 (pchar '$') (pFRIdentifier |>> GetValue1) 
                (fun a b ->(addCharAndString a b))) |>> FRVarName.Global
             
        let pFRVarInstance =
            (pipe2 (pchar '@') (pFRIdentifier |>> GetValue1) 
                (fun a b ->(addCharAndString a b))) |>> FRVarName.Instance

        let pFRVarNormal: Parser<_, unit> = 
            ((pipe2 (asciiLower <|> pchar '_') 
                (many (asciiLetter <|> pchar '_' <|> digit)) 
                (fun a b -> a.ToString() + (ListToString b)))) |>> FRVarName.Normal

        let pFRVarName = 
          (pFRVarGlobal |>> FRVarName) <|> (pFRVarInstance |>> FRVarName) <|>
            (pFRVarNormal |>> FRVarName)

    // We handle key words in 'passitive mode',
    // that is, to check if an identifier is a key word,
    // and fail when it't a key word
        let FRKeyWordSet =
            System.Collections.Generic.HashSet<_>(
                [|"end"; "else"; "elsif"; "if"; "while" ; "class" ; "module"|]
            )

        let resultSatisfies predicate msg (p: Parser<_,_>) : Parser<_,_> =
            let error = messageError msg
            fun stream ->
              let state = stream.State
              let reply = p stream
              if reply.Status = Ok && (predicate reply.Result = true) then reply
              else
                  stream.BacktrackTo(state) // backtrack to beginning
                  Reply(Error, error)

        let pFRVarNameNode = 
            (resultSatisfies (fun t -> not(FRKeyWordSet.Contains((GetFRVarNameValue t)))) 
                            "Is a key word!" 
                            pFRVarName) |>> Terminal

    // Parse Interger
        let pFRInterger:Parser<FRTerminal, unit> = 
            (pint64 .>>? (notFollowedBy (pchar '.' .>> digit)) |>> FRInt)

        let pFRIntNode = pFRInterger |>> Terminal
    // Parse Float
        let pFRFloat:Parser<FRTerminal, unit> = (pfloat |>> FRFloat)
        
        let pFRFloatNode = pFRFloat |>> Terminal
    // Parse String Literal
        let str = pstring

        let stringLiteral =
            let escape =  anyOf "\"\\/bfnrt"
                          |>> function
                              | 'b' -> "\b"
                              | 'f' -> "\u000C"
                              | 'n' -> "\n"
                              | 'r' -> "\r"
                              | 't' -> "\t"
                              | c   -> string c // every other char is mapped to itself
        
            let unicodeEscape =
            /// converts a hex char ([0-9a-fA-F]) to its integer number (0-15)
                let hex2int c = (int c &&& 15) + (int c >>> 6)*9
        
                str "u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
                    (hex2int h3)*4096 + (hex2int h2)*256 + (hex2int h1)*16 + hex2int h0
                    |> char |> string
                )
        
            let escapedCharSnippet = str "\\" >>. (escape <|> unicodeEscape)
            let normalCharSnippet  = manySatisfy (fun c -> c <> '"' && c <> '\\')
        
            between (str "\"") (str "\"")
                    (stringsSepBy normalCharSnippet escapedCharSnippet)
        
        let pFRString:Parser<FRTerminal, unit> = stringLiteral |>> FRString
        let pFRStringNode = pFRString |>> Terminal

    // Create Stmt Ref
        let pFRStmt, pFRStmtRef = createParserForwardedToRef<FRAstNode, unit>()
    // Create Expr Ref
        let pFRExpr, pFRExprRef = createParserForwardedToRef<FRAstNode, unit>()

    // Create Arg Ref
        let pFRArg, pFRArgRef = createParserForwardedToRef<FRAstNode, unit>()

    // Create Primary Ref
        let pFRPrimary, pFRPrimaryRef = createParserForwardedToRef<FRAstNode, unit>()

    // Create CompStmt 
        let backTraceToBegin (p: Parser<_,_>) : Parser<_,_> = 
            fun stream ->
                let state = stream.State
                let reply = p stream
                if reply.Status = Ok then 
                    stream.BacktrackTo(state)
                    reply
                else
                    Reply(Error, messageError "SyntaxError: Not Complete Block")
            
        let pFRCompStmt endWith = 
            pFRStmt .>>. (manyTill (pFRTerms >>. pFRStmt) ( opt (pFRWhite) >>? (backTraceToBegin endWith))) 
                |>> (fun (a, b) -> a :: b) |>> FRFormCompStmt <!> "CompStmt"

    // Parse lhs
        let pFRLhs = 
            (pFRVarNormal |>> FRVarName) |>> Terminal

    // Parse if Primary
        let pFRIfBetween = pFRTerm <|> (pstring "then" |>> ignore) 
        let pFRIfEndKey = attempt(pstring "end") <|> attempt (pstring "else") <|> attempt (pstring "elsif") <!> "if end"
        let pFRIfElement = pipe4 pFRExpr pFRSpace pFRIfBetween (pFRCompStmt pFRIfEndKey) (fun a b c d -> (a, d))
        let pFRIfBegin = pFRWhite >>. pFRKeyNode "if" >>? pFRWhite1 >>. pFRIfElement
        let pFRElsif = pstring "elsif" >>? pFRWhite1 >>. pFRIfElement <!> "elsif"
        let pFRIfMiddle = many pFRElsif
        let pFRIfEnd = opt (pstring "else" >>? pFRWhite1 >>. pFRCompStmt (pstring "end")) .>> pstring "end"
        let pFRIfPrimary = (pipe5 pFRIfBegin pFRWhite
            pFRIfMiddle pFRWhite pFRIfEnd (fun a b c d e -> (a, c, e))) |>> FRFormIfPrimary <!> "if"
            
    // Parse while Primary
        let pFRWhileBetween = pFRNewLine1 <|> (pstring ";" |>> ignore) <|> (pstring "do" |>> ignore)
        let pFRWhileBody = 
            pipe5 pFRExpr pFRSpace pFRWhileBetween 
                pFRWhite (pFRCompStmt (pstring "end")) (fun a b c d e -> (a, e))
        let pFRWhilePrimary = 
            pstring "while" >>? pFRWhite1 >>. pFRWhileBody .>> pstring "end" |>> FRFormWhilePrimary <!> "while"
            
    // Parse class Primary
        let pipe6 p1 p2 p3 p4 p5 p6 f =
            pipe4 p1 p2 p3 (tuple3 p4 p5 p6)
                  (fun x1 x2 x3 (x4, x5, x6) -> f x1 x2 x3 x4 x5 x6)

        let pFRClassBegin = (pstring "class") >>. pFRWhite1
        let pFRClassInherit = pFRSpace >>. opt ((pstring "<") >>. pFRWhite >>. pFRIdentifierNode)
        let pFRClassPrimary = 
            attempt ((pipe6 pFRClassBegin pFRIdentifierNode 
                pFRClassInherit pFRWhite1 (pFRCompStmt (pstring "end")) (pstring "end")
                (fun a b c d e f -> (b, c, e)))|>> FRFormClassPrimary) <!> "class"

    // Parse Method 
        let pipe7 p1 p2 p3 p4 p5 p6 p7 f =
            pipe4 p1 p2 p3 (tuple4 p4 p5 p6 p7)
                 (fun x1 x2 x3 (x4, x5, x6, x7) -> f x1 x2 x3 x4 x5 x6 x7)

    // Parse Module Def
        let pFRModulePrimary = 
            attempt ((pipe6 (pstring "module") pFRSpace1 pFRIdentifierNode 
                pFRWhite (pFRCompStmt (pstring "end")) (pstring "end") 
                (fun a b c d e f -> c , e))|>> FRFormModulePrimary) <!> "module"
    
    // Parse method invocation

    // For FParsec don't support left recursion, 
    // we have to us Terminal to exclude Binary Operation from FPExpr 
        
    // Parse Arg
        let pFRArgRaw = OperatorPrecedenceParser<FRAstNode, unit, unit>()
        
        let pFRAddInfix prefix precedence associativity =
            let op = InfixOperator(prefix, getPosition >>. pFRSpace,
                         precedence, associativity, (),
                         fun remOpChars expr1 expr2 ->
                             FRFormInfixArg(prefix, expr1, expr2))
            pFRArgRaw.AddOperator(op)

        pFRArgRaw.TermParser <- pFRSpace >>. pFRPrimary .>> pFRSpace
    
        /// Add some Binary Operators
        pFRAddInfix ">=" 11 Associativity.None
        pFRAddInfix ">" 11 Associativity.None
        pFRAddInfix "<=" 11 Associativity.None
        pFRAddInfix "<" 11 Associativity.None
        pFRAddInfix "==" 12 Associativity.None
        pFRAddInfix "!=" 12 Associativity.None
        pFRAddInfix "+" 13 Associativity.Left
        pFRAddInfix "-" 13 Associativity.Left
        pFRAddInfix "*" 14 Associativity.Left
        pFRAddInfix "/" 14 Associativity.Left
        pFRAddInfix "%" 14 Associativity.Left
        
        /// Parse 'lhs = arg'
        let pFRAssignPrimary = 
           attempt (pipe5 pFRLhs pFRWhite (pstring "=") pFRWhite pFRArg 
                (fun a b c d e -> (a, e)) ) |>> FRFormAssignPrimary <!> "Assign"
            
        do pFRArgRef := pFRArgRaw.ExpressionParser

    // Parse Stmt
        do pFRStmtRef := pFRExpr <!> "Stmt"
        do pFRExprRef := pFRArg <!> "expr"

    // Parse Primary
        let pFRParenPrimary = pstring "(" >>. pFRPrimary .>> pstring ")"
        do pFRPrimaryRef := pFRParenPrimary <|> pFRIntNode <|> pFRFloatNode <|> pFRStringNode <|> pFRAssignPrimary
            <|> pFRIfPrimary <|> pFRWhilePrimary <|> pFRClassPrimary <|> pFRModulePrimary <|> pFRVarNameNode

    // Parse all
        let pFR:Parser<FRAstNode, unit> = (pFRCompStmt eof) .>> eof

                

