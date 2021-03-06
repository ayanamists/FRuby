﻿namespace FRuby

    [<AutoOpen>]
    module Parser =
        open System
        open FSharp.Collections
        open FParsec
        open Ast

       // Debug helper
        let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
           fun stream ->
               RealPrint "%A: Entering %s\n" stream.Position label
               let reply = p stream
               RealPrint "%A: Leaving %s (%A)\n" stream.Position label reply.Status
               reply
 
        let pFRWhite = spaces
        let pFRWhite1 = spaces1
        let pFRSpace : Parser<unit,unit> = skipMany (pstring " ")
        let pFRSpace1: Parser<unit,unit>  = skipMany1 (pstring " ")
        let pFRNewLine1: Parser<unit, unit> = skipNewline
        let pFRTerm:Parser<unit, unit> = pFRNewLine1 <|> (pstring ";" |>> ignore) <!> "Term"
        let pFRTerms:Parser<unit, unit> = many1 pFRTerm |>> ignore <!> "Terms"

        let pFRStrNode(a): Parser<FRAstNode, unit> = pstring a |>> FRString |>> Terminal
        let pFRKeyNode(a): Parser<FRAstNode, unit> = pstring a |>> FRKeyWord |>> Terminal
    
    
    // Parse Identifiers
        let ListToString (a:list<char>) =
            let str = Text.StringBuilder()
            for i in a do 
                str.Append(i) |> ignore
            str.ToString()
        
        let pFRIdentifierStartWith x = 
            ((pipe2 x 
                (many (asciiLetter <|> pchar '_' <|> digit)) 
                (fun a b -> a.ToString() + (ListToString b))))
        let pFRIdentifierRaw: Parser<_, unit> = 
            pFRIdentifierStartWith (asciiLetter <|> pchar '_')
        
        /// Nor means 'normal' which is not a constant
        let pFRIdentifierNor: Parser<_, unit> = 
            pFRIdentifierStartWith (asciiLower <|> pchar '_')
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
            (pipe2 (pchar '$') (pFRIdentifierStartWith (asciiLetter <|> digit <|> pchar '_')) 
                (fun a b ->(addCharAndString a b))) |>> FRVarName.Global
             
        let pFRVarInstance =
            (pipe2 (pchar '@') (pFRIdentifier |>> GetValue1) 
                (fun a b ->(addCharAndString a b))) |>> FRVarName.Instance

        let pFRVarNormal: Parser<_, unit> = pFRIdentifierNor |>> FRVarName.Normal

        let pFRVarName = 
          (pFRVarGlobal |>> FRVarName) <|> (pFRVarInstance |>> FRVarName) <|>
            (pFRVarNormal |>> FRVarName)

    // We handle key words in 'passitive mode',
    // that is, to check if an identifier is a key word,
    // and fail when it't a key word
        let FRKeyWordSet =
            System.Collections.Generic.HashSet<_>(
                [|"end"; "else"; "elsif"; "if"; "while" ; "class" ; "module"; "def"|]
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
    // Parse Constant
        let pFRConstant = pFRIdentifierStartWith (asciiUpper) |>> FRConstant |>> Terminal

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
            
        /// 使用此元素的约定：在外部显式消除第一个Stmt前的空字符，
        /// endWith是结束符，仍然需要在外部显式消除
        let pFRCompStmt endWith = 
            opt (pFRStmt .>>. (manyTill (pFRTerms >>. pFRWhite >>. pFRStmt) 
                (opt (pFRWhite) >>? (backTraceToBegin endWith)))) |>> 
                    (fun x -> match x with 
                        | Some(a, b) -> a::b
                        | _ -> []) |>> FRFormCompStmt <!> "CompStmt"

    // Parse lhs  
        let pFRLhs = pFRVarNameNode

    // Parse if Primary
        let pFRIfBetween = pFRTerm <|> (pstring "then" |>> ignore) 
        let pFRIfEndKey = attempt(pstring "end") <|> attempt (pstring "else") <|> attempt (pstring "elsif") <!> "if end"
        let pFRIfElement = pipe4 pFRExpr pFRSpace pFRIfBetween (pFRCompStmt pFRIfEndKey) (fun a b c d -> (a, d))
        let pFRIfBegin = pFRKeyNode "if" >>? pFRWhite1 >>. pFRIfElement
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
            attempt ((pipe6 pFRClassBegin pFRConstant
                pFRClassInherit pFRWhite1 (pFRCompStmt (pstring "end")) 
                    (pstring "end") (fun a b c d e f -> (b, c, e)))|>> FRFormClassPrimary) <!> "class"

    // Parse Method Def Primary
        let pipe7 p1 p2 p3 p4 p5 p6 p7 f =
            pipe4 p1 p2 p3 (tuple4 p4 p5 p6 p7)
                 (fun x1 x2 x3 (x4, x5, x6, x7) -> f x1 x2 x3 x4 x5 x6 x7)

        let pFRNorArgu = pFRIdentifierNor |>> FRIdentifier |>> Terminal
        let pFRBlockArgu = 
            pstring "&" .>>. pFRIdentifierRaw |>> 
                (fun (a,b) -> a + b) |>> FRIdentifier |>> Terminal
        let pFRRestArgu = 
            pstring "*" .>>. pFRIdentifierRaw |>> 
                (fun (a,b) -> a + b) |>> FRIdentifier |>> Terminal
        let pFRAssignArgu = 
            pipe5 pFRNorArgu pFRWhite (pstring "=") pFRWhite pFRArg
                (fun a b c d e -> (c, a, e)) |>> FRFormInfixArg
 
        let pFRArguItem = 
            attempt(pFRAssignArgu) <|> pFRBlockArgu <|> pFRRestArgu <|> pFRNorArgu <!> "ArguItem"

        let pFRArgus = sepBy pFRArguItem (attempt(pFRWhite >>. pstring "," .>> pFRWhite)) <!> "Argus"
        let pFRArguList = 
            (pstring "(" >>. pFRArgus .>> pstring ")" <|>
                (pFRArgus .>> pFRTerm)) .>> pFRWhite |>> FRFormArgList <!> "ArguList"

        let pFRMethodNameNode = 
            ((pipe2 (asciiLower <|> pchar '_')
                (many (asciiLetter <|> pchar '_' <|> pchar '?' <|> pchar '!' <|> digit <|> pchar '=')) 
                (fun a b -> a.ToString() + (ListToString b)))) |>> FRIdentifier |>> Terminal
       
        let pFRMethodDefPrimary = 
            pipe7 (pstring "def") pFRWhite1 pFRMethodNameNode  
                pFRSpace pFRArguList  (pFRCompStmt (pstring "end")) (pstring "end")
                    (fun a b c d e f g -> (c, e, f)) |>> FRFormMethodDefPrimary

    // Parse Module Def Primary
        let pFRModulePrimary = 
            attempt ((pipe6 (pstring "module") pFRSpace1 pFRConstant 
                pFRWhite (pFRCompStmt (pstring "end")) (pstring "end") 
                (fun a b c d e f -> c , e))|>> FRFormModulePrimary) <!> "module"
    
    

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
        let pFRStmtRaw = pFRExpr
        do pFRStmtRef := getPosition .>>. pFRStmtRaw  |>> FRFormStmt <!> "Stmt"
        do pFRExprRef := pFRArg <!> "expr"

    // Parse Method Invocation Cell Primary
    // Method Invocation Cell 意为 没有指定接收者的函数调用
        let pFRArgValue = pFRArg
        let pFRArgListRaw = sepBy pFRArg (attempt(pFRWhite >>. pstring "," .>> pFRWhite))
        let pFRArgList = pstring "(" >>. pFRArgListRaw .>> pstring ")" |>> FRFormArgList <!> "ArgList"
        let pFRBlockVarItem = pFRArguItem
        let pFRBlockVarRaw = sepBy pFRBlockVarItem (attempt(pFRSpace >>. pstring "," .>> pFRSpace)) |>> FRFormArgList
        let pFRBlockVar = pstring "|" >>. pFRBlockVarRaw .>> pstring "|"
        let pFRBlockRaw endWith = 
            pipe5 pFRWhite pFRBlockVar pFRWhite (pFRCompStmt endWith) pFRWhite 
                (fun a b c d e -> (b, d)) |>> FRFormBlock
        let pFRParenBlock = pstring "{" >>. pFRBlockRaw (pstring "}") .>> pstring "}"
        let pFRDoBlock = pstring "do" >>. pFRBlockRaw (pstring "end") .>> pstring "end"
        let pFRBlock = pFRParenBlock <|> pFRDoBlock
        let pFRMethodCell = 
            pFRMethodNameNode .>>.? pFRArgList .>>. pFRSpace .>>. (opt pFRBlock) 
                |>> (fun(((a,b),c),d)-> (a, b, d)) |>> FRFormMethodCell<!> "method invocation cell"

    // Parse Primary
        let pFRParenPrimary = pstring "(" >>. (pFRCompStmt (pstring ")"))  .>> pstring ")"

        /// NormalPrimary means primary which is not a dot-style method invocation
        /// NormalPrimary 意为不含 点式method Invocation 的 Primary
        let pFRNormalPrimary = 
            pFRParenPrimary <|> pFRIntNode <|> pFRFloatNode <|> pFRStringNode <|> pFRAssignPrimary <|> 
            pFRConstant <|> pFRIfPrimary <|> pFRMethodCell <|> pFRVarNameNode <|>
            pFRWhilePrimary <|> pFRClassPrimary <|> pFRModulePrimary <|> pFRMethodDefPrimary

    // Parse method invocation Primary
        let pFRMethodInvocationRaw = OperatorPrecedenceParser<FRAstNode, unit, unit>()
        let dot = 
            InfixOperator(".", pFRWhite, 1, Associativity.Left, 
                (), (fun a b c -> FRFormMethodInvocation( ".", b, c)))
        let Colon2 = 
            InfixOperator("::", pFRWhite, 1, Associativity.Left, 
                (), (fun a b c -> FRFormMethodInvocation("::", b, c)))
        pFRMethodInvocationRaw.TermParser <- pFRNormalPrimary
        pFRMethodInvocationRaw.AddOperator dot
        pFRMethodInvocationRaw.AddOperator Colon2
        let pFRMethodInvocation = pFRMethodInvocationRaw.ExpressionParser <!> "method invocation"
        
        do pFRPrimaryRef := pFRMethodInvocation
            
    // Parse all
        let pFR:Parser<FRAstNode, unit> = (pFRCompStmt eof) .>> eof

                

