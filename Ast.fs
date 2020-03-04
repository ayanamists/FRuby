namespace FRuby
    module Ast = 
        
        type FRType = 
            | FRLiteral = 1
            | FRVariable = 6
            | FRSingleton = 7
            | FRArglist = 8
            | FRArgdecl = 9
            | FRArgs = 10
            | FRCallArgs = 11
            | FRMlhs = 12
            | FRMrhs = 14
            | FRBlockVar = 16
            | FRWhenArgs = 19
            | FRPrimary = 20
            | FRArg = 21
            | FRFunction = 22
            | FRCommand = 23
            | FRCall = 24
            | FRExpr = 25
            | FRStmt = 26
            | FRCompstmt = 27
            | FRProgram = 28

        type FRStmtSubType = 
            | CallBlock = 0
            | Expr = 1

        type FRExprSubType= 
            | Command = 0
            | Arg = 1

        type FRCallSubType = 
            | Function = 0
            | Command = 1

        type FRArgSubType = 
            | Infix = 0
            | Prefix = 1
            | Primary = 2

        type FRPrimarySubType = 
            | Compstmt = 0       // `(' COMPSTMT `)'
            | Literal = 1
            | Variable = 2
            | Field = 3          // PRIMARY `::' IDENTIFIER
            | Get = 4            // PRIMARY `[' [ARGS] `]'
            | Return = 5         // return [`(' [CALL_ARGS] `)']
            | Yield = 6          // yield [`(' [CALL_ARGS] `)']
            | Function = 7       // FUNCTION
            | If = 8             // If block
            | While = 9          // While block
            | Class = 10         // Class block
            | Module = 11        // Module block
            | Def = 12           // Def block
            | Equ = 13           // lhs '=' Arg

        type LiteralSubType = 
            | FRInt = 0
            | FRFLoat = 1
            | FRString = 2

        type FRIdentifier = string
        
        type FRFName = string

        type FRString = string

        type FRInt = int64

        type FRFloat = float

        type FRKeyWord = string

        type FRVarName =
            | Global of string
            | Instance of string
            | Normal of FRIdentifier

        type FRTerminal = 
            | FRFName of FRFName
            | FRString of FRString
            | FRInt of FRInt
            | FRIdentifier of FRIdentifier
            | FRFloat of FRFloat
            | FRVarName of FRVarName
            | FRKeyWord of FRKeyWord
            | FRNull
            with
                member t.ToString = 
                    match t with
                    | FRFName(n) -> "FRFName: "  + n.ToString()
                    | FRString(n) -> "FRString: " +  n.ToString()
                    | FRInt(n) -> "FRInt: " +  n.ToString()
                    | FRIdentifier(n) -> "FRIdentifier: " + n.ToString()
                    | FRFloat(n) -> "FRFloat: " + n.ToString()
                    | FRVarName(n) -> 
                        match n with 
                        | Global(g) -> "FRVarName->Global: " +  g.ToString()
                        | Instance(i) -> "FRVarName->Instace: " + i.ToString()
                        | Normal(n) -> "FRVarName->Normal: " +  n.ToString()
                    | FRKeyWord(n) -> "FRKeyWord: " +  n.ToString()
                    | FRNull -> ""

        type FRSubType = obj

        type FRAstNode = 
            | NonTerminal of FRType * FRSubType * list<FRAstNode>
            | Terminal of FRTerminal

        let FRFormTerminalAst(x:'a):FRAstNode = Terminal x

        let FRFormCompStmt(x) = 
            FRAstNode.NonTerminal(FRType.FRCompstmt, null , x)

        let FRFormIfPrimary(x) = 
            match x with
            | (a,b,c:FRAstNode option) -> 
                let Start = 
                    match a with
                    | (expr, block) -> 
                       Terminal(FRKeyWord "if")::expr::[block;]
                let Middle = 
                    let mutable temp = []
                    for i in b do 
                        let t = match i with
                                | (expr, block) ->
                                    Terminal(FRKeyWord "elsif")::expr::[block;]
                        temp <- temp @ t
                    temp
                let End = 
                    match c with
                    | Some(block) ->
                        Terminal(FRKeyWord "else")::[block;]
                    | unit -> []

                FRAstNode.NonTerminal(FRType.FRPrimary, FRPrimarySubType.If, Start@Middle@End)

        let FRFormWhilePrimary x =
            match x with
            | (a, b) -> 
                FRAstNode.NonTerminal(FRType.FRPrimary, FRPrimarySubType.While, Terminal(FRKeyWord("while"))::a::[b;])

        let FRFormClassPrimary x =
            match x with
            |(a, b, c) ->
                let t = 
                    match b with
                    |Some(k) -> k
                    | _ -> FRNull |> Terminal
                FRAstNode.NonTerminal(FRType.FRPrimary, FRPrimarySubType.Class, 
                    Terminal(FRKeyWord("class"))::a::Terminal(FRKeyWord("superclass"))::t::[c;])

        let FRFormAssignPrimary x = 
            match x with
            |(a, b) ->
                FRAstNode.NonTerminal(FRType.FRPrimary, FRPrimarySubType.Equ, Terminal(FRKeyWord("="))::a::[b;])

        let FRFormModulePrimary x = 
            match x with
            | (a, b) ->
            FRAstNode.NonTerminal(FRType.FRPrimary, FRPrimarySubType.Module, 
                Terminal(FRKeyWord("module"))::a::[b;])

        let FRFormInfixArg x =
            match x with
            | (a, b, c) ->
            FRAstNode.NonTerminal(FRType.FRArg, FRArgSubType.Infix, Terminal(FRKeyWord(a))::b::[c;])

        let FRFormArgList x = 
            FRAstNode.NonTerminal(FRType.FRArglist, null, x)

        exception TypeError of string

        let GetStringValue x =
            match x with
            | FRVarName.Global(g) -> g
            | FRVarName.Normal(n) -> n
            | FRVarName.Instance(i) -> i

        let GetFRIntValue x = 
            match x with 
            | FRInt(i) -> i
            | _ -> raise(TypeError("x is not Int"))

        let GetFRNameValue x = 
            match x with 
            | FRFName(i) -> i
            | _ -> raise(TypeError("x is not FRName"))

        let GetFRIdentifierValue x =
            match x with
            | FRIdentifier(i) -> i
            | _ -> raise (TypeError("x is not FRIdentifier"))
         
        let GetFRFloatValue x =
            match x with 
            | FRFloat(f) -> f
            | _ -> raise (TypeError("x is not FRFloat"))

        let GetFRVarNameValue x =
            match x with
            | FRVarName (f) ->
                match f with
                | Global(g) -> g
                | Normal(n) -> n
                | Instance(i) -> i
            | _ -> raise (TypeError("x is not FRVarName"))

        let GetFRKeyWordValue x =
            match x with
            | FRKeyWord(k) -> k
            | _ -> raise(TypeError("x is not FRKeyWord"))