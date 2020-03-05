namespace FRuby

[<AutoOpen>]
module AstPrint =
    open Ast
    open FSharp.Core
    open System

    let buffer = Text.StringBuilder()

    let DumpToConsole = 
        printf "%s" (buffer.ToString())

    let DumpToLog name = 
        IO.File.WriteAllText(name, (buffer.ToString()))

    let RealPrint format = 
        Printf.bprintf buffer format        
        
    let RealPrintWithTab tab format = 
        for i = 1 to tab do
            RealPrint " "
        RealPrint format

    let mutable PrintNonTerminal = fun a b c d -> ()

    let nextTab x = x + 2

    let rec internal internalPrint x tab = 
        match x with
        | Terminal(t) -> RealPrintWithTab tab "%s\n" t.ToString |> ignore
        | NonTerminal(typeOut, typeIn, child) -> (PrintNonTerminal typeOut typeIn child tab)

    let FRPrintAst(x:FRAstNode) = 
        internalPrint x 0

    let internal PrintFRCompStmt child tab = 
        for i in child do
            internalPrint i tab

    let internal PrintFRExpr subType (child:list<FRAstNode>) tab = 
        match subType with
        | FRExprSubType.Arg ->
            internalPrint child.Head tab
        | _ -> ()

    let internal PrintFRArg (child:list<FRAstNode>) tab = 
        for i in child do 
            internalPrint i tab
    
    let internal PrintFRPrimary (child:list<FRAstNode>) tab = 
        for i in child do 
            internalPrint i tab

    let internal PrintOther (child:list<FRAstNode>) tab = 
        for i in child do
            internalPrint i tab

    do PrintNonTerminal <-
        fun typeOut typeIn child tab ->
            RealPrintWithTab tab "%A:%A\n" typeOut typeIn
            match typeOut with
            | FRType.FRCompstmt -> PrintFRCompStmt child (nextTab tab)
            | FRType.FRExpr -> PrintFRExpr (typeIn :?> FRExprSubType) child (nextTab tab)
            | FRType.FRArg -> PrintFRArg child (nextTab tab)
            | FRType.FRPrimary -> PrintFRPrimary child (nextTab tab) 
            | _ -> PrintOther child (nextTab tab)
                
                
                

        