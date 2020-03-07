// Learn more about F# at http://fsharp.org
namespace FRuby
module TestParse = 
    open System
    open FParsec

    let test p str =
        match run p str with
        | Success(result, _, _)   -> FRPrintAst result
        | Failure(errorMsg, _, _) -> RealPrint "Failure: %s" errorMsg

    // 1 has pass
    let ParserTestCase1 = [|"a"; "@adf"; "$1";"CO"; "_a"; "C_"; "C_2"; "a_2"; "2_a"; "@1"|]

    // 2 has pass?
    let ParserTestCase2 = [|"-1";"10000";"1.1223";"12.,32";"2\\n233";"0x1"; "1.a"|]

    // 3
    let ParserTestCase3 = [|"\"haha\\n\\t\"";"\"a+9\\r"|]

    // 4
    let ParserTestCase4 = [|"1+1";"(1+
    (1*10))"; "-1-(10 -2)";"a + 2"; "a + \"str\""; "1 + 2 % 10 != 1000.10"|]

    let runTest x = 
        for i in x do 
            test pFR i

    let TestPath = __SOURCE_DIRECTORY__ + "\\ParseTestCase\\"
    let runRealTest x =
        test pFR (IO.File.ReadAllText (TestPath + x))

    let runAllTest argv =
        runRealTest "test_if.rb"
        runRealTest "test_while.rb"
        runRealTest "test_class.rb"
        runRealTest "test_module.rb"
        runRealTest "test_method_invocation.rb"
        runRealTest "test_assign.rb"
        runRealTest "test_def.rb"
        DumpToLog (TestPath + "a.log")
        0 // return an integer exit code
