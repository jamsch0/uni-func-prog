module UG3FuncProg.Tests

open AbstractSyntaxTree
open ParseResult
open Parser
open ProgrammingLanguage
open NUnit.Framework

let unwrap result =
    let option = match result with
                 | Okay value -> Some value
                 | _ -> None
    Option.get option

let unwrapErr result =
    let option = match result with
                 | Err value -> Some value
                 | _ -> None
    Option.get option

let isErr = function
    | Err _ -> true
    | _ -> false

[<Test>]
let ``result bind`` () =
    let result = Okay 2 |> ParseResult.bind (fun i -> Okay (i * 2))
    Assert.AreEqual (unwrap result, 4)

[<Test>]
let ``result map`` () =
    let result = Okay 2 |> ParseResult.map (fun i -> i * 2)
    Assert.AreEqual (unwrap result, 4)

[<Test>]
let ``result mapErr`` () =
    let result = Err 2 |> ParseResult.mapErr (fun i -> i * 2)
    Assert.AreEqual (unwrapErr result, 4)


[<Test>]
let ``result either`` () =
    let result = Err 2 |> ParseResult.either (fun i -> Okay (i * 2))
                                             (fun i -> Okay (i * 4))
    Assert.AreEqual (unwrap result, 8)

[<Test>]
let ``parse empty`` () =
    let parser = Parser.empty

    let result = unwrap (Parser.parse parser "")
    Assert.AreEqual (result, (char -1, ""))

    let result = Parser.parse parser "abc"
    Assert.IsTrue (isErr result)

[<Test>]
let ``parse any character`` () =
    let parser = Parser.anyCharacter

    let result = unwrap (Parser.parse parser "abc")
    Assert.AreEqual (result, ('a', "bc"))

    let (result, _) = unwrapErr (Parser.parse parser "")
    Assert.AreEqual (result, char -1)

[<Test>]
let ``parse character`` () =
    let parser = Parser.character 'a'

    let result = unwrap (Parser.parse parser "abc")
    Assert.AreEqual (result, ('a', "bc"))

    let result = Parser.parse parser "cba"
    Assert.IsTrue (isErr result)

    let (result, _) = unwrapErr (Parser.parse parser "")
    Assert.AreEqual (result, char -1)

[<Test>]
let ``parse not character`` () =
    let parser = Parser.notCharacter 'c'

    let result = unwrap (Parser.parse parser "abc")
    Assert.AreEqual (result, ('a', "bc"))

    let result = Parser.parse parser "cba"
    Assert.IsTrue (isErr result)

    let (result, _) = unwrapErr (Parser.parse parser "")
    Assert.AreEqual (result, char -1)

[<Test>]
let ``parse optional`` () =
    let parser = Parser.optional (Parser.character 'a')

    let result = unwrap (Parser.parse parser "abc")
    Assert.AreEqual (result, (Some 'a', "bc"))

    let result = unwrap (Parser.parse parser "cba")
    Assert.AreEqual (result, (Option<char>.None, "cba"))

[<Test>]
let ``parse peek`` () =
    let parser = Parser.peek (Parser.character 'a')

    let result = unwrap (Parser.parse parser "abc")
    Assert.AreEqual (result, ('a', "abc"))

    let result = Parser.parse parser "cba"
    Assert.IsTrue (isErr result)

[<Test>]
let ``parse both`` () =
    let parser = Parser.both (Parser.character 'a') (Parser.character 'b')

    let result = unwrap (Parser.parse parser "abc")
    Assert.AreEqual (result, (('a', 'b'), "c"))

    let result = Parser.parse parser "cba"
    Assert.IsTrue (isErr result)

    let result = Parser.parse parser "acb"
    Assert.IsTrue (isErr result)

[<Test>]
let ``parse either`` () =
    let parser = Parser.either (Parser.character 'a') (Parser.character 'b')

    let result = unwrap (Parser.parse parser "abc")
    Assert.AreEqual (result, ('a', "bc"))

    let result = unwrap (Parser.parse parser "bca")
    Assert.AreEqual (result, ('b', "ca"))

    let result = Parser.parse parser "cba"
    Assert.IsTrue (isErr result)

[<Test>]
let ``parse preceded`` () =
    let parser = Parser.preceded (Parser.character 'a')
                                  (Parser.character 'b')

    let result = unwrap (Parser.parse parser "abc")
    Assert.AreEqual (result, ('b', "c"))

    let result = Parser.parse parser "cba"
    Assert.IsTrue (isErr result)

    let result = Parser.parse parser "acb"
    Assert.IsTrue (isErr result)

[<Test>]
let ``parse terminated`` () =
    let parser = Parser.terminated (Parser.character 'a')
                                   (Parser.character 'b')

    let result = unwrap (Parser.parse parser "abc")
    Assert.AreEqual (result, ('a', "c"))

    let result = Parser.parse parser "cba"
    Assert.IsTrue (isErr result)

    let result = Parser.parse parser "acb"
    Assert.IsTrue (isErr result)

[<Test>]
let ``parse delimited`` () =
    let parser = Parser.delimited (Parser.character 'a')
                                  (Parser.character 'b')
                                  (Parser.character 'c')

    let result = unwrap (Parser.parse parser "abc")
    Assert.AreEqual (result, ('b', ""))

    let result = Parser.parse parser "cba"
    Assert.IsTrue (isErr result)

    let result = Parser.parse parser "acb"
    Assert.IsTrue (isErr result)

    let result = Parser.parse parser "abb"
    Assert.IsTrue (isErr result)

[<Test>]
let ``parse separated`` () =
    let parser = Parser.separated (Parser.character 'a')
                                  (Parser.character 'b')
                                  (Parser.character 'c')

    let result = unwrap (Parser.parse parser "abc")
    Assert.AreEqual (result, (('a', 'c'), ""))

    let result = Parser.parse parser "cba"
    Assert.IsTrue (isErr result)

    let result = Parser.parse parser "acb"
    Assert.IsTrue (isErr result)

    let result = Parser.parse parser "abb"
    Assert.IsTrue (isErr result)

[<Test>]
let ``parse all`` () =
    let parser = Parser.all [Parser.character 'a';
                             Parser.character 'b';
                             Parser.character 'c']

    let result = unwrap (Parser.parse parser "abc")
    Assert.AreEqual (result, (['a'; 'b'; 'c'], ""))

    let result = Parser.parse parser "cba"
    Assert.IsTrue (isErr result)

    let result = Parser.parse parser "acb"
    Assert.IsTrue (isErr result)

    let result = Parser.parse parser "abb"
    Assert.IsTrue (isErr result)

[<Test>]
let ``parse any`` () =
    let parser = Parser.any [Parser.character 'a';
                             Parser.character 'b';
                             Parser.character 'c']

    let result = unwrap (Parser.parse parser "abc")
    Assert.AreEqual (result, ('a', "bc"))

    let result = unwrap (Parser.parse parser "cba")
    Assert.AreEqual (result, ('c', "ba"))

    let result = unwrap (Parser.parse parser "bca")
    Assert.AreEqual (result, ('b', "ca"))

    let result = Parser.parse parser "dcba"
    Assert.IsTrue (isErr result)

[<Test>]
let ``parse zero or more`` () =
    let parser = Parser.many (Parser.character 'a')

    let result = unwrap (Parser.parse parser "aabc")
    Assert.AreEqual (result, (['a'; 'a'], "bc"))

    let result = unwrap (Parser.parse parser "cba")
    Assert.AreEqual (result, (List<char>.Empty, "cba"))

[<Test>]
let ``parse one or more`` () =
    let parser = Parser.many1 (Parser.character 'a')

    let result = unwrap (Parser.parse parser "aabc")
    Assert.AreEqual (result, (['a'; 'a'], "bc"))

    let result = Parser.parse parser "cba"
    Assert.IsTrue (isErr result)

[<Test>]
let ``parse separated list`` () =
    let parser = Parser.separatedList (Parser.character 'a')
                                      (Parser.character 'b')

    let result = unwrap (Parser.parse parser "ac")
    Assert.AreEqual (result, (['a'], "c"))

    let result = unwrap (Parser.parse parser "abc")
    Assert.AreEqual (result, (['a'], "c"))

    let result = unwrap (Parser.parse parser "abac")
    Assert.AreEqual (result, (['a'; 'a'], "c"))

    let result = unwrap (Parser.parse parser "ababc")
    Assert.AreEqual (result, (['a'; 'a'], "c"))

    let result = unwrap (Parser.parse parser "cba")
    Assert.AreEqual (result, (List<char>.Empty, "cba"))

[<Test>]
let ``parse string`` () =
    let parser = Parser.string "abc"

    let result = unwrap (Parser.parse parser "abc")
    Assert.AreEqual (result, ("abc", ""))

    let result = Parser.parse parser "cba"
    Assert.IsTrue (isErr result)

    let result = Parser.parse parser "acb"
    Assert.IsTrue (isErr result)

    let result = Parser.parse parser "abb"
    Assert.IsTrue (isErr result)

[<Test>]
let ``parse boolean literal`` () =
    let parser = ProgrammingLanguage.literal

    let result = unwrap (Parser.parse parser "true")
    Assert.AreEqual (result, (Literal.Boolean true, ""))

    let result = unwrap (Parser.parse parser "false")
    Assert.AreEqual (result, (Literal.Boolean false, ""))

[<Test>]
let ``parse integer literal`` () =
    let parser = ProgrammingLanguage.literal

    let result = unwrap (Parser.parse parser "1234")
    Assert.AreEqual (result, (Literal.Integer 1234, ""))

[<Test>]
let ``parse floating point literal`` () =
    let parser = ProgrammingLanguage.literal

    let result = unwrap (Parser.parse parser "12.34")
    Assert.AreEqual (result, (Literal.FloatingPoint 12.34, ""))

    let result = unwrap (Parser.parse parser ".1234")
    Assert.AreEqual (result, (Literal.FloatingPoint 0.1234, ""))

[<Test>]
let ``parse character literal`` () =
    let parser = ProgrammingLanguage.literal

    let result = unwrap (Parser.parse parser "'a'")
    Assert.AreEqual (result, (Literal.Character 'a', ""))

    let result = Parser.parse parser "''"
    Assert.IsTrue (isErr result)

    let result = Parser.parse parser "'aa'"
    Assert.IsTrue (isErr result)

[<Test>]
let ``parse string literal`` () =
    let parser = ProgrammingLanguage.literal

    let result = unwrap (Parser.parse parser "\"abc\"")
    Assert.AreEqual (result, (Literal.String "abc", ""))

    let result = unwrap (Parser.parse parser "\"\"")
    Assert.AreEqual (result, (Literal.String "", ""))

[<Test>]
let ``parse identifier`` () =
    let parser = ProgrammingLanguage.identifier

    let result = unwrap (Parser.parse parser "some_VAR1")
    Assert.AreEqual (result, (Identifier "some_VAR1", ""))

    let result = Parser.parse parser "_someVAR1"
    Assert.IsTrue (isErr result)

    let result = Parser.parse parser "1some_VAR"
    Assert.IsTrue (isErr result)

[<Test>]
let ``parse whitespace`` () =
    let parser = ProgrammingLanguage.whitespace (Parser.character 'a')

    let result = unwrap (Parser.parse parser "a")
    Assert.AreEqual (result, ('a', ""))

    let result = unwrap (Parser.parse parser "\t a   \r\n")
    Assert.AreEqual (result, ('a', ""))

[<Test>]
let ``parse literal expression`` () =
    let parser = ProgrammingLanguage.expression

    let result = unwrap (Parser.parse parser "true")
    Assert.AreEqual (result, (Expression.Literal (Literal.Boolean true), ""))

    let result = unwrap (Parser.parse parser "'a'")
    Assert.AreEqual (result, (Expression.Literal (Literal.Character 'a'), ""))

[<Test>]
let ``parser identifier expression`` () =
    let parser = ProgrammingLanguage.expression

    let result = unwrap (Parser.parse parser "some_VAR1")
    Assert.AreEqual (result,
                     (Expression.Identifier (Identifier "some_VAR1"), ""))

[<Test>]
let ``parse array expression`` () =
    let parser = ProgrammingLanguage.expression

    let result = unwrap (Parser.parse parser "[]")
    Assert.AreEqual (result, (Expression.Array List<Expression>.Empty, ""))

    let result = unwrap (Parser.parse parser "[ 1, 2, ]")
    Assert.AreEqual (result, (Expression.Array [
                                  Expression.Literal (Literal.Integer 1);
                                  Expression.Literal (Literal.Integer 2)
                             ], ""))

[<Test>]
let ``parse function call expression`` () =
    let parser = ProgrammingLanguage.expression

    let result = unwrap (Parser.parse parser "someFunc()")
    Assert.AreEqual (result,
                     (Expression.FunctionCall (Identifier "someFunc",
                                               List<Expression>.Empty), ""))

    let result = unwrap (Parser.parse parser "someFunc ( true )")
    Assert.AreEqual (result,
        (Expression.FunctionCall (Identifier "someFunc", [Expression.Literal (Literal.Boolean true)]), ""))

[<Test>]
let ``parse unary operation expression`` () =
    let parser = ProgrammingLanguage.expression

    let result = unwrap (Parser.parse parser "-1")
    Assert.AreEqual (result, (Expression.UnaryOperation (UnaryOperator.Negate, Expression.Literal (Literal.Integer 1)), ""))

    let result = unwrap (Parser.parse parser "! x")
    Assert.AreEqual (result,
        (Expression.UnaryOperation (UnaryOperator.Not, Expression.Identifier (Identifier "x")), ""))

[<Test>]
let ``parse binary operation expression`` () =
    let parser = ProgrammingLanguage.expression

    let result = unwrap (Parser.parse parser "x + y")
    Assert.AreEqual (result,
                     (Expression.BinaryOperation (
                          Expression.Identifier (Identifier "x"),
                          BinaryOperator.Add,
                          Expression.Identifier (Identifier "y")), ""))

[<Test>]
let ``parse parenthesised expression`` () =
    let parser = ProgrammingLanguage.expression

    let result = unwrap (Parser.parse parser "(false)")
    Assert.AreEqual (result, (Expression.Literal (Literal.Boolean false), ""))

    let result = unwrap (Parser.parse parser "( x )")
    Assert.AreEqual (result, (Expression.Identifier (Identifier "x"), ""))

[<Test>]
let ``parse if expression`` () =
    let parser = ProgrammingLanguage.expression

    let result = unwrap (Parser.parse parser "if x {}")
    Assert.AreEqual (result,
                     (Expression.If (
                          (Expression.Identifier (Identifier "x"),
                           Block List<Statement>.Empty),
                          List<(Expression * Block)>.Empty,
                          Option<Block>.None), ""))

    let result = unwrap (Parser.parse parser "if x {} else {}")
    Assert.AreEqual (result,
                     (Expression.If (
                          (Expression.Identifier (Identifier "x"),
                           Block List<Statement>.Empty),
                          List<(Expression * Block)>.Empty,
                          Some (Block List<Statement>.Empty)), ""))

    let result = unwrap (Parser.parse parser "if x {} elif y {} else {}")
    Assert.AreEqual (result,
                     (Expression.If (
                          (Expression.Identifier (Identifier "x"),
                           Block List<Statement>.Empty),
                          [(Expression.Identifier (Identifier "y"),
                            Block List<Statement>.Empty)],
                          Some (Block List<Statement>.Empty)), ""))

[<Test>]
let ``parse break statement`` () =
    let parser = ProgrammingLanguage.statement

    let result = unwrap (Parser.parse parser "break;")
    Assert.AreEqual (result, (Statement.Break, ""))

[<Test>]
let ``parse continue statement`` () =
    let parser = ProgrammingLanguage.statement

    let result = unwrap (Parser.parse parser "continue;")
    Assert.AreEqual (result, (Statement.Continue, ""))

[<Test>]
let ``parse return statement`` () =
    let parser = ProgrammingLanguage.statement

    let result = unwrap (Parser.parse parser "return;")
    Assert.AreEqual (result, (Statement.Return Option<Expression>.None, ""))

    let result = unwrap (Parser.parse parser "return x;")
    Assert.AreEqual (result,
                     (Statement.Return (
                          Some (Expression.Identifier (Identifier "x"))), ""))

[<Test>]
let ``parse variable binding statement`` () =
    let parser = ProgrammingLanguage.statement

    let result = unwrap (Parser.parse parser "let x = false;")
    Assert.AreEqual (result,
                     (Statement.Binding (
                          Identifier "x",
                          Expression.Literal (Literal.Boolean false)), ""))

[<Test>]
let ``parse expression statement`` () =
    let parser = ProgrammingLanguage.statement

    let result = unwrap (Parser.parse parser "someFunc();")
    Assert.AreEqual (result,
                     (Statement.Expression (
                          Expression.FunctionCall (
                              Identifier "someFunc",
                              List<Expression>.Empty)), ""))

[<Test>]
let ``parse while statement`` () =
    let parser = ProgrammingLanguage.statement

    let result = unwrap (Parser.parse parser "while true {}")
    Assert.AreEqual (result,
                     (Statement.While (
                          Expression.Literal (Literal.Boolean true),
                          Block List<Statement>.Empty), ""))

[<Test>]
let ``parse for statement`` () =
    let parser = ProgrammingLanguage.statement

    let result = unwrap (Parser.parse parser "for i in [1, 2] {}")
    Assert.AreEqual (result,
                     (Statement.For (
                          Identifier "i",
                          Expression.Array [
                              Expression.Literal (Literal.Integer 1);
                              Expression.Literal (Literal.Integer 2)],
                          Block List<Statement>.Empty), ""))

[<Test>]
let ``parse block`` () =
    let parser = ProgrammingLanguage.block

    let result = unwrap (Parser.parse parser "{}")
    Assert.AreEqual (result, (Block List<Statement>.Empty, ""))

    let result = unwrap (Parser.parse parser "{ return someFunc(x); }")
    Assert.AreEqual (result, (
                     Block [
                         Statement.Return (
                             Some (Expression.FunctionCall (
                                       Identifier "someFunc",
                                       [Expression.Identifier (
                                            Identifier "x")])))
                           ], ""))

[<Test>]
let ``parse function definition`` () =
    let parser = ProgrammingLanguage.functionDefinition

    let result = unwrap (Parser.parse parser "fn someFunc() {}")
    Assert.AreEqual (result,
                     (Function (Identifier "someFunc",
                                List<Identifier>.Empty,
                                Block List<Statement>.Empty), ""))

    let result = unwrap (Parser.parse parser "fn someFunc (x, y) {}")
    Assert.AreEqual (result,
                     (Function (Identifier "someFunc",
                                [Identifier "x"; Identifier "y"],
                                Block List<Statement>.Empty), ""))
