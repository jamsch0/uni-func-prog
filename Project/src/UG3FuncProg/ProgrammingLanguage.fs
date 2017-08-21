module ProgrammingLanguage

open AbstractSyntaxTree
open Parser
open System

let literal =
    let booleanLiteral =
        Parser.either (Parser.string "true" |> Parser.map (fun _ -> Literal.Boolean true))
                      (Parser.string "false" |> Parser.map (fun _ -> Literal.Boolean false))

    let integerLiteral =
        Parser.many1 Parser.digit |> Parser.toString |> Parser.map (Int32.Parse >> Literal.Integer)

    let floatingPointLiteral =
        Parser.separated (Parser.many Parser.digit |> Parser.toString)
                         (Parser.character '.')
                         (Parser.many1 Parser.digit |> Parser.toString)
        |> Parser.map ((fun (left, right) -> left + "." + right) >> Double.Parse >> Literal.FloatingPoint)

    let characterLiteral =
        Parser.delimited (Parser.character '\'')
                         (Parser.anyCharacter |> Parser.map Literal.Character)
                         (Parser.character '\'')

    let stringLiteral =
        Parser.delimited (Parser.character '"')
                         (Parser.many (Parser.notCharacter '"') |> Parser.toString |> Parser.map Literal.String)
                         (Parser.character '"')

    Parser.any [
        booleanLiteral;
        floatingPointLiteral;
        integerLiteral;
        characterLiteral;
        stringLiteral
    ]

let identifier =
    Parser.both Parser.alpha (Parser.many (Parser.either Parser.alphanumeric (Parser.character '_')))
    |> Parser.map List.Cons |> Parser.toString |> Parser.map Identifier |> Parser.mapErr (fun (err, _) -> (err, "identifier"))

let whitespace p =
    Parser.delimited (Parser.many Parser.whitespace)
                     p
                     (Parser.many Parser.whitespace)
    |> Parser.rename (sprintf " %s " p.name)

let rec expression =
    let rec expression =
        whitespace (Parser.any [
                        literalExpr;
                        ifExpr;
                        functionCall;
                        identifierExpr;
                        blockExpr;
                        array;
                        unaryOperation;
                        // binaryOperation;
                        parenExpr
        ]) |> Parser.mapErr (fun (err, _) -> (err, "expression"))

    and literalExpr =
        literal |> Parser.map Expression.Literal

    and identifierExpr =
        identifier |> Parser.map Expression.Identifier

    and blockExpr =
        block |> Parser.map Expression.Block

    and array =
        Parser.character '[' |> Parser.bind (fun _ ->
            Parser.terminated (Parser.separatedList expression (Parser.character ',') |> Parser.map Expression.Array)
                              (whitespace (Parser.character ']'))
        )

    and functionCall =
        Parser.both (whitespace identifier)
                    (Parser.character '(' |> Parser.bind (fun _ ->
                        Parser.terminated (Parser.separatedList expression (Parser.character ','))
                                          (whitespace (Parser.character ')'))
                        |> Parser.complete
                    ))
        |> Parser.map Expression.FunctionCall

    and unaryOperation =
        let unaryOperator =
            Parser.any [
                Parser.character '!' |> Parser.map (fun _ -> UnaryOperator.Not);
                Parser.character '-' |> Parser.map (fun _ -> UnaryOperator.Negate)
            ]
        
        unaryOperator |> Parser.bind (fun op ->
            expression |> Parser.complete
                       |> Parser.map (fun expr -> Expression.UnaryOperation (op, expr))
        )

//  and binaryOperation =
//      let binaryOperator =
//          Parser.any [
//              Parser.character '+' |> Parser.map (fun _ -> BinaryOperator.Add);
//              Parser.character '-' |> Parser.map (fun _ -> BinaryOperator.Subtract);
//              Parser.character '*' |> Parser.map (fun _ -> BinaryOperator.Multiply);
//              Parser.character '/' |> Parser.map (fun _ -> BinaryOperator.Divide);
//              Parser.character '%' |> Parser.map (fun _ -> BinaryOperator.Modulus);
//              Parser.string "&&" |> Parser.map (fun _ -> BinaryOperator.And);
//              Parser.string "||" |> Parser.map (fun _ -> BinaryOperator.Or);
//              Parser.string "==" |> Parser.map (fun _ -> BinaryOperator.Equal);
//              Parser.string "!=" |> Parser.map (fun _ -> BinaryOperator.NotEqual);
//              Parser.character '<' |> Parser.map (fun _ -> BinaryOperator.LessThan);
//              Parser.character '>' |> Parser.map (fun _ -> BinaryOperator.GreaterThan);
//              Parser.string "<=" |> Parser.map (fun _ -> BinaryOperator.LessThanOrEqual);
//              Parser.string ">=" |> Parser.map (fun _ -> BinaryOperator.GreaterThanOrEqual)
//          ]
//          |> Parser.rename "operator"
//
//      Parser.unit () |> Parser.bind (fun _ -> Parser.both expression (Parser.both binaryOperator expression))
//      |> Parser.map (fun (left, (op, right)) -> Expression.BinaryOperation (left, op, right))

    and ifExpr =
        Parser.string "if" |> Parser.bind (fun _ ->
            Parser.both
                (Parser.both
                    (Parser.both (expression |> Parser.complete)
                                (block |> Parser.complete))
                    (Parser.many (Parser.preceded (whitespace (Parser.string "elif"))
                                                  (Parser.both (expression |> Parser.complete)
                                                               (block |> Parser.complete)))))
                (Parser.optional (Parser.preceded (whitespace (Parser.string "else"))
                                                  (block |> Parser.complete)))
            |> Parser.map (fun ((ifExpr, elifExprs), elseExpr) ->
                Expression.If (ifExpr, elifExprs, elseExpr)
            )
        )

    and parenExpr =
        Parser.character '(' |> Parser.bind (fun _ ->
            Parser.terminated expression (Parser.character ')')
        )

    expression

and statement =
    let expressionStmt =
        Parser.terminated expression (Parser.character ';' |> Parser.complete)
        |> Parser.map Statement.Expression

    let binding =
        Parser.separated (Parser.preceded (Parser.string "let") (whitespace identifier |> Parser.complete))
                         (Parser.character '=' |> Parser.complete)
                         (Parser.terminated expression (Parser.character ';') |> Parser.complete)
        |> Parser.map Statement.Binding

    let breakStmt =
        Parser.terminated (Parser.string "break") (Parser.character ';' |> Parser.complete)
        |> Parser.map (fun _ -> Statement.Break)

    let continueStmt =
        Parser.terminated (Parser.string "continue") (Parser.character ';' |> Parser.complete)
        |> Parser.map (fun _ -> Statement.Continue)

    let returnStmt =
        Parser.terminated (Parser.both (Parser.string "return") (Parser.optional expression))
                          (Parser.character ';' |> Parser.complete)
        |> Parser.map (fun (_, expr) -> Statement.Return expr)

    let functionStmt =
        functionDefinition |> Parser.map Statement.Function

    let whileStmt =
        Parser.preceded (Parser.string "while")
                        (Parser.both (expression |> Parser.complete) (block |> Parser.complete))
        |> Parser.map Statement.While

    let forStmt =
        Parser.preceded (Parser.string "for")
                        (Parser.both (Parser.separated (whitespace identifier |> Parser.complete)
                                                       (Parser.string "in" |> Parser.complete)
                                                       expression |> Parser.complete)
                                     block |> Parser.complete)
        |> Parser.map (fun ((var, expr), block) -> Statement.For (var, expr, block))

    whitespace (Parser.any [
                    binding;
                    breakStmt;
                    continueStmt;
                    returnStmt;
                    whileStmt;
                    forStmt;
                    functionStmt;
                    expressionStmt
    ]) |> Parser.rename "statement"

and block =
    Parser.character '{' |> Parser.bind (fun _ ->
        Parser.terminated (Parser.many statement |> Parser.complete)
                          (whitespace (Parser.character '}'))
        |> Parser.map Block
        |> Parser.rename "block"
    )

and functionDefinition =
    Parser.both (Parser.both (Parser.preceded (Parser.string "fn")
                                              (whitespace identifier |> Parser.complete))
                             (Parser.delimited (Parser.character '(')
                                               (Parser.separatedList (whitespace identifier)
                                                                     (Parser.character ',')
                                               |> Parser.complete)
                                               (Parser.character ')')
                             |> Parser.complete))
                (whitespace block)
    |> Parser.map (fun ((name, parameters), block) -> Function (name, parameters, block))

let parser =
    Parser.parse (Parser.terminated (whitespace (Parser.many functionDefinition))
                                    Parser.empty)
