module AbstractSyntaxTree

type UnaryOperator =
    | Negate
    | Not

type BinaryOperator =
    | Add
    | Subtract
    | Multiply
    | Divide
    | Modulus
    | And
    | Or
    | Equal
    | NotEqual
    | LessThan
    | GreaterThan
    | LessThanOrEqual
    | GreaterThanOrEqual

type Literal =
    | Boolean of bool
    | Integer of int
    | FloatingPoint of float
    | Character of char
    | String of string

type Expression =
    | Block of Block
    | Literal of Literal
    | Identifier of Identifier
    | Array of Expression list
    | FunctionCall of Identifier * Expression list
    | UnaryOperation of UnaryOperator * Expression
    | BinaryOperation of Expression * BinaryOperator * Expression
    | If of (Expression * Block) * (Expression * Block) list * Block option

and Statement =
    | Expression of Expression
    | Binding of Identifier * Expression
    | Function of Function
    | Break
    | Continue
    | Return of Expression option
    | While of Expression * Block
    | For of Identifier * Expression * Block

and Block =
    Block of Statement list

and Identifier =
    Identifier of string

and Function =
    Function of Identifier * Identifier list * Block
