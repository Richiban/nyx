module Nyx.Parser.AST

type CommonIdentifier = CommonIdentifier of string
type TypeIdentifier = TypeIdentifier of string
type ModuleIdentifier = ModuleIdentifier of string

type Expression =
    | StringLiteral of string
    | IntLiteral of int
    | Block of Expression list

type ObjectField = {
    name: CommonIdentifier option
    fieldType: TypeExpression
} with
    static member mk name value = { name = name; fieldType = value }

and TypeExpression =
    | Identifier of TypeIdentifier
    | Tag of CommonIdentifier
    | Intersection of TypeExpression * TypeExpression
    | Union of TypeExpression * TypeExpression
    | FunctionType of TypeExpression * TypeExpression
    | TypeCall of TypeExpression * TypeExpression
    //| ObjectTuple of (CommonIdentifier * TypeExpression) list
    | NamedField of CommonIdentifier * TypeExpression
    | Tuple of TypeExpression list
    | Unit

type FunctionArgument = {
    name: CommonIdentifier
    argumentType: TypeExpression option 
} with
    static member mk(name, argumentType) = { name = name; argumentType = argumentType }

and ValueDefinition = { 
    name: CommonIdentifier
    value: Block
} with 
    static member mk(name, value) = { name = name; value = value }

and FunctionDefinition = { 
    name: CommonIdentifier
    arguments: FunctionArgument list
    body: Block 
} with 
    static member mk(name, arguments, body) = { name = name; arguments = arguments; body = body }

and TypeDefinition = {
    name: TypeIdentifier
    value: TypeExpression 
} with 
        static member mk(name, value) = { name = name; value = value }

and Definition = 
    | Val of ValueDefinition
    | Func of FunctionDefinition
    | Type of TypeDefinition

and Statement = 
    | Def of Definition
    | Expr of Expression

and Block = Block of Statement list

type ImportTarget = ImportTarget of string
type ImportSection = ImportSection of ImportTarget list

type ModuleDefinition = { 
    name: ModuleIdentifier
    imports: ImportSection option
    definitions: Definition list 
} with
    static member mk name imports definitions = { name = name; imports = imports; definitions = definitions }
