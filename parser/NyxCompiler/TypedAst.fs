namespace NyxCompiler

open Parser.Program

/// Internal types used during type inference.
type TyVar =
    { Id: int
      Name: string option }

type Ty =
    | TyVar of TyVar
    | TyPrimitive of string
    | TyFunc of Ty list * Ty
    | TyTuple of Ty list
    | TyRecord of Map<string, Ty>
    | TyTag of string * Ty option
    | TyUnion of Ty list

/// A simple typed expression placeholder for future expansion.
type TypedExpr =
    { Expr: Expression
      Type: Ty }

type TypedDefinition =
    | TypedValueDef of Identifier * TypeExpr option * TypedExpr
    | TypedTypeDef of Identifier * TypeDefModifier list * (Identifier * TypeExpr option) list * TypeExpr

type TypedTopLevelItem =
    | TypedModuleDecl of ModuleName
    | TypedImport of ImportItem list
    | TypedDef of TypedDefinition
    | TypedExprItem of TypedExpr

type TypedModule =
    { Module: Module
      Types: Map<string, Ty>
      Items: TypedTopLevelItem list }
