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
    { Expr: Expression; Type: Ty; Body: TypedExpr option; Statements: TypedStatement list option; MatchArms: TypedMatchArm list option }

and TypedPattern =
    { Pattern: Pattern; Type: Ty }

and TypedMatchArm = TypedPattern list * TypedExpr

and TypedStatement =
    | TypedDefStatement of Identifier * TypeExpr option * TypedExpr
    | TypedExprStatement of TypedExpr
    | TypedImportStatement of ImportItem list
    | TypedTypeDefStatement of Identifier * TypeDefModifier list * (Identifier * TypeExpr option) list * TypeExpr

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
