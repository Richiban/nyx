module Transpiler.CodeGen

open Parser.Program

// JavaScript code generation for Nyx AST

let rec transpileLiteral (lit: Literal) : string =
    match lit with
    | StringLit s -> sprintf "\"%s\"" (s.Replace("\"", "\\\""))
    | IntLit i -> string i
    | FloatLit f -> string f
    | BoolLit b -> if b then "true" else "false"

let rec transpileExpression (expr: Expression) : string =
    match expr with
    | LiteralExpr lit -> transpileLiteral lit
    
    | IdentifierExpr id -> id
    
    | MemberAccess(obj, field) ->
        sprintf "%s.%s" (transpileExpression obj) field
    
    | BinaryOp(op, left, right) ->
        let jsOp = 
            match op with
            | "==" -> "==="
            | "!=" -> "!=="
            | _ -> op
        sprintf "(%s %s %s)" (transpileExpression left) jsOp (transpileExpression right)
    
    | TupleExpr exprs ->
        let items = exprs |> List.map transpileExpression |> String.concat ", "
        sprintf "[%s]" items
    
    | RecordExpr fields ->
        let transpileField field =
            match field with
            | NamedField(name, expr) ->
                sprintf "%s: %s" name (transpileExpression expr)
            | PositionalField expr ->
                // For positional fields in a record, we could use numeric keys
                // For now, this is an error case - records should have named fields
                failwith "Positional fields in records not supported in JS transpilation"
        let fieldStrs = fields |> List.map transpileField |> String.concat ", "
        sprintf "{ %s }" fieldStrs
    
    | ListExpr exprs ->
        let items = exprs |> List.map transpileExpression |> String.concat ", "
        sprintf "[%s]" items
    
    | FunctionCall(name, args) ->
        match args with
        | [] -> sprintf "%s()" name
        | [single] -> 
            // Single argument - could be a tuple
            match single with
            | TupleExpr items ->
                // Spread tuple elements as separate arguments
                let argStrs = items |> List.map transpileExpression |> String.concat ", "
                sprintf "%s(%s)" name argStrs
            | _ -> sprintf "%s(%s)" name (transpileExpression single)
        | multiple ->
            // Multiple arguments (shouldn't happen with tupled form, but handle it)
            let argStrs = multiple |> List.map transpileExpression |> String.concat ", "
            sprintf "%s(%s)" name argStrs
    
    | Lambda(params', body) ->
        let paramStr = String.concat ", " params'
        sprintf "(%s) => %s" paramStr (transpileExpression body)
    
    | Pipe(expr, funcName, args) ->
        // Transform pipe into function call with expr as first argument
        // expr \f becomes f(expr)
        // expr \f(args) becomes f(expr, ...args)
        let exprStr = transpileExpression expr
        match args with
        | [] -> sprintf "%s(%s)" funcName exprStr
        | [TupleExpr items] ->
            let argStrs = items |> List.map transpileExpression |> String.concat ", "
            sprintf "%s(%s, %s)" funcName exprStr argStrs
        | [single] ->
            sprintf "%s(%s, %s)" funcName exprStr (transpileExpression single)
        | multiple ->
            let argStrs = multiple |> List.map transpileExpression |> String.concat ", "
            sprintf "%s(%s, %s)" funcName exprStr argStrs
    
    | Block stmts ->
        let stmtStrs = stmts |> List.map transpileStatement |> String.concat "\n  "
        sprintf "(() => {\n  %s\n})()" stmtStrs
    
    | IfExpr(cond, thenExpr, elseExpr) ->
        sprintf "(%s ? %s : %s)" 
            (transpileExpression cond)
            (transpileExpression thenExpr)
            (transpileExpression elseExpr)
    
    | TagExpr(tagName, payloadOpt) ->
        match payloadOpt with
        | None -> sprintf "{ tag: \"%s\" }" tagName
        | Some payload -> 
            sprintf "{ tag: \"%s\", value: %s }" tagName (transpileExpression payload)
    
    | Match(exprs, arms) ->
        // Match expressions are complex - for now, generate nested if-else
        // This is a simplified implementation
        failwith "Match expression transpilation not yet implemented"

and transpileStatement (stmt: Statement) : string =
    match stmt with
    | DefStatement(name, expr) ->
        sprintf "const %s = %s;" name (transpileExpression expr)
    | ExprStatement expr ->
        sprintf "%s;" (transpileExpression expr)

let transpileTopLevelItem (item: TopLevelItem) : string =
    match item with
    | ModuleDecl name ->
        sprintf "// Module: %s" name
    | Def(ValueDef(name, expr)) ->
        sprintf "const %s = %s;" name (transpileExpression expr)

let transpileModule (module': Module) : string =
    module'
    |> List.map transpileTopLevelItem
    |> String.concat "\n"
