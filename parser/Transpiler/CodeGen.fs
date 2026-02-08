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
        let paramStr = params' |> List.map fst |> String.concat ", "
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
    | UseIn(binding, body) ->
        let bindingExpr = transpileUseBinding binding
        sprintf "(() => { %s; return %s; })()" bindingExpr (transpileExpression body)
    
    | Block stmts ->
        let stmtStrs =
            stmts
            |> List.map transpileStatement
            |> List.filter (fun stmt -> stmt <> "")
            |> String.concat "\n  "
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
        let jsComparisonOp op =
            match op with
            | "==" -> "==="
            | "!=" -> "!=="
            | _ -> op

        let rec patternToJs (scrutinee: string) (pat: Pattern) : string list * string list =
            match pat with
            | IdentifierPattern name -> ([], [sprintf "const %s = %s;" name scrutinee])
            | WildcardPattern
            | ElsePattern -> ([], [])
            | LiteralPattern lit -> ([sprintf "%s === %s" scrutinee (transpileLiteral lit)], [])
            | RangePattern (startExpr, endExpr) ->
                ([sprintf "%s >= %s" scrutinee (transpileExpression startExpr)
                  sprintf "%s <= %s" scrutinee (transpileExpression endExpr)], [])
            | GuardPattern (op, guardExpr) ->
                ([sprintf "%s %s %s" scrutinee (jsComparisonOp op) (transpileExpression guardExpr)], [])
            | TagPattern (tagName, payloadOpt) ->
                let baseConds = [sprintf "%s && typeof %s === \"object\"" scrutinee scrutinee]
                let tagCheck = sprintf "%s.tag === \"%s\"" scrutinee tagName
                match payloadOpt with
                | None -> (baseConds @ [tagCheck], [])
                | Some inner ->
                    let innerConds, innerBinds = patternToJs (sprintf "%s.value" scrutinee) inner
                    (baseConds @ (tagCheck :: innerConds), innerBinds)
            | TuplePattern patterns ->
                let baseConds =
                    [sprintf "Array.isArray(%s)" scrutinee
                     sprintf "%s.length === %d" scrutinee patterns.Length]
                let conds, binds =
                    patterns
                    |> List.mapi (fun idx pat -> patternToJs (sprintf "%s[%d]" scrutinee idx) pat)
                    |> List.fold (fun (accConds, accBinds) (conds, binds) ->
                        (accConds @ conds, accBinds @ binds)) ([], [])
                (baseConds @ conds, binds)
            | ListPattern (patterns, splatOpt) ->
                let minLen = patterns.Length
                let lengthCond =
                    match splatOpt with
                    | Some _ -> sprintf "%s.length >= %d" scrutinee minLen
                    | None -> sprintf "%s.length === %d" scrutinee minLen

                let baseConds = [sprintf "Array.isArray(%s)" scrutinee; lengthCond]
                let conds, binds =
                    patterns
                    |> List.mapi (fun idx pat -> patternToJs (sprintf "%s[%d]" scrutinee idx) pat)
                    |> List.fold (fun (accConds, accBinds) (conds, binds) ->
                        (accConds @ conds, accBinds @ binds)) ([], [])

                let splatBindings =
                    match splatOpt with
                    | Some (IdentifierPattern name) ->
                        [sprintf "const %s = %s.slice(%d);" name scrutinee minLen]
                    | Some _ -> failwith "Unsupported list splat pattern"
                    | None -> []

                (baseConds @ conds, binds @ splatBindings)
            | ListSplatMiddle (beforePatterns, afterPatterns) ->
                let minLen = beforePatterns.Length + afterPatterns.Length
                let baseConds =
                    [sprintf "Array.isArray(%s)" scrutinee
                     sprintf "%s.length >= %d" scrutinee minLen]

                let beforeConds, beforeBinds =
                    beforePatterns
                    |> List.mapi (fun idx pat -> patternToJs (sprintf "%s[%d]" scrutinee idx) pat)
                    |> List.fold (fun (accConds, accBinds) (conds, binds) ->
                        (accConds @ conds, accBinds @ binds)) ([], [])

                let afterConds, afterBinds =
                    afterPatterns
                    |> List.mapi (fun idx pat ->
                        patternToJs (sprintf "%s[%s.length - %d]" scrutinee scrutinee (afterPatterns.Length - idx)) pat)
                    |> List.fold (fun (accConds, accBinds) (conds, binds) ->
                        (accConds @ conds, accBinds @ binds)) ([], [])

                (baseConds @ beforeConds @ afterConds, beforeBinds @ afterBinds)
            | RecordPattern (_, patterns) ->
                let baseConds = [sprintf "%s && typeof %s === \"object\"" scrutinee scrutinee]
                let conds, binds =
                    patterns
                    |> List.mapi (fun idx pat -> patternToJs (sprintf "%s[%d]" scrutinee idx) pat)
                    |> List.fold (fun (accConds, accBinds) (conds, binds) ->
                        (accConds @ conds, accBinds @ binds)) ([], [])
                (baseConds @ conds, binds)
            | RecordMemberPattern members ->
                let baseConds = [sprintf "%s && typeof %s === \"object\"" scrutinee scrutinee]
                let memberConds, memberBinds =
                    members
                    |> List.map (fun (name, pat) ->
                        let memberAccess = sprintf "%s.%s" scrutinee name
                        let nameCheck = sprintf "\"%s\" in %s" name scrutinee
                        let conds, binds = patternToJs memberAccess pat
                        (nameCheck :: conds, binds))
                    |> List.fold (fun (accConds, accBinds) (conds, binds) ->
                        (accConds @ conds, accBinds @ binds)) ([], [])

                (baseConds @ memberConds, memberBinds)

        let scrutineeNames = exprs |> List.mapi (fun i _ -> sprintf "_match%d" i)
        let scrutineeDecls =
            (exprs, scrutineeNames)
            ||> List.map2 (fun expr name -> sprintf "const %s = %s;" name (transpileExpression expr))

        let armToJs (patterns: Pattern list, bodyExpr: Expression) =
            if List.length patterns <> List.length scrutineeNames then
                failwith "Match arm pattern count does not match scrutinee count"

            let conds, binds =
                List.zip patterns scrutineeNames
                |> List.map (fun (pat, scrutinee) -> patternToJs scrutinee pat)
                |> List.fold (fun (accConds, accBinds) (conds, binds) ->
                    (accConds @ conds, accBinds @ binds)) ([], [])

            let condition =
                match conds with
                | [] -> "true"
                | _ -> String.concat " && " conds

            let bindingBlock =
                match binds with
                | [] -> ""
                | _ -> (binds |> String.concat "\n    ") + "\n    "

            let bodyCode = transpileExpression bodyExpr
            condition, sprintf "{\n    %sreturn %s;\n  }" bindingBlock bodyCode

        let armBlocks = arms |> List.map armToJs
        let ifChain =
            armBlocks
            |> List.mapi (fun idx (cond, body) ->
                if idx = 0 then
                    sprintf "if (%s) %s" cond body
                else
                    sprintf "else if (%s) %s" cond body)
            |> String.concat "\n  "

        let matchBody =
            sprintf "(() => {\n  %s\n  %s\n  throw new Error(\"Match failed\");\n})()"
                (scrutineeDecls |> String.concat "\n  ")
                ifChain

        matchBody

and transpileUseBinding (binding: UseBinding) : string =
    match binding with
    | UseValue expr -> transpileExpression expr
    | UseAssign(_, expr) -> transpileExpression expr

and transpileStatement (stmt: Statement) : string =
    match stmt with
    | DefStatement(_, name, _, expr) ->
        sprintf "const %s = %s;" name (transpileExpression expr)
    | TypeDefStatement(_, _, _, _) ->
        ""
    | ImportStatement _ ->
        ""
    | UseStatement binding ->
        sprintf "%s;" (transpileUseBinding binding)
    | ExprStatement expr ->
        sprintf "%s;" (transpileExpression expr)

let transpileTopLevelItem (item: TopLevelItem) : string =
    match item with
    | ModuleDecl name ->
        sprintf "// Module: %s" name
    | Def(ValueDef(_, name, _, expr)) ->
        sprintf "const %s = %s;" name (transpileExpression expr)
    | Def(TypeDef(_, _, _, _)) ->
        ""
    | Import _ ->
        ""
    | Expr expr ->
        sprintf "%s;" (transpileExpression expr)

let transpileModule (module': Module) : string =
    module'
    |> List.map transpileTopLevelItem
    |> List.filter (fun line -> line <> "")
    |> String.concat "\n"
