module Transpiler.CodeGen

open System
open Parser.Program

type TranspileEnv = { ContextMembers: Map<string, Set<string>>; ContextFunctions: Map<string, Set<string>>; RecordTypes: Set<string>; ContextVar: string option; BoundNames: Set<string>; NextCtxId: int }

let emptyEnv = { ContextMembers = Map.empty; ContextFunctions = Map.empty; RecordTypes = Set.empty; ContextVar = None; BoundNames = Set.empty; NextCtxId = 0 }

let private ctxVarName (id: int) =
    if id = 0 then "__ctx" else $"__ctx{id}"

let private jsReserved =
    [ "await"; "async"; "break"; "case"; "catch"; "class"; "const"; "continue"; "debugger"; "default"; "delete"; "do"
      "else"; "enum"; "export"; "extends"; "false"; "finally"; "for"; "function"; "if"; "import"; "in"; "instanceof"
      "new"; "null"; "return"; "super"; "switch"; "this"; "throw"; "true"; "try"; "typeof"; "var"; "void"; "while"
      "with"; "yield"; "let"; "static"; "implements"; "interface"; "package"; "private"; "protected"; "public" ]
    |> Set.ofList

let private escapeString (value: string) =
    let normalized = value.Replace("\r\n", "\n")
    normalized
        .Replace("\\", "\\\\")
        .Replace("\"", "\\\"")
        .Replace("\n", "\\n")
        .Replace("\r", "\\r")
        .Replace("\t", "\\t")

let private escapeTemplateText (value: string) =
    value.Replace("\\", "\\\\").Replace("`", "\\`").Replace("${", "\\${")

let private isValidJsIdentifier (name: string) =
    if String.IsNullOrWhiteSpace name then
        false
    else
        let isStart c = c = '_' || c = '$' || Char.IsLetter c
        let isPart c = isStart c || Char.IsDigit c
        isStart name.[0] && name |> Seq.skip 1 |> Seq.forall isPart

let private sanitizeIdentifier (name: string) =
    let sanitized =
        name
        |> Seq.map (fun c -> if Char.IsLetterOrDigit c || c = '_' || c = '$' then c else '_')
        |> Seq.toArray
        |> String
    let baseName =
        if String.IsNullOrWhiteSpace sanitized then
            "_"
        elif Char.IsDigit sanitized.[0] then
            "_" + sanitized
        else
            sanitized
    if jsReserved.Contains baseName then
        "_" + baseName
    else
        baseName

let private escapeIdentifier name = sanitizeIdentifier name

let private isSafePropertyName name = isValidJsIdentifier name && not (jsReserved.Contains name)

let private renderPropertyKey name =
    if isSafePropertyName name then name else sprintf "\"%s\"" (escapeString name)

let private renderMemberAccess (baseExpr: string) (name: string) =
    if isSafePropertyName name then sprintf "%s.%s" baseExpr name else sprintf "%s[\"%s\"]" baseExpr (escapeString name)

let private renderQualifiedName (name: string) =
    let parts = name.Split('.')
    if parts.Length = 1 then
        escapeIdentifier name
    else
        let first = escapeIdentifier parts.[0]
        parts
        |> Array.skip 1
        |> Array.fold (fun acc part -> renderMemberAccess acc part) first

let private renderDestructure (ctxName: string) (names: Set<string>) =
    if names.IsEmpty then
        []
    else
        let entries =
            names
            |> Set.toList
            |> List.map (fun name ->
                let escaped = escapeIdentifier name
                if escaped = name then name else sprintf "%s: %s" name escaped)
        [ sprintf "const { %s } = %s;" (String.concat ", " entries) ctxName ]

let private extractContextNames (typeExpr: TypeExpr) : TypeExpr list =
    match typeExpr with
    | TypeWithContext(ctxs, _) -> ctxs
    | TypeContext ctxs -> ctxs
    | _ -> []

let private contextNameFromTypeExpr (typeExpr: TypeExpr) : string option =
    match typeExpr with
    | TypeName name -> Some name
    | TypeApply(name, _) -> Some name
    | _ -> None

let private contextMemberNamesFromRecord (fields: TypeRecordField list) =
    fields
    |> List.choose (function
        | TypeField(name, _, _, _, _) -> Some name
        | TypeMember(name, _) -> Some name)
    |> Set.ofList

let private resolveContextMembers (contextDefs: Map<string, TypeExpr>) =
    let rec membersFromTypeExpr seen (typeExpr: TypeExpr) =
        match typeExpr with
        | TypeRecord fields -> contextMemberNamesFromRecord fields
        | TypeIntersection items
        | TypeUnion items ->
            items
            |> List.map (membersFromTypeExpr seen)
            |> Set.unionMany
        | TypeContext ctxs ->
            ctxs
            |> List.map (membersFromTypeExpr seen)
            |> Set.unionMany
        | TypeName name
        | TypeApply(name, _) ->
            if seen |> Set.contains name then
                Set.empty
            else
                match contextDefs |> Map.tryFind name with
                | Some ctxBody -> membersFromTypeExpr (seen |> Set.add name) ctxBody
                | None -> Set.empty
        | _ -> Set.empty

    contextDefs
    |> Map.map (fun name body -> membersFromTypeExpr (Set.singleton name) body)

let private resolveRecordTypes (typeDefs: Map<string, TypeExpr>) =
    let rec isRecordType seen typeExpr =
        match typeExpr with
        | TypeRecord _ -> true
        | TypeIntersection items
        | TypeUnion items -> items |> List.exists (isRecordType seen)
        | TypeContext items -> items |> List.exists (isRecordType seen)
        | TypeName name
        | TypeApply(name, _) ->
            if seen |> Set.contains name then
                false
            else
                match typeDefs |> Map.tryFind name with
                | Some body -> isRecordType (seen |> Set.add name) body
                | None -> false
        | _ -> false

    typeDefs
    |> Map.fold (fun acc name body ->
        if isRecordType (Set.singleton name) body then acc |> Set.add name else acc) Set.empty

let rec private contextMembersFromTypeExpr (env: TranspileEnv) (typeExpr: TypeExpr) : Set<string> =
    match contextNameFromTypeExpr typeExpr with
    | Some name -> env.ContextMembers |> Map.tryFind name |> Option.defaultValue Set.empty
    | None ->
        match typeExpr with
        | TypeIntersection items
        | TypeUnion items ->
            items
            |> List.map (contextMembersFromTypeExpr env)
            |> Set.unionMany
        | TypeContext items ->
            items
            |> List.map (contextMembersFromTypeExpr env)
            |> Set.unionMany
        | _ -> Set.empty

let private contextMembersFromUseExpr (env: TranspileEnv) (expr: Expression) : Set<string> =
    match expr with
    | RecordExpr fields ->
        fields
        |> List.choose (function
            | NamedField(name, _) -> Some name
            | PositionalField _ -> None)
        |> Set.ofList
    | FunctionCall(name, _) ->
        env.ContextMembers |> Map.tryFind name |> Option.defaultValue Set.empty
    | IdentifierExpr name ->
        env.ContextMembers |> Map.tryFind name |> Option.defaultValue Set.empty
    | _ -> Set.empty

let private isRecordConstructor (env: TranspileEnv) (name: string) (args: Expression list) =
    match args with
    | [RecordExpr _] -> env.RecordTypes |> Set.contains name
    | _ -> false

// JavaScript code generation for Nyx AST

let rec transpileLiteral (lit: Literal) : string =
    match lit with
    | StringLit s -> sprintf "\"%s\"" (escapeString s)
    | IntLit i -> string i
    | FloatLit f -> string f
    | BoolLit b -> if b then "true" else "false"

let rec private transpileExpressionWithEnv (env: TranspileEnv) (expr: Expression) : string =
    match expr with
    | UnitExpr -> "undefined"
    | LiteralExpr lit -> transpileLiteral lit

    | InterpolatedString parts ->
        let content =
            parts
            |> List.map (function
                | StringText text -> escapeTemplateText text
                | StringExpr inner -> "$" + "{" + transpileExpressionWithEnv env inner + "}")
            |> String.concat ""
        sprintf "`%s`" content
    
    | IdentifierExpr id -> escapeIdentifier id
    
    | MemberAccess(obj, field) ->
        renderMemberAccess (transpileExpressionWithEnv env obj) field
    
    | BinaryOp(op, left, right) ->
        let jsOp = 
            match op with
            | "==" -> "==="
            | "!=" -> "!=="
            | _ -> op
        sprintf "(%s %s %s)" (transpileExpressionWithEnv env left) jsOp (transpileExpressionWithEnv env right)
    
    | TupleExpr exprs ->
        let items = exprs |> List.map (transpileExpressionWithEnv env) |> String.concat ", "
        sprintf "[%s]" items
    
    | RecordExpr fields ->
        let transpileField field =
            match field with
            | NamedField(name, expr) ->
                sprintf "%s: %s" (renderPropertyKey name) (transpileExpressionWithEnv env expr)
            | PositionalField expr ->
                // For positional fields in a record, we could use numeric keys
                // For now, this is an error case - records should have named fields
                failwith "Positional fields in records not supported in JS transpilation"
        let fieldStrs = fields |> List.map transpileField |> String.concat ", "
        sprintf "{ %s }" fieldStrs
    
    | ListExpr exprs ->
        let items = exprs |> List.map (transpileExpressionWithEnv env) |> String.concat ", "
        sprintf "[%s]" items
    
    | FunctionCall(name, args) ->
        let emittedName = renderQualifiedName name
        if isRecordConstructor env name args then
            match args with
            | [recordExpr] -> transpileExpressionWithEnv env recordExpr
            | _ -> emittedName
        else
        let baseCall =
            if env.ContextFunctions |> Map.containsKey name then
                let ctxExpr = env.ContextVar |> Option.defaultValue "{}"
                sprintf "%s(%s)" emittedName ctxExpr
            else
                emittedName
        match args with
        | [] -> sprintf "%s()" baseCall
        | [single] ->
            match single with
            | TupleExpr items ->
                let argStrs = items |> List.map (transpileExpressionWithEnv env) |> String.concat ", "
                sprintf "%s(%s)" baseCall argStrs
            | _ -> sprintf "%s(%s)" baseCall (transpileExpressionWithEnv env single)
        | multiple ->
            let argStrs = multiple |> List.map (transpileExpressionWithEnv env) |> String.concat ", "
            sprintf "%s(%s)" baseCall argStrs
    
    | Lambda(params', body) ->
        let paramStr = params' |> List.map (fst >> escapeIdentifier) |> String.concat ", "
        sprintf "(%s) => %s" paramStr (transpileExpressionWithEnv env body)
    
    | Pipe(expr, funcName, args) ->
        // Transform pipe into function call with expr as first argument
        // expr \f becomes f(expr)
        // expr \f(args) becomes f(expr, ...args)
        let exprStr = transpileExpressionWithEnv env expr
        let baseCall =
            if env.ContextFunctions |> Map.containsKey funcName then
                let ctxExpr = env.ContextVar |> Option.defaultValue "{}"
                sprintf "%s(%s)" (renderQualifiedName funcName) ctxExpr
            else
                renderQualifiedName funcName
        match args with
        | [] -> sprintf "%s(%s)" baseCall exprStr
        | [TupleExpr items] ->
            let argStrs = items |> List.map (transpileExpressionWithEnv env) |> String.concat ", "
            sprintf "%s(%s, %s)" baseCall exprStr argStrs
        | [single] ->
            sprintf "%s(%s, %s)" baseCall exprStr (transpileExpressionWithEnv env single)
        | multiple ->
            let argStrs = multiple |> List.map (transpileExpressionWithEnv env) |> String.concat ", "
            sprintf "%s(%s, %s)" baseCall exprStr argStrs
    | UseIn(binding, body) ->
        let baseCtx = env.ContextVar |> Option.defaultValue "{}"
        let ctxName = ctxVarName env.NextCtxId
        let envWithCtx = { env with ContextVar = Some ctxName; BoundNames = Set.empty; NextCtxId = env.NextCtxId + 1 }
        let bindingExpr = transpileUseBinding envWithCtx binding
        let merged = sprintf "%s = { ...%s, ...%s };" ctxName ctxName bindingExpr
        let newMembers =
            match binding with
            | UseValue expr -> contextMembersFromUseExpr envWithCtx expr
        let newNames = newMembers |> Set.difference envWithCtx.BoundNames
        let destructure = renderDestructure ctxName newNames
        let envForBody = { envWithCtx with BoundNames = envWithCtx.BoundNames |> Set.union newMembers }
        let bodyExpr = transpileExpressionWithEnv envForBody body
        let lines =
            [sprintf "let %s = %s;" ctxName baseCtx
             merged]
            @ destructure
        sprintf "(() => { %s return %s; })()" (String.concat " " lines) bodyExpr
    | WorkflowBindExpr _
    | WorkflowReturnExpr _ ->
        failwith "Workflow expressions must be desugared before transpilation"
    
    | Block stmts ->
        let hasUse = stmts |> List.exists (function UseStatement _ -> true | _ -> false)
        let ctxName, envWithCtx, initLines =
            if hasUse then
                let ctxName = ctxVarName env.NextCtxId
                let baseCtx = env.ContextVar |> Option.defaultValue "{}"
                let envWithCtx = { env with ContextVar = Some ctxName; BoundNames = Set.empty; NextCtxId = env.NextCtxId + 1 }
                ctxName, envWithCtx, [sprintf "let %s = %s;" ctxName baseCtx]
            else
                env.ContextVar |> Option.defaultValue "", env, []

        let mutable currentEnv = envWithCtx
        let stmtLines =
            stmts
            |> List.collect (fun stmt ->
                let lines, nextEnv = transpileStatement currentEnv stmt
                currentEnv <- nextEnv
                lines)
            |> List.filter (fun stmt -> stmt <> "")

        let allLines = initLines @ stmtLines
        sprintf "(() => {\n  %s\n})()" (String.concat "\n  " allLines)
    
    | IfExpr(cond, thenExpr, elseExpr) ->
        sprintf "(%s ? %s : %s)" 
            (transpileExpressionWithEnv env cond)
            (transpileExpressionWithEnv env thenExpr)
            (transpileExpressionWithEnv env elseExpr)
    
    | TagExpr(tagName, payloadOpt) ->
        match payloadOpt with
        | None -> sprintf "{ tag: \"%s\" }" tagName
        | Some payload -> 
            sprintf "{ tag: \"%s\", value: %s }" tagName (transpileExpressionWithEnv env payload)
    
    | Match(exprs, arms) ->
        let jsComparisonOp op =
            match op with
            | "==" -> "==="
            | "!=" -> "!=="
            | _ -> op

        let rec patternToJs (scrutinee: string) (pat: Pattern) : string list * string list =
            match pat with
            | IdentifierPattern name -> ([], [sprintf "const %s = %s;" (escapeIdentifier name) scrutinee])
            | WildcardPattern
            | ElsePattern -> ([], [])
            | LiteralPattern lit -> ([sprintf "%s === %s" scrutinee (transpileLiteral lit)], [])
            | RangePattern (startExpr, endExpr) ->
                ([ sprintf "%s >= %s" scrutinee (transpileExpressionWithEnv env startExpr)
                   sprintf "%s <= %s" scrutinee (transpileExpressionWithEnv env endExpr) ], [])
            | GuardPattern (op, guardExpr) ->
                ([ sprintf "%s %s %s" scrutinee (jsComparisonOp op) (transpileExpressionWithEnv env guardExpr) ], [])
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
                        [sprintf "const %s = %s.slice(%d);" (escapeIdentifier name) scrutinee minLen]
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
                        let memberAccess = renderMemberAccess scrutinee name
                        let nameCheck = sprintf "\"%s\" in %s" name scrutinee
                        let conds, binds = patternToJs memberAccess pat
                        (nameCheck :: conds, binds))
                    |> List.fold (fun (accConds, accBinds) (conds, binds) ->
                        (accConds @ conds, accBinds @ binds)) ([], [])

                (baseConds @ memberConds, memberBinds)

        let scrutineeNames = exprs |> List.mapi (fun i _ -> sprintf "_match%d" i)
        let scrutineeDecls =
            (exprs, scrutineeNames)
            ||> List.map2 (fun expr name -> sprintf "const %s = %s;" name (transpileExpressionWithEnv env expr))

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

            let bodyCode = transpileExpressionWithEnv env bodyExpr
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

and transpileUseBinding (env: TranspileEnv) (binding: UseBinding) : string =
    match binding with
    | UseValue expr -> transpileExpressionWithEnv env expr

and transpileStatement (env: TranspileEnv) (stmt: Statement) : string list * TranspileEnv =
    match stmt with
    | DefStatement(_, name, typeOpt, expr) ->
        let contextMembers =
            match typeOpt with
            | Some typeExpr ->
                extractContextNames typeExpr
                |> List.map (contextMembersFromTypeExpr env)
                |> Set.unionMany
            | None -> Set.empty
        let hasContext = not contextMembers.IsEmpty
        let nextEnv =
            if hasContext then
                { env with ContextFunctions = env.ContextFunctions |> Map.add name contextMembers }
            else
                env
        let definitionName = renderQualifiedName name
        let isQualified = name.Contains(".")
        let definition =
            if hasContext then
                let ctxParam = ctxVarName 0
                let destructure =
                    match renderDestructure ctxParam contextMembers with
                    | [] -> ""
                    | lines -> String.concat " " lines
                let innerEnv = { nextEnv with ContextVar = Some ctxParam; BoundNames = Set.empty }
                let bodyExpr = transpileExpressionWithEnv innerEnv expr
                if isQualified then
                    sprintf "%s = (%s) => { %s return %s; };" definitionName ctxParam destructure bodyExpr
                else
                    sprintf "const %s = (%s) => { %s return %s; };" definitionName ctxParam destructure bodyExpr
            else
                if isQualified then
                    sprintf "%s = %s;" definitionName (transpileExpressionWithEnv env expr)
                else
                    sprintf "const %s = %s;" definitionName (transpileExpressionWithEnv env expr)
        [ definition ], nextEnv
    | TypeDefStatement _ -> [ "" ], env
    | ImportStatement _ -> [ "" ], env
    | UseStatement binding ->
        match env.ContextVar with
        | None ->
            let bindingExpr = transpileUseBinding env binding
            [ sprintf "%s;" bindingExpr ], env
        | Some ctxName ->
            let bindingExpr = transpileUseBinding env binding
            let mergedLine = sprintf "%s = { ...%s, ...%s };" ctxName ctxName bindingExpr
            let newMembers =
                match binding with
                | UseValue expr -> contextMembersFromUseExpr env expr
            let newNames = newMembers |> Set.difference env.BoundNames
            let destructureLines = renderDestructure ctxName newNames
            let nextEnv = { env with BoundNames = env.BoundNames |> Set.union newMembers }
            [ mergedLine ] @ destructureLines, nextEnv
    | ExprStatement expr -> [ sprintf "%s;" (transpileExpressionWithEnv env expr) ], env

let private transpileTopLevelItem (env: TranspileEnv) (item: TopLevelItem) : string list * TranspileEnv =
    match item with
    | ModuleDecl name -> [ sprintf "// Module: %s" name ], env
    | Def(ValueDef(_, name, typeOpt, expr)) ->
        transpileStatement env (DefStatement(false, name, typeOpt, expr))
    | Def(TypeDef(name, _, _, body)) ->
        let contextMembers = contextMembersFromTypeExpr env body
        let env' =
            if contextMembers.IsEmpty then env else { env with ContextMembers = env.ContextMembers |> Map.add name contextMembers }
        [ "" ], env'
    | Import _ -> [ "" ], env
    | Expr expr -> [ sprintf "%s;" (transpileExpressionWithEnv env expr) ], env

let transpileModule (module': Module) : string =
    let typeDefs =
        module'
        |> List.choose (function
            | Def (TypeDef(name, _, _, body)) -> Some(name, body)
            | _ -> None)
        |> Map.ofList
    let contextMembers = resolveContextMembers typeDefs
    let recordTypes = resolveRecordTypes typeDefs
    let mutable env = { emptyEnv with ContextMembers = contextMembers; RecordTypes = recordTypes }
    let lines =
        module'
        |> List.collect (fun item ->
            let itemLines, nextEnv = transpileTopLevelItem env item
            env <- nextEnv
            itemLines)
        |> List.filter (fun line -> line <> "")
    String.concat "\n" lines

let transpileExpression (expr: Expression) : string =
    transpileExpressionWithEnv emptyEnv expr
