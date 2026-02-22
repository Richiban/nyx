module Transpiler.Wasm.CodeGen

open System
open Parser.Program

type private TranspileEnv =
    { KnownFunctions: Set<string>;
            Locals: Map<string, string> }

let private sanitizeIdentifier (name: string) =
    let sanitized =
        name
        |> Seq.map (fun c -> if Char.IsLetterOrDigit c || c = '_' then c else '_')
        |> Seq.toArray
        |> String
    if String.IsNullOrWhiteSpace sanitized then "_"
    elif Char.IsDigit sanitized.[0] then "_" + sanitized
    else sanitized

let private wasmBinaryInstruction op =
    match op with
    | "+" -> Some "i32.add"
    | "-" -> Some "i32.sub"
    | "*" -> Some "i32.mul"
    | "/" -> Some "i32.div_s"
    | "%" -> Some "i32.rem_s"
    | "==" -> Some "i32.eq"
    | "!=" -> Some "i32.ne"
    | "<" -> Some "i32.lt_s"
    | "<=" -> Some "i32.le_s"
    | ">" -> Some "i32.gt_s"
    | ">=" -> Some "i32.ge_s"
    | _ -> None

let private flattenCallArgs (args: Expression list) =
    match args with
    | [TupleExpr items] -> items
    | _ -> args

let rec private collectExprLocalNames (expr: Expression) : string list =
    match expr with
    | Block statements -> collectStatementLocalNames statements
    | BinaryOp(_, left, right) -> (collectExprLocalNames left) @ (collectExprLocalNames right)
    | IfExpr(cond, thenExpr, elseExpr) ->
        (collectExprLocalNames cond) @ (collectExprLocalNames thenExpr) @ (collectExprLocalNames elseExpr)
    | FunctionCall(_, _, args) -> args |> List.collect collectExprLocalNames
    | Lambda(_, body) -> collectExprLocalNames body
    | Pipe(value, _, _, args) -> (collectExprLocalNames value) @ (args |> List.collect collectExprLocalNames)
    | TupleExpr items
    | ListExpr items -> items |> List.collect collectExprLocalNames
    | RecordExpr fields ->
        fields
        |> List.collect (function
            | NamedField(_, value) -> collectExprLocalNames value
            | PositionalField value -> collectExprLocalNames value)
    | Match(values, arms) ->
        let valueLocals = values |> List.collect collectExprLocalNames
        let armLocals =
            arms
            |> List.collect (fun (_, armExpr) -> collectExprLocalNames armExpr)
        valueLocals @ armLocals
    | UseIn(_, body) -> collectExprLocalNames body
    | MemberAccess(inner, _, _) -> collectExprLocalNames inner
    | TagExpr(_, payload) -> payload |> Option.map collectExprLocalNames |> Option.defaultValue []
    | InterpolatedString parts ->
        parts
        |> List.collect (function
            | StringText _ -> []
            | StringExpr inner -> collectExprLocalNames inner)
    | WorkflowBindExpr(_, value)
    | WorkflowReturnExpr value -> collectExprLocalNames value
    | UnitExpr
    | LiteralExpr _
    | IdentifierExpr _ -> []

and private collectStatementLocalNames (statements: Statement list) : string list =
    statements
    |> List.collect (function
        | DefStatement(_, name, _, rhs) -> name :: collectExprLocalNames rhs
        | ExprStatement expr -> collectExprLocalNames expr
        | ImportStatement _
        | TypeDefStatement _
        | UseStatement _ -> [])

let private collectBlockLocalNames (statements: Statement list) =
    collectStatementLocalNames statements |> List.distinct

let private createNameMap (names: string list) =
    names
    |> List.fold (fun (acc, used) name ->
        let baseName = sanitizeIdentifier name
        let rec findAvailable candidate index =
            if used |> Set.contains candidate then
                findAvailable ($"{baseName}_{index}") (index + 1)
            else
                candidate
        let chosen = findAvailable baseName 1
        (acc |> Map.add name chosen, used |> Set.add chosen)) (Map.empty, Set.empty)
    |> fst

let rec private transpileExpression (env: TranspileEnv) (expr: Expression) : string =
    match expr with
    | LiteralExpr(IntLit value) ->
        $"i32.const {value}"
    | IdentifierExpr(name, _) ->
        match env.Locals |> Map.tryFind name with
        | Some localName -> $"local.get ${localName}"
        | None when env.KnownFunctions.Contains name -> $"call ${sanitizeIdentifier name}"
        | None -> failwith $"Unsupported identifier for WASM MVP backend: {name}"
    | BinaryOp(op, left, right) ->
        match wasmBinaryInstruction op with
        | Some instr ->
            let leftCode = transpileExpression env left
            let rightCode = transpileExpression env right
            $"{leftCode}\n    {rightCode}\n    {instr}"
        | None ->
            failwith $"Unsupported binary operator for WASM MVP backend: {op}"
    | IfExpr(cond, thenExpr, elseExpr) ->
        let condCode = transpileExpression env cond
        let thenCode = transpileExpression env thenExpr
        let elseCode = transpileExpression env elseExpr
        $"{condCode}\n    (if (result i32)\n      (then\n        {thenCode}\n      )\n      (else\n        {elseCode}\n      )\n    )"
    | FunctionCall(name, _, args) ->
        if not (env.KnownFunctions.Contains name) then
            failwith $"Unsupported function call for WASM MVP backend: {name}"
        else
            let argsCode =
                flattenCallArgs args
                |> List.map (transpileExpression env)
            match argsCode with
            | [] -> $"call ${sanitizeIdentifier name}"
            | _ ->
                let prefix = argsCode |> String.concat "\n    "
                $"{prefix}\n    call ${sanitizeIdentifier name}"
    | Lambda(args, body) when args.IsEmpty ->
        transpileExpression env body
    | Block statements ->
        let rec transpileStatements items =
            match items with
            | [] -> failwith "Unsupported block form for WASM MVP backend: block does not end in expression"
            | [ExprStatement expr] -> transpileExpression env expr
            | DefStatement(_, name, _, rhs) :: tail ->
                match env.Locals |> Map.tryFind name with
                | Some localName ->
                    let rhsCode = transpileExpression env rhs
                    let restCode = transpileStatements tail
                    $"{rhsCode}\n    local.set ${localName}\n    {restCode}"
                | None ->
                    failwith $"Unsupported block local for WASM MVP backend: {name}"
            | ExprStatement expr :: tail ->
                let exprCode = transpileExpression env expr
                let restCode = transpileStatements tail
                $"{exprCode}\n    drop\n    {restCode}"
            | ImportStatement _ :: _
            | TypeDefStatement _ :: _
            | UseStatement _ :: _ ->
                failwith "Unsupported statement form for WASM MVP backend"
        transpileStatements statements
    | _ ->
        failwith $"Unsupported expression for WASM MVP backend: {expr}"

let private defBodyAndParameters (expr: Expression) =
    match expr with
    | Lambda(args, body) -> args, body
    | _ -> [], expr

let transpileModuleToWat (module': Module) : string =
    let defs =
        module'
        |> List.choose (function
            | Def (ValueDef(_, name, _, expr)) -> Some(name, expr)
            | _ -> None)

    let knownFunctions = defs |> List.map fst |> Set.ofList

    let renderedFunctions =
        defs
        |> List.map (fun (name, expr) ->
            let parameters, bodyExpr = defBodyAndParameters expr
            let parameterNames = parameters |> List.map fst
            let parameterMap = createNameMap parameterNames
            let localNames =
                match bodyExpr with
                | Block statements ->
                    collectBlockLocalNames statements
                    |> List.filter (fun n -> not (parameterMap |> Map.containsKey n))
                | _ -> []
            let localMap =
                localNames
                |> List.fold (fun acc name ->
                    if acc |> Map.containsKey name then acc
                    else
                        let baseName = sanitizeIdentifier name
                        let mutable candidate = baseName
                        let mutable index = 1
                        while (parameterMap |> Map.exists (fun _ v -> v = candidate)) || (acc |> Map.exists (fun _ v -> v = candidate)) do
                            candidate <- $"{baseName}_{index}"
                            index <- index + 1
                        acc |> Map.add name candidate) Map.empty
            let locals = parameterMap |> Map.fold (fun acc k v -> acc |> Map.add k v) localMap
            let env = { KnownFunctions = knownFunctions; Locals = locals }
            let fn = sanitizeIdentifier name
            let parameterSig =
                parameters
                |> List.map (fun (paramName, _) -> $"(param ${parameterMap.[paramName]} i32)")
                |> String.concat " "
            let localsSig =
                localNames
                |> List.map (fun localName -> $"(local ${localMap.[localName]} i32)")
                |> String.concat " "
            let functionHeader =
                [ if not (String.IsNullOrWhiteSpace parameterSig) then parameterSig
                  "(result i32)"
                  if not (String.IsNullOrWhiteSpace localsSig) then localsSig ]
                |> String.concat " "
            let bodyCode = transpileExpression env bodyExpr
            $"  (func ${fn} {functionHeader}\n    {bodyCode}\n  )\n  (export \"{name}\" (func ${fn}))")
        |> String.concat "\n\n"

    if String.IsNullOrWhiteSpace renderedFunctions then
        "(module)"
    else
        $"(module\n{renderedFunctions}\n)"
