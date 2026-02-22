module Transpiler.Wasm.CodeGen

open System
open Parser.Program

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

let rec private transpileExpression (knownFunctions: Set<string>) (expr: Expression) : string =
    match expr with
    | LiteralExpr(IntLit value) ->
        $"i32.const {value}"
    | IdentifierExpr(name, _) when knownFunctions.Contains name ->
        $"call ${sanitizeIdentifier name}"
    | BinaryOp(op, left, right) ->
        match wasmBinaryInstruction op with
        | Some instr ->
            let leftCode = transpileExpression knownFunctions left
            let rightCode = transpileExpression knownFunctions right
            $"{leftCode}\n    {rightCode}\n    {instr}"
        | None ->
            failwith $"Unsupported binary operator for WASM MVP backend: {op}"
    | IfExpr(cond, thenExpr, elseExpr) ->
        let condCode = transpileExpression knownFunctions cond
        let thenCode = transpileExpression knownFunctions thenExpr
        let elseCode = transpileExpression knownFunctions elseExpr
        $"{condCode}\n    (if (result i32)\n      (then\n        {thenCode}\n      )\n      (else\n        {elseCode}\n      )\n    )"
    | Lambda(args, body) when args.IsEmpty ->
        transpileExpression knownFunctions body
    | Block statements ->
        let lastExpr =
            statements
            |> List.rev
            |> List.tryPick (function
                | ExprStatement expr -> Some expr
                | DefStatement _
                | ImportStatement _
                | TypeDefStatement _
                | UseStatement _ -> None)
        match lastExpr with
        | Some expr -> transpileExpression knownFunctions expr
        | None -> failwith "Unsupported block form for WASM MVP backend: block does not end in expression"
    | _ ->
        failwith $"Unsupported expression for WASM MVP backend: {expr}"

let private defExpression (expr: Expression) =
    match expr with
    | Lambda(args, body) when args.IsEmpty -> body
    | _ -> expr

let transpileModuleToWat (module': Module) : string =
    let defs =
        module'
        |> List.choose (function
            | Def (ValueDef(_, name, _, expr)) -> Some(name, defExpression expr)
            | _ -> None)

    let knownFunctions = defs |> List.map fst |> Set.ofList

    let renderedFunctions =
        defs
        |> List.map (fun (name, expr) ->
            let fn = sanitizeIdentifier name
            let bodyCode = transpileExpression knownFunctions expr
            $"  (func ${fn} (result i32)\n    {bodyCode}\n  )\n  (export \"{name}\" (func ${fn}))")
        |> String.concat "\n\n"

    if String.IsNullOrWhiteSpace renderedFunctions then
        "(module)"
    else
        $"(module\n{renderedFunctions}\n)"
