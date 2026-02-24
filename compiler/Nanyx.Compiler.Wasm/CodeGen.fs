module Transpiler.Wasm.CodeGen

open System
open System.Text
open Parser.Program

type private ContextFn = string * int * int

type private TranspileEnv = { KnownFunctions: Set<string>; Locals: Map<string, string>; DbgTempLocal: string option; TagIds: Map<string, int>; MatchTempNames: string list; NextMatchTemp: int; StringLiterals: Map<string, int>; ContextFunctions: Map<string, ContextFn>; PendingUseBindings: Map<string, ContextFn> list ref }

let private ctxFnName (name: string, _, _) = name
let private ctxFnSlot (_, slot: int, _) = slot
let private ctxFnArity (_, _, arity: int) = arity

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

let private sanitizeImportSuffix (name: string) = sanitizeIdentifier name

let private tagPayloadMask = 65535
let private tagDiscriminantMask = -65536

let private tagDiscriminantBase (id: int) = id <<< 16

let private stringDataBaseOffset = 256

let private dbgDataBaseOffset = 128

let private dbgNewlineOffset = dbgDataBaseOffset + 64

let private escapeWasmData (bytes: byte array) =
    let sb = StringBuilder()
    for b in bytes do
        let c = char b
        if b >= 0x20uy && b <= 0x7Euy && c <> '"' && c <> '\\' then
            sb.Append c |> ignore
        else
            sb.Append(sprintf "\\%02x" b) |> ignore
    sb.ToString()

let private buildStringLiteralMap (literals: string list) =
    let mutable offset = stringDataBaseOffset
    let entries =
        literals
        |> List.map (fun value ->
            let bytes = Encoding.UTF8.GetBytes value
            let lengthBytes = BitConverter.GetBytes bytes.Length
            let dataBytes = Array.concat [ lengthBytes; bytes; [| 0uy |] ]
            let currentOffset = offset
            offset <- offset + dataBytes.Length
            value, currentOffset, dataBytes)
    let map = entries |> List.map (fun (value, baseOffset, _) -> value, baseOffset + 4) |> Map.ofList
    map, entries

let private collectContextUseBindings (expr: Expression) : (string * Expression) list list =
    let rec collectFromStatement (stmt: Statement) =
        match stmt with
        | UseStatement (UseValue (FunctionCall(_, _, args))) ->
            match args with
            | [RecordExpr fields] ->
                let bindings =
                    fields
                    |> List.choose (function
                        | NamedField(name, value) -> Some (name, value)
                        | PositionalField _ -> None)
                [bindings]
            | _ -> []
        | DefStatement(_, _, _, rhs) -> collectFromExpr rhs
        | ExprStatement value -> collectFromExpr value
        | ImportStatement _
        | TypeDefStatement _ -> []
        | _ -> []
    and collectFromExpr value =
        match value with
        | Block statements ->
            statements |> List.collect collectFromStatement
        | IfExpr(cond, thenExpr, elseExpr) ->
            collectFromExpr cond @ collectFromExpr thenExpr @ collectFromExpr elseExpr
        | Match(values, arms) ->
            let valueBindings = values |> List.collect collectFromExpr
            let armBindings = arms |> List.collect (fun (_, armExpr) -> collectFromExpr armExpr)
            valueBindings @ armBindings
        | Lambda(_, body) -> collectFromExpr body
        | Pipe(value, _, _, args) ->
            collectFromExpr value @ (args |> List.collect collectFromExpr)
        | FunctionCall(_, _, args) -> args |> List.collect collectFromExpr
        | TupleExpr items
        | ListExpr items -> items |> List.collect collectFromExpr
        | RecordExpr fields ->
            fields
            |> List.collect (function
                | NamedField(_, value) -> collectFromExpr value
                | PositionalField value -> collectFromExpr value)
        | UseIn(binding, body) ->
            let bindingBindings =
                match binding with
                | UseValue inner -> collectFromExpr inner
            bindingBindings @ collectFromExpr body
        | TagExpr(_, payload) -> payload |> Option.map collectFromExpr |> Option.defaultValue []
        | MemberAccess(inner, _, _) -> collectFromExpr inner
        | InterpolatedString parts ->
            parts
            |> List.collect (function
                | StringText _ -> []
                | StringExpr inner -> collectFromExpr inner)
        | WorkflowBindExpr(_, value)
        | WorkflowReturnExpr value -> collectFromExpr value
        | UnitExpr
        | LiteralExpr _
        | IdentifierExpr _ -> []
        | _ -> []

    collectFromExpr expr

let rec private countMatchExpressions (expr: Expression) : int =
    match expr with
    | Match(values, arms) ->
        let arity = values.Length
        let maxArityInArms =
            arms |> List.map (fun (_, armExpr) -> countMatchArity armExpr) |> List.max
        max arity maxArityInArms
    | FunctionCall(_, _, args) -> args |> List.sumBy countMatchExpressions
    | BinaryOp(_, left, right) -> countMatchExpressions left + countMatchExpressions right
    | IfExpr(cond, thenExpr, elseExpr) ->
        countMatchExpressions cond + countMatchExpressions thenExpr + countMatchExpressions elseExpr
    | Lambda(_, body) -> countMatchExpressions body
    | Pipe(value, _, _, args) -> countMatchExpressions value + (args |> List.sumBy countMatchExpressions)
    | Block statements ->
        statements
        |> List.sumBy (function
            | DefStatement(_, _, _, rhs) -> countMatchExpressions rhs
            | ExprStatement value -> countMatchExpressions value
            | ImportStatement _
            | TypeDefStatement _
            | UseStatement _ -> 0)
    | TupleExpr values -> values |> List.sumBy countMatchExpressions
    | ListExpr values -> values |> List.sumBy countMatchExpressions
    | RecordExpr fields ->
        fields
        |> List.sumBy (function
            | NamedField(_, value) -> countMatchExpressions value
            | PositionalField value -> countMatchExpressions value)
    | UseIn(_, body) -> countMatchExpressions body
    | MemberAccess(value, _, _) -> countMatchExpressions value
    | TagExpr(_, payload) -> payload |> Option.map countMatchExpressions |> Option.defaultValue 0
    | InterpolatedString parts ->
        parts
        |> List.sumBy (function
            | StringText _ -> 0
            | StringExpr value -> countMatchExpressions value)
    | WorkflowBindExpr(_, value) -> countMatchExpressions value
    | WorkflowReturnExpr value -> countMatchExpressions value
    | UnitExpr -> 0
    | LiteralExpr _ -> 0
    | IdentifierExpr _ -> 0

and countMatchArity (expr: Expression) : int =
    match expr with
    | Match(values, arms) ->
        let arity = values.Length
        let maxArityInArms =
            arms |> List.map (fun (_, armExpr) -> countMatchArity armExpr) |> List.max
        max arity maxArityInArms
    | FunctionCall(_, _, args) -> args |> List.map countMatchArity |> List.max
    | BinaryOp(_, left, right) -> max (countMatchArity left) (countMatchArity right)
    | IfExpr(cond, thenExpr, elseExpr) ->
        [countMatchArity cond; countMatchArity thenExpr; countMatchArity elseExpr] |> List.max
    | Lambda(_, body) -> countMatchArity body
    | Pipe(value, _, _, args) -> max (countMatchArity value) (args |> List.map countMatchArity |> List.max)
    | Block statements ->
        statements
        |> List.map (function
            | DefStatement(_, _, _, rhs) -> countMatchArity rhs
            | ExprStatement value -> countMatchArity value
            | ImportStatement _
            | TypeDefStatement _
            | UseStatement _ -> 0)
        |> List.max
    | TupleExpr values
    | ListExpr values -> values |> List.map countMatchArity |> List.max
    | RecordExpr fields ->
        fields
        |> List.map (function
            | NamedField(_, value) -> countMatchArity value
            | PositionalField value -> countMatchArity value)
        |> List.max
    | UseIn(_, body) -> countMatchArity body
    | MemberAccess(value, _, _) -> countMatchArity value
    | TagExpr(_, payload) -> payload |> Option.map countMatchArity |> Option.defaultValue 0
    | InterpolatedString parts ->
        parts
        |> List.map (function
            | StringText _ -> 0
            | StringExpr value -> countMatchArity value)
        |> List.max
    | WorkflowBindExpr(_, value)
    | WorkflowReturnExpr value -> countMatchArity value
    | UnitExpr
    | LiteralExpr _
    | IdentifierExpr _ -> 0

let rec private containsDbgCallExpr (expr: Expression) : bool =
    match expr with
    | FunctionCall(name, _, _) when name = "dbg" -> true
    | FunctionCall(_, _, args) -> args |> List.exists containsDbgCallExpr
    | BinaryOp(_, left, right) -> containsDbgCallExpr left || containsDbgCallExpr right
    | IfExpr(cond, thenExpr, elseExpr) ->
        containsDbgCallExpr cond || containsDbgCallExpr thenExpr || containsDbgCallExpr elseExpr
    | Block statements -> containsDbgCallStatements statements
    | Lambda(_, body) -> containsDbgCallExpr body
    | Pipe(value, _, _, args) -> containsDbgCallExpr value || (args |> List.exists containsDbgCallExpr)
    | TupleExpr items
    | ListExpr items -> items |> List.exists containsDbgCallExpr
    | RecordExpr fields ->
        fields
        |> List.exists (function
            | NamedField(_, value) -> containsDbgCallExpr value
            | PositionalField value -> containsDbgCallExpr value)
    | Match(values, arms) ->
        (values |> List.exists containsDbgCallExpr)
        || (arms |> List.exists (fun (_, armExpr) -> containsDbgCallExpr armExpr))
    | UseIn(_, body) -> containsDbgCallExpr body
    | MemberAccess(inner, _, _) -> containsDbgCallExpr inner
    | TagExpr(_, payload) -> payload |> Option.exists containsDbgCallExpr
    | InterpolatedString parts ->
        parts
        |> List.exists (function
            | StringText _ -> false
            | StringExpr inner -> containsDbgCallExpr inner)
    | WorkflowBindExpr(_, value)
    | WorkflowReturnExpr value -> containsDbgCallExpr value
    | UnitExpr
    | LiteralExpr _
    | IdentifierExpr _ -> false

and private containsDbgCallStatements (statements: Statement list) : bool =
    statements
    |> List.exists (function
        | DefStatement(_, _, _, rhs) -> containsDbgCallExpr rhs
        | ExprStatement expr -> containsDbgCallExpr expr
        | ImportStatement _
        | TypeDefStatement _
        | UseStatement _ -> false)

let rec private collectDbgTagNamesExpr (expr: Expression) : string list =
    let flattenDbgArgs args =
        match args with
        | [TupleExpr items] -> items
        | _ -> args

    match expr with
    | FunctionCall(name, _, args) when name = "dbg" ->
        match flattenDbgArgs args with
        | [TagExpr(tagName, None)] -> [ tagName ]
        | _ -> []
    | FunctionCall(_, _, args) -> args |> List.collect collectDbgTagNamesExpr
    | BinaryOp(_, left, right) -> collectDbgTagNamesExpr left @ collectDbgTagNamesExpr right
    | IfExpr(cond, thenExpr, elseExpr) ->
        collectDbgTagNamesExpr cond @ collectDbgTagNamesExpr thenExpr @ collectDbgTagNamesExpr elseExpr
    | Lambda(_, body) -> collectDbgTagNamesExpr body
    | Pipe(value, _, _, args) -> collectDbgTagNamesExpr value @ (args |> List.collect collectDbgTagNamesExpr)
    | Block statements -> collectDbgTagNamesStatements statements
    | TupleExpr values
    | ListExpr values -> values |> List.collect collectDbgTagNamesExpr
    | RecordExpr fields ->
        fields
        |> List.collect (function
            | NamedField(_, value) -> collectDbgTagNamesExpr value
            | PositionalField value -> collectDbgTagNamesExpr value)
    | Match(values, arms) ->
        let valueTags = values |> List.collect collectDbgTagNamesExpr
        let armTags = arms |> List.collect (fun (_, armExpr) -> collectDbgTagNamesExpr armExpr)
        valueTags @ armTags
    | UseIn(_, body) -> collectDbgTagNamesExpr body
    | MemberAccess(value, _, _) -> collectDbgTagNamesExpr value
    | TagExpr(_, payload) -> payload |> Option.map collectDbgTagNamesExpr |> Option.defaultValue []
    | InterpolatedString parts ->
        parts
        |> List.collect (function
            | StringText _ -> []
            | StringExpr value -> collectDbgTagNamesExpr value)
    | WorkflowBindExpr(_, value)
    | WorkflowReturnExpr value -> collectDbgTagNamesExpr value
    | UnitExpr
    | LiteralExpr _
    | IdentifierExpr _ -> []

and private collectStringLiteralsExpr (expr: Expression) : string list =
    match expr with
    | LiteralExpr(StringLit value) -> [ value ]
    | Block statements -> collectStringLiteralsStatements statements
    | BinaryOp(_, left, right) -> collectStringLiteralsExpr left @ collectStringLiteralsExpr right
    | IfExpr(cond, thenExpr, elseExpr) ->
        collectStringLiteralsExpr cond @ collectStringLiteralsExpr thenExpr @ collectStringLiteralsExpr elseExpr
    | FunctionCall(_, _, args) -> args |> List.collect collectStringLiteralsExpr
    | Lambda(_, body) -> collectStringLiteralsExpr body
    | Pipe(value, _, _, args) -> collectStringLiteralsExpr value @ (args |> List.collect collectStringLiteralsExpr)
    | TupleExpr items
    | ListExpr items -> items |> List.collect collectStringLiteralsExpr
    | RecordExpr fields ->
        fields
        |> List.collect (function
            | NamedField(_, value) -> collectStringLiteralsExpr value
            | PositionalField value -> collectStringLiteralsExpr value)
    | Match(values, arms) ->
        let valueStrings = values |> List.collect collectStringLiteralsExpr
        let armStrings = arms |> List.collect (fun (_, armExpr) -> collectStringLiteralsExpr armExpr)
        valueStrings @ armStrings
    | UseIn(_, body) -> collectStringLiteralsExpr body
    | MemberAccess(inner, _, _) -> collectStringLiteralsExpr inner
    | TagExpr(_, payload) -> payload |> Option.map collectStringLiteralsExpr |> Option.defaultValue []
    | InterpolatedString _
    | WorkflowBindExpr _
    | WorkflowReturnExpr _
    | UnitExpr
    | LiteralExpr _
    | IdentifierExpr _ -> []

and private collectStringLiteralsStatements (statements: Statement list) : string list =
    statements
    |> List.collect (function
        | DefStatement(_, _, _, rhs) -> collectStringLiteralsExpr rhs
        | ExprStatement expr -> collectStringLiteralsExpr expr
        | ImportStatement _
        | TypeDefStatement _
        | UseStatement _ -> [])

and private collectDbgTagNamesStatements (statements: Statement list) : string list =
    statements
    |> List.collect (function
        | DefStatement(_, _, _, value) -> collectDbgTagNamesExpr value
        | ExprStatement value -> collectDbgTagNamesExpr value
        | ImportStatement _
        | TypeDefStatement _
        | UseStatement _ -> [])

let rec private collectDbgTagPayloadNamesExpr (expr: Expression) : string list =
    let flattenDbgArgs args =
        match args with
        | [TupleExpr items] -> items
        | _ -> args

    match expr with
    | FunctionCall(name, _, args) when name = "dbg" ->
        match flattenDbgArgs args with
        | [TagExpr(tagName, Some _)] -> [ tagName ]
        | _ -> []
    | FunctionCall(_, _, args) -> args |> List.collect collectDbgTagPayloadNamesExpr
    | BinaryOp(_, left, right) -> collectDbgTagPayloadNamesExpr left @ collectDbgTagPayloadNamesExpr right
    | IfExpr(cond, thenExpr, elseExpr) ->
        collectDbgTagPayloadNamesExpr cond @ collectDbgTagPayloadNamesExpr thenExpr @ collectDbgTagPayloadNamesExpr elseExpr
    | Lambda(_, body) -> collectDbgTagPayloadNamesExpr body
    | Pipe(value, _, _, args) -> collectDbgTagPayloadNamesExpr value @ (args |> List.collect collectDbgTagPayloadNamesExpr)
    | Block statements -> collectDbgTagPayloadNamesStatements statements
    | TupleExpr values
    | ListExpr values -> values |> List.collect collectDbgTagPayloadNamesExpr
    | RecordExpr fields ->
        fields
        |> List.collect (function
            | NamedField(_, value) -> collectDbgTagPayloadNamesExpr value
            | PositionalField value -> collectDbgTagPayloadNamesExpr value)
    | Match(values, arms) ->
        let valueTags = values |> List.collect collectDbgTagPayloadNamesExpr
        let armTags = arms |> List.collect (fun (_, armExpr) -> collectDbgTagPayloadNamesExpr armExpr)
        valueTags @ armTags
    | UseIn(_, body) -> collectDbgTagPayloadNamesExpr body
    | MemberAccess(value, _, _) -> collectDbgTagPayloadNamesExpr value
    | TagExpr(_, payload) -> payload |> Option.map collectDbgTagPayloadNamesExpr |> Option.defaultValue []
    | InterpolatedString parts ->
        parts
        |> List.collect (function
            | StringText _ -> []
            | StringExpr value -> collectDbgTagPayloadNamesExpr value)
    | WorkflowBindExpr(_, value)
    | WorkflowReturnExpr value -> collectDbgTagPayloadNamesExpr value
    | UnitExpr
    | LiteralExpr _
    | IdentifierExpr _ -> []

and private collectDbgTagPayloadNamesStatements (statements: Statement list) : string list =
    statements
    |> List.collect (function
        | DefStatement(_, _, _, value) -> collectDbgTagPayloadNamesExpr value
        | ExprStatement value -> collectDbgTagPayloadNamesExpr value
        | ImportStatement _
        | TypeDefStatement _
        | UseStatement _ -> [])

let rec private containsRegularDbgExpr (expr: Expression) : bool =
    let flattenDbgArgs args =
        match args with
        | [TupleExpr items] -> items
        | _ -> args

    match expr with
    | FunctionCall(name, _, args) when name = "dbg" ->
        match flattenDbgArgs args with
        | [TagExpr(_, None)] -> false
        | _ -> true
    | FunctionCall(_, _, args) -> args |> List.exists containsRegularDbgExpr
    | BinaryOp(_, left, right) -> containsRegularDbgExpr left || containsRegularDbgExpr right
    | IfExpr(cond, thenExpr, elseExpr) ->
        containsRegularDbgExpr cond || containsRegularDbgExpr thenExpr || containsRegularDbgExpr elseExpr
    | Lambda(_, body) -> containsRegularDbgExpr body
    | Pipe(value, _, _, args) -> containsRegularDbgExpr value || (args |> List.exists containsRegularDbgExpr)
    | Block statements -> containsRegularDbgStatements statements
    | TupleExpr values
    | ListExpr values -> values |> List.exists containsRegularDbgExpr
    | RecordExpr fields ->
        fields
        |> List.exists (function
            | NamedField(_, value) -> containsRegularDbgExpr value
            | PositionalField value -> containsRegularDbgExpr value)
    | Match(values, arms) ->
        (values |> List.exists containsRegularDbgExpr)
        || (arms |> List.exists (fun (_, armExpr) -> containsRegularDbgExpr armExpr))
    | UseIn(_, body) -> containsRegularDbgExpr body
    | MemberAccess(value, _, _) -> containsRegularDbgExpr value
    | TagExpr(_, payload) -> payload |> Option.exists containsRegularDbgExpr
    | InterpolatedString parts ->
        parts
        |> List.exists (function
            | StringText _ -> false
            | StringExpr value -> containsRegularDbgExpr value)
    | WorkflowBindExpr(_, value)
    | WorkflowReturnExpr value -> containsRegularDbgExpr value
    | UnitExpr
    | LiteralExpr _
    | IdentifierExpr _ -> false

and private containsDbgStringLiteralExpr (expr: Expression) : bool =
    let flattenDbgArgs args =
        match args with
        | [TupleExpr items] -> items
        | _ -> args

    match expr with
    | FunctionCall(name, _, args) when name = "dbg" ->
        match flattenDbgArgs args with
        | [LiteralExpr(StringLit _)] -> true
        | _ -> false
    | FunctionCall(_, _, args) -> args |> List.exists containsDbgStringLiteralExpr
    | BinaryOp(_, left, right) -> containsDbgStringLiteralExpr left || containsDbgStringLiteralExpr right
    | IfExpr(cond, thenExpr, elseExpr) ->
        containsDbgStringLiteralExpr cond || containsDbgStringLiteralExpr thenExpr || containsDbgStringLiteralExpr elseExpr
    | Lambda(_, body) -> containsDbgStringLiteralExpr body
    | Pipe(value, _, _, args) -> containsDbgStringLiteralExpr value || (args |> List.exists containsDbgStringLiteralExpr)
    | Block statements -> statements |> List.exists (function
        | DefStatement(_, _, _, rhs) -> containsDbgStringLiteralExpr rhs
        | ExprStatement expr -> containsDbgStringLiteralExpr expr
        | ImportStatement _
        | TypeDefStatement _
        | UseStatement _ -> false)
    | TupleExpr values
    | ListExpr values -> values |> List.exists containsDbgStringLiteralExpr
    | RecordExpr fields ->
        fields
        |> List.exists (function
            | NamedField(_, value) -> containsDbgStringLiteralExpr value
            | PositionalField value -> containsDbgStringLiteralExpr value)
    | Match(values, arms) ->
        (values |> List.exists containsDbgStringLiteralExpr)
        || (arms |> List.exists (fun (_, armExpr) -> containsDbgStringLiteralExpr armExpr))
    | UseIn(_, body) -> containsDbgStringLiteralExpr body
    | MemberAccess(value, _, _) -> containsDbgStringLiteralExpr value
    | TagExpr(_, payload) -> payload |> Option.exists containsDbgStringLiteralExpr
    | InterpolatedString parts ->
        parts
        |> List.exists (function
            | StringText _ -> false
            | StringExpr inner -> containsDbgStringLiteralExpr inner)
    | WorkflowBindExpr(_, value)
    | WorkflowReturnExpr value -> containsDbgStringLiteralExpr value
    | UnitExpr
    | LiteralExpr _
    | IdentifierExpr _ -> false

and private containsRegularDbgStatements (statements: Statement list) : bool =
    statements
    |> List.exists (function
        | DefStatement(_, _, _, value) -> containsRegularDbgExpr value
        | ExprStatement value -> containsRegularDbgExpr value
        | ImportStatement _
        | TypeDefStatement _
        | UseStatement _ -> false)

let rec private collectTagNamesExpr (expr: Expression) : string list =
    match expr with
    | TagExpr(name, payload) ->
        match payload with
        | Some value -> name :: collectTagNamesExpr value
        | None -> [ name ]
    | BinaryOp(_, left, right) -> collectTagNamesExpr left @ collectTagNamesExpr right
    | IfExpr(cond, thenExpr, elseExpr) ->
        collectTagNamesExpr cond @ collectTagNamesExpr thenExpr @ collectTagNamesExpr elseExpr
    | FunctionCall(_, _, args) -> args |> List.collect collectTagNamesExpr
    | Lambda(_, body) -> collectTagNamesExpr body
    | Pipe(value, _, _, args) -> collectTagNamesExpr value @ (args |> List.collect collectTagNamesExpr)
    | Block statements -> collectTagNamesStatements statements
    | TupleExpr values
    | ListExpr values -> values |> List.collect collectTagNamesExpr
    | RecordExpr fields ->
        fields
        |> List.collect (function
            | NamedField(_, value) -> collectTagNamesExpr value
            | PositionalField value -> collectTagNamesExpr value)
    | Match(values, arms) ->
        let valueTags = values |> List.collect collectTagNamesExpr
        let armTags =
            arms
            |> List.collect (fun (patterns, armExpr) ->
                (patterns |> List.collect collectTagNamesPattern) @ (collectTagNamesExpr armExpr))
        valueTags @ armTags
    | UseIn(_, body) -> collectTagNamesExpr body
    | MemberAccess(value, _, _) -> collectTagNamesExpr value
    | InterpolatedString parts ->
        parts
        |> List.collect (function
            | StringText _ -> []
            | StringExpr value -> collectTagNamesExpr value)
    | WorkflowBindExpr(_, value)
    | WorkflowReturnExpr value -> collectTagNamesExpr value
    | UnitExpr
    | LiteralExpr _
    | IdentifierExpr _ -> []

and private collectTagNamesStatements (statements: Statement list) : string list =
    statements
    |> List.collect (function
        | DefStatement(_, _, _, value) -> collectTagNamesExpr value
        | ExprStatement value -> collectTagNamesExpr value
        | ImportStatement _
        | TypeDefStatement _
        | UseStatement _ -> [])

and private collectTagNamesPattern (pattern: Pattern) : string list =
    match pattern with
    | TagPattern(tagName, payloadOpt) ->
        match payloadOpt with
        | Some payload -> tagName :: collectTagNamesPattern payload
        | None -> [ tagName ]
    | TuplePattern patterns
    | ListPattern(patterns, _) -> patterns |> List.collect collectTagNamesPattern
    | ListSplatMiddle(beforePatterns, afterPatterns) ->
        (beforePatterns |> List.collect collectTagNamesPattern)
        @ (afterPatterns |> List.collect collectTagNamesPattern)
    | RecordPattern(_, patterns) -> patterns |> List.collect collectTagNamesPattern
    | RecordMemberPattern members -> members |> List.collect (fun (_, p) -> collectTagNamesPattern p)
    | GuardPattern(_, value)
    | RangePattern(value, _) -> collectTagNamesExpr value
    | LiteralPattern _
    | IdentifierPattern _
    | WildcardPattern
    | ElsePattern -> []

let rec private transpileExpression (env: TranspileEnv) (expr: Expression) : string =
    match expr with
    | LiteralExpr(IntLit value) ->
        $"i32.const {value}"
    | LiteralExpr(StringLit value) ->
        match env.StringLiterals |> Map.tryFind value with
        | Some offset -> $"i32.const {offset}"
        | None -> failwith $"Internal error: missing string literal offset for {value}"
    | IdentifierExpr(name, _) ->
        match env.Locals |> Map.tryFind name with
        | Some localName -> $"local.get ${localName}"
        | None when env.KnownFunctions.Contains name -> $"call ${sanitizeIdentifier name}"
        | None -> failwith $"Unsupported identifier for WASM MVP backend: {name}"
    | TagExpr(name, None) ->
        match env.TagIds |> Map.tryFind name with
        | Some id -> $"i32.const {tagDiscriminantBase id}"
        | None -> failwith $"Internal error: missing tag discriminant for #{name}"
    | TagExpr(name, Some payloadExpr) ->
        let tagBase =
            match env.TagIds |> Map.tryFind name with
            | Some id -> tagDiscriminantBase id
            | None -> failwith $"Internal error: missing tag discriminant for #{name}"
        let payloadCode = transpileExpression env payloadExpr
        $"{payloadCode}\n    i32.const {tagPayloadMask}\n    i32.and\n    i32.const {tagBase}\n    i32.or"
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
    // ...existing code...
    | Match(values, arms) ->
        // Multi-scrutinee support
        let matchTempNames =
            env.MatchTempNames
            |> List.skip env.NextMatchTemp
            |> List.take values.Length
        if matchTempNames.Length <> values.Length then
            failwith "Internal error: not enough match temp locals for multi-scrutinee match"

        let envForNested = { env with NextMatchTemp = env.NextMatchTemp + values.Length }

        let scrutineeCodes = values |> List.map (transpileExpression envForNested)
        let setScrutineeLocals =
            List.zip matchTempNames scrutineeCodes
            |> List.map (fun (name, code) -> $"{code}\n    local.set ${name}")
            |> String.concat "\n    "

        let patternIsCatchAll pattern =
            match pattern with
            | WildcardPattern
            | ElsePattern
            | IdentifierPattern _ -> true
            | _ -> false

        let patternCondition pattern tempName =
            match pattern with
            | LiteralPattern(IntLit value) ->
                $"local.get ${tempName}\n      i32.const {value}\n      i32.eq"
            | LiteralPattern(BoolLit value) ->
                let asInt = if value then 1 else 0
                $"local.get ${tempName}\n      i32.const {asInt}\n      i32.eq"
            | TagPattern(tagName, payloadOpt) ->
                let tagBase =
                    match env.TagIds |> Map.tryFind tagName with
                    | Some id -> tagDiscriminantBase id
                    | None -> failwith $"Internal error: missing tag discriminant for pattern #{tagName}"
                match payloadOpt with
                | None ->
                    $"local.get ${tempName}\n      i32.const {tagDiscriminantMask}\n      i32.and\n      i32.const {tagBase}\n      i32.eq"
                | Some (LiteralPattern(IntLit payload)) ->
                    let payloadMasked = payload &&& tagPayloadMask
                    $"local.get ${tempName}\n      i32.const {tagDiscriminantMask}\n      i32.and\n      i32.const {tagBase}\n      i32.eq\n      local.get ${tempName}\n      i32.const {tagPayloadMask}\n      i32.and\n      i32.const {payloadMasked}\n      i32.eq\n      i32.and"
                | Some (LiteralPattern(BoolLit payload)) ->
                    let payloadMasked = if payload then 1 else 0
                    $"local.get ${tempName}\n      i32.const {tagDiscriminantMask}\n      i32.and\n      i32.const {tagBase}\n      i32.eq\n      local.get ${tempName}\n      i32.const {tagPayloadMask}\n      i32.and\n      i32.const {payloadMasked}\n      i32.eq\n      i32.and"
                | Some WildcardPattern
                | Some ElsePattern ->
                    $"local.get ${tempName}\n      i32.const {tagDiscriminantMask}\n      i32.and\n      i32.const {tagBase}\n      i32.eq"
                | Some (IdentifierPattern ident) ->
                    $"local.get ${tempName}\n      i32.const {tagDiscriminantMask}\n      i32.and\n      i32.const {tagBase}\n      i32.eq"
                | Some _ ->
                    failwith $"Unsupported tag payload pattern for WASM MVP backend: {tagName}"
            | GuardPattern(op, guardExpr) ->
                match wasmBinaryInstruction op with
                | Some instr ->
                    let guardCode = transpileExpression env guardExpr
                    $"local.get ${tempName}\n      {guardCode}\n      {instr}"
                | None ->
                    failwith $"Unsupported guard operator for WASM MVP backend: {op}"
            | WildcardPattern
            | ElsePattern
            | IdentifierPattern _ -> "i32.const 1"
            | _ -> failwith $"Unsupported match pattern for WASM MVP backend: {pattern}"

        let rec emitArms remainingArms =
            match remainingArms with
            | [] -> "unreachable"
            | (patterns, armExpr) :: rest ->
                if List.length patterns <> values.Length then
                    failwith "Unsupported match arm: pattern count does not match scrutinee count"
                let conds : string list =
                    List.zip patterns matchTempNames
                    |> List.map (fun (pat, temp) -> patternCondition pat temp)
                let allConds =
                    match conds with
                    | [] -> "i32.const 1"
                    | [single] -> single
                    | many ->
                        let many : string list = many
                        let n = many.Length
                        many |> String.concat "\n      " |> fun s -> $"{s}\n      " + (List.init (n - 1) (fun _ -> "i32.and") |> String.concat "\n      ")
                // Add identifier payloads to envForNested.Locals before binding
                let payloadIdents =
                    patterns
                    |> List.choose (function
                        | TagPattern(_, Some (IdentifierPattern ident)) -> Some ident
                        | _ -> None)
                let envWithPayloadLocals =
                    payloadIdents
                    |> List.fold (fun acc ident ->
                        let localName = $"__payload_{ident}"
                        { acc with Locals = acc.Locals |> Map.add ident localName }) envForNested
                let patternPayloadBinds =
                    List.zip patterns matchTempNames
                    |> List.choose (fun (pat, temp) ->
                        match pat with
                        | TagPattern(_, Some (IdentifierPattern ident)) ->
                            let payloadLocal =
                                match envWithPayloadLocals.Locals |> Map.tryFind ident with
                                | Some localName -> localName
                                | None -> failwith $"Internal error: missing local for payload variable {ident}"
                            Some $"local.get ${temp}\n        i32.const {tagPayloadMask}\n        i32.and\n        local.set ${payloadLocal}"
                        | _ -> None)
                    |> String.concat "\n    "
                let armCode =
                    if patternPayloadBinds = "" then
                        transpileExpression envWithPayloadLocals armExpr
                    else
                        $"{patternPayloadBinds}\n    {transpileExpression envWithPayloadLocals armExpr}"
                let elseCode = emitArms rest
                $"{allConds}\n    (if (result i32)\n      (then\n        {armCode}\n      )\n      (else\n        {elseCode}\n      )\n    )"

        let armsCode = emitArms arms
        $"{setScrutineeLocals}\n    {armsCode}"
    // ...existing code...
    | FunctionCall(name, _, args) when name = "dbg" ->
        let dbgLocal =
            match env.DbgTempLocal with
            | Some localName -> localName
            | None -> failwith "Internal error: dbg local is not configured for WASM backend"
        let argExpr =
            match flattenCallArgs args with
            | [single] -> single
            | _ -> failwith "dbg expects a single argument"
        let argCode =
            match argExpr with
            | TagExpr(tagName, None) ->
                let tagId =
                    match env.TagIds |> Map.tryFind tagName with
                    | Some id -> id
                    | None -> failwith $"Internal error: missing tag discriminant for #{tagName}"
                let importFn = $"dbg_tag_{sanitizeImportSuffix tagName}"
                let tagBase = tagDiscriminantBase tagId
                $"i32.const {tagBase}\n    local.set ${dbgLocal}\n    call ${importFn}\n    local.get ${dbgLocal}"
            | TagExpr(tagName, Some payloadExpr) ->
                let tagId =
                    match env.TagIds |> Map.tryFind tagName with
                    | Some id -> id
                    | None -> failwith $"Internal error: missing tag discriminant for #{tagName}"
                let importFn = $"dbg_tag_payload_{sanitizeImportSuffix tagName}"
                let tagBase = tagDiscriminantBase tagId
                let payloadCode = transpileExpression env payloadExpr
                $"{payloadCode}\n    local.tee ${dbgLocal}\n    call ${importFn}\n    local.get ${dbgLocal}\n    i32.const {tagPayloadMask}\n    i32.and\n    i32.const {tagBase}\n    i32.or"
            | LiteralExpr(BoolLit value) -> if value then "i32.const 1" else "i32.const 0"
            | _ -> transpileExpression env argExpr
        match argExpr with
        | TagExpr _ -> argCode
        | LiteralExpr(StringLit _) -> $"{argCode}\n    local.tee ${dbgLocal}\n    call $dbg_str\n    local.get ${dbgLocal}"
        | _ -> $"{argCode}\n    local.tee ${dbgLocal}\n    call $dbg\n    local.get ${dbgLocal}"
    | FunctionCall(name, _, args) ->
        let argsCode =
            flattenCallArgs args
            |> List.map (transpileExpression env)
        match env.ContextFunctions |> Map.tryFind name with
        | Some ctxFn ->
            let prefix = argsCode |> String.concat "\n    "
            if prefix = "" then
                $"i32.const {ctxFnSlot ctxFn}\n    call_indirect (type $ctx_fn_{ctxFnArity ctxFn})"
            else
                $"{prefix}\n    i32.const {ctxFnSlot ctxFn}\n    call_indirect (type $ctx_fn_{ctxFnArity ctxFn})"
        | None ->
            if not (env.KnownFunctions.Contains name) then
                failwith $"Unsupported function call for WASM MVP backend: {name}"
            else
                match argsCode with
                | [] -> $"call ${sanitizeIdentifier name}"
                | _ ->
                    let prefix = argsCode |> String.concat "\n    "
                    $"{prefix}\n    call ${sanitizeIdentifier name}"
    | Lambda(args, body) when args.IsEmpty ->
        transpileExpression env body
    | Block statements ->
        let rec transpileStatements env items =
            match items with
            | [] -> failwith "Unsupported block form for WASM MVP backend: block does not end in expression"
            | [ExprStatement expr] -> transpileExpression env expr
            | DefStatement(_, name, _, rhs) :: tail ->
                match env.Locals |> Map.tryFind name with
                | Some localName ->
                    let rhsCode = transpileExpression env rhs
                    let restCode = transpileStatements env tail
                    $"{rhsCode}\n    local.set ${localName}\n    {restCode}"
                | None ->
                    failwith $"Unsupported block local for WASM MVP backend: {name}"
            | ExprStatement expr :: tail ->
                let exprCode = transpileExpression env expr
                let restCode = transpileStatements env tail
                $"{exprCode}\n    drop\n    {restCode}"
            | UseStatement _ :: tail ->
                let pending = env.PendingUseBindings.Value
                match pending with
                | [] ->
                    failwith "Use statement encountered without collected bindings"
                | nextBindings :: rest ->
                    env.PendingUseBindings.Value <- rest
                    let updatedEnv =
                        nextBindings
                        |> Map.fold (fun acc name ctxFn ->
                            { acc with ContextFunctions = acc.ContextFunctions |> Map.add name ctxFn }) env
                    transpileStatements updatedEnv tail
            | ImportStatement _ :: _
            | TypeDefStatement _ :: _ ->
                failwith "Unsupported statement form for WASM MVP backend"
        transpileStatements env statements
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

    let mutable nextContextIndex = 0
    let mutable contextDefs: (string * Expression) list = []
    let mutable contextFnList: ContextFn list = []
    let contextBindingsByDef =
        defs
        |> List.map (fun (defName, expr) ->
            let bindings = collectContextUseBindings expr
            let bindingMaps =
                bindings
                |> List.map (fun bindingList ->
                    bindingList
                    |> List.map (fun (bindingName, valueExpr) ->
                        match valueExpr with
                        | Lambda(args, body) ->
                            let fnName = $"__ctx_{sanitizeIdentifier defName}_{sanitizeIdentifier bindingName}_{nextContextIndex}"
                            let ctxFn: ContextFn = fnName, nextContextIndex, args.Length
                            nextContextIndex <- nextContextIndex + 1
                            contextDefs <- contextDefs @ [fnName, Lambda(args, body)]
                            contextFnList <- contextFnList @ [ctxFn]
                            bindingName, ctxFn
                        | _ ->
                            failwith $"Unsupported context binding for {bindingName}: only lambdas are supported in WASM MVP")
                    |> Map.ofList)
            defName, bindingMaps)
        |> Map.ofList

    let knownFunctions =
        (defs |> List.map fst) @ (contextDefs |> List.map fst)
        |> Set.ofList
    let moduleUsesDbg = defs |> List.exists (fun (_, expr) -> containsDbgCallExpr expr)
    let moduleUsesRegularDbg = defs |> List.exists (fun (_, expr) -> containsRegularDbgExpr expr)
    let moduleUsesDbgString = defs |> List.exists (fun (_, expr) -> containsDbgStringLiteralExpr expr)
    let dbgTagNames =
        defs
        |> List.collect (fun (_, expr) -> collectDbgTagNamesExpr expr)
        |> List.distinct
        |> List.sort
    let dbgTagPayloadNames =
        defs
        |> List.collect (fun (_, expr) -> collectDbgTagPayloadNamesExpr expr)
        |> List.distinct
        |> List.sort
    let tagIdMap =
        defs
        |> List.collect (fun (_, expr) -> collectTagNamesExpr expr)
        |> List.distinct
        |> List.sort
        |> List.mapi (fun index name -> name, index + 1)
        |> Map.ofList

    let stringLiterals =
        defs
        |> List.collect (fun (_, expr) -> collectStringLiteralsExpr expr)
        |> List.distinct

    let stringLiteralMap, stringLiteralData = buildStringLiteralMap stringLiterals

    let renderFunction (name: string) (expr: Expression) (exportFn: bool) (pendingUseBindings: Map<string, ContextFn> list) =
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
            let usesDbg = containsDbgCallExpr bodyExpr
            let dbgLocalName =
                if usesDbg then
                    let baseName = "__dbg_tmp"
                    let mutable candidate = baseName
                    let mutable index = 1
                    while (parameterMap |> Map.exists (fun _ v -> v = candidate)) || (localMap |> Map.exists (fun _ v -> v = candidate)) do
                        candidate <- $"{baseName}_{index}"
                        index <- index + 1
                    Some candidate
                else
                    None
            let locals = parameterMap |> Map.fold (fun acc k v -> acc |> Map.add k v) localMap
            let mutable usedLocalNames =
                locals
                |> Map.toList
                |> List.map snd
                |> Set.ofList

            let matchArity = countMatchArity bodyExpr
            let matchTempNames =
                [ for index in 0 .. (matchArity - 1) do
                    let baseName = $"__match_tmp_{index}"
                    let mutable candidate = baseName
                    let mutable suffix = 1
                    while usedLocalNames |> Set.contains candidate do
                        candidate <- $"{baseName}_{suffix}"
                        suffix <- suffix + 1
                    usedLocalNames <- usedLocalNames |> Set.add candidate
                    yield candidate ]

            let env =
                { KnownFunctions = knownFunctions
                  Locals = locals
                  DbgTempLocal = dbgLocalName
                  TagIds = tagIdMap
                  MatchTempNames = matchTempNames
                  NextMatchTemp = 0
                  StringLiterals = stringLiteralMap
                  ContextFunctions = Map.empty
                  PendingUseBindings = ref pendingUseBindings }
            let fn = sanitizeIdentifier name
            let parameterSig =
                parameters
                |> List.map (fun (paramName, _) -> $"(param ${parameterMap.[paramName]} i32)")
                |> String.concat " "
            // Collect all identifier payloads in match patterns
            let rec collectPayloadLocals expr =
                match expr with
                | Match(_, arms) ->
                    arms |> List.collect (fun (patterns, armExpr) ->
                        let payloads = patterns |> List.choose (function
                            | TagPattern(_, Some (IdentifierPattern ident)) -> Some ident
                            | _ -> None)
                        List.append payloads (collectPayloadLocals armExpr))
                | FunctionCall(_, _, args) -> args |> List.collect collectPayloadLocals
                | BinaryOp(_, left, right) -> List.append (collectPayloadLocals left) (collectPayloadLocals right)
                | IfExpr(cond, thenExpr, elseExpr) ->
                    List.append (collectPayloadLocals cond) (List.append (collectPayloadLocals thenExpr) (collectPayloadLocals elseExpr))
                | Lambda(_, body) -> collectPayloadLocals body
                | Pipe(value, _, _, args) -> List.append (collectPayloadLocals value) (args |> List.collect collectPayloadLocals)
                | Block statements ->
                    statements |> List.collect (function
                        | DefStatement(_, _, _, rhs) -> collectPayloadLocals rhs
                        | ExprStatement value -> collectPayloadLocals value
                        | _ -> [])
                | TupleExpr values
                | ListExpr values -> values |> List.collect collectPayloadLocals
                | RecordExpr fields ->
                    fields |> List.collect (function
                        | NamedField(_, value) -> collectPayloadLocals value
                        | PositionalField value -> collectPayloadLocals value)
                | UseIn(_, body) -> collectPayloadLocals body
                | MemberAccess(value, _, _) -> collectPayloadLocals value
                | TagExpr(_, payload) -> payload |> Option.map collectPayloadLocals |> Option.defaultValue []
                | InterpolatedString parts ->
                    parts |> List.collect (function
                        | StringText _ -> []
                        | StringExpr value -> collectPayloadLocals value)
                | WorkflowBindExpr(_, value)
                | WorkflowReturnExpr value -> collectPayloadLocals value
                | UnitExpr
                | LiteralExpr _
                | IdentifierExpr _ -> []

            let payloadLocals = collectPayloadLocals bodyExpr |> List.distinct
            let payloadLocalNames = payloadLocals |> List.map (fun ident -> $"__payload_{ident}")
            let localsSig =
                [ yield! (localNames |> List.map (fun localName -> localMap.[localName]))
                  yield! matchTempNames
                  yield! payloadLocalNames
                  match dbgLocalName with
                  | Some name -> yield name
                  | None -> () ]
                |> List.map (fun localName -> $"(local ${localName} i32)")
                |> String.concat " "
            let functionHeader =
                [ if not (String.IsNullOrWhiteSpace parameterSig) then parameterSig
                  "(result i32)"
                  if not (String.IsNullOrWhiteSpace localsSig) then localsSig ]
                |> String.concat " "
            let bodyCode = transpileExpression env bodyExpr
            if exportFn then
                $"  (func ${fn} {functionHeader}\n    {bodyCode}\n  )\n  (export \"{name}\" (func ${fn}))"
            else
                $"  (func ${fn} {functionHeader}\n    {bodyCode}\n  )"

    let renderedUserFunctions =
        defs
        |> List.map (fun (name, expr) ->
            let pending = contextBindingsByDef |> Map.tryFind name |> Option.defaultValue []
            renderFunction name expr true pending)

    let renderedContextFunctions =
        contextDefs
        |> List.map (fun (name, expr) -> renderFunction name expr false [])

    let renderedFunctions =
        renderedUserFunctions @ renderedContextFunctions
        |> String.concat "\n\n"

    let moduleUsesDbgHelpers = moduleUsesRegularDbg || moduleUsesDbgString

    let dbgPrelude =
        if moduleUsesDbgHelpers then
            [ "  (import \"wasi_snapshot_preview1\" \"fd_write\" (func $fd_write (param i32 i32 i32 i32) (result i32)))"
              "  (memory 1)"
              "  (export \"memory\" (memory 0))"
              $"  (data (i32.const {dbgDataBaseOffset}) \"dbg: \")"
              $"  (data (i32.const {dbgNewlineOffset}) \"\\0a\")"
              "  (func $dbg (param $x i32)"
              "    (local $n i32)"
              "    (local $isneg i32)"
              "    (local $pos i32)"
              "    (local $start i32)"
              "    (local $lo i32)"
              "    (local $hi i32)"
              "    (local $tmp i32)"
              "    local.get $x"
              "    local.set $n"
              "    local.get $n"
              "    i32.const 0"
              "    i32.lt_s"
              "    if"
              "      i32.const 1"
              "      local.set $isneg"
              "      local.get $n"
              "      i32.const -1"
              "      i32.mul"
              "      local.set $n"
              "    end"
              $"    i32.const {dbgNewlineOffset}"
              "    local.set $pos"
              "    local.get $isneg"
              "    if"
              "      local.get $pos"
              "      i32.const 45"
              "      i32.store8"
              "      local.get $pos"
              "      i32.const 1"
              "      i32.add"
              "      local.set $pos"
              "    end"
              "    local.get $n"
              "    i32.const 0"
              "    i32.eq"
              "    if"
              "      local.get $pos"
              "      i32.const 48"
              "      i32.store8"
              "      local.get $pos"
              "      i32.const 1"
              "      i32.add"
              "      local.set $pos"
              "    else"
              "      local.get $pos"
              "      local.set $start"
              "      loop $digits"
              "        local.get $pos"
              "        local.get $n"
              "        i32.const 10"
              "        i32.rem_u"
              "        i32.const 48"
              "        i32.add"
              "        i32.store8"
              "        local.get $pos"
              "        i32.const 1"
              "        i32.add"
              "        local.set $pos"
              "        local.get $n"
              "        i32.const 10"
              "        i32.div_u"
              "        local.set $n"
              "        local.get $n"
              "        i32.const 0"
              "        i32.ne"
              "        br_if $digits"
              "      end"
              "      local.get $start"
              "      local.set $lo"
              "      local.get $pos"
              "      i32.const 1"
              "      i32.sub"
              "      local.set $hi"
              "      block $rev_done"
              "        loop $rev"
              "          local.get $lo"
              "          local.get $hi"
              "          i32.ge_u"
              "          br_if $rev_done"
              "          local.get $lo"
              "          i32.load8_u"
              "          local.set $tmp"
              "          local.get $lo"
              "          local.get $hi"
              "          i32.load8_u"
              "          i32.store8"
              "          local.get $hi"
              "          local.get $tmp"
              "          i32.store8"
              "          local.get $lo"
              "          i32.const 1"
              "          i32.add"
              "          local.set $lo"
              "          local.get $hi"
              "          i32.const 1"
              "          i32.sub"
              "          local.set $hi"
              "          br $rev"
              "        end"
              "      end"
              "    end"
              "    local.get $pos"
              "    i32.const 10"
              "    i32.store8"
              "    local.get $pos"
              "    i32.const 1"
              "    i32.add"
              "    local.set $pos"
              "    i32.const 0"
              $"    i32.const {dbgDataBaseOffset}"
              "    i32.store"
              "    i32.const 4"
              "    local.get $pos"
              $"    i32.const {dbgDataBaseOffset}"
              "    i32.sub"
              "    i32.store"
              "    i32.const 1"
              "    i32.const 0"
              "    i32.const 1"
              "    i32.const 8"
              "    call $fd_write"
              "    drop"
              "  )"
              "  (func $dbg_str (param $ptr i32)"
              "    (local $len i32)"
              "    local.get $ptr"
              "    i32.const 4"
              "    i32.sub"
              "    i32.load"
              "    local.set $len"
              "    i32.const 0"
              $"    i32.const {dbgDataBaseOffset}"
              "    i32.store"
              "    i32.const 4"
              "    i32.const 5"
              "    i32.store"
              "    i32.const 1"
              "    i32.const 0"
              "    i32.const 1"
              "    i32.const 8"
              "    call $fd_write"
              "    drop"
              "    i32.const 0"
              "    local.get $ptr"
              "    i32.store"
              "    i32.const 4"
              "    local.get $len"
              "    i32.store"
              "    i32.const 1"
              "    i32.const 0"
              "    i32.const 1"
              "    i32.const 8"
              "    call $fd_write"
              "    drop"
              "    i32.const 0"
              $"    i32.const {dbgNewlineOffset}"
              "    i32.store"
              "    i32.const 4"
              "    i32.const 1"
              "    i32.store"
              "    i32.const 1"
              "    i32.const 0"
              "    i32.const 1"
              "    i32.const 8"
              "    call $fd_write"
              "    drop"
              "  )" ]
            |> String.concat "\n"
        else
            ""

    let contextTypeDefs =
        contextFnList
        |> List.map ctxFnArity
        |> Set.ofList
        |> Set.toList
        |> List.sort
        |> List.map (fun arity ->
            let paramSig =
                [ for _ in 1 .. arity -> "i32" ]
                |> String.concat " "
            if String.IsNullOrWhiteSpace paramSig then
                $"  (type $ctx_fn_{arity} (func (result i32)))"
            else
                $"  (type $ctx_fn_{arity} (func (param {paramSig}) (result i32)))")
        |> String.concat "\n"

    let contextTable =
        if contextFnList.IsEmpty then
            ""
        else
            $"  (table $ctx_table {contextFnList.Length} funcref)"

    let contextElem =
        if contextFnList.IsEmpty then
            ""
        else
            let ordered = contextFnList |> List.sortBy ctxFnSlot
            let elems = ordered |> List.map (fun ctxFn -> $"${ctxFnName ctxFn}") |> String.concat " "
            $"  (elem (i32.const 0) {elems})"

    let contextPrelude =
        [ contextTypeDefs; contextTable; contextElem ]
        |> List.filter (fun line -> not (String.IsNullOrWhiteSpace line))
        |> String.concat "\n"

    let dbgTagImports =
        dbgTagNames
        |> List.map (fun tagName ->
            let importName = $"dbg_tag_{sanitizeImportSuffix tagName}"
            $"  (import \"env\" \"{importName}\" (func ${importName}))")

    let dbgTagPayloadImports =
        dbgTagPayloadNames
        |> List.map (fun tagName ->
            let importName = $"dbg_tag_payload_{sanitizeImportSuffix tagName}"
            $"  (import \"env\" \"{importName}\" (func ${importName} (param i32)))")

    let dataSegments =
        stringLiteralData
        |> List.map (fun (_, offset, bytes) ->
            $"  (data (i32.const {offset}) \"{escapeWasmData bytes}\")")
        |> String.concat "\n"

    let allImports =
        [ if not (String.IsNullOrWhiteSpace dbgPrelude) then dbgPrelude
          if not (String.IsNullOrWhiteSpace contextPrelude) then contextPrelude
          yield! dbgTagImports
          yield! dbgTagPayloadImports ]
        |> List.filter (fun line -> not (String.IsNullOrWhiteSpace line))
        |> String.concat "\n\n"

    if String.IsNullOrWhiteSpace renderedFunctions && String.IsNullOrWhiteSpace allImports && String.IsNullOrWhiteSpace dataSegments then
        "(module)"
    else
        let parts =
            [ if not (String.IsNullOrWhiteSpace allImports) then allImports
              if not (String.IsNullOrWhiteSpace dataSegments) then dataSegments
              if not (String.IsNullOrWhiteSpace renderedFunctions) then renderedFunctions ]
        let body = String.concat "\n\n" parts
        $"(module\n{body}\n)"
