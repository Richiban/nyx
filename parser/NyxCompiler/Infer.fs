module NyxCompiler.Infer

open Parser.Program

type InferState =
    { NextId: int
      Constraints: ConstraintSet
      TypeVars: Map<string, TyVar> }

let emptyState =
    { NextId = 0
      Constraints = []
      TypeVars = Map.empty }

let private freshVar (state: InferState) =
    let var = { Id = state.NextId; Name = None }
    TyVar var, { state with NextId = state.NextId + 1 }

let private addConstraint left right (state: InferState) =
    { state with Constraints = (left, right) :: state.Constraints }

let private primitiveType name =
    TyPrimitive name

let private literalType literal =
    match literal with
    | StringLit _ -> primitiveType "string"
    | IntLit _ -> primitiveType "int"
    | FloatLit _ -> primitiveType "float"
    | BoolLit _ -> primitiveType "bool"

let private lookupPrimitive (name: string) =
    match name with
    | "int" | "Int" -> Some (primitiveType "int")
    | "float" | "Float" -> Some (primitiveType "float")
    | "bool" | "Bool" -> Some (primitiveType "bool")
    | "string" | "String" -> Some (primitiveType "string")
    | "unit" | "Unit" -> Some (primitiveType "unit")
    | _ -> None

let rec private typeExprToTy (state: InferState) (typeExpr: TypeExpr) : Ty * InferState =
    match typeExpr with
    | TypeName name ->
        match lookupPrimitive name with
        | Some prim -> prim, state
        | None -> TyPrimitive name, state
    | TypeVar name ->
        match state.TypeVars |> Map.tryFind name with
        | Some existing -> TyVar existing, state
        | None ->
            let tyVar, next = freshVar state
            match tyVar with
            | TyVar var -> tyVar, { next with TypeVars = next.TypeVars |> Map.add name var }
            | _ -> tyVar, next
    | TypeUnit -> TyPrimitive "unit", state
    | TypeTuple items ->
        let mutable current = state
        let tys =
            items
            |> List.map (fun item ->
                let ty, next = typeExprToTy current item
                current <- next
                ty)
        TyTuple tys, current
    | TypeRecord fields ->
        let mutable current = state
        let fieldMap =
            fields
            |> List.choose (function
                | TypeField(name, _, _, typeOpt, _) -> Some (name, typeOpt)
                | TypeMember(name, typeOpt) -> Some (name, typeOpt))
            |> List.map (fun (name, typeOpt) ->
                let ty, next =
                    match typeOpt with
                    | Some t -> typeExprToTy current t
                    | None -> freshVar current
                current <- next
                name, ty)
            |> Map.ofList
        TyRecord fieldMap, current
    | TypeApply(name, args) ->
        let mutable current = state
        let argTys =
            args
            |> List.map (fun arg ->
                let ty, next = typeExprToTy current arg
                current <- next
                ty)
        TyUnion (TyPrimitive name :: argTys), current
    | TypeFunc(args, ret) ->
        let mutable current = state
        let argTys =
            args
            |> List.map (fun arg ->
                let ty, next = typeExprToTy current arg
                current <- next
                ty)
        let retTy, next = typeExprToTy current ret
        TyFunc(argTys, retTy), next
    | TypeTag(name, payloadOpt) ->
        match payloadOpt with
        | Some payload ->
            let payloadTy, next = typeExprToTy state payload
            TyTag(name, Some payloadTy), next
        | None -> TyTag(name, None), state
    | TypeUnion items ->
        let mutable current = state
        let tys =
            items
            |> List.map (fun item ->
                let ty, next = typeExprToTy current item
                current <- next
                ty)
        TyUnion tys, current
    | TypeIntersection items ->
        let mutable current = state
        let tys =
            items
            |> List.map (fun item ->
                let ty, next = typeExprToTy current item
                current <- next
                ty)
        TyUnion tys, current
    | TypeOptional inner ->
        let innerTy, next = typeExprToTy state inner
        TyUnion [ innerTy; TyTag("none", None) ], next
    | TypeConstraint(_, baseType, _) ->
        typeExprToTy state baseType
    | TypeWhere(baseType, _) ->
        typeExprToTy state baseType

let private instantiate (state: InferState) (scheme: TypeScheme) : Ty * InferState =
    if scheme.Quantified.IsEmpty then
        scheme.Type, state
    else
        let mutable current = state
        let substitutions =
            scheme.Quantified
            |> Seq.fold (fun subst id ->
                let tyVar, next = freshVar current
                current <- next
                subst |> Map.add id tyVar) Map.empty
        Unifier.apply substitutions scheme.Type, current

let rec private inferExpr (env: TypeEnv) (state: InferState) (expr: Expression) : Result<Ty * InferState, Diagnostic list> =
    match expr with
    | LiteralExpr literal ->
        Ok (literalType literal, state)
    | IdentifierExpr name ->
        match TypeEnv.tryFind name env with
        | Some scheme ->
            let ty, next = instantiate state scheme
            Ok (ty, next)
        | None -> Error [ Diagnostics.error ($"Unknown identifier '{name}'") ]
    | FunctionCall(name, args) ->
        match TypeEnv.tryFind name env with
        | None -> Error [ Diagnostics.error ($"Unknown function '{name}'") ]
        | Some scheme ->
            let mutable current = state
            let funcTy, nextState = instantiate current scheme
            current <- nextState
            let argResults =
                args
                |> List.map (fun arg ->
                    match inferExpr env current arg with
                    | Ok (ty, next) ->
                        current <- next
                        Ok ty
                    | Error err -> Error err)
            let errors = argResults |> List.choose (function Error err -> Some err | _ -> None) |> List.concat
            if errors.IsEmpty then
                let argTys = argResults |> List.choose (function Ok ty -> Some ty | _ -> None)
                let retTy, nextState = freshVar current
                let withConstraint = addConstraint funcTy (TyFunc(argTys, retTy)) nextState
                Ok (retTy, withConstraint)
            else
                Error errors
    | Lambda(args, body) ->
        let mutable current = state
        let mutable env' = env
        let argTys =
            args
            |> List.map (fun (name, typeOpt) ->
                let ty, next =
                    match typeOpt with
                    | Some typeExpr -> typeExprToTy current typeExpr
                    | None -> freshVar current
                current <- next
                env' <- TypeEnv.extend name (TypeEnv.mono ty) env'
                ty)
        match inferExpr env' current body with
        | Ok (bodyTy, next) -> Ok (TyFunc(argTys, bodyTy), next)
        | Error err -> Error err
    | BinaryOp(_, left, right) ->
        match inferExpr env state left with
        | Error err -> Error err
        | Ok (leftTy, next) ->
            match inferExpr env next right with
            | Error err -> Error err
            | Ok (rightTy, nextState) ->
                let constrained = addConstraint leftTy rightTy nextState
                Ok (leftTy, constrained)
    | Pipe(value, name, args) ->
        let callArgs = value :: args
        inferExpr env state (FunctionCall(name, callArgs))
    | Block statements ->
        inferBlock env state statements
    | TupleExpr items ->
        let mutable current = state
        let tys =
            items
            |> List.map (fun item ->
                match inferExpr env current item with
                | Ok (ty, next) ->
                    current <- next
                    Ok ty
                | Error err -> Error err)
        let errors = tys |> List.choose (function Error err -> Some err | _ -> None) |> List.concat
        if errors.IsEmpty then
            let itemTys = tys |> List.choose (function Ok ty -> Some ty | _ -> None)
            Ok (TyTuple itemTys, current)
        else
            Error errors
    | RecordExpr fields ->
        let positional, named =
            fields
            |> List.fold (fun (pos, named) field ->
                match field with
                | PositionalField expr -> (expr :: pos, named)
                | NamedField(name, expr) -> (pos, (name, expr) :: named)) ([], [])
        match positional, named with
        | [], namedFields ->
            let mutable current = state
            let mapped =
                namedFields
                |> List.rev
                |> List.map (fun (name, expr) ->
                    match inferExpr env current expr with
                    | Ok (ty, next) ->
                        current <- next
                        Ok (name, ty)
                    | Error err -> Error err)
            let errors = mapped |> List.choose (function Error err -> Some err | _ -> None) |> List.concat
            if errors.IsEmpty then
                let fieldMap = mapped |> List.choose (function Ok item -> Some item | _ -> None) |> Map.ofList
                Ok (TyRecord fieldMap, current)
            else
                Error errors
        | positionalFields, [] ->
            let mutable current = state
            let itemResults =
                positionalFields
                |> List.rev
                |> List.map (fun expr ->
                    match inferExpr env current expr with
                    | Ok (ty, next) ->
                        current <- next
                        Ok ty
                    | Error err -> Error err)
            let errors = itemResults |> List.choose (function Error err -> Some err | _ -> None) |> List.concat
            if errors.IsEmpty then
                let itemTys = itemResults |> List.choose (function Ok ty -> Some ty | _ -> None)
                Ok (TyTuple itemTys, current)
            else
                Error errors
        | _ -> Error [ Diagnostics.error "Mixed positional and named record fields are not supported yet" ]
    | ListExpr items ->
        let mutable current = state
        let itemResults =
            items
            |> List.map (fun item ->
                match inferExpr env current item with
                | Ok (ty, next) ->
                    current <- next
                    Ok ty
                | Error err -> Error err)
        let errors = itemResults |> List.choose (function Error err -> Some err | _ -> None) |> List.concat
        if errors.IsEmpty then
            match itemResults |> List.choose (function Ok ty -> Some ty | _ -> None) with
            | [] -> Ok (TyPrimitive "list", current)
            | head :: tail ->
                let constrained = tail |> List.fold (fun st ty -> addConstraint head ty st) current
                Ok (TyPrimitive "list", constrained)
        else
            Error errors
    | TagExpr(name, payloadOpt) ->
        match payloadOpt with
        | None -> Ok (TyTag(name, None), state)
        | Some payload ->
            match inferExpr env state payload with
            | Ok (payloadTy, next) -> Ok (TyTag(name, Some payloadTy), next)
            | Error err -> Error err
    | IfExpr(condition, thenExpr, elseExpr) ->
        match inferExpr env state condition with
        | Error err -> Error err
        | Ok (condTy, next) ->
            let withConstraint = addConstraint condTy (primitiveType "bool") next
            match inferExpr env withConstraint thenExpr with
            | Error err -> Error err
            | Ok (thenTy, nextState) ->
                match inferExpr env nextState elseExpr with
                | Error err -> Error err
                | Ok (elseTy, finalState) ->
                    let constrained = addConstraint thenTy elseTy finalState
                    Ok (thenTy, constrained)
    | Match(scrutinees, arms) ->
        let mutable current = state
        let scrutineeResults =
            scrutinees
            |> List.map (fun expr ->
                match inferExpr env current expr with
                | Ok (ty, next) ->
                    current <- next
                    Ok ty
                | Error err -> Error err)
        let scrutineeErrors = scrutineeResults |> List.choose (function Error err -> Some err | _ -> None) |> List.concat
        if not scrutineeErrors.IsEmpty then
            Error scrutineeErrors
        else
            let scrutineeTypes = scrutineeResults |> List.choose (function Ok ty -> Some ty | _ -> None)
            let rec inferPattern (env': TypeEnv) (state': InferState) (scrutineeTy: Ty) (pattern: Pattern) =
                match pattern with
                | WildcardPattern
                | ElsePattern -> Ok (env', state')
                | IdentifierPattern name ->
                    let scheme = TypeEnv.mono scrutineeTy
                    Ok (TypeEnv.extend name scheme env', state')
                | LiteralPattern literal ->
                    let constrained = addConstraint scrutineeTy (literalType literal) state'
                    Ok (env', constrained)
                | TuplePattern patterns ->
                    let mutable currentState = state'
                    let itemTypes, nextState =
                        patterns
                        |> List.map (fun _ ->
                            let ty, next = freshVar currentState
                            currentState <- next
                            ty)
                        |> fun tys -> tys, currentState
                    let constrained = addConstraint scrutineeTy (TyTuple itemTypes) nextState
                    let mutable innerEnv = env'
                    let mutable innerState = constrained
                    let mutable innerErrors: Diagnostic list = []
                    for (pat, ty) in List.zip patterns itemTypes do
                        match inferPattern innerEnv innerState ty pat with
                        | Ok (nextEnv, nextState) ->
                            innerEnv <- nextEnv
                            innerState <- nextState
                        | Error err -> innerErrors <- err
                    if innerErrors.IsEmpty then Ok (innerEnv, innerState) else Error innerErrors
                | TagPattern(tagName, payloadOpt) ->
                    match payloadOpt with
                    | None ->
                        let constrained = addConstraint scrutineeTy (TyTag(tagName, None)) state'
                        Ok (env', constrained)
                    | Some payload ->
                        let payloadTy, nextState = freshVar state'
                        let constrained = addConstraint scrutineeTy (TyTag(tagName, Some payloadTy)) nextState
                        inferPattern env' constrained payloadTy payload
                | _ -> Ok (env', state')

            let mutable armTypes: Ty list = []
            let mutable errors: Diagnostic list = []
            for (patterns, expr) in arms do
                if errors.IsEmpty then
                    let mutable env' = env
                    let mutable state' = current
                    let patternPairs = List.zip patterns scrutineeTypes
                    for (pattern, scrutineeTy) in patternPairs do
                        match inferPattern env' state' scrutineeTy pattern with
                        | Ok (nextEnv, nextState) ->
                            env' <- nextEnv
                            state' <- nextState
                        | Error err -> errors <- err
                    if errors.IsEmpty then
                        match inferExpr env' state' expr with
                        | Ok (ty, next) ->
                            current <- next
                            armTypes <- ty :: armTypes
                        | Error err -> errors <- err
            if errors.IsEmpty then
                match List.rev armTypes with
                | [] -> Ok (TyPrimitive "unit", current)
                | head :: tail ->
                    let constrained = tail |> List.fold (fun st ty -> addConstraint head ty st) current
                    Ok (head, constrained)
            else
                Error errors
    | MemberAccess(_, _) ->
        let ty, next = freshVar state
        Ok (ty, next)

and inferBlock (env: TypeEnv) (state: InferState) (statements: Statement list) : Result<Ty * InferState, Diagnostic list> =
    match statements with
    | [] -> Ok (TyPrimitive "unit", state)
    | _ ->
        let mutable current = state
        let mutable env' = env
        let mutable lastTy = TyPrimitive "unit"
        let mutable errors: Diagnostic list = []
        for statement in statements do
            match statement with
            | DefStatement(name, typeOpt, expr) ->
                if errors.IsEmpty then
                    match inferExpr env' current expr with
                    | Ok (exprTy, next) ->
                        let declaredTy, nextState =
                            match typeOpt with
                            | Some typeExpr ->
                                let ty, st = typeExprToTy next typeExpr
                                ty, addConstraint exprTy ty st
                            | None -> exprTy, next
                        current <- nextState
                        let scheme = TypeEnv.generalize env' declaredTy
                        env' <- TypeEnv.extend name scheme env'
                        lastTy <- declaredTy
                    | Error err -> errors <- err
            | ImportStatement _ -> ()
            | TypeDefStatement _ -> ()
            | ExprStatement expr ->
                if errors.IsEmpty then
                    match inferExpr env' current expr with
                    | Ok (exprTy, next) ->
                        current <- next
                        lastTy <- exprTy
                    | Error err -> errors <- err
        if errors.IsEmpty then Ok (lastTy, current) else Error errors

let inferModule (module': Module) : Result<Map<string, Ty> * InferState, Diagnostic list> =
    let mutable env = TypeEnv.empty
    let mutable state = emptyState
    let mutable types = Map.empty
    let mutable errors: Diagnostic list = []

    for item in module' do
        match item with
        | Def (ValueDef(name, typeOpt, expr)) when errors.IsEmpty ->
            match inferExpr env state expr with
            | Ok (exprTy, next) ->
                let declaredTy, nextState =
                    match typeOpt with
                    | Some typeExpr ->
                        let ty, st = typeExprToTy next typeExpr
                        ty, addConstraint exprTy ty st
                    | None -> exprTy, next
                state <- nextState
                let scheme = TypeEnv.generalize env declaredTy
                env <- TypeEnv.extend name scheme env
                types <- types |> Map.add name declaredTy
            | Error err -> errors <- err
        | Expr expr when errors.IsEmpty ->
            match inferExpr env state expr with
            | Ok (_, next) -> state <- next
            | Error err -> errors <- err
        | Expr _ -> ()
        | Import _ -> ()
        | ModuleDecl _ -> ()
        | Def (TypeDef _) -> ()
        | Def (ValueDef _) -> ()

    if errors.IsEmpty then Ok (types, state) else Error errors
