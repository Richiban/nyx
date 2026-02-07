module NyxCompiler.Infer

open Parser.Program
open NyxCompiler

type TypedExpr = NyxCompiler.TypedExpr
type TypedStatement = NyxCompiler.TypedStatement

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

let private mkTypedExpr expr ty body statements matchArms : TypedExpr =
    { Expr = expr; Type = ty; Body = body; Statements = statements; MatchArms = matchArms }

let rec private inferExpr (env: TypeEnv) (state: InferState) (expr: Expression) : Result<TypedExpr * InferState, Diagnostic list> =
    match expr with
    | LiteralExpr literal ->
        Ok (mkTypedExpr expr (literalType literal) None None None, state)
    | IdentifierExpr name ->
        match TypeEnv.tryFind name env with
        | Some scheme ->
            let ty, next = instantiate state scheme
            Ok (mkTypedExpr expr ty None None None, next)
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
                let argTys =
                    argResults
                    |> List.choose (function Ok typedExpr -> Some typedExpr.Type | _ -> None)
                let retTy, nextState = freshVar current
                let withConstraint = addConstraint funcTy (TyFunc(argTys, retTy)) nextState
                Ok (mkTypedExpr expr retTy None None None, withConstraint)
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
        | Ok (bodyExpr, next) ->
            Ok (mkTypedExpr expr (TyFunc(argTys, bodyExpr.Type)) (Some bodyExpr) None None, next)
        | Error err -> Error err
    | BinaryOp(_, left, right) ->
        match inferExpr env state left with
        | Error err -> Error err
        | Ok (leftExpr, next) ->
            match inferExpr env next right with
            | Error err -> Error err
            | Ok (rightExpr, nextState) ->
                let constrained = addConstraint leftExpr.Type rightExpr.Type nextState
                Ok (mkTypedExpr expr leftExpr.Type None None None, constrained)
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
                | Ok (typedExpr, next) ->
                    current <- next
                    Ok typedExpr
                | Error err -> Error err)
        let errors = tys |> List.choose (function Error err -> Some err | _ -> None) |> List.concat
        if errors.IsEmpty then
            let itemExprs = tys |> List.choose (function Ok ty -> Some ty | _ -> None)
            let itemTys = itemExprs |> List.map (fun typedExpr -> typedExpr.Type)
            Ok (mkTypedExpr expr (TyTuple itemTys) None None None, current)
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
                    | Ok (typedExpr, next) ->
                        current <- next
                        Ok (name, typedExpr)
                    | Error err -> Error err)
            let errors = mapped |> List.choose (function Error err -> Some err | _ -> None) |> List.concat
            if errors.IsEmpty then
                let fieldMap =
                    mapped
                    |> List.choose (function Ok (name, typedExpr) -> Some (name, typedExpr.Type) | _ -> None)
                    |> Map.ofList
                Ok (mkTypedExpr expr (TyRecord fieldMap) None None None, current)
            else
                Error errors
        | positionalFields, [] ->
            let mutable current = state
            let itemResults =
                positionalFields
                |> List.rev
                |> List.map (fun expr ->
                    match inferExpr env current expr with
                    | Ok (typedExpr, next) ->
                        current <- next
                        Ok typedExpr
                    | Error err -> Error err)
            let errors = itemResults |> List.choose (function Error err -> Some err | _ -> None) |> List.concat
            if errors.IsEmpty then
                let itemExprs = itemResults |> List.choose (function Ok ty -> Some ty | _ -> None)
                let itemTys = itemExprs |> List.map (fun typedExpr -> typedExpr.Type)
                Ok (mkTypedExpr expr (TyTuple itemTys) None None None, current)
            else
                Error errors
        | _ -> Error [ Diagnostics.error "Mixed positional and named record fields are not supported yet" ]
    | ListExpr items ->
        let mutable current = state
        let itemResults =
            items
            |> List.map (fun item ->
                match inferExpr env current item with
                | Ok (typedExpr, next) ->
                    current <- next
                    Ok typedExpr
                | Error err -> Error err)
        let errors = itemResults |> List.choose (function Error err -> Some err | _ -> None) |> List.concat
        if errors.IsEmpty then
            match itemResults |> List.choose (function Ok ty -> Some ty | _ -> None) with
            | [] -> Ok (mkTypedExpr expr (TyPrimitive "list") None None None, current)
            | head :: tail ->
                let constrained = tail |> List.fold (fun st ty -> addConstraint head.Type ty.Type st) current
                Ok (mkTypedExpr expr (TyPrimitive "list") None None None, constrained)
        else
            Error errors
    | TagExpr(name, payloadOpt) ->
        match payloadOpt with
        | None -> Ok (mkTypedExpr expr (TyTag(name, None)) None None None, state)
        | Some payload ->
            match inferExpr env state payload with
            | Ok (payloadExpr, next) -> Ok (mkTypedExpr expr (TyTag(name, Some payloadExpr.Type)) None None None, next)
            | Error err -> Error err
    | IfExpr(condition, thenExpr, elseExpr) ->
        match inferExpr env state condition with
        | Error err -> Error err
        | Ok (condExpr, next) ->
            let withConstraint = addConstraint condExpr.Type (primitiveType "bool") next
            match inferExpr env withConstraint thenExpr with
            | Error err -> Error err
            | Ok (thenExprTyped, nextState) ->
                match inferExpr env nextState elseExpr with
                | Error err -> Error err
                | Ok (elseExprTyped, finalState) ->
                    let constrained = addConstraint thenExprTyped.Type elseExprTyped.Type finalState
                    Ok (mkTypedExpr expr thenExprTyped.Type None None None, constrained)
    | Match(scrutinees, arms) ->
        let mutable current = state
        let scrutineeResults =
            scrutinees
            |> List.map (fun expr ->
                match inferExpr env current expr with
                | Ok (typedExpr, next) ->
                    current <- next
                    Ok typedExpr
                | Error err -> Error err)
        let scrutineeErrors = scrutineeResults |> List.choose (function Error err -> Some err | _ -> None) |> List.concat
        if not scrutineeErrors.IsEmpty then
            Error scrutineeErrors
        else
            let scrutineeExprs = scrutineeResults |> List.choose (function Ok ty -> Some ty | _ -> None)
            let scrutineeTypes = scrutineeExprs |> List.map (fun typedExpr -> typedExpr.Type)
            let mkTypedPattern pattern ty = { Pattern = pattern; Type = ty }
            let rec inferPattern (env': TypeEnv) (state': InferState) (scrutineeTy: Ty) (pattern: Pattern) =
                match pattern with
                | WildcardPattern
                | ElsePattern -> Ok (env', state', mkTypedPattern pattern scrutineeTy)
                | IdentifierPattern name ->
                    let scheme = TypeEnv.mono scrutineeTy
                    Ok (TypeEnv.extend name scheme env', state', mkTypedPattern pattern scrutineeTy)
                | LiteralPattern literal ->
                    let constrained = addConstraint scrutineeTy (literalType literal) state'
                    Ok (env', constrained, mkTypedPattern pattern scrutineeTy)
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
                    let mutable innerPatterns: TypedPattern list = []
                    for (pat, ty) in List.zip patterns itemTypes do
                        match inferPattern innerEnv innerState ty pat with
                        | Ok (nextEnv, nextState, typedPattern) ->
                            innerEnv <- nextEnv
                            innerState <- nextState
                            innerPatterns <- innerPatterns @ [typedPattern]
                        | Error err -> innerErrors <- err
                    if innerErrors.IsEmpty then
                        Ok (innerEnv, innerState, mkTypedPattern pattern (TyTuple itemTypes))
                    else
                        Error innerErrors
                | TagPattern(tagName, payloadOpt) ->
                    match payloadOpt with
                    | None ->
                        let constrained = addConstraint scrutineeTy (TyTag(tagName, None)) state'
                        Ok (env', constrained, mkTypedPattern pattern scrutineeTy)
                    | Some payload ->
                        let payloadTy, nextState = freshVar state'
                        let constrained = addConstraint scrutineeTy (TyTag(tagName, Some payloadTy)) nextState
                        match inferPattern env' constrained payloadTy payload with
                        | Ok (nextEnv, nextState, _) ->
                            Ok (nextEnv, nextState, mkTypedPattern pattern scrutineeTy)
                        | Error err -> Error err
                | _ -> Ok (env', state', mkTypedPattern pattern scrutineeTy)

            let mutable armExprs: TypedExpr list = []
            let mutable typedArms: TypedMatchArm list = []
            let mutable errors: Diagnostic list = []
            for (patterns, expr) in arms do
                if errors.IsEmpty then
                    let mutable env' = env
                    let mutable state' = current
                    let patternPairs = List.zip patterns scrutineeTypes
                    let mutable typedPatterns: TypedPattern list = []
                    for (pattern, scrutineeTy) in patternPairs do
                        match inferPattern env' state' scrutineeTy pattern with
                        | Ok (nextEnv, nextState, typedPattern) ->
                            env' <- nextEnv
                            state' <- nextState
                            typedPatterns <- typedPatterns @ [typedPattern]
                        | Error err -> errors <- err
                    if errors.IsEmpty then
                        match inferExpr env' state' expr with
                        | Ok (typedExpr, next) ->
                            current <- next
                            armExprs <- typedExpr :: armExprs
                            typedArms <- typedArms @ [ (typedPatterns, typedExpr) ]
                        | Error err -> errors <- err
            if errors.IsEmpty then
                match List.rev armExprs with
                | [] -> Ok (mkTypedExpr expr (TyPrimitive "unit") None None (Some typedArms), current)
                | head :: tail ->
                    let constrained = tail |> List.fold (fun st typedExpr -> addConstraint head.Type typedExpr.Type st) current
                    Ok (mkTypedExpr expr head.Type None None (Some typedArms), constrained)
            else
                Error errors
    | MemberAccess(_, _) ->
        let ty, next = freshVar state
        Ok (mkTypedExpr expr ty None None None, next)

and inferBlock (env: TypeEnv) (state: InferState) (statements: Statement list) : Result<TypedExpr * InferState, Diagnostic list> =
    match statements with
    | [] -> Ok (mkTypedExpr (Block []) (TyPrimitive "unit") None (Some []) None, state)
    | _ ->
        let mutable current = state
        let mutable env' = env
        let mutable lastExpr = mkTypedExpr (LiteralExpr (IntLit 0)) (TyPrimitive "unit") None None None
        let mutable typedStatements: TypedStatement list = []
        let mutable errors: Diagnostic list = []
        for statement in statements do
            match statement with
            | DefStatement(name, typeOpt, expr) ->
                if errors.IsEmpty then
                    match inferExpr env' current expr with
                    | Ok (typedExpr, next) ->
                        let declaredTy, nextState =
                            match typeOpt with
                            | Some typeExpr ->
                                let ty, st = typeExprToTy next typeExpr
                                ty, addConstraint typedExpr.Type ty st
                            | None -> typedExpr.Type, next
                        current <- nextState
                        let scheme = TypeEnv.generalize env' declaredTy
                        env' <- TypeEnv.extend name scheme env'
                        typedStatements <- typedStatements @ [ TypedDefStatement(name, typeOpt, typedExpr) ]
                        lastExpr <- typedExpr
                    | Error err -> errors <- err
            | ImportStatement items ->
                typedStatements <- typedStatements @ [ TypedImportStatement items ]
            | TypeDefStatement(name, modifiers, parameters, body) ->
                typedStatements <- typedStatements @ [ TypedTypeDefStatement(name, modifiers, parameters, body) ]
            | ExprStatement expr ->
                if errors.IsEmpty then
                    match inferExpr env' current expr with
                    | Ok (typedExpr, next) ->
                        current <- next
                        typedStatements <- typedStatements @ [ TypedExprStatement typedExpr ]
                        lastExpr <- typedExpr
                    | Error err -> errors <- err
        if errors.IsEmpty then
            let blockExpr = mkTypedExpr (Block statements) lastExpr.Type (Some lastExpr) (Some typedStatements) None
            Ok (blockExpr, current)
        else
            Error errors

let inferModule (module': Module) : Result<Map<string, Ty> * TypedTopLevelItem list * InferState, Diagnostic list> =
    let mutable env = TypeEnv.empty
    let mutable state = emptyState
    let mutable types = Map.empty
    let mutable items: TypedTopLevelItem list = []
    let mutable errors: Diagnostic list = []

    for item in module' do
        match item with
        | Def (ValueDef(name, typeOpt, expr)) when errors.IsEmpty ->
            match inferExpr env state expr with
            | Ok (typedExpr, next) ->
                let declaredTy, nextState =
                    match typeOpt with
                    | Some typeExpr ->
                        let ty, st = typeExprToTy next typeExpr
                        ty, addConstraint typedExpr.Type ty st
                    | None -> typedExpr.Type, next
                state <- nextState
                let scheme = TypeEnv.generalize env declaredTy
                env <- TypeEnv.extend name scheme env
                types <- types |> Map.add name declaredTy
                items <- items @ [ TypedDef (TypedValueDef(name, typeOpt, typedExpr)) ]
            | Error err -> errors <- err
        | Expr expr when errors.IsEmpty ->
            match inferExpr env state expr with
            | Ok (typedExpr, next) ->
                state <- next
                items <- items @ [ TypedExprItem typedExpr ]
            | Error err -> errors <- err
        | Expr _ -> ()
        | Import itemsList -> items <- items @ [ TypedImport itemsList ]
        | ModuleDecl name -> items <- items @ [ TypedModuleDecl name ]
        | Def (TypeDef(name, modifiers, parameters, body)) ->
            items <- items @ [ TypedDef (TypedTypeDef(name, modifiers, parameters, body)) ]
        | Def (ValueDef _) -> ()

    if errors.IsEmpty then Ok (types, items, state) else Error errors
