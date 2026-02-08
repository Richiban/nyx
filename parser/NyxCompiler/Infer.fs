module NyxCompiler.Infer

open Parser.Program
open NyxCompiler

type TypedExpr = NyxCompiler.TypedExpr
type TypedStatement = NyxCompiler.TypedStatement

type InferState = { NextId: int; Constraints: ConstraintSet; TypeVars: Map<string, TyVar>; TypeDefs: Map<string, Ty * bool> }

let emptyState = { NextId = 0; Constraints = []; TypeVars = Map.empty; TypeDefs = Map.empty }

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

let private listType elementTy =
    TyTag("list", Some elementTy)

let private numericType =
    TyPrimitive "int"

let private tupleFieldName index =
    $"item{index}"

let private tupleRecordType (items: Ty list) =
    items
    |> List.mapi (fun index ty -> tupleFieldName (index + 1), ty)
    |> Map.ofList
    |> TyRecord

let private inputTypeFromArgs argTys =
    match argTys with
    | [] -> TyPrimitive "unit"
    | [ single ] -> single
    | _ -> tupleRecordType argTys

let private isPrivateType modifiers =
    modifiers |> List.exists (fun modifier -> modifier = TypeDefModifier.Private)


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
        | None ->
            match state.TypeDefs |> Map.tryFind name with
            | Some (underlying, isPrivate) -> TyNominal(name, underlying, isPrivate), state
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
        tupleRecordType tys, current
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
    | TypeFunc(arg, ret) ->
        let argTy, next = typeExprToTy state arg
        let retTy, nextState = typeExprToTy next ret
        TyFunc(argTy, retTy), nextState
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

let private registerTypeDef (state: InferState) (name: Identifier) (modifiers: TypeDefModifier list) (body: TypeExpr) =
    if name.StartsWith("@") then
        let underlyingTy, next = typeExprToTy state body
        let isPrivate = isPrivateType modifiers
        { next with TypeDefs = next.TypeDefs |> Map.add name (underlyingTy, isPrivate) }
    else
        state

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

let private argTypesFromExpected (state: InferState) (args: (Identifier * TypeExpr option) list) (expectedInput: Ty) =
    let mutable current = state
    let argTys =
        match expectedInput with
        | TyRecord fields ->
            args
            |> List.mapi (fun index (name, _) ->
                match fields |> Map.tryFind name with
                | Some ty -> ty
                | None ->
                    let tupleName = tupleFieldName (index + 1)
                    match fields |> Map.tryFind tupleName with
                    | Some ty -> ty
                    | None ->
                        let ty, next = freshVar current
                        current <- next
                        ty)
        | _ when args.Length = 1 -> [ expectedInput ]
        | _ ->
            args
            |> List.map (fun _ ->
                let ty, next = freshVar current
                current <- next
                ty)
    argTys, current


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
                let inputTy = inputTypeFromArgs argTys
                let withConstraint = addConstraint funcTy (TyFunc(inputTy, retTy)) nextState
                Ok (mkTypedExpr expr retTy None None None, withConstraint)
            else
                Error errors
    | Lambda(args, body) ->
        inferLambdaWithExpected env state args body None None
    | BinaryOp(_, left, right) ->
        match inferExpr env state left with
        | Error err -> Error err
        | Ok (leftExpr, next) ->
            match inferExpr env next right with
            | Error err -> Error err
            | Ok (rightExpr, nextState) ->
                let constrained = addConstraint leftExpr.Type rightExpr.Type nextState
                match expr with
                | BinaryOp(op, _, _) when ["+"; "-"; "*"; "/"] |> List.contains op ->
                    let numericConstrained =
                        constrained
                        |> addConstraint leftExpr.Type numericType
                    Ok (mkTypedExpr expr leftExpr.Type None None None, numericConstrained)
                | BinaryOp(op, _, _) when ["<"; ">"; "<="; ">="] |> List.contains op ->
                    let numericConstrained =
                        constrained
                        |> addConstraint leftExpr.Type numericType
                    Ok (mkTypedExpr expr (TyPrimitive "bool") None None None, numericConstrained)
                | BinaryOp(op, _, _) when ["=="; "!="] |> List.contains op ->
                    Ok (mkTypedExpr expr (TyPrimitive "bool") None None None, constrained)
                | _ -> Ok (mkTypedExpr expr leftExpr.Type None None None, constrained)
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
            Ok (mkTypedExpr expr (tupleRecordType itemTys) None None None, current)
        else
            Error errors
    | RecordExpr fields ->
        let positional, named =
            fields
            |> List.fold (fun (pos, named) field ->
                match field with
                | PositionalField expr -> (expr :: pos, named)
                | NamedField(name, expr) -> (pos, (name, expr) :: named)) ([], [])
        let mutable current = state
        let positionalResults =
            positional
            |> List.rev
            |> List.map (fun expr ->
                match inferExpr env current expr with
                | Ok (typedExpr, next) ->
                    current <- next
                    Ok typedExpr
                | Error err -> Error err)
        let namedResults =
            named
            |> List.rev
            |> List.map (fun (name, expr) ->
                match inferExpr env current expr with
                | Ok (typedExpr, next) ->
                    current <- next
                    Ok (name, typedExpr)
                | Error err -> Error err)
        let errors =
            positionalResults
            |> List.choose (function Error err -> Some err | _ -> None)
            |> List.concat
            |> fun errs ->
                let namedErrors = namedResults |> List.choose (function Error err -> Some err | _ -> None) |> List.concat
                errs @ namedErrors
        if errors.IsEmpty then
            let positionalExprs = positionalResults |> List.choose (function Ok ty -> Some ty | _ -> None)
            let positionalFieldsMap =
                positionalExprs
                |> List.mapi (fun index typedExpr -> tupleFieldName (index + 1), typedExpr.Type)
            let namedFieldsMap =
                namedResults
                |> List.choose (function Ok (name, typedExpr) -> Some (name, typedExpr.Type) | _ -> None)
            let fieldMap =
                positionalFieldsMap @ namedFieldsMap
                |> Map.ofList
            Ok (mkTypedExpr expr (TyRecord fieldMap) None None None, current)
        else
            Error errors
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
            | [] ->
                let elementTy, nextState = freshVar current
                Ok (mkTypedExpr expr (listType elementTy) None None None, nextState)
            | head :: tail ->
                let constrained = tail |> List.fold (fun st ty -> addConstraint head.Type ty.Type st) current
                Ok (mkTypedExpr expr (listType head.Type) None None None, constrained)
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
                | GuardPattern(_, guardExpr) ->
                    match inferExpr env' state' guardExpr with
                    | Ok (guardTyped, nextState) ->
                        let constrained = addConstraint guardTyped.Type (literalType (BoolLit true)) nextState
                        Ok (env', constrained, mkTypedPattern pattern scrutineeTy)
                    | Error err -> Error err
                | RangePattern(startExpr, endExpr) ->
                    match inferExpr env' state' startExpr with
                    | Error err -> Error err
                    | Ok (startTyped, nextState) ->
                        match inferExpr env' nextState endExpr with
                        | Error err -> Error err
                        | Ok (endTyped, finalState) ->
                            let constrained =
                                finalState
                                |> addConstraint scrutineeTy startTyped.Type
                                |> addConstraint startTyped.Type endTyped.Type
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
                    let tupleRecord = tupleRecordType itemTypes
                    let constrained = addConstraint scrutineeTy tupleRecord nextState
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
                        Ok (innerEnv, innerState, mkTypedPattern pattern tupleRecord)
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
                | ListPattern(patterns, splatOpt) ->
                    let elementTy, nextState = freshVar state'
                    let constrained = addConstraint scrutineeTy (listType elementTy) nextState
                    let mutable innerEnv = env'
                    let mutable innerState = constrained
                    let mutable innerErrors: Diagnostic list = []
                    for pat in patterns do
                        match inferPattern innerEnv innerState elementTy pat with
                        | Ok (nextEnv, nextState, _) ->
                            innerEnv <- nextEnv
                            innerState <- nextState
                        | Error err -> innerErrors <- err
                    let innerStateWithSplat =
                        match splatOpt with
                        | Some (IdentifierPattern name) ->
                            let scheme = TypeEnv.mono (listType elementTy)
                            let updatedEnv = TypeEnv.extend name scheme innerEnv
                            innerEnv <- updatedEnv
                            innerState
                        | _ -> innerState
                    if innerErrors.IsEmpty then
                        Ok (innerEnv, innerStateWithSplat, mkTypedPattern pattern scrutineeTy)
                    else
                        Error innerErrors
                | ListSplatMiddle(prefix, suffix) ->
                    let elementTy, nextState = freshVar state'
                    let constrained = addConstraint scrutineeTy (listType elementTy) nextState
                    let mutable innerEnv = env'
                    let mutable innerState = constrained
                    let mutable innerErrors: Diagnostic list = []
                    for pat in prefix @ suffix do
                        match inferPattern innerEnv innerState elementTy pat with
                        | Ok (nextEnv, nextState, _) ->
                            innerEnv <- nextEnv
                            innerState <- nextState
                        | Error err -> innerErrors <- err
                    if innerErrors.IsEmpty then
                        Ok (innerEnv, innerState, mkTypedPattern pattern scrutineeTy)
                    else
                        Error innerErrors
                | RecordPattern(typeName, patterns) ->
                    let mutable currentState = state'
                    let itemTypes, nextState =
                        patterns
                        |> List.map (fun _ ->
                            let ty, next = freshVar currentState
                            currentState <- next
                            ty)
                        |> fun tys -> tys, currentState
                    let constrained = addConstraint scrutineeTy (TyRecord (Map.empty)) nextState
                    let mutable innerEnv = env'
                    let mutable innerState = constrained
                    let mutable innerErrors: Diagnostic list = []
                    for (pat, ty) in List.zip patterns itemTypes do
                        match inferPattern innerEnv innerState ty pat with
                        | Ok (nextEnv, nextState, _) ->
                            innerEnv <- nextEnv
                            innerState <- nextState
                        | Error err -> innerErrors <- err
                    if innerErrors.IsEmpty then
                        Ok (innerEnv, innerState, mkTypedPattern pattern scrutineeTy)
                    else
                        Error innerErrors
                | RecordMemberPattern members ->
                    let mutable innerEnv = env'
                    let mutable innerState = state'
                    let mutable innerErrors: Diagnostic list = []
                    for (_, pat) in members do
                        let memberTy, nextState = freshVar innerState
                        innerState <- nextState
                        match inferPattern innerEnv innerState memberTy pat with
                        | Ok (nextEnv, nextState, _) ->
                            innerEnv <- nextEnv
                            innerState <- nextState
                        | Error err -> innerErrors <- err
                    if innerErrors.IsEmpty then
                        Ok (innerEnv, innerState, mkTypedPattern pattern scrutineeTy)
                    else
                        Error innerErrors

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
                let hasRestUnion =
                    scrutineeTypes
                    |> List.exists (function
                        | TyUnion items -> items |> List.exists (function TyVar _ -> true | _ -> false)
                        | _ -> false)
                let restError =
                    if hasRestUnion then
                        let hasCatchAll =
                            arms
                            |> List.exists (fun (patterns, _) ->
                                patterns
                                |> List.exists (function
                                    | WildcardPattern
                                    | ElsePattern
                                    | IdentifierPattern _ -> true
                                    | _ -> false))
                        if hasCatchAll then
                            None
                        else
                            Some (Diagnostics.error "Tag union rest parameters require a catch-all match arm")
                    else
                        None
                match restError with
                | Some diag -> Error [ diag ]
                | None ->
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

and inferLambdaWithExpected (env: TypeEnv) (state: InferState) (args: (Identifier * TypeExpr option) list) (body: Expression) (expectedInputOpt: Ty option) (expectedReturnOpt: Ty option) =
    let mutable current = state
    let mutable env' = env
    let argTys =
        match expectedInputOpt with
        | Some expectedInput ->
            let tys, next = argTypesFromExpected current args expectedInput
            current <- next
            tys
        | None ->
            args
            |> List.map (fun (_, typeOpt) ->
                let ty, next =
                    match typeOpt with
                    | Some typeExpr -> typeExprToTy current typeExpr
                    | None -> freshVar current
                current <- next
                ty)
    for ((name, _), ty) in List.zip args argTys do
        env' <- TypeEnv.extend name (TypeEnv.mono ty) env'
    match inferExpr env' current body with
    | Ok (bodyExpr, next) ->
        let withReturnConstraint =
            match expectedReturnOpt with
            | Some expectedReturn -> addConstraint bodyExpr.Type expectedReturn next
            | None -> next
        let inputTy =
            match expectedInputOpt with
            | Some expectedInput -> expectedInput
            | None -> inputTypeFromArgs argTys
        Ok (mkTypedExpr (Lambda(args, body)) (TyFunc(inputTy, bodyExpr.Type)) (Some bodyExpr) None None, withReturnConstraint)
    | Error err -> Error err

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
                    let expectedInputOpt, expectedReturnOpt, expectedDeclaredOpt, nextState =
                        match typeOpt with
                        | Some typeExpr ->
                            let ty, st = typeExprToTy current typeExpr
                            match ty with
                            | TyFunc(inputTy, returnTy) -> Some inputTy, Some returnTy, Some ty, st
                            | _ -> None, None, Some ty, st
                        | None -> None, None, None, current
                    let inferResult =
                        match expr with
                        | Lambda(args, body) when expectedDeclaredOpt.IsSome ->
                            inferLambdaWithExpected env' nextState args body expectedInputOpt expectedReturnOpt
                        | _ -> inferExpr env' nextState expr
                    match inferResult with
                    | Ok (typedExpr, next) ->
                        let declaredTy, finalState =
                            match expectedDeclaredOpt with
                            | Some declaredTy -> declaredTy, addConstraint typedExpr.Type declaredTy next
                            | None -> typedExpr.Type, next
                        current <- finalState
                        let scheme = TypeEnv.generalize env' declaredTy
                        env' <- TypeEnv.extend name scheme env'
                        typedStatements <- typedStatements @ [ TypedDefStatement(name, typeOpt, typedExpr) ]
                        lastExpr <- typedExpr
                    | Error err -> errors <- err
            | ImportStatement items ->
                typedStatements <- typedStatements @ [ TypedImportStatement items ]
            | TypeDefStatement(name, modifiers, parameters, body) ->
                current <- registerTypeDef current name modifiers body
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
            let expectedInputOpt, expectedReturnOpt, expectedDeclaredOpt, nextState =
                match typeOpt with
                | Some typeExpr ->
                    let ty, st = typeExprToTy state typeExpr
                    match ty with
                    | TyFunc(inputTy, returnTy) -> Some inputTy, Some returnTy, Some ty, st
                    | _ -> None, None, Some ty, st
                | None -> None, None, None, state
            let inferResult =
                match expr with
                | Lambda(args, body) when expectedDeclaredOpt.IsSome ->
                    inferLambdaWithExpected env nextState args body expectedInputOpt expectedReturnOpt
                | _ -> inferExpr env nextState expr
            match inferResult with
            | Ok (typedExpr, next) ->
                let declaredTy, finalState =
                    match expectedDeclaredOpt with
                    | Some declaredTy -> declaredTy, addConstraint typedExpr.Type declaredTy next
                    | None -> typedExpr.Type, next
                state <- finalState
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
            state <- registerTypeDef state name modifiers body
            items <- items @ [ TypedDef (TypedTypeDef(name, modifiers, parameters, body)) ]
        | Def (ValueDef _) -> ()

    if errors.IsEmpty then Ok (types, items, state) else Error errors
