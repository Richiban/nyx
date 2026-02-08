namespace NyxCompiler

open System.Collections.Generic

/// Substitution map for type variables.
type Subst = Map<int, Ty>

module Unifier =
    let private tupleFieldName index =
        $"item{index}"

    let private tupleFieldMap items =
        items
        |> List.mapi (fun index ty -> tupleFieldName (index + 1), ty)
        |> Map.ofList

    let private tryTupleLikeRecord (fields: Map<string, Ty>) : Ty list option * (string * Ty) list =
        let tupleFields, namedFields =
            fields
            |> Map.toList
            |> List.fold (fun (tupleAcc, namedAcc) (name, ty) ->
                if name.StartsWith("item") then
                    let indexStr = name.Substring(4)
                    match System.Int32.TryParse(indexStr) with
                    | true, index when index > 0 ->
                        ((index, ty) :: tupleAcc, namedAcc)
                    | _ -> (tupleAcc, (name, ty) :: namedAcc)
                else
                    (tupleAcc, (name, ty) :: namedAcc)) ([], [])
        let orderedTuple = tupleFields |> List.sortBy fst
        let expectedIndices = [1 .. orderedTuple.Length]
        let hasAllIndices =
            orderedTuple
            |> List.map fst
            |> fun indices -> indices = expectedIndices
        if hasAllIndices then
            let tupleItems = orderedTuple |> List.map snd
            Some tupleItems, namedFields
        else
            None, namedFields

    let rec private tyToString (ty: Ty) : string =
        match ty with
        | TyPrimitive name -> name
        | TyVar v ->
            match v.Name with
            | Some name -> $"'{name}"
            | None -> $"'t{v.Id}"
        | TyFunc(arg, ret) -> $"{tyToString arg} -> {tyToString ret}"
        | TyTuple items ->
            items
            |> List.map tyToString
            |> String.concat ", "
            |> fun inner -> $"({inner})"
        | TyRecord fields ->
            match tryTupleLikeRecord fields with
            | Some tupleItems, namedFields ->
                let tupleText =
                    tupleItems
                    |> List.map tyToString
                    |> String.concat ", "
                let namedText =
                    namedFields
                    |> List.sortBy fst
                    |> List.map (fun (name, fieldTy) -> $"{name}: {tyToString fieldTy}")
                    |> String.concat ", "
                if namedText = "" then
                    $"({tupleText})"
                else
                    $"({tupleText}, {namedText})"
            | None, _ ->
                fields
                |> Map.toList
                |> List.map (fun (name, fieldTy) -> $"{name}: {tyToString fieldTy}")
                |> String.concat ", "
                |> fun inner -> $"{{ {inner} }}"
        | TyTag(name, payloadOpt) ->
            match payloadOpt with
            | Some payload -> $"{name}({tyToString payload})"
            | None -> name
        | TyUnion items ->
            items
            |> List.map tyToString
            |> String.concat " | "

    let private mismatchMessage left right =
        $"Type mismatch: {tyToString left} vs {tyToString right}."

    let private tupleLengthMessage leftCount rightCount left right =
        $"Tuple length mismatch: {leftCount} vs {rightCount}. {mismatchMessage left right}"

    let private unionLengthMessage leftCount rightCount left right =
        $"Union length mismatch: {leftCount} vs {rightCount}. {mismatchMessage left right}"

    let private recordKeyMessage leftKeys rightKeys left right =
        let leftKeyList = leftKeys |> Set.toList |> List.sort |> String.concat ", "
        let rightKeyList = rightKeys |> Set.toList |> List.sort |> String.concat ", "
        $"Record field mismatch: {{{leftKeyList}}} vs {{{rightKeyList}}}. {mismatchMessage left right}"

    let private keySet (fields: Map<string, Ty>) =
        fields |> Map.keys |> Set.ofSeq

    let empty : Subst = Map.empty

    let rec apply (subst: Subst) (ty: Ty) : Ty =
        match ty with
        | TyVar v ->
            match subst |> Map.tryFind v.Id with
            | Some replacement -> apply subst replacement
            | None -> ty
        | TyFunc(arg, ret) -> TyFunc(apply subst arg, apply subst ret)
        | TyTuple items -> TyTuple(items |> List.map (apply subst))
        | TyRecord fields ->
            fields |> Map.map (fun _ v -> apply subst v) |> TyRecord
        | TyTag(name, payload) -> TyTag(name, payload |> Option.map (apply subst))
        | TyUnion items -> TyUnion(items |> List.map (apply subst))
        | TyPrimitive _ -> ty

    let rec occurs (varId: int) (ty: Ty) : bool =
        match ty with
        | TyVar v -> v.Id = varId
        | TyFunc(arg, ret) -> occurs varId arg || occurs varId ret
        | TyTuple items -> items |> List.exists (occurs varId)
        | TyRecord fields -> fields |> Map.exists (fun _ v -> occurs varId v)
        | TyTag(_, payload) -> payload |> Option.exists (occurs varId)
        | TyUnion items -> items |> List.exists (occurs varId)
        | TyPrimitive _ -> false

    let unify (constraints: ConstraintSet) : Result<Subst, string> =
        let rec loop (subst: Subst) (remaining: ConstraintSet) =
            match remaining with
            | [] -> Ok subst
            | (left, right) :: rest ->
                let l = apply subst left
                let r = apply subst right
                if l = r then
                    loop subst rest
                else
                    match l, r with
                    | TyVar v, ty
                    | ty, TyVar v ->
                        if occurs v.Id ty then
                            Error $"Occurs check failed: {tyToString ty} contains 't{v.Id}."
                        else
                            let newSubst = subst |> Map.add v.Id ty
                            loop newSubst rest
                    | TyPrimitive lName, TyPrimitive rName when lName = rName ->
                        loop subst rest
                    | TyFunc(lArg, lRet), TyFunc(rArg, rRet) ->
                        loop subst ((lArg, rArg) :: (lRet, rRet) :: rest)
                    | TyTuple lItems, TyTuple rItems when lItems.Length = rItems.Length ->
                        loop subst (List.zip lItems rItems @ rest)
                    | TyTuple lItems, TyTuple rItems ->
                        Error (tupleLengthMessage lItems.Length rItems.Length l r)
                    | TyTuple lItems, TyRecord rFields ->
                        let lFields = tupleFieldMap lItems
                        let lKeys = keySet lFields
                        let rKeys = keySet rFields
                        if lFields.Count = rFields.Count && lKeys = rKeys then
                            let pairs = lFields |> Map.toList |> List.map (fun (k, v) -> (v, rFields.[k]))
                            loop subst (pairs @ rest)
                        else
                            Error (recordKeyMessage lKeys rKeys l r)
                    | TyRecord lFields, TyRecord rFields ->
                        let lKeys = keySet lFields
                        let rKeys = keySet rFields
                        if lFields.Count = rFields.Count && lKeys = rKeys then
                            let pairs = lFields |> Map.toList |> List.map (fun (k, v) -> (v, rFields.[k]))
                            loop subst (pairs @ rest)
                        else
                            Error (recordKeyMessage lKeys rKeys l r)
                    | TyRecord lFields, TyTuple rItems ->
                        let rFields = tupleFieldMap rItems
                        let lKeys = keySet lFields
                        let rKeys = keySet rFields
                        if lFields.Count = rFields.Count && lKeys = rKeys then
                            let pairs = lFields |> Map.toList |> List.map (fun (k, v) -> (v, rFields.[k]))
                            loop subst (pairs @ rest)
                        else
                            Error (recordKeyMessage lKeys rKeys l r)
                    | TyTag(lName, lPayload), TyTag(rName, rPayload) when lName = rName ->
                        match lPayload, rPayload with
                        | None, None -> loop subst rest
                        | Some lTy, Some rTy -> loop subst ((lTy, rTy) :: rest)
                        | _ -> Error $"Tag payload mismatch: {lName}. {mismatchMessage l r}"
                    | TyUnion lItems, TyUnion rItems when lItems.Length = rItems.Length ->
                        loop subst (List.zip lItems rItems @ rest)
                    | TyUnion lItems, TyUnion rItems ->
                        Error (unionLengthMessage lItems.Length rItems.Length l r)
                    | _ -> Error (mismatchMessage l r)
        loop empty constraints
