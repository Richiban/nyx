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

    let private tagUnionLengthMessage leftCount rightCount left right =
        $"Tag union length mismatch: {leftCount} vs {rightCount}. {mismatchMessage left right}"

    let private recordKeyMessage leftKeys rightKeys left right =
        let leftKeyList = leftKeys |> Set.toList |> List.sort |> String.concat ", "
        let rightKeyList = rightKeys |> Set.toList |> List.sort |> String.concat ", "
        $"Record field mismatch: {{{leftKeyList}}} vs {{{rightKeyList}}}. {mismatchMessage left right}"

    let private keySet (fields: Map<string, Ty>) =
        fields |> Map.keys |> Set.ofSeq

    let private ensureTagUnion items =
        items
        |> List.forall (function
            | TyTag _ -> true
            | TyVar _ -> true
            | _ -> false)


    let private normalizeUnionItems (items: Ty list) =
        items
        |> List.fold (fun acc ty ->
            let key = tyToString ty
            if acc |> Map.containsKey key then acc else acc |> Map.add key ty) Map.empty

    let private parseTagUnion items =
        if ensureTagUnion items then
            let mutable restVar: TyVar option = None
            let tagMap =
                items
                |> List.choose (function
                    | TyTag _ as tag -> Some tag
                    | TyVar v ->
                        match restVar with
                        | None ->
                            restVar <- Some v
                            None
                        | Some _ ->
                            restVar <- restVar
                            None
                    | _ -> None)
                |> normalizeUnionItems
            Ok (tagMap, restVar)
        else
            Error "Only tag unions are supported"

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
            let unifyRecordFields (lFields: Map<string, Ty>) (rFields: Map<string, Ty>) rest left right =
                let lKeys = keySet lFields
                let rKeys = keySet rFields
                if Set.isSubset lKeys rKeys || Set.isSubset rKeys lKeys then
                    let commonKeys = Set.intersect lKeys rKeys
                    let pairs =
                        commonKeys
                        |> Seq.map (fun key -> (lFields.[key], rFields.[key]))
                        |> Seq.toList
                    loop subst (pairs @ rest)
                else
                    Error (recordKeyMessage lKeys rKeys left right)
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
                        unifyRecordFields lFields rFields rest l r
                    | TyRecord lFields, TyRecord rFields ->
                        unifyRecordFields lFields rFields rest l r
                    | TyRecord lFields, TyTuple rItems ->
                        let rFields = tupleFieldMap rItems
                        unifyRecordFields lFields rFields rest l r
                    | TyTag(lName, lPayload), TyTag(rName, rPayload) when lName = rName ->
                        match lPayload, rPayload with
                        | None, None -> loop subst rest
                        | Some lTy, Some rTy -> loop subst ((lTy, rTy) :: rest)
                        | _ -> Error $"Tag payload mismatch: {lName}. {mismatchMessage l r}"
                    | TyUnion lItems, TyUnion rItems ->
                        match parseTagUnion lItems, parseTagUnion rItems with
                        | Ok (lMap, lRest), Ok (rMap, rRest) ->
                            let lKeys = lMap |> Map.keys |> Set.ofSeq
                            let rKeys = rMap |> Map.keys |> Set.ofSeq
                            let extraLeft = Set.difference lKeys rKeys
                            let extraRight = Set.difference rKeys lKeys
                            let commonKeys = Set.intersect lKeys rKeys
                            let pairs =
                                commonKeys
                                |> Seq.map (fun key -> (lMap.[key], rMap.[key]))
                                |> Seq.toList
                            let restConstraints =
                                [ match lRest with
                                  | Some restVar when not extraRight.IsEmpty ->
                                      let extraTags = extraRight |> Seq.map (fun key -> rMap.[key]) |> Seq.toList
                                      yield (TyVar restVar, TyUnion extraTags)
                                  | _ -> ()
                                  match rRest with
                                  | Some restVar when not extraLeft.IsEmpty ->
                                      let extraTags = extraLeft |> Seq.map (fun key -> lMap.[key]) |> Seq.toList
                                      yield (TyVar restVar, TyUnion extraTags)
                                  | _ -> ()
                                  match lRest, rRest with
                                  | Some lVar, Some rVar -> yield (TyVar lVar, TyVar rVar)
                                  | _ -> () ]
                            if lRest.IsNone && rRest.IsNone then
                                if extraLeft.IsEmpty || extraRight.IsEmpty then
                                    loop subst (pairs @ rest)
                                else
                                    Error (tagUnionLengthMessage lMap.Count rMap.Count l r)
                            else if (lRest.IsSome || extraLeft.IsEmpty) && (rRest.IsSome || extraRight.IsEmpty) then
                                loop subst (pairs @ restConstraints @ rest)
                            else
                                Error (tagUnionLengthMessage lMap.Count rMap.Count l r)
                        | Error message, _ -> Error message
                        | _, Error message -> Error message
                    | TyUnion lItems, ty ->
                        match parseTagUnion lItems with
                        | Ok (lMap, lRest) ->
                            let tags = lMap |> Map.toList |> List.map snd
                            let rec tryItems items =
                                match items with
                                | [] ->
                                    match lRest with
                                    | Some restVar -> loop subst ((TyVar restVar, ty) :: rest)
                                    | None -> Error (mismatchMessage l r)
                                | item :: restItems ->
                                    match loop subst ((item, ty) :: rest) with
                                    | Ok result -> Ok result
                                    | Error _ -> tryItems restItems
                            tryItems tags
                        | Error message -> Error message
                    | ty, TyUnion rItems ->
                        match parseTagUnion rItems with
                        | Ok (rMap, rRest) ->
                            let tags = rMap |> Map.toList |> List.map snd
                            let rec tryItems items =
                                match items with
                                | [] ->
                                    match rRest with
                                    | Some restVar -> loop subst ((ty, TyVar restVar) :: rest)
                                    | None -> Error (mismatchMessage l r)
                                | item :: restItems ->
                                    match loop subst ((ty, item) :: rest) with
                                    | Ok result -> Ok result
                                    | Error _ -> tryItems restItems
                            tryItems tags
                        | Error message -> Error message
                    | _ -> Error (mismatchMessage l r)
        loop empty constraints
