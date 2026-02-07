namespace NyxCompiler

open System.Collections.Generic

/// Substitution map for type variables.
type Subst = Map<int, Ty>

module Unifier =
    let empty : Subst = Map.empty

    let rec apply (subst: Subst) (ty: Ty) : Ty =
        match ty with
        | TyVar v ->
            match subst |> Map.tryFind v.Id with
            | Some replacement -> apply subst replacement
            | None -> ty
        | TyFunc(args, ret) -> TyFunc(args |> List.map (apply subst), apply subst ret)
        | TyTuple items -> TyTuple(items |> List.map (apply subst))
        | TyRecord fields ->
            fields |> Map.map (fun _ v -> apply subst v) |> TyRecord
        | TyTag(name, payload) -> TyTag(name, payload |> Option.map (apply subst))
        | TyUnion items -> TyUnion(items |> List.map (apply subst))
        | TyPrimitive _ -> ty

    let rec occurs (varId: int) (ty: Ty) : bool =
        match ty with
        | TyVar v -> v.Id = varId
        | TyFunc(args, ret) -> args |> List.exists (occurs varId) || occurs varId ret
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
                            Error "Occurs check failed"
                        else
                            let newSubst = subst |> Map.add v.Id ty
                            loop newSubst rest
                    | TyPrimitive lName, TyPrimitive rName when lName = rName ->
                        loop subst rest
                    | TyFunc(lArgs, lRet), TyFunc(rArgs, rRet) when lArgs.Length = rArgs.Length ->
                        loop subst (List.zip lArgs rArgs @ ((lRet, rRet) :: rest))
                    | TyTuple lItems, TyTuple rItems when lItems.Length = rItems.Length ->
                        loop subst (List.zip lItems rItems @ rest)
                    | TyRecord lFields, TyRecord rFields when lFields.Count = rFields.Count && lFields.Keys = rFields.Keys ->
                        let pairs = lFields |> Map.toList |> List.map (fun (k, v) -> (v, rFields.[k]))
                        loop subst (pairs @ rest)
                    | TyTag(lName, lPayload), TyTag(rName, rPayload) when lName = rName ->
                        match lPayload, rPayload with
                        | None, None -> loop subst rest
                        | Some lTy, Some rTy -> loop subst ((lTy, rTy) :: rest)
                        | _ -> Error "Tag payload mismatch"
                    | TyUnion lItems, TyUnion rItems when lItems.Length = rItems.Length ->
                        loop subst (List.zip lItems rItems @ rest)
                    | _ -> Error "Type mismatch"
        loop empty constraints
