namespace NyxCompiler

/// A type scheme with generalized variables.
type TypeScheme =
    { Quantified: Set<int>
      Type: Ty }

type TypeEnv = Map<string, TypeScheme>

module TypeEnv =
    let empty : TypeEnv = Map.empty

    let extend (name: string) (scheme: TypeScheme) (env: TypeEnv) =
        env |> Map.add name scheme

    let tryFind (name: string) (env: TypeEnv) =
        env |> Map.tryFind name

    let mono (ty: Ty) : TypeScheme =
        { Quantified = Set.empty
          Type = ty }

    let rec freeTyVars (ty: Ty) : Set<int> =
        match ty with
        | TyVar v -> Set.singleton v.Id
        | TyPrimitive _ -> Set.empty
        | TyNominal(_, underlying, _) -> freeTyVars underlying
        | TyFunc(arg, ret) ->
            Set.union (freeTyVars arg) (freeTyVars ret)
        | TyTuple items ->
            items
            |> List.map freeTyVars
            |> List.fold Set.union Set.empty
        | TyRecord fields ->
            fields
            |> Map.values
            |> Seq.map freeTyVars
            |> Seq.fold Set.union Set.empty
        | TyTag(_, payloadOpt) ->
            payloadOpt |> Option.map freeTyVars |> Option.defaultValue Set.empty
        | TyApply(_, args) ->
            args
            |> List.map freeTyVars
            |> List.fold Set.union Set.empty
        | TyUnion items ->
            items
            |> List.map freeTyVars
            |> List.fold Set.union Set.empty

    let freeSchemeVars (scheme: TypeScheme) : Set<int> =
        Set.difference (freeTyVars scheme.Type) scheme.Quantified

    let freeEnvVars (env: TypeEnv) : Set<int> =
        env
        |> Map.values
        |> Seq.map freeSchemeVars
        |> Seq.fold Set.union Set.empty

    let generalize (env: TypeEnv) (ty: Ty) : TypeScheme =
        let envVars = freeEnvVars env
        let tyVars = freeTyVars ty
        { Quantified = Set.difference tyVars envVars
          Type = ty }
