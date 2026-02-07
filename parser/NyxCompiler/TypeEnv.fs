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
