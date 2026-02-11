namespace NyxCompiler

type ConstraintKind =
    | Equal
    | Assignable

/// A constraint between two types.
type Constraint = Ty * Ty * ConstraintKind

type ConstraintSet = Constraint list

module Constraints =
    let empty : ConstraintSet = []

    let add constraint' (constraints: ConstraintSet) =
        constraint' :: constraints
