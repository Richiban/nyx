namespace NyxCompiler

/// A constraint between two types that must be equal.
type Constraint = Ty * Ty

type ConstraintSet = Constraint list

module Constraints =
    let empty : ConstraintSet = []

    let add constraint' (constraints: ConstraintSet) =
        constraint' :: constraints
