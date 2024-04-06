
anon_field_def ::=
    | expression

named_field_def ::=
    | identifier ':' expression
    | identifier ':' expression '=' expression

type_expression ::=
    | '(' named_field_def+ ')'
    | '(' anon_field_def{2,} ')'
    | '(' type_expression ')'
    | identifier 
    | call
    | union
    | intersection



---

Samples

# 1
`type A = string`

TypeDefinition {
    name = "A"
    value = TypeIdentifier (BuiltInType "string")
}

# 2
`type A = (string)`

TypeDefinition {
    name = "A"
    value = TypeIdentifier (BuiltInType "string")
}

# 3
`type A = (a: string)`

TypeDefinition {
    name = "A"
    value = Tuple [NamedField ("a", (BuiltInType "string"))]
}

# 4
`type A = (string, string)`

TypeDefinition {
    name = "A"
    value = Tuple [AnonymousField (BuiltInType "string"), AnonymousField (BuiltInType "string")]
}

# 5
`type A = (a: string, b: string)`

TypeDefinition {
    name = "A"
    value = Tuple [NamedField ("a", (BuiltInType "string")), NamedField ("b", (BuiltInType "string"))]
}

# 6

`type A = list(string)`

TypeDefinition {
    name = "A"
    value = TypeCall (BuiltInType "list", BuiltInType "string")
}

# 7

`type A = list(string, string)`

TypeDefinition {
    name = "A"
    value = TypeCall (Tuple [AnonymousField (BuiltInType "list"), AnonymousField (BuiltInType "string")])
}

# 7

`type A = B | list(B) | list(left: C, right: D)`

TypeDefinition {
    name = "A"
    value = Union [
        TypeIdentifier (CustomType "B"), 
        TypeCall (BuiltInType "list", TypeIdentifier (CustomType "B")), 
        TypeCall (
            Tuple [
                NamedField ("left", TypeIdentifier (CustomType "C")),
                NamedField ("right", TypeIdentifier (CustomType "D"))
            ]
        )
    ]
}

# 8

> Note that, for tuples, named and anonymous fields can be mixed but the anonymous fields must come first.

`type A = (string, named: string)`

TypeDefinition {
    name = "A"
    value = Tuple [
        AnonymousField (BuiltInType "string"),
        NamedField ("named", BuiltInType "string")
    ]
}

# 9

> Introducing tags:

`type A = #some_tag`

TypeDefinition {
    name = "A"
    value = Tag "some_tag"
}