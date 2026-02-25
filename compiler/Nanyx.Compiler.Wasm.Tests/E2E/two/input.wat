(module
  (func $double (param $x i32) (result i32)
    local.get $x
    i32.const 2
    i32.mul
  )
  (export "double" (func $double))

  (func $main (result i32)
    i32.const 5
    call $double
  )
  (export "main" (func $main))
)