(module
  (func $classify (param $value i32) (result i32)
    local.get $value
    i32.const 1
    i32.eq
    (if (result i32)
      (then
        i32.const 1
      )
      (else
        i32.const 0
      )
    )
  )
  (export "classify" (func $classify))

  (func $main (result i32)
    i32.const 1
    call $classify
  )
  (export "main" (func $main))
)