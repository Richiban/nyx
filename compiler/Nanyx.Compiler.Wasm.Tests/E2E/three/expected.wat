(module
  (func $isPositive (param $x i32) (result i32)
    local.get $x
    i32.const 0
    i32.gt_s
  )
  (export "isPositive" (func $isPositive))

  (func $main (result i32) (local $a i32) (local $b i32) (local $positive i32)
    i32.const 1
    local.set $a
    i32.const 0
    local.set $b
    i32.const 5
    call $isPositive
    local.set $positive
    local.get $positive
    (if (result i32)
      (then
        i32.const 1
      )
      (else
        i32.const 0
      )
    )
  )
  (export "main" (func $main))
)