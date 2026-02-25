(module
  (func $factorial (param $n i32) (result i32)
    local.get $n
    i32.const 1
    i32.le_s
    (if (result i32)
      (then
        i32.const 1
      )
      (else
        local.get $n
    local.get $n
    i32.const 1
    i32.sub
    call $factorial
    i32.mul
      )
    )
  )
  (export "factorial" (func $factorial))

  (func $fib (param $n i32) (result i32)
    local.get $n
    i32.const 1
    i32.le_s
    (if (result i32)
      (then
        local.get $n
      )
      (else
        local.get $n
    i32.const 1
    i32.sub
    call $fib
    local.get $n
    i32.const 2
    i32.sub
    call $fib
    i32.add
      )
    )
  )
  (export "fib" (func $fib))

  (func $main (result i32) (local $f5 i32) (local $fib10 i32)
    i32.const 5
    call $factorial
    local.set $f5
    i32.const 10
    call $fib
    local.set $fib10
    local.get $f5
    local.get $fib10
    i32.add
  )
  (export "main" (func $main))
)