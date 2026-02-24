(module
  (func $s (result i32)
    i32.const 42
  )
  (export "s" (func $s))

  (func $r (result i32)
    call $s
    i32.const 1
    i32.add
  )
  (export "r" (func $r))

  (func $x (result i32) (local $__match_tmp_0 i32)
    i32.const 2
    local.set $__match_tmp_0
    local.get $__match_tmp_0
      i32.const 1
      i32.eq
    (if (result i32)
      (then
        i32.const 0
      )
      (else
        local.get $__match_tmp_0
      i32.const 2
      i32.eq
    (if (result i32)
      (then
        i32.const 2
      )
      (else
        unreachable
      )
    )
      )
    )
  )
  (export "x" (func $x))
)