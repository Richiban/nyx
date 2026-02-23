(module
  (func $f (param $arg1 i32) (result i32) (local $__match_tmp_0 i32)
    local.get $arg1
    local.set $__match_tmp_0
    local.get $__match_tmp_0
      i32.const -65536
      i32.and
      i32.const 65536
      i32.eq
    (if (result i32)
      (then
        i32.const 1
      )
      (else
        local.get $__match_tmp_0
      i32.const -65536
      i32.and
      i32.const 131072
      i32.eq
    (if (result i32)
      (then
        i32.const 2
      )
      (else
        local.get $__match_tmp_0
      i32.const -65536
      i32.and
      i32.const 196608
      i32.eq
    (if (result i32)
      (then
        i32.const 3
      )
      (else
        local.get $__match_tmp_0
      i32.const -65536
      i32.and
      i32.const 262144
      i32.eq
    (if (result i32)
      (then
        i32.const 4
      )
      (else
        unreachable
      )
    )
      )
    )
      )
    )
      )
    )
  )
  (export "f" (func $f))

  (func $r (result i32)
    i32.const 65536
    call $f
  )
  (export "r" (func $r))
)