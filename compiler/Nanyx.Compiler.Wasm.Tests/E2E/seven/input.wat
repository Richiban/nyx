(module
  (func $inRange (param $x i32) (param $lo i32) (param $hi i32) (result i32)
    local.get $x
    local.get $lo
    i32.ge_s
    local.get $x
    local.get $hi
    i32.le_s
    i32.and
  )
  (export "inRange" (func $inRange))

  (func $check (param $a i32) (param $b i32) (result i32)
    local.get $a
    local.get $b
    i32.or
    (if (result i32)
      (then
        i32.const 1
      )
      (else
        i32.const 0
      )
    )
  )
  (export "check" (func $check))

  (func $main (result i32) (local $r1 i32) (local $r2 i32) (local $c1 i32) (local $c2 i32)
    i32.const 5
    i32.const 1
    i32.const 10
    call $inRange
    local.set $r1
    i32.const 15
    i32.const 1
    i32.const 10
    call $inRange
    local.set $r2
    i32.const 1
    i32.const 0
    call $check
    local.set $c1
    i32.const 0
    i32.const 0
    call $check
    local.set $c2
    local.get $r1
    local.get $c1
    i32.const 1
    i32.eq
    i32.and
    local.get $r2
    i32.const 0
    i32.eq
    i32.and
    local.get $c2
    i32.const 0
    i32.eq
    i32.and
    (if (result i32)
      (then
        i32.const 42
      )
      (else
        i32.const 0
      )
    )
  )
  (export "main" (func $main))
)