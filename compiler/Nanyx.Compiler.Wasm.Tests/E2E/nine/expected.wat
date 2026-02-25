(module
  (func $boolToInt (param $b i32) (result i32)
    local.get $b
    (if (result i32)
      (then
        i32.const 1
      )
      (else
        i32.const 0
      )
    )
  )
  (export "boolToInt" (func $boolToInt))

  (func $main (result i32) (local $b0 i32) (local $b1 i32) (local $b2 i32) (local $b3 i32) (local $b4 i32) (local $b5 i32) (local $b6 i32) (local $b7 i32) (local $b8 i32) (local $b9 i32) (local $b10 i32) (local $b11 i32)
    i32.const 5
    i32.const 10
    i32.lt_s
    call $boolToInt
    local.set $b0
    i32.const 10
    i32.const 5
    i32.lt_s
    call $boolToInt
    local.set $b1
    i32.const 5
    i32.const 10
    i32.gt_s
    call $boolToInt
    local.set $b2
    i32.const 10
    i32.const 5
    i32.gt_s
    call $boolToInt
    local.set $b3
    i32.const 5
    i32.const 5
    i32.le_s
    call $boolToInt
    local.set $b4
    i32.const 5
    i32.const 4
    i32.le_s
    call $boolToInt
    local.set $b5
    i32.const 5
    i32.const 5
    i32.ge_s
    call $boolToInt
    local.set $b6
    i32.const 4
    i32.const 5
    i32.ge_s
    call $boolToInt
    local.set $b7
    i32.const 5
    i32.const 5
    i32.eq
    call $boolToInt
    local.set $b8
    i32.const 5
    i32.const 6
    i32.eq
    call $boolToInt
    local.set $b9
    i32.const 5
    i32.const 6
    i32.ne
    call $boolToInt
    local.set $b10
    i32.const 5
    i32.const 5
    i32.ne
    call $boolToInt
    local.set $b11
    local.get $b0
    local.get $b1
    i32.const 2
    i32.mul
    i32.add
    local.get $b2
    i32.const 4
    i32.mul
    i32.add
    local.get $b3
    i32.const 8
    i32.mul
    i32.add
    local.get $b4
    i32.const 16
    i32.mul
    i32.add
    local.get $b5
    i32.const 32
    i32.mul
    i32.add
    local.get $b6
    i32.const 64
    i32.mul
    i32.add
    local.get $b7
    i32.const 128
    i32.mul
    i32.add
    local.get $b8
    i32.const 256
    i32.mul
    i32.add
    local.get $b9
    i32.const 512
    i32.mul
    i32.add
    local.get $b10
    i32.const 1024
    i32.mul
    i32.add
    local.get $b11
    i32.const 2048
    i32.mul
    i32.add
  )
  (export "main" (func $main))
)