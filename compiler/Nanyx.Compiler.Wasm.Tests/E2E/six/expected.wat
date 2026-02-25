(module
  (func $isEven (param $n i32) (result i32)
    local.get $n
    i32.const 2
    i32.rem_s
    i32.const 0
    i32.eq
  )
  (export "isEven" (func $isEven))

  (func $fizzbuzz (param $n i32) (result i32) (local $mod3 i32) (local $mod5 i32) (local $__match_tmp_0 i32) (local $__match_tmp_1 i32)
    local.get $n
    i32.const 3
    i32.rem_s
    local.set $mod3
    local.get $n
    i32.const 5
    i32.rem_s
    local.set $mod5
    local.get $mod3
    local.set $__match_tmp_0
    local.get $mod5
    local.set $__match_tmp_1
    local.get $__match_tmp_0
      i32.const 0
      i32.eq
      local.get $__match_tmp_1
      i32.const 0
      i32.eq
      i32.and
    (if (result i32)
      (then
        i32.const 15
      )
      (else
        local.get $__match_tmp_0
      i32.const 0
      i32.eq
      i32.const 1
      i32.and
    (if (result i32)
      (then
        i32.const 3
      )
      (else
        i32.const 1
      local.get $__match_tmp_1
      i32.const 0
      i32.eq
      i32.and
    (if (result i32)
      (then
        i32.const 5
      )
      (else
        i32.const 1
      i32.const 1
      i32.and
    (if (result i32)
      (then
        local.get $n
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
  (export "fizzbuzz" (func $fizzbuzz))

  (func $main (result i32) (local $a i32) (local $b i32) (local $c i32) (local $d i32)
    i32.const 15
    call $fizzbuzz
    local.set $a
    i32.const 9
    call $fizzbuzz
    local.set $b
    i32.const 10
    call $fizzbuzz
    local.set $c
    i32.const 7
    call $fizzbuzz
    local.set $d
    local.get $a
    local.get $b
    i32.add
    local.get $c
    i32.add
    local.get $d
    i32.add
  )
  (export "main" (func $main))
)