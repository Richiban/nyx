(module
  (memory 1)
  (export "memory" (memory 0))

  (type $ctx_fn_1 (func (param i32) (result i32)))
  (table $ctx_table 1 funcref)
  (elem (i32.const 0) $__ctx_main_println_0)

  (data (i32.const 256) "\05\00\00\00Hello\00")

  (func $f (param $x i32) (param $y i32) (param $__ctx_println i32) (result i32)
    i32.const 260
    local.get $__ctx_println
    call_indirect (type $ctx_fn_1)
    drop
    local.get $x
    local.get $y
    i32.add
  )
  (export "f" (func $f))

  (func $main (result i32)
    i32.const 3
    i32.const 4
    i32.const 0
    call $f
  )
  (export "main" (func $main))

  (func $__ctx_main_println_0 (param $s i32) (result i32) (local $__dbg_tmp i32)
    local.get $s
    local.tee $__dbg_tmp
    call $dbg
    local.get $__dbg_tmp
    drop
    i32.const 0
  )
)