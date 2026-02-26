(module
  (type $ctx_fn_1 (func (param i32) (result i32)))
  (table $ctx_table 2 funcref)
  (elem (i32.const 0) $__ctx_main_double_0 $__ctx_main_triple_1)

  (func $main (result i32)
    i32.const 5
    i32.const 0
    call_indirect (type $ctx_fn_1)
    i32.const 10
    i32.const 1
    call_indirect (type $ctx_fn_1)
    i32.add
  )
  (export "main" (func $main))

  (func $__ctx_main_double_0 (param $x i32) (result i32)
    local.get $x
    i32.const 2
    i32.mul
  )

  (func $__ctx_main_triple_1 (param $x i32) (result i32)
    local.get $x
    i32.const 3
    i32.mul
  )
)