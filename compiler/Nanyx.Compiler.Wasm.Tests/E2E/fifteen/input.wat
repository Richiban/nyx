(module
  (type $ctx_fn_0 (func (result i32)))
  (type $ctx_fn_2 (func (param i32 i32) (result i32)))
  (table $ctx_table 2 funcref)
  (elem (i32.const 0) $__ctx_main_add_0 $__ctx_main_getVal_1)

  (func $main (result i32)
    i32.const 1
    call_indirect (type $ctx_fn_0)
    i32.const 8
    i32.const 0
    call_indirect (type $ctx_fn_2)
  )
  (export "main" (func $main))

  (func $__ctx_main_add_0 (param $a i32) (param $b i32) (result i32)
    local.get $a
    local.get $b
    i32.add
  )

  (func $__ctx_main_getVal_1 (result i32)
    i32.const 42
  )
)