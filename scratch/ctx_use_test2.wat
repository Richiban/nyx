(module
  (memory 1)
  (export "memory" (memory 0))

  (type $ctx_fn_1 (func (param i32) (result i32)))
  (table $ctx_table 1 funcref)
  (elem (i32.const 0) $__ctx_main_println_0)

  (data (i32.const 256) "\02\00\00\00hi\00")

  (func $main (result i32)
    i32.const 260
    i32.const 0
    call_indirect (type $ctx_fn_1)
  )
  (export "main" (func $main))

  (func $__ctx_main_println_0 (param $msg i32) (result i32)
    i32.const 0
  )
)