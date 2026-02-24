(module
  (import "env" "dbg" (func $dbg (param i32)))

  (func $main (result i32) (local $s i32) (local $__dbg_tmp i32)
    i32.const 42
    local.set $s
    i32.const 42
    local.tee $__dbg_tmp
    call $dbg
    local.get $__dbg_tmp
    drop
    i32.const 0
    local.tee $__dbg_tmp
    call $dbg
    local.get $__dbg_tmp
  )
  (export "main" (func $main))
)