(module
  (import "env" "dbg" (func $dbg (param i32)))

  (import "env" "dbg_tag_test" (func $dbg_tag_test))

  (func $main (result i32) (local $__dbg_tmp i32)
    i32.const 1
    local.set $__dbg_tmp
    call $dbg_tag_test
    local.get $__dbg_tmp
    drop
    i32.const 1
    local.tee $__dbg_tmp
    call $dbg
    local.get $__dbg_tmp
  )
  (export "main" (func $main))
)