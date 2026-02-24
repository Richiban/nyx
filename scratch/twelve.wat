(module
  (import "env" "dbg" (func $dbg (param i32)))

  (import "env" "dbg_tag_test" (func $dbg_tag_test))

  (import "env" "dbg_tag_payload_some" (func $dbg_tag_payload_some (param i32)))

  (func $main (result i32) (local $__dbg_tmp i32)
    i32.const 131072
    local.set $__dbg_tmp
    call $dbg_tag_test
    local.get $__dbg_tmp
    drop
    i32.const 42
    local.tee $__dbg_tmp
    call $dbg_tag_payload_some
    local.get $__dbg_tmp
    i32.const 65535
    i32.and
    i32.const 65536
    i32.or
    drop
    i32.const 1
    local.tee $__dbg_tmp
    call $dbg
    local.get $__dbg_tmp
  )
  (export "main" (func $main))
)