(module
  (memory 1)
  (export "memory" (memory 0))

  (global $heap_ptr (mut i32) (i32.const 4096))

  (func $main (result i32) (local $t i32)
    global.get $heap_ptr
    i32.const 0
    i32.add
    i32.const 10
    i32.store
    global.get $heap_ptr
    i32.const 4
    i32.add
    i32.const 20
    i32.store
    global.get $heap_ptr
    i32.const 8
    i32.add
    i32.const 30
    i32.store
    global.get $heap_ptr
    global.get $heap_ptr
    i32.const 12
    i32.add
    global.set $heap_ptr
    local.set $t
    local.get $t
    i32.const 0
    i32.add
    i32.load
    local.get $t
    i32.const 4
    i32.add
    i32.load
    i32.add
    local.get $t
    i32.const 8
    i32.add
    i32.load
    i32.add
  )
  (export "main" (func $main))
)