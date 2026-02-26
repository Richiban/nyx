(module
  (memory 1)
  (export "memory" (memory 0))

  (global $heap_ptr (mut i32) (i32.const 4096))

  (func $f (param $x i32) (param $y i32) (result i32)
    local.get $x
    local.get $y
    i32.add
  )
  (export "f" (func $f))

  (func $main (result i32) (local $r1 i32) (local $args i32) (local $r2 i32)
    i32.const 3
    i32.const 4
    call $f
    local.set $r1
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
    global.get $heap_ptr
    i32.const 8
    i32.add
    global.set $heap_ptr
    local.set $args
    local.get $args
    i32.const 0
    i32.add
    i32.load
    local.get $args
    i32.const 4
    i32.add
    i32.load
    call $f
    local.set $r2
    local.get $r1
    local.get $r2
    i32.add
  )
  (export "main" (func $main))
)