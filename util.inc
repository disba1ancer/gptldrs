macro struct name {
  postpone \{
    virtual at 0
      name name
      name\#.sizeof = $
    end virtual
  \}
  struc name
}

macro string val& {
local .start
.start:
        db      val, 0
.sizeof = $ - .start
}

macro strline val& {
string val, 10
}

macro begin_stack {
virtual at bp - .stack_size
.stack_start:
}

macro end_stack {
.stack_size = $ - $$
end virtual
}

macro begin_arguments {
virtual at bp
        dw      ?
        dw      ?
}

macro begin_arguments_far {
virtual at bp
        dw      ?
        dw      ?
        dw      ?
}

macro end_arguments{
end virtual
}

macro dalign value, base=0 {
        db      value - 1 - ($ - base + value - 1) mod value dup ?
}

struct far_ptr
{
.offset  dw     ?
.segment dw     ?
}

macro assume reg, name {
    restore name
    name equ reg
}
