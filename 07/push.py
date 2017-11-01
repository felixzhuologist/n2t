from util import (load_constant_into_d, get_segment_name, concat,
  get_pointer_segment, get_temp_addr, get_static_varname, incr_sp,
  push_d_onto_stack)


def push(segment: str, value: str):
  return get_push_f(segment)(get_segment_name(segment), value)

def get_push_f(segment):
  map_ = {
    'static': push_static,
    'constant': push_constant,
    'pointer': push_pointer,
    'temp': push_temp
  }
  return map_.get(segment, push_heap)

def push_constant(_, c):
  return concat(
    load_constant_into_d(c),
    push_d_onto_stack(),
    incr_sp()
  )

def push_heap(segment, index):
  return concat(
    load_heap_val_into_d(segment, index),
    push_d_onto_stack(),
    incr_sp()
  )

def push_pointer(_, val):
  segment = get_pointer_segment(val)
  return concat(
    [f'@{segment}', 'D=M'],
    push_d_onto_stack(),
    incr_sp()
  )

def push_temp(_, index):
  addr = get_temp_addr(index)
  return concat(
    [f'@{addr}', 'D=M'],
    push_d_onto_stack(),
    incr_sp()
  )

def push_static(_, index):
  varname = get_static_varname(index)
  return concat(
    [f'@{varname}', 'D=M'],
    push_d_onto_stack(),
    incr_sp()
  )

def load_heap_val_into_d(segment, index):
  return concat(
    load_constant_into_d(index),
    [f'@{segment}', 'A=D+M', 'D=M']
  )
