"""
Functions are implemented as follows:
stack after a function call (from bottom of stack to top):

caller stack | args | stack frame | locals | callee stack
             ^                    ^                      ^
            ARG                  LCL                     SP

on function call
- push the stack frame (return addr, lcl, arg, this, that)
- set ARG ptr to sp - 5 - nargs (5 is the size of stack frame)
- setup lcl pointer for new scope
- jump to function
- add label for return address after the jump

at the start of function body:
- allocate space on the stack for local variables (by pushing 0 for each local variable)

on return:
- write return value and restore SP by using ARG
- restore stack frame (by backtracking from LCL)
"""
from push import push_constant
from util import (concat, load_stack_top_into_d, decr_sp, incr_sp,
  load_constant_into_d, push_d_onto_stack, goto)

_ = object()

def label(labelname):
  return [f'({labelname})']

def cond_goto(labelname):
  return concat(
    load_stack_top_into_d(),
    decr_sp(),
    [f'@{labelname}', 'D;JNE']
  )

def func_def(func_name, nvars):
  return concat(
    label(get_func_start_label(func_name)),
    set_segment_to_sp('LCL'),
    push_constant(_, '0') * nvars # this can be optimized by writing 0 directly
  )

def func_call(func_name, nargs):
  start_address = get_func_start_label(func_name)
  return_address = get_func_end_label(func_name)
  return concat(
    push_stack_frame(func_name),

    set_segment_to_sp('ARG'),
    concat(load_constant_into_d(5), ['@ARG', 'M=M-D']),
    concat(load_constant_into_d(nargs), ['@ARG', 'M=M-D']),

    set_segment_to_sp('LCL'),

    goto(start_address),
    label(return_address),
  )  

def func_return():
  return concat(
    load_stack_top_into_d(),
    ['@ARG', 'A=M', 'M=D'],          # *ARG = D
    ['@ARG', 'D=M', '@SP', 'M=D+1'], # SP = ARG + 1
    unpack_stack_frame(),
    goto('R13')
  )

def push_stack_frame(func_name):
  """ stack frame (bottom to top): [return address, LCL, ARG, THIS, THAT] """
  return concat(
    push_return_address(func_name),
    push_segment_pointer('LCL'),
    push_segment_pointer('ARG'),
    push_segment_pointer('THIS'),
    push_segment_pointer('THAT')
  )

def push_segment_pointer(segment):
  return concat(
    [f'@{segment}', 'D=M'],
    push_d_onto_stack(),
    incr_sp()
  )

def push_return_address(func_name):
  return_address = get_func_end_label(func_name)
  return concat(
    load_constant_into_d(return_address),
    push_d_onto_stack(),
    incr_sp()
  )

def unpack_stack_frame():
  """
  precondition: LCL points to right after the stack frame
  postcondition: stack frame values written into lcl, arg, this, that
    return address is written into R13
  """
  return concat(
    pop_segment_pointer('THAT'),
    pop_segment_pointer('THIS'),
    pop_segment_pointer('ARG'),
    # set R13 = LCL - 2 before overwritting LCL
    ['@LCL', 'D=M', '@R13', 'M=D-1', 'M=M-1'],
    pop_segment_pointer('LCL'),
  )

def set_segment_to_sp(segment):
  return ['@SP', 'D=M', f'@{segment}', 'M=D']

# TODO
def get_func_start_label(func_name):
  return 'translate.myfunc'

# TODO
def get_func_end_label(func_name):
  return 'translate.myfunc$ret.1'

def pop_segment_pointer(segment):
  return concat(
    decr_lcl(),
    ['@LCL', 'A=M', 'D=M'], # D = *LCL
    [f'@{segment}', 'M=D']
  )

def decr_lcl():
  return ['@LCL', 'M=M-1']
