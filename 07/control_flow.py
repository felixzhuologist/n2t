from util import (concat, load_stack_top_into_d, decr_sp, incr_sp,
  load_constant_into_d, push_d_onto_stack, goto)

def label(labelname):
  return [f'({labelname})']

def cond_goto(labelname):
  return concat(
    load_stack_top_into_d(),
    decr_sp(),
    [f'@{labelname}', 'D;JNE']
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

def push_stack_frame(func_name):
  """ stack frame: [return address, LCL, ARG, THIS, THAT] """
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

def set_segment_to_sp(segment):
  return ['@SP', 'D=M', f'@{segment}', 'M=D']

# TODO
def get_func_start_label(func_name):
  return 'translate.myfunc'

def get_func_end_label(func_name):
  return 'translate.myfunc$ret.1'

