from util import (concat, incr_sp, decr_sp, load_stack_top_into_d)

def binary_op(op):
  return concat(
    decr_sp(),
    load_stack_top_into_d(),
    decr_sp(),
    ['@SP', 'A=M', f'M=M{op}D'],
    incr_sp()
  )

def unary_op(op):
  return concat(
    decr_sp(),
    ['@SP', 'A=M', f'M={op}M'],
    incr_sp()
  )
