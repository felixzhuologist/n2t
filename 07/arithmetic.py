from util import (concat, incr_sp, decr_sp, load_stack_top_into_d, if_else)

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

def binary_comp(comp):
  return concat(
    decr_sp(),
    load_stack_top_into_d(),
    decr_sp(),
    ['@SP', 'A=M', 'D=M-D'],
    if_else(
      comparator=comp,
      true_block=['@SP', 'A=M', 'M=-1'], # recall: in HW -1 represents true, 0 false
      false_block=['@SP', 'A=M', 'M=0']
    ),
    incr_sp()
  )