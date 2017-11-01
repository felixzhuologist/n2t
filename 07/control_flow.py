from util import (concat, load_stack_top_into_d, decr_sp)

def label(labelname):
  return [f'({labelname})']

def cond_goto(labelname):
  return concat(
    load_stack_top_into_d(),
    decr_sp(),
    [f'@{labelname}', 'D;JNE']
  )
