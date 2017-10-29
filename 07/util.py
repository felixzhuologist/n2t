#!/usr/local/bin/python3

from enum import Enum
from itertools import count
import os

# base address for temp memory segment
TEMP_BASE_ADDR = 5
GENERAL_PURPOSE_REGISTERS = ['R13', 'R14', 'R15']
# generate unique ids for each set of if/else statement labels
IF_LABEL_ID_GEN = count()

vm_to_asm_segment = {
  'local': 'LCL',
  'argument': 'ARG'
}

# map from vm version of a segment's name to its variable name in asm
def get_segment_name(segment: str) -> str:
  return vm_to_asm_segment.get(segment, segment.upper())

def if_else(comparator, true_block, false_block=None):
  assert comparator in ['JEQ', 'JGT', 'JGE', 'JLT', 'JNE', 'JLE', 'JMP']
  false_block = false_block or []
  label_id = next(IF_LABEL_ID_GEN)
  return concat( # assumes correct value is already loaded into D register
    [f'@IF_EQUAL_{label_id}', f'D;{comparator}'], # if true skip to true block
    false_block,
    [f'@END_{label_id}', '0;JMP'], # jump over true block
    [f'(IF_EQUAL_{label_id})'],
    true_block,
    [f'(END_{label_id})']
  )

def get_pointer_segment(pointer_val):
  return 'THIS' if str(pointer_val) == '0' else 'THAT'

def get_temp_addr(index):
  return TEMP_BASE_ADDR + int(index)

def get_static_varname(index):
  module = os.path.basename(__file__)
  return f'{module}.{index}'

def concat(*commands):
  return [c for command in commands for c in command]

def load_constant_into_d(c):
  return [f'@{c}', 'D=A']

def incr_sp():
  return ['@SP', 'M=M+1']

def decr_sp():
  return ['@SP', 'M=M-1']

def load_stack_top_into_d():
  """ dereferences the stack pointer and stores it into the data register """
  return ['@SP', 'A=M', 'D=M']
