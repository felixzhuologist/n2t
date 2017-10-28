#!/usr/local/bin/python3

from enum import Enum
import os

# base address for temp memory segment
TEMP_BASE_ADDR = 5
GENERAL_PURPOSE_REGISTERS = ['R13', 'R14', 'R15']

class Segment(Enum):
  S_LCL = 1 # heap segments
  S_ARG = 2
  S_THIS = 3
  S_THAT = 4
  S_STATIC = 5 # global segments
  S_CONSTANT = 6 # virtual segments
  S_POINTER = 7 # ??
  S_TEMP = 8 # fixed base address of 5

_ = ''
# names of segment base addr variables
segment_names = [_, 'LCL', 'ARG', 'THIS', 'THAT', _, _, _, _]

vm_to_asm_segment = {
  'local': 'LCL',
  'argument': 'ARG'
}

# map from vm version of a segment's name to its variable name in asm
def get_segment_name(segment: str) -> str:
  return vm_to_asm_segment.get(segment, segment.upper())

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