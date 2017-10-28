#!/usr/local/bin/python3

import argparse
from enum import Enum
from functools import partial
import os

# base address for temp memory segment
TEMP_BASE_ADDR = 5

class Command(Enum):
  C_ARITHMETIC = 1
  C_PUSH = 2
  C_POP = 3
  C_LABEL = 4
  C_GOTO = 5
  C_IF = 6
  C_FUNCTION = 7
  C_RETURN = 8
  C_CALL = 9

class Segment(Enum):
  S_LCL = 1 # heap segments
  S_ARG = 2
  S_THIS = 3
  S_THAT = 4
  S_STATIC = 5 # global segments
  S_CONSTANT = 6 # virtual segments
  S_POINTER = 7 # ??
  S_TEMP = 8 # fixed base address of 5

# names of segment base addr variables
_ = ''
segment_names = [_, 'LCL', 'ARG', 'THIS', 'THAT', _, _, _, _]

def get_segment_name(segment: Segment) -> str:
  return segment_names[segment.value]

def push(segment: Segment, value: str):
  return get_push_f(segment)(get_segment_name(segment), value)

def pop(segment: Segment, value: str):
  return get_pop_f(segment)(get_segment_name(segment), value)

def get_push_f(segment):
  map_ = {
    Segment.S_STATIC: push_static,
    Segment.S_CONSTANT: push_constant,
    Segment.S_POINTER: push_pointer,
    Segment.S_TEMP: push_temp
  }
  return map_.get(segment, push_heap)

def get_pop_f(segment):
  map_ = {
    Segment.S_STATIC: pop_static,
    Segment.S_POINTER: pop_pointer,
    Segment.S_TEMP: pop_temp
  }
  return map_.get(segment)

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
    [f'@{segment}', 'A=M', 'D=M'],
    push_d_onto_stack(),
    incr_sp()
  )

def pop_pointer(_, val):
  segment = get_pointer_segment(val)
  return concat(
    decr_sp(),
    load_stack_top_into_d(),
    [f'@{segment}', 'A=M', 'M=D']
  )

def get_pointer_segment(pointer_val):
  return 'THIS' if str(pointer_val) == '0' else 'THAT'

def push_temp(_, index):
  addr = get_temp_addr(index)
  return concat(
    [f'@{addr}', 'D=M'],
    push_d_onto_stack(),
    incr_sp()
  )

def pop_temp(_, index):
  addr = get_temp_addr(index)
  return concat(
    decr_sp(),
    load_stack_top_into_d(),
    [f'@{addr}', 'M=D']
  )

def get_temp_addr(index):
  return TEMP_BASE_ADDR + int(index)

def push_static(_, index):
  varname = get_static_varname(index)
  return concat(
    [f'@{varname}', 'D=M'],
    push_d_onto_stack(),
    incr_sp()
  )

def pop_static(_, index):
  varname = get_static_varname(index)
  return concat(
    decr_sp(),
    load_stack_top_into_d(),
    [f'@{varname}', 'M=D']
  )

def get_static_varname(index):
  module = os.path.basename(__file__)
  return f'{module}.{index}'

def load_constant_into_d(c):
  return [f'@{c}', 'D=A']

def push_d_onto_stack():
  return ['@SP', 'A=M', 'M=D']

def load_stack_top_into_d():
  """ dereferences the stack pointer and stores it into the data register """
  return ['@SP', 'A=M', 'D=M']

def load_heap_val_into_d(segment, index):
  return concat(
    load_constant_into_d(index),
    [f'@{segment}', 'A=D+M', 'D=M']
  )

def incr_sp():
  return ['@SP', 'M=M+1']

def decr_sp():
  return ['@SP', 'M=M-1']

def concat(*commands):
  return [c for command in commands for c in command]

