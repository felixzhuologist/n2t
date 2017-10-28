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

def get_push_f(segment):
  map_ = {
    Segment.S_STATIC: push_static,
    Segment.S_CONSTANT: push_constant,
    Segment.S_POINTER: push_pointer,
    Segment.S_TEMP: push_temp
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
  segment = 'THIS' if val == '0' else 'THAT'
  return concat(
    load_pointer_val_into_d(segment),
    push_d_onto_stack(),
    incr_sp()
  )

def push_temp(_, index):
  addr = TEMP_BASE_ADDR + int(index)
  return concat(
    [f'@{addr}', 'D=M'],
    push_d_onto_stack(),
    incr_sp()
  )

def push_static(_, index):
  module = os.path.basename(__file__)
  varname = f'{module}.{index}'
  return concat(
    [f'@{varname}', 'D=M'],
    push_d_onto_stack(),
    incr_sp()
  )

def load_constant_into_d(c):
  return [f'@{c}', 'D=A']

def load_pointer_val_into_d(pointer):
  return [f'@{pointer}', 'A=M', 'D=M']

def push_d_onto_stack():
  return ['@SP', 'A=M', 'M=D']

def load_heap_val_into_d(segment, index):
  return load_constant_into_d(index) + [f'@{segment}', 'A=D+M', 'D=M']

def incr_sp():
  return ['@SP', 'M=M+1']

def decr_sp():
  return ['@SP', 'M=M-1']

def concat(*commands):
  return [c for command in commands for c in command]

