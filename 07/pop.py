#!/usr/local/bin/python3

from util import (load_constant_into_d, get_segment_name, concat,
  get_pointer_segment, get_temp_addr, get_static_varname,
  decr_sp, load_stack_top_into_d)


def pop(segment: str, value: str):
  return get_pop_f(segment)(get_segment_name(segment), value)

def get_pop_f(segment):
  map_ = {
    'static': pop_static,
    'pointer': pop_pointer,
    'temp': pop_temp
  }
  return map_.get(segment, pop_heap)

def pop_heap(segment, index):
  return concat(
    load_constant_into_d(index),
    [f'@{segment}', 'D=M+D'], # load addr into D
    ['@R13', 'M=D'], # store addr into mem
    decr_sp(),
    load_stack_top_into_d(),
    ['@R13', 'A=M', 'M=D']
  )

def pop_pointer(_, val):
  segment = get_pointer_segment(val)
  return concat(
    decr_sp(),
    load_stack_top_into_d(),
    [f'@{segment}', 'A=M', 'M=D']
  )

def pop_temp(_, index):
  addr = get_temp_addr(index)
  return concat(
    decr_sp(),
    load_stack_top_into_d(),
    [f'@{addr}', 'M=D']
  )

def pop_static(_, index):
  varname = get_static_varname(index)
  return concat(
    decr_sp(),
    load_stack_top_into_d(),
    [f'@{varname}', 'M=D']
  )
