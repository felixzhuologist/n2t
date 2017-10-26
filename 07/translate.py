import argparse
from enum import Enum
from functools import partial
import os

symbols = {
  'stack': 'SP',
  'local': 'LCL',
  'argument': 'ARG',
  'this': 'THIS',
  'that': 'THAT',
}

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
  S_LCL = 1
  S_ARG = 2
  S_THIS = 3
  S_THAT = 4

class VM:

  def __init__(self, out_path):
    self.out = open(out_path, 'at')
    self.write = partial(print, file=self.out)
    self.class = os.path.basename(__file__)

  def push(self, segment, index):
    if segment == 'constant':
      # D = val
      self.load_const(index)
    else:
      # D = *(segment + index)
      self.load_const(index) # D = index
      self.dereference(segment) # M = segment base
      self.write('A=D+M')
      self.write('D=M')

    # *sp = *addr
    self.dereference('SP')
    self.write('M=D')

    # sp++
    self.write('@SP')
    self.write('M=M+1')

  def pop(self, segment, index):
    """ pop value of stack and write into segment[index] """
    # D = *(segment + index)
    self.load_const(index)
    self.dereference(segment)
    self.write('A=D+M')
    self.write('D=M')

    # sp--
    self.write('@SP')
    self.write('M=M-1')

    # *addr = *sp
    self.dereference('SP')
    self.write('D=M')
    self.dereference()
    self.write('')

  def pop_static(self, val):
    # sp--
    self.write('@SP')
    self.write('M=M-1')

    # D = *sp
    self.dereference('SP')
    self.write('D=M')

    self.write('@{0}.{1}'.format(self.class, val))
    self.write('M=D')

  # push/pop temp (base address of 5)
  # push pointer 0/1 -> *sp = THIS/THAT, sp++
  # pop pointer 0/1 -> sp--, THIS/THAT = *sp
  # SP LCL ARG THIS THAT temp, registers, static, stack

  def load_const(self, val):
    """ loads const into D register. """
    self.write('@' + str(val))
    self.write('D=A')

  def dereference(self, register):
    """ Loads value at memory location stored in register into M """
    self.write('@' + str(val))
    self.write('A=M')

  def pop(self, segment, index):
       self._load_seg(seg, index, indir)
        self._comp_to_reg(R_COPY, 'D')      # R_COPY=D
        self._stack_to_dest('D')            # D=*SP
        self._reg_to_dest('A', R_COPY)      # A=R_COPY
        self._c_command('M', 'D')           # *(seg+index)=D
    



if __name__ == '__main__':
  parser = argparse.ArgumentParser(description='translate vm code to assembly')
  parser.add_argument('vm_file', help='path to vm file to translate')
  args = parser.parse_args()
  
  out_file = vm_file[:-2] + 'asm'
  with open(vm_file, 'rt') as f:
    with open(out_file, 'wt') as out:
      for line in f:
        if not line.lstrip().startswith('//'):
          command, *args = line.split()
          if command == 'pop':
            dest, = args
          elif command == 'push':
            val, = args
            vm.stack.append(val)
            vm.sp += 1
            print()
