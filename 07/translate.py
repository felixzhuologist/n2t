import argparse
from functools import partial

from arithmetic import binary_op, unary_op, binary_comp
from control_flow import label, cond_goto, func_def, func_call, func_return
from push import push
from pop import pop
from util import concat, goto


parser = argparse.ArgumentParser(description='translate vm code to assembly')
parser.add_argument('path', help='path to vm file')

command_to_f = {
  'push': push,
  'pop': pop,
  'add': partial(binary_op, op='+'),
  'sub': partial(binary_op, op='-'),
  'and': partial(binary_op, op='&'),
  'or': partial(binary_op, op='|'),
  'neg': partial(unary_op, op='-'),
  'not': partial(unary_op, op='!'),
  'eq': partial(binary_comp, comp='JEQ'),
  'gt': partial(binary_comp, comp='JGT'),
  'lt': partial(binary_comp, comp='JLT'),
  'label': label,
  'goto': goto
  'if-goto': cond_goto,
  'function': func_def,
  'return': func_return,
  'call': func_call,
}

def not_yet_implemented(*args):
  return ' '.join(args)

def parse_line(line):
  command, *args = line.split()
  handler = command_to_f.get(command, not_yet_implemented)
  return '\n'.join(handler(*args))

def is_code(line):
  return line.strip() and (not line.lstrip().startswith('//'))

if __name__ == '__main__':
  args = parser.parse_args()
  out_path = args.path.replace('.vm', '.asm')
  with open(args.path, 'rt') as f:
    out_lines = concat(map(parse_line, filter(is_code, f)))

  with open(out_path, 'wt') as f:
    for line in out_lines:
      print(line, file=f)

