import argparse
from functools import partial
import os

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
  'goto': goto,
  'if-goto': cond_goto,
  'function': func_def,
  'return': func_return,
  'call': func_call,
}

def not_yet_implemented(*args):
  return ' '.join(args)

def parse_line(line):
  if line.find('//') != -1:
    line = line[:line.find('//')].strip()

  command, *args = line.split()
  handler = command_to_f.get(command, not_yet_implemented)
  return '\n'.join(handler(*args))

def is_code(line):
  return line.strip() and (not line.lstrip().startswith('//'))

def translate_file(filepath):
  with open(filepath, 'rt') as f:
    return concat(map(parse_line, filter(is_code, f)))

if __name__ == '__main__':
  args = parser.parse_args()
  out_path = args.path
  out_lines = []

  if args.path.endswith('.vm'): # compiling single file
    out_lines = translate_file(args.path)
  else: # compiling dir
    # put Sys.vm entry point at start of list
    vm_files = [f for f in os.listdir(args.path) if f.endswith('.vm')]
    filenames = sorted(vm_files, key=lambda s: s != 'Sys.vm')
    assert filenames[0] == 'Sys.vm'
    print(f"compiling files: {', '.join(filenames)}")
    for filename in filenames:
      full_path = os.path.join(args.path, filename)
      out_lines += translate_file(full_path)

  out_path = args.path.replace('.vm', '.asm') if args.path.endswith('.vm') else \
    os.path.join(args.path, os.path.basename(args.path) + '.asm')
  print(f'wrote compiled file to: {out_path}')
  with open(out_path, 'wt') as f:
    for line in out_lines:
      print(line, file=f)
