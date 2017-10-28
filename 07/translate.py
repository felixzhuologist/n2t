import argparse
from functools import partial

from arithmetic import binary_op, unary_op
from push import push
from pop import pop
from util import concat


parser = argparse.ArgumentParser(description='translate vm code to assembly')
parser.add_argument('path', help='path to vm file')

command_to_f = {
  'push': push,
  'pop': pop,
  'add': partial(binary_op, op='+'),
  'sub': partial(binary_op, op='-'),
  'neg': partial(unary_op, op='-'),
  'not': partial(unary_op, op='!')
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

