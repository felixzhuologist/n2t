from difflib import unified_diff

from arithmetic import binary_op, unary_op, binary_comp
from push import push
from pop import pop


def test_command(f, *args, expected):
  result = f(*args)
  diff = unified_diff(result, expected)
  print(f'diff for {f.__name__}{args}: ')
  for line in diff:
    print(line)
  print()

def test_push():
  # push constant
  test_command(push, 'constant', 9, expected=[
    '@9',
    'D=A',

    '@SP',
    'A=M',
    'M=D',

    '@SP',
    'M=M+1',
  ])

  # push from heap segment
  test_command(push, 'local', 3, expected=[
    '@3',
    'D=A',
    '@LCL',
    'A=D+M',
    'D=M',

    '@SP',
    'A=M',
    'M=D',

    '@SP',
    'M=M+1',
  ])

  # push from pointer
  test_command(push, 'pointer', 1, expected=[
    '@THAT',
    'D=M',

    '@SP',
    'A=M',
    'M=D',

    '@SP',
    'M=M+1'
  ])

  # push from temp
  test_command(push, 'temp', 3, expected=[
    '@8',
    'D=M',

    '@SP',
    'A=M',
    'M=D',

    '@SP',
    'M=M+1'
  ])

  # push from static
  test_command(push, 'static', 4, expected=[
    '@test_translate.4',
    'D=M',

    '@SP',
    'A=M',
    'M=D',

    '@SP',
    'M=M+1'
  ])

def test_pop():
  # pop onto heap segment
  test_command(pop, 'argument', 3, expected=[
    '@3',
    'D=A',
    '@ARG',
    'D=M+D',
    
    '@R13',
    'M=D',

    '@SP',
    'M=M-1',

    '@SP',
    'A=M',
    'D=M',

    '@R13',
    'A=M',
    'M=D'
  ])

  # pop onto pointer
  test_command(pop, 'pointer', 0, expected=[
    '@SP',
    'M=M-1',

    '@SP',
    'A=M',
    'D=M',

    '@THIS',
    'M=D'
  ])

  # pop onto temp
  test_command(pop, 'temp', 2, expected=[
    '@SP',
    'M=M-1',

    '@SP',
    'A=M',
    'D=M',

    '@7',
    'M=D'
  ])

  # pop onto global segment
  test_command(pop, 'static', 4, expected=[
    '@SP',
    'M=M-1',

    '@SP',
    'A=M',
    'D=M',

    '@test_translate.4',
    'M=D'
  ])

def test_arithmetic():
  test_command(binary_op, '+', expected=[
    '@SP',
    'M=M-1',

    '@SP',
    'A=M',
    'D=M',

    '@SP',
    'M=M-1',

    '@SP',
    'A=M',
    'M=M+D',

    '@SP',
    'M=M+1',
  ])

  test_command(binary_op, '-', expected=[
    '@SP',
    'M=M-1',

    '@SP',
    'A=M',
    'D=M',

    '@SP',
    'M=M-1',

    '@SP',
    'A=M',
    'M=M-D',

    '@SP',
    'M=M+1',
  ])

  test_command(binary_op, '&', expected=[
    '@SP',
    'M=M-1',

    '@SP',
    'A=M',
    'D=M',

    '@SP',
    'M=M-1',

    '@SP',
    'A=M',
    'M=M&D',

    '@SP',
    'M=M+1',
  ])

  test_command(unary_op, '-', expected=[
    '@SP',
    'M=M-1',

    '@SP',
    'A=M',
    'M=-M',

    '@SP',
    'M=M+1',
  ])

  test_command(unary_op, '!', expected=[
    '@SP',
    'M=M-1',

    '@SP',
    'A=M',
    'M=!M',

    '@SP',
    'M=M+1',
  ])

  test_command(binary_comp, 'JEQ', expected=[
    '@SP',
    'M=M-1',

    '@SP',
    'A=M',
    'D=M',

    '@SP',
    'M=M-1',

    '@SP',
    'A=M',
    'D=M-D',

    '@IF_EQUAL_0',
    'D;JEQ',

    '@SP',
    'A=M',
    'M=0',

    '@END_0',
    '0;JMP',

    '(IF_EQUAL_0)',
    '@SP',
    'A=M',
    'M=-1',
    '(END_0)',

    '@SP',
    'M=M+1'
  ])

  test_command(binary_comp, 'JGT', expected=[
    '@SP',
    'M=M-1',

    '@SP',
    'A=M',
    'D=M',

    '@SP',
    'M=M-1',

    '@SP',
    'A=M',
    'D=M-D',

    '@IF_EQUAL_1',
    'D;JGT',

    '@SP',
    'A=M',
    'M=0',

    '@END_1',
    '0;JMP',

    '(IF_EQUAL_1)',
    '@SP',
    'A=M',
    'M=-1',
    '(END_1)',

    '@SP',
    'M=M+1'
  ])

if __name__ == '__main__':
  test_push()
  test_pop()
  test_arithmetic()
