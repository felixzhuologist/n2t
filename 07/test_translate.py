def push(segment, index):
  pass

def pop(segment, index):
  pass

def test():
  assert push('constant', 9) == [
    '@9',
    'D=A',

    '@SP',
    'A=M',
    'M=D',

    '@SP',
    'M=M+1',
  ]

  assert push('local', 3) == [
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
  ]

  assert push('pointer', 1) == [
    '@3',
    'D=A',
    '@THIS',
    'A=D+M',
    'D=M',

    '@SP',
    'A=M',
    'M=D',

    '@SP',
    'M=M+1'
  ]