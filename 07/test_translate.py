from difflib import unified_diff

from translate import Segment, push

def test_command(f, segment, arg, expected):
  result = f(segment, arg)
  diff = unified_diff(result, expected)
  print(f'diff for {f.__name__}({segment}, {arg}): ')
  for line in diff:
    print(line)
  print()

def test_push():
  # push constant
  test_command(push, Segment.S_CONSTANT, 9, [
    '@9',
    'D=A',

    '@SP',
    'A=M',
    'M=D',

    '@SP',
    'M=M+1',
  ])

  # push from heap segment
  test_command(push, Segment.S_LCL, 3, [
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
  test_command(push, Segment.S_POINTER, 1, [
    '@THAT',
    'A=M',
    'D=M',

    '@SP',
    'A=M',
    'M=D',

    '@SP',
    'M=M+1'
  ])

  # push from temp
  test_command(push, Segment.S_TEMP, 3, [
    '@8',
    'D=M',

    '@SP',
    'A=M',
    'M=D',

    '@SP',
    'M=M+1'
  ])

  # push from static
  test_command(push, Segment.S_STATIC, 4, [
    '@test_translate.4',
    'D=M',

    '@SP',
    'A=M',
    'M=D',

    '@SP',
    'M=M+1'
  ])

# def test_pop()
#   # pop onto heap segment

#   # pop onto pointer
#   assert pop(Segment.S_POINTER, 0) == [
#     '@SP',
#     'M=M-1',

#     '@Sp',
#     'A=M',
#     'D=M',

#     '@THIS',
#     'A=M',
#     'M=D'
#   ]

#   # pop onto global segment
#   assert pop(Segment.S_STATIC, 4) == [
#     '@SP',
#     'M=M-1',

#     '@SP',
#     'A=M',
#     'D=M',

#     '@test_translate.4',
#     'M=D'
#   ]

if __name__ == '__main__':
  test_push()