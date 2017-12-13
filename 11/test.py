"""
Compare codegen implementation against the reference implementation (included
in the tools directory of the course files). First compile the codegen code
(in the project 10 directory):
  ghc Codegen.hs -main-is Codegen.main -o Codegen
Then run this test script:
  python test.py ./ConvertToBin/Main.jack
which will write out a Main.vm (the reference) and a Main.vm.comp, and print out
a diff of the two files.
"""

import argparse
from difflib import unified_diff
import subprocess

parser = argparse.ArgumentParser(description='diff codegen implementation against the reference implementation')
parser.add_argument('path', help='path to .jack file')
parser.add_argument('reference',
                    nargs='?',
                    help='path to reference implementation',
                    default='../../tools/JackCompiler.sh')
parser.add_argument('codegen',
                    nargs='?',
                    help='path to your implementation',
                    default='../10/Codegen')

if __name__ == '__main__':
  args = parser.parse_args()
  subprocess.call([args.reference, args.path])
  subprocess.call([args.codegen, args.path, args.path.replace('.jack', '.vm.comp')])
  ref = list(open(args.path.replace('.jack', '.vm'), 'r'))
  comp = list(open(args.path.replace('.jack', '.vm.comp'), 'r'))
  for line in unified_diff(comp, ref):
    print(line, end='')
