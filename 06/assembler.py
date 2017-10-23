
registers = {}
labels = {}

def get_lines(filename):
  with open(filename, 'rb') as f:
    numlines = 0
    for line in f:
      if line and not line.strip().startswith('#'):
        yield line

def line_handler(line):
  if line.startswith('@'):
    return a_command(line)
  else:
    return c_command(line)

def a_command(line):
  location = line.strip()[1:]
  if location.isnumeric():
    return
  else:
    return

def c_command(line):
  pass

def get_register_value(var_name):
  if var_name not in registers:
    registers[var_name] = get_next_available_register(registers)
  return registers[var_name]

def get_next_available_register():
  return max(registers.values()) + 1

def get_labels(lines) -> 'lines':
  numlines = 0
  for line in lines:
    if line.startswith('('):
      labels[line[1:-1]] = numlines
    else:
      numlines += 1
      yield line

def main():
  fname = sys.argv[1]
  all_lines = get_lines(fname)
  code_lines = get_labels(all_lines)
  output_lines = map(line_handler, code_line)
  # do stuff with output lines
