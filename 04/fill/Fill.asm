// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel;
// the screen should remain fully black as long as the key is pressed. 
// When no key is pressed, the program clears the screen, i.e. writes
// "white" in every pixel;
// the screen should remain fully clear as long as no key is pressed.

// Put your code here.

// init fillvall and R1
  @fillval
  M = 0
  @SCREEN
  D = A
  @8192
  D = D + A
  @R1
  M = D

(START)
  // reset fillval to 0
  @fillval
  M = 0

  // load keyboard register
  @KBD
  D = M

  // if equal to 0, skip setting fillval to -1 and jump to loop
  @LOOPSETUP
  D; JEQ
  @fillval
  M = -1

// initialize i to screen start
(LOOPSETUP)
  @SCREEN
  D = A
  @i
  M = D
  
// fill screen registers (set SCREEN[i] to -1 for i = 0...8191)
(LOOP)
  // if i - (SCREEN + 8192) == 0, jump back to start
  @i
  D = M
  @R1
  D = D - M
  @START
  D; JEQ

  // assign fillval to M[i]
  @fillval
  D = M
  @i
  A = M
  M = D

  // i++ and loop back
  @i
  M = M + 1
  @LOOP
  0; JMP
