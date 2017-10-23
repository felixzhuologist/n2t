// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Mult.asm

// Multiplies R0 and R1 and stores the result in R2.
// (R0, R1, R2 refer to RAM[0], RAM[1], and RAM[2], respectively.)

// Put your code here.

  // initialize product as 0 in R2
  @R2
  M = 0
  @i
  M = 0

(LOOP)
  // if i - r0 >= 0, jump to end
  @i
  D = M
  @R0
  D = D - M
  @END
  D; JGE

  // store r1 + r2 in r2
  @R1
  D = M
  @R2
  M = M + D

  // increment i
  @i
  M = M + 1

  // jump to if statement
  @LOOP
  0; JMP

(END)
  // end loop
  @END
  0; JMP