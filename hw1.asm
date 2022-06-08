* Yao Xu Homework 1
* Register use: A accumulator: character data to be filled
*               B accumulator: counter, number of filled locations
*               X register:    memory address pointer
*
* Memory use: RAM Locations from $3000 to $3009
*
* Input: Parameters hard coded in the program
*
* Output: Data filled in memory locations,
* from $3000 to $3009 changed
*
* Observation: This program is desgined for instruction purpose.
* This program can be used as a 'loop' template
*
* Comments: This program is developed and simulated using CodeWorrior
* development software. This program is used to put 202 stars in certain memory location
*
***********************************************************************

* Parameter Declearation Section
*
* Export symbols
        XDEF      pgstart ; export 'pgstart' symbol
        ABSENTRY  pgstart ; for assembly entry point
* Symbols and Macros
PORTA   EQU       $0000   ; i/o port addresses
PORTB   EQU       $0001 
DDRA    EQU       $0002
DDRB    EQU       $0003
***********************************************************************

* Data Section
*

        ORG       $3000   ;reserved memory starting address
here    DS.B      $CA     ;202 memory locations reserved
count   DC.B      $CA     ;constant, star count = 202
*
***********************************************************************

* Program Section
*
        ORG       $3100   ;Program start address, in RAM
pgstart ldaa      #'*'    ;load '*' into accumulator A
        ldab      count   ;load star counter into B
        ldx       #here   ;load address pointer into X
loop    staa      0,x     ;put a star
        inx               ;point to next location
        decb              ;decrease counter
        bne       loop    ;if not done, repeat
done    bra       done    ;task finished,
                          ; do nothing
*
* Add any aubroutines here
*
        END               ;last line of a file
