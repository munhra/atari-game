;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare the files that will be used and the processor type
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    PROCESSOR 6502
    INCLUDE "vcs.h"
    INCLUDE "macro.h"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare the variables starting from address $80
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    SEG.U Variables
    ORG $80
JetXPos             byte      ;player-0 X position
JetYPos             byte      ;player-0 Y position
BomberXPos          byte      ;player-1 X position, enemy will be player 1
BomberYPos          byte      ;player-1 Y position, enemy will be player 1
Score               byte      ;2-digits stored as BCD
Timer               byte      ;2-digits stored as BCD
Temp                byte      ;auxiliary variable
OnesDigitOffset     word      ;lookup table offset of 1 digits
TensDigitOffset     word      ;lookup table offset or 10 digits
JetSpritePtr        word      ;pointer to player-0 look up table
JetColorPtr         word      ;pointer to player-0 color look up table
BomberSpritePtr     word      ;pointer to bomber sprite look up table
BomberColorPtr      word      ;pointer to bomber color look up table
JetAnimationOffset  byte      ;player 0 sprite frame offset
Random              byte      ;random number generated to set enemy position
ScoreSprite         byte      ;store score sprite 
TimerSprite         byte      ;store timer sprite
TerrainColor        byte      ;color of the terrain
RiverColor          byte      ;color of the river
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
JET_HEIGHT = 9                ;player 0 constante to height size
BOMBER_HEIGHT = 9             ;player 1 constante to height size
DIGITS_HEIGHT = 5             ;height of the numbers font of score board 
                              ;5 something is wrong here had to change 
                              ;from 5 to 6
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start ROM code at memory address $F000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    SEG CODE
    org $F000

Reset:
    CLEAN_START             ;call macro to reset memory and registers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize RAM variables and TIA registers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    LDA #10
    STA JetYPos             ;give default value for Jet Y position
    LDA #100
    STA JetXPos             ;give default value for Jet X position
    LDA #83
    STA BomberYPos          ;initialize default Y bomber position
    LDA #90
    STA BomberXPos          ;initilize default X bomber position
    LDA #%11010100          ;initialize random seed
    STA Random              ;with value D4
    LDA #6
    STA Score               ;initialize score with 0
    LDA #9
    STA Timer               ;initialize timer with 0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize the pointers to the correct table position for color and sprite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    LDA #<JetSprite         ;load in register the lo-byte of the address of the 
                            ;lookup table
    STA JetSpritePtr        ;store the lo-byte into the pointer
    LDA #>JetSprite         ;load in register the hi-byte of the address of the
                            ;lookup table
    STA JetSpritePtr+1      ;store the hi-byte on the pointer

    LDA #<JetColor      
    STA JetColorPtr
    LDA #>JetColor     
    STA JetColorPtr+1

    LDA #<BomberSprite      
    STA BomberSpritePtr
    LDA #>BomberSprite     
    STA BomberSpritePtr+1

    LDA #<BomberColor      
    STA BomberColorPtr
    LDA #>BomberColor     
    STA BomberColorPtr+1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start main display loop and frame rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
StartFrame:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display VSYNC and VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    LDA #2
    STA VBLANK         ;Turn on VBLANK
    STA VSYNC          ;Turn on VSYNC
    REPEAT 3
        STA WSYNC      ;Display 3 recomended VSYNC lines
    REPEND
    LDA #0
    STA VSYNC          ;Turn off VSYNC
    ;LDA #2
    ;STA VBLANK
    REPEAT 35          ;How many lines are left ??? when doing some extra
                       ;Process how many clock cycles the result is 37 - 4 
                       ;33 clocks are consumed below
        STA WSYNC      ;Display 37 recomended VBLANK
    REPEND
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations and tasks performed in the pre VBLANK
;; VBLANK is a good place to perform some calculations that can take some time
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    LDA JetXPos                   ;3 |
    LDY #0                        ;2 |
    ;JSR SET_OBJECT_X_POSITION    ;6 | set player 0 horizontal position + WSYNC
    LDA BomberXPos                ;3 |
    LDY #1                        ;2 |
    ;JSR SET_OBJECT_X_POSITION    ;6 | set player 1 horizontal position + WSYNC
    JSR CalculateDigitOffSet     ;6 | calculate the score board digit lookup offset
    STA WSYNC                     ; included here as a replacement of JSR
    ;STA WSYNC                     ;3 |
    ;STA HMOVE                    ;3 | this HMOVE was generating that strange pixel
    
    LDA #0                        ;2 |
    STA VBLANK                    ;3 | Turn off VBLANK, in other words end the VBLANK
                                  ; up to here 37 scan lines need to be done
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display score board 20 lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    LDA #0  
    STA COLUBK               ; clear TIA Register before display score
    STA PF0
    STA PF1
    STA PF2
    STA GRP0
    STA GRP1
    LDA #$1E                 ; set score board bright yellow
    STA COLUPF
    LDA #%00000000           ; load 0 
    STA CTRLPF               ; store 0 to not reflect the playfield
    LDX #DIGITS_HEIGHT       ; start x counter 5 height of digits
    ;STA WSYNC
.ScoreDigitLoop:
    LDY TensDigitOffset      ; get the tens digit offset of the score
    LDA Digits,Y             ; load the bit pattern from lookup table
    AND #$F0                 ; mask/remove graphics for the ones digit
    STA ScoreSprite          ; save the score and digit pattern in variable
    
    LDY OnesDigitOffset      ; get the ones digit offset of the score
    LDA Digits,Y             ; load digit pattern from lookup table
    AND #$0F                 ; mask to remove the tens digit
    
    ORA ScoreSprite          ; merge it with the saved tens digit graphics
    STA ScoreSprite          ; and save it
    
    STA PF1                  ; update playfield to display the score on the screen
    ;STA WSYNC                ; wait for the next scanline
    
    JSR Sleep12Cycles        ; wast some cicles this will give time for the bean
    JSR Sleep12Cycles

    LDY TensDigitOffset+1    ; get the tens digit of the timer
    LDA Digits,y             ; load digit pattern from lookup table
    AND #$F0                 ; mask and remove the ones digits
    STA TimerSprite
    
    LDY OnesDigitOffset+1    ; get the ones digits offset for the timer
    LDA Digits,Y             ; load digits pattern from the table
    AND #$0F                 ; mask ones digits
    
    ORA TimerSprite          ; join both tens and ones
    STA TimerSprite          ; store joined timer sprite, remember each pass of this
                             ; loop is one line of the score/timer sprite
                             
    
                             ; to reach the other side and after that we can change
                             ; PF1 values to be asymetrical
    STA PF1
    LDY ScoreSprite          ; Preload for the next scan line
    STA WSYNC
    ;STA WSYNC
    STY PF1                  ; Load the sprite in PF1 for exibition


    INC TensDigitOffset      ; Increment score tens number sprite line
    INC TensDigitOffset+1    ; Increment timer tens number sprite line 
    INC OnesDigitOffset      ; Increment score ones number sprite line
    INC OnesDigitOffset+1    ; Increment timer ones number sprite line

    JSR Sleep12Cycles        ; sleep again 12 cycles for the bean to reach
                             ; the required position

    ;STY PF1

    DEX                      ; X--
    STA PF1                  ; update PF1 with the timer
    BNE .ScoreDigitLoop      ; if dex !=0 branch to score loop
    

    STA WSYNC
    ;STA WSYNC

    ;LDA #0                   ;2 |
    ;STA VBLANK               ;3 | Turn off VBLANK, in other words end the VBLANK
    LDA #0
    STA PF1
    STA PF2
    STA PF0
    STA WSYNC
 
    STA WSYNC
    STA WSYNC
    STA WSYNC
    STA WSYNC
    STA WSYNC
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display the 192 visible scanlines for the main part of the game
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GameVisibleLine:
    LDA TerrainColor           
    STA COLUPF              ;Set background to blue
    LDA RiverColor                ;Set color for the playfield
    STA COLUBK
    LDA #%00000001
    STA CTRLPF              ;Reflect player field
    LDA #$F0                ;Setting PF0 pattern
    STA PF0
    LDA #$FC
    STA PF1
    LDA #0
    STA PF2
    LDX #88                 ;Remaining scan lines, two lines kernel
.GameLineLoop:
    ;STA WSYNC
.AreInsideJetSprite:
    TXA                     ;Transfer X to A    
    SEC                     ;Reset carrier before subtraction
    SBC JetYPos             ;subtract the position
    CMP JET_HEIGHT          ;are we inside the sprite boundary
    BCC .DrawSpriteP0       ;if result < SpriteHeight call draw rountine
    LDA #0                  ;else set lookup index to 0
.DrawSpriteP0
    CLC                     ;clear carry for adition
    ADC JetAnimationOffset  ;add ofset for the turn frame this wil change the look up table
    TAY                     ;transfer value of register A to register Y
                            ;it is required to transfer as Y a index register
                            ;for memory addressing
    LDA (JetSpritePtr),Y               
    STA WSYNC               ;hit scan line
    STA GRP0                ;set graphics for the player 0
    LDA (JetColorPtr),Y     ;load sprite 0 color lookup table
    STA COLUP0              ;store the color at the color register

.AreInsideBomberSprite:
    TXA                     
    SEC                     
    SBC BomberYPos             
    CMP BOMBER_HEIGHT
    BCC .DrawSpriteP1       
    LDA #0                  
.DrawSpriteP1
    TAY       
    LDA #%00000101          ;pattern to stretch
    STA NUSIZ1              ;Strech horizontal the player 1
                                       
    LDA (BomberSpritePtr),Y               
    STA WSYNC               
    STA GRP1             
    LDA (BomberColorPtr),Y     
    STA COLUP1              

    DEX                     ;x--
    BNE .GameLineLoop       ;repeat until finished
    LDA #0
    STA JetAnimationOffset  ;reset jet animation offset
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Over Scan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    LDA #2
    STA VBLANK         ;turn on VBLANK
    REPEAT 30
        STA WSYNC      ;Output 30 scanlines
    REPEND
    LDA #0             ;turn off VBLANK
    STA VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process Joystick input for player 0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CHECK_P0_UP:
    LDA #%00010000                   ; player 0 joystick up
    BIT SWCHA
    BNE CHECK_P0_DOWN
    ;
    ; Up logic goes here
    ;
    INC JetYPos
    LDA #0
    STA JetAnimationOffset
CHECK_P0_DOWN:
    LDA #%00100000                    ; player 0 joystick down
    BIT SWCHA
    BNE CHECK_P0_LEFT
    ;
    ; Down logic goes here
    ;
    DEC JetYPos
    LDA #0
    STA JetAnimationOffset
CHECK_P0_LEFT:
    LDA #%01000000                    ; player 0 joystick left
    BIT SWCHA
    BNE CHECK_P0_RIGHT
    ;
    ; Left logic goes here
    ;
    ;DEC PLAYER0_X_POS
    DEC JetXPos
    LDA JET_HEIGHT                    ; load animation offset for the turn frame
    STA JetAnimationOffset
CHECK_P0_RIGHT:
    LDA #%10000000                    ; player 0 joystick right
    BIT SWCHA
    BNE P0_DIRECT_NOT_DETECTED
    ;
    ; Right logic goes here
    ;
    ;INC PLAYER0_X_POS
    INC JetXPos
    LDA JET_HEIGHT                    ; load animation offset for the turn frame
    STA JetAnimationOffset
P0_DIRECT_NOT_DETECTED:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations to update position for next frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
UpdateBomberPosition:
    ;LDA BomberYPos
    ;CLC
    ;CMP #0                           ;compare bomber Y position with 0
    ;BMI .ResetBomberPosition         ;if it is < 0 then reset y position to top
    ;DEC BomberYPos                   ;else, decrement enemy y position
    ;JMP EndPositionUpdate            ;jump and does not reset to initial
;.ResetBomberPosition
    ;JSR GetRandomBomberPos           ;call sub routine for random X position
EndPositionUpdate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check colision of the objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    LDA #$C2
    STA TerrainColor                ; set terrain color to green
    LDA #$84                        ; was 84
    STA RiverColor                  ; set tiver color to blue

;CheckCilisionP0P1:
;    LDA #%10000000                    ; CXPPMM bit 7 detects P1 and P0 colision
;    BIT CXPPMM                        ; it works like an AND operation
;    BNE .P0P1Collided                 ; collision happened
;    JSR SetTerrainRiverColor          ; else set play field to green/blue
;    JMP EndCollisionCheck             ; else skip to PF colision check
;.P0P1Collided
    ;JSR GameOver                      ; if collision go to gameover
;CheckColisionP0PF
;    LDA #%10000000                   ; CXP0FB bit 7 detects P1 and P0 colision
;    BIT CXP0FB                       ; and operation with TIA register
;    BNE .CollisionP0PF               ; collision with playfield happened
                                      ; jump tp game over
;    JMP EndCollisionCheck            ; else jump to end collision check
;.CollisionP0PF
;    JSR GameOver
EndCollisionCheck:
    STA CXCLR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loop back to the frame loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    JMP StartFrame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set the color to river and terrain for green and blue
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetTerrainRiverColor
    LDA #$C2
    STA TerrainColor                ; set terrain color to green
    LDA #$84                        ; was 84
    STA RiverColor                  ; set tiver color to blue
    RTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example o how to define a sub-routine, this subroutine will process the
;; X position for all moveable objects according the parameters below
;; A Register contains the value of X to be positioned
;; Y = 0 : Player 0
;; Y = 1 : Player 1
;; Y = 2 : Missle 0
;; Y = 3 : Missle 1
;; Y = 4 : Ball 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SET_OBJECT_X_POSITION:
    STA WSYNC
    SEC
.SPRT_HORIZONTAL_LOOP
    SBC #15
    BCS .SPRT_HORIZONTAL_LOOP
    EOR #7
    ASL
    ASL
    ASL
    ASL
    STA HMP0,Y
    STA RESP0,Y
    RTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine Random number generator, Linear Feedback shift register random
;; Generate LFSR, divde by 4 to limit the size or result
;; Add 30 to compensenta the position
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GetRandomBomberPos
    LDA Random
    ASL
    EOR Random
    ASL
    EOR Random
    ASL 
    ASL
    EOR Random
    ASL
    ROL Random
    LSR
    LSR                    ;devide the number by 4 with 2 right shifts
    STA BomberXPos         ;store the X position on the bomber
    LDA #30
    ADC BomberXPos         ;add 30 to bomber X pos to compensate the left field
    STA BomberXPos         ;Save to the memory
    LDA #96
    STA BomberYPos
    RTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to handle score board digits to be displayed on the screen 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert the high and low nibbles of variable score and time
;; into the offsets of digits lookup table, so the variables can be displaye
;; each digit has a high of 5 bytes
;;
;; For the low nibble we need to multiply by five as each digit has 5 of height
;; - we can use left shifts to perform multiplications by 2
;; - for any number N. the value of N * 5 = (N*2*2) + N
;;
;; For the upper nibble, since it is already times 16 wee need to divide it
;; by 16 and multiply by 5:
;; - we can use right shifts to perform division by 2
;; - for any number N. the value of (N/16) * 5 = (N/2/2) + (N/2/2/2/2)
;;   it is required to divide it by 16 as it is the upper nibble
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CalculateDigitOffSet
    LDX #1                  ; X register is the loop counter
.PrepareScoreLoop           ; Loop twice one for x=1 and other for x=0
    LDA Score,X             ; Score + 1 is the timer and Score + 0 is score
    AND #$0F                ; remove the last 4 bits, remove the ten digits
    STA Temp                ; save the value of A to a temporary variable
    ASL                     ; shift left equals to (A = A * 2)
    ASL                     ; shift left equals to (A = A * 2)
    ADC Temp                ; add N to A and we have A = (N*2*2) + N
    STA OnesDigitOffset,X   ; it stores both ones digits for score and timer

    LDA Score,X             ; Load A with the timer X=1 or score X=0
    AND #$F0                ; remove the ones digits by masking with 4 digits
    LSR                     ; it is now N/2
    LSR                     ; it is now N/4
    STA Temp                ; save the value on the temp variable the one 
                            ; divided by 4 is now saved on temp variable
    LSR                     ; it is now N/8
    LSR                     ; it is now N/16
    ADC Temp                ; N/16 + N/4
    STA TensDigitOffset,X   ; store the result in tens digit offset, for x=1
                            ; and X=0 both score and timer
    DEX                     ; X--
    BPL .PrepareScoreLoop   ; while x >= 0 loop pass a second time
    RTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to just spend 12 cycles to mainly to make the bean reach the
;; desired position
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Sleep12Cycles
    RTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game Over subroutine 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JSR Takes 6 cycles
;; RTS Takes 6 cycles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GameOver
    LDA #$30
    STA TerrainColor            ; Terrain color change to red to represent
                                ; Game over
    STA RiverColor              ; River color change to red ti represent
                                ; Game Over
    LDA #0
    STA ScoreSprite             ; zeroe score value
    RTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rom look up tables, declare here sprites and other elements that will be
;; stored in tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Digits:
    .byte %01110111
    .byte %01010101
    .byte %01010101
    .byte %01010101
    .byte %01110111
        
    .byte %00010001
    .byte %00010001
    .byte %00010001
    .byte %00010001        
    .byte %00010001
        
    .byte %01110111
    .byte %00010001
    .byte %01110111
    .byte %01000100
    .byte %01110111
        
    .byte %01110111
    .byte %00010001
    .byte %00110011
    .byte %00010001
    .byte %01110111
        
    .byte %01010101
    .byte %01010101
    .byte %01110111
    .byte %00010001
    .byte %00010001
        
    .byte %01110111
    .byte %01000100
    .byte %01110111
    .byte %00010001
    .byte %01110111
           
    .byte %01110111
    .byte %01000100
    .byte %01110111
    .byte %01010101
    .byte %01110111
        
    .byte %01110111
    .byte %00010001
    .byte %00010001
    .byte %00010001
    .byte %00010001
        
    .byte %01110111
    .byte %01010101
    .byte %01110111
    .byte %01010101
    .byte %01110111
        
    .byte %01110111
    .byte %01010101
    .byte %01110111
    .byte %00010001
    .byte %01110111
        
    .byte %00100010
    .byte %01010101
    .byte %01110111
    .byte %01010101
    .byte %01010101
         
    .byte %01100110
    .byte %01010101
    .byte %01100110
    .byte %01010101
    .byte %01100110
        
    .byte %00110011
    .byte %01000100
    .byte %01000100
    .byte %01000100
    .byte %00110011
        
    .byte %01100110
    .byte %01010101
    .byte %01010101
    .byte %01010101
    .byte %01100110
        
    .byte %01110111
    .byte %01000100
    .byte %01100110
    .byte %01000100
    .byte %01110111
        
    .byte %01110111
    .byte %01000100
    .byte %01100110
    .byte %01000100
    .byte %01000100

JetSprite:
    .byte #%00000000
    .byte #%00001000;$34
    .byte #%01111111;$06
    .byte #%00111110;$02
    .byte #%00011100;$06
    .byte #%00011100;$A8
    .byte #%00001000;$02
    .byte #%00001000;$B8
    .byte #%00001000;$04
JetSpriteTurn
    .byte #%00000000
    .byte #%00001000;$34
    .byte #%00111110;$06
    .byte #%00011100;$02
    .byte #%00011100;$06
    .byte #%00011100;$A8
    .byte #%00001000;$02
    .byte #%00001000;$B8
    .byte #%00001000;$04
BomberSprite:
    .byte #%00000000
    .byte #%01000010;$06
    .byte #%01000010;$06
    .byte #%01011010;$06
    .byte #%01100110;$06
    .byte #%01100110;$06
    .byte #%01011010;--
    .byte #%01000010;--
    .byte #%01000010;--
JetColor:
    .byte #$00
    .byte #$34;
    .byte #$06;
    .byte #$02;
    .byte #$06;
    .byte #$A8;
    .byte #$02;
    .byte #$B8;
    .byte #$04;
JetColorTurn:
    .byte #$00
    .byte #$34;
    .byte #$06;
    .byte #$02;
    .byte #$06;
    .byte #$A8;
    .byte #$02;
    .byte #$B8;
    .byte #$04;
BomberColor:
    .byte #$06;
    .byte #$06;
    .byte #$06;
    .byte #$06;
    .byte #$06;
    .byte #$0E;
    .byte #$0E;
    .byte #$0E;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complete ROM size with 4KB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ORG $FFFC          ;move to position $FFFC
    .WORD Reset
    .WORD Reset