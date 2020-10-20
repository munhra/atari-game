;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define the type o processor and load vcs.h header that contains 
;; Menemonics to Atari important registers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    processor 6502
    include "vcs.h"
    include "macro.h"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define the memory variables starting at RAM address position $80
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    SEG.U VARIABLES
    ORG $80
VARIABLE1 DS 1                ; Example to define 1 byte for varaible1
VARIABLE2 DS 1                ; Example to define 1 byte for variable2
PLAYER0_HEIGHT DS 1           ; Player 0 Height used to control sprite 
                              ; player position in memory
PLAYER0_Y_POS  DS 1           ; Player 0 initial Y position
PLAYER0_HEIGHT_Y_POS DS 1     ; PLAYER0_HEIGHT + PLAYER0_HEIGHT_Y_POS
PLAYER0_HEIGHT_Y_MINUS_1 DS 1 ; PLAYER0_HEIGHT + PLAYER0_HEIGHT_Y_POS - 1

PLAYER0_X_POS DS 1            ; Player 0 initial X position
SPARE_VARIABLE_1 DS 1         ; Just a spare variable for some testing 
                              ; purpouses

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define the start of the code at ROM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    SEG ORG
    ORG $F000
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
;; Define the start of the program label, a reset will put the exection from
;; from this point, macro clean_start will be used to intialize the registers
;; and memory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
START:
    CLEAN_START
    LDX #$FF
    TXS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example how use the stack to write a value
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    LDA #$AA
    PHA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start of a new frame 
;; from this point, macro clean_start will be used to intialize the registers
;; and memory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    LDA #50                  ; Initialize player 0 Y position with value 50
                             ; change here to modify the Y position
    STA PLAYER0_Y_POS    
    LDA #2
    STA PLAYER0_X_POS        ; Initialize Player X Position
    LDA #$08                 ; Initialize player height with value 8, the 
                             ; sprite size, change here to modify sprite 
                             ; height
    STA PLAYER0_HEIGHT
    ADC PLAYER0_Y_POS        ; PLAYER0_HEIGHT_Y_POS = PLAYER0_HEIGHT + 
                             ; PLAYER0_HEIGHT
    STA PLAYER0_HEIGHT_Y_POS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To initialize player 0 position change the value of variable PLAYER0_Y_POS,
;; if the heitght of the sprite is diferent from 8 change variable 
;; PLAYER0_HEIGHT with the desired value
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
START_FRAME:
    LDA #$40       ; Load value $84, color red to A
    STA COLUP0     ; Store color red in player 0 color register
    LDA #0         ; Load value $0, in register A
    STA GRP0       ; Reset value of player 0 register to 0 
    LDA #$1E       ; Load value 1E in register A
    STA COLUBK     ; Store value 1E in COLUBK to set background do yellow
    LDA #$0        ; Load this value to multiply or stretch the sprite
    STA NUSIZ0     ; Store value in this register to control stretch and mult
    LDA #$1        ; Write 1 to reflect or 0 not reflect the sprite
    ;STA REFP0      ; This is the register that reflect the sprite 0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; First write 3 scan lines required for the VERTICAL SYNC, set B1 of VSYNC
;; write 3 WSYNC lines and disable VSYNC by writting 0 on it
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    LDA #2
    STA VSYNC
    STA WSYNC
    STA WSYNC
    STA WSYNC
    LDA #0
    STA VSYNC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example on how to attribute a value to a variable in memory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    LDA #$FF
    STA VARIABLE1
    LDA #20
    STA VARIABLE2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Read Controller UP, DOWN, LEFT, RIGHT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CHECK_P0_UP:
    LDA #%00010000
    BIT SWCHA
    BNE CHECK_P0_DOWN
    ;
    ; Up logic goes here
    ;
    INC PLAYER0_Y_POS
CHECK_P0_DOWN:
    LDA #%00100000
    BIT SWCHA
    BNE CHECK_P0_LEFT
    ;
    ; Down logic goes here
    ;
    DEC PLAYER0_Y_POS
CHECK_P0_LEFT:
    LDA #%01000000
    BIT SWCHA
    BNE CHECK_P0_RIGHT
    ;
    ; Left logic goes here
    ;
    DEC PLAYER0_X_POS
CHECK_P0_RIGHT:
    LDA #%10000000
    BIT SWCHA
    BNE P0_DIRECT_NOT_DETECTED
    ;
    ; Right logic goes here
    ;
    INC PLAYER0_X_POS
P0_DIRECT_NOT_DETECTED:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Move player 0 sprite from bottom to top of the screen, adding 1 to 
;; the position on every frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    LDA PLAYER0_Y_POS
    ;ADC #1                   ; this addition will make the sprite move from
                              ; bottom to top without command
    STA PLAYER0_Y_POS
    LDA PLAYER0_HEIGHT
    ADC PLAYER0_Y_POS
    STA PLAYER0_HEIGHT_Y_POS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Just before VERTICAL SYNC, let's set the sprite horizontal position
;; calling a subroutine that available at label 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    LDA PLAYER0_X_POS
    AND #$7F
    LDY #0
    JSR SET_OBJECT_X_POSITION
    STA WSYNC
    STA HMOVE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; First write 3 scan lines required for the VERTICAL SYNC, set B1 of VSYNC
;; write 3 WSYNC lines and disable VSYNC by writting 0 on it
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    LDA #0
    STA VBLANK
    LDX #37
VBLANK_LINES_LOOP:
    STA WSYNC
    DEX
    BNE VBLANK_LINES_LOOP
    LDA #0
    STA VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; First write 37 scan lines required for the VERTICAL BLANK, using a loop
;; that will run 37 times
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    LDX #192
    LDY #0
VISIBLE_LINES:
    CPX PLAYER0_Y_POS
    BCC END_SPRITE
    CPX PLAYER0_HEIGHT_Y_POS    ; This value needs to be 
                                ; PLAYER0_Y_POS + PLAYER0_HEIGHT
    BCS END_SPRITE
    LDA HEART_BITMAP,Y
    STA GRP0
    INY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If scanlines are between 50 and 58 draw the sprite loading each of the 8
;; lines into register A using LDA that points to the bitmap in memory
;; and store this value in GPR0 player register
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
END_SPRITE:
    LDA PLAYER0_Y_POS
    SEC
    SBC #1
    STA PLAYER0_HEIGHT_Y_MINUS_1
    CPX PLAYER0_HEIGHT_Y_MINUS_1
    BNE CLEAR_SPRITE
    LDA #0
    STA GRP0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check if it is the last sprite line 49 and clear the register or it will 
;; flood to the bottom of the screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CLEAR_SPRITE:
    STA WSYNC
    DEX
    BNE VISIBLE_LINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Draw 192 game visible lines, the game will came here, create a loop that
;; will run 192 times and will print a mock information
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    LDX #30
OVERSCAN_LINES:
    LDA #2
    STA VBLANK
    STA WSYNC
    DEX
    BNE OVERSCAN_LINES
    JMP START_FRAME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Draw 30 lines, required for the overscan, first set VBLANK store anything on 
;; WSYNC, after that a new frame can be draw
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ORG $FFF4
HEART_BITMAP:
    .BYTE #%11000110
    .BYTE #%11101110
    .BYTE #%11111110
    .BYTE #%11111110
    .BYTE #%11111110
    .BYTE #%01111100
    .BYTE #%00111000
    .BYTE #%00010000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example to define a bitmap to be displayed as player 1 or player 2
;; this bitmap is stored at ROM address
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ORG $FFFC
    .WORD START       ; directly write on the rom position 
    .WORD START
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Close the rom as required by the system 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;










