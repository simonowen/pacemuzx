; Pac-Man hardware emulation for the Sinclair ZX Spectrum
;
; http://simonowen.com/spectrum/pacemuzx/

debug: equ 0

; Memory maps
;
; Loading (normal paging R/5/2/0):
; a000-afff - emulation code
; b000-bfff - unshifted tiles+sprites
; c000-ffff - 16K Pac-Man ROM
;
; Emulation (special paging 0/1/2/3):
; 0000-3fff - 16K Pac-Man ROM
; 4000-50ff - Pac-Man display, I/O and working RAM
; 5100-7fff - unused
; 8000-9fff - 2nd half of sprite data
; a000-afff - emulation code
; b000-bfff - look-up tables
; c000-dfff - 8K sound table
; e000-ffff - first 8K of Pac-Man ROM (unpatched)
;
; Graphics (normal paging R/5/2/7):
; 0000-3fff - Spectrum ROM
; 4000-5aff - Spectrum display (normal)
; 5b00-5be3 - screen data behind sprites (normal)
; 5be4-9fff - pre-rotated sprite graphics
; a000-afff - emulation code
; b000-bfff - look-up tables
; c000-daff - Spectrum display (alt)
; db00-dbe3 - screen data behind sprites (alt)
; dbe4-ffff - pre-rotated tile graphics

default_attr:  equ &07              ; default = white on black

kempston:      equ 31               ; Kempston joystick in bits 4-0
divide:        equ 227              ; DivIDE interface
border:        equ 254              ; Border colour in bits 2-0
keyboard:      equ 254              ; Keyboard matrix in bits 4-0

pac_footer:    equ &4000            ; credit and fruit display
pac_chars:     equ &4040            ; start of main Pac-Man display (skipping the score rows)
pac_header:    equ &43c0            ; 64 bytes containing the score

; address of saved sprite block followed by the data itself
spr_save_2:    equ &5b00
spr_save_3:    equ spr_save_2+2+(3*12)
spr_save_4:    equ spr_save_3+2+(3*12)
spr_save_5:    equ spr_save_4+2+(3*12)
spr_save_6:    equ spr_save_5+2+(3*12)
spr_save_7:    equ spr_save_6+2+(3*12)
spr_save_end:  equ spr_save_7+2+(3*12)

; pre-shifted sprite graphics
spr_data_0:    equ spr_save_end
spr_data_1:    equ spr_data_0 + (76*2*12)  ; 11111111 11110000
spr_data_2:    equ spr_data_1 + (76*2*12)  ; 01111111 11111000
spr_data_3:    equ spr_data_2 + (76*2*12)  ; 00111111 11111100
spr_data_4:    equ spr_data_3 + (76*2*12)  ; 00011111 11111110
spr_data_5:    equ spr_data_4 + (76*2*12)  ; 00001111 11111111
spr_data_6:    equ spr_data_5 + (76*3*12)  ; 00000111 11111111 10000000
spr_data_7:    equ spr_data_6 + (76*3*12)  ; 00000011 11111111 11000000
spr_data_end:  equ spr_data_7 + (76*3*12)  ; 00000001 11111111 11100000

; pre-shifted tile graphics
tile_data_0:   equ &8000 + spr_data_0
tile_data_6:   equ tile_data_0 + (192*1*6) ; 11111100
tile_data_4:   equ tile_data_6 + (192*2*6) ; 00000011 11110000
tile_data_2:   equ tile_data_4 + (192*2*6) ;          00001111 11000000
end_tile_data: equ tile_data_2 + (192*1*6) ;                   00111111

; sound look-up table
sound_table:   equ &c000


MACRO set_border, colour
IF debug
    ld a,colour
    out (border),a
ENDIF
ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

               org &a000
start:         jr  start2

; Dips at fixed address for easy poking
dip_5000:      defb %11111111       ; c21tdrlu     c=credit, 2=coin2, 1=coin1, t=rack test on/off ; down, right, left, up
                                    ; default: rack test off, nothing else signalled
dip_5040:      defb %11111111       ; c--sdrlu     c=cocktail/upright ; s=service mode on/off ; down, right, left, up (player 2)
                                    ; default: upright mode, service mode off, no controls pressed
dip_5080:      defb %11001001       ; -dbbllcc      d=hard/normal bb=bonus life at 10K/15K/20K/none ; ll=1/2/3/5 lives ; cc=freeplay/1coin1credit/1coin2credits/2coins1credit
                                    ; default: normal, bonus life at 10K, 3 lives, 1 coin 1 credit

; First, check that +2A/+3 paging is available
start2:        di
               ld  a,(&5b5c)        ; sysvar holding 128K paging
               ex  af,af'           ; keep safe
               ld  a,%00000001      ; special paging, banks 0/1/2/3
               ld  bc,&1ffd         ; +3 paging port
               out (c),a            ; attempt paging
               ld  a,(3)            ; peek value in Pac-Man ROM
               cp  &ed              ; as expected?
               jr  z,start3         ; if so, start up

               ex  af,af'
               call &0d6b           ; CLS
               ld  a,2              ; main screen
               call &1601           ; CHAN-OPEN
               out (c),a            ; restore 128K paging (disturbed due to partial address decoding)
               ei

               ld  hl,fail_msg
msg_lp:        ld  a,(hl)
               and a
               ret z
               rst 16               ; PRINT-A
               inc l
               jr  msg_lp

fail_msg:      defm "This program requires a +2A/+3"
               defb 0

; Next, move any data from load position to final location
start3:        ld  sp,new_stack

               xor a
               out (border),a       ; black border

               ld  hl,&0000
               ld  de,&e000
               ld  bc,&2000
               ldir                 ; copy 8K of ROM to &e000 (start of page 3)

               call patch_rom       ; patch the ROM while it's at the correct location

               ld  a,%00000100      ; +3 normal paging
               ld  bc,&1ffd
               out (c),a            ; restore R3/5/2/0, for ROM access at &c000

               ld  a,%10000011      ; DivIDE page 3
               out (divide),a       ; page in at &2000, if present

               ld  hl,&c000
               ld  de,&2000
               ld  bc,&2000
               ldir                 ; copy first 8K of ROM to DivIDE page 3

               ld  a,%10000000      ; DivIDE page 0
               out (divide),a       ; page in at &2000, if present

               ld  de,&2000
               ld  bc,&2000
               ldir                 ; copy last 8K of ROM to DivIDE page 0

               ld  a,%01000000      ; MAPRAM
               out (divide),a       ; page out DivIDE, if present

               ld  a,(scr_page)     ; normal display, page 7
               ld  bc,&7ffd
               out (c),a            ; page 7 at &c000
               ld  ixh,&80          ; write to alt screen

               ld  hl,load_tiles
               ld  de,tile_data_0
               ld  bc,&0480
               ldir                 ; copy unshifted tile data

               ld  hl,load_sprites
               ld  de,spr_data_0
               ld  bc,&0720
               ldir                 ; copy unshifted sprite data

; Clear both screens and set the default attribute colour
               ld  b,2              ; 2 screens to prepare
scrinit_lp:    push bc

               ld  hl,&4000
               ld  de,&4001
               ld  bc,&1800
               ld  a,h
               or  ixh
               ld  h,a
               ld  d,a
               ld  (hl),l           ; clear display data
               ldir

               ld  bc,&0300         ; &300 bytes to fill
               ld  a,(attr_colour)
               ld  (hl),a           ; fill display attrs
               ldir

               ld  bc,spr_save_end-spr_save_2
               ld  (hl),l           ; clear sprite restore data
               ldir

               ld  hl,&5800+(10*32)+4 ; left column of tunnel trim 
               ld  a,h
               or  ixh
               ld  h,a
               ld  bc,23            ; right column to left on next row
               ld  de,32-24         ; left column to right column
               ld  a,5              ; 5 blocks per column 
attr_lp:       ld  (hl),b           ; hide left column (6 pixels needed)
               add hl,bc
               ld  (hl),b           ; hide right column 0
               inc l
               ld  (hl),b           ; hide right column 1 (2 pixels needed)
               add hl,de
               dec a
               jr nz,attr_lp

               call do_flip         ; switch to other display
               pop bc
               djnz scrinit_lp      ; finish both screens


               call make_tables     ; create all the look-up tables and pre-shift sprites
               call page_rom        ; page in sound table and ROM
               call sound_init      ; enable sound chip

               ld  hl,&5000         ; Pac-Man I/O area
               xor a
clear_io:      ld  (hl),a           ; zero fill it
               inc l
               jr  nz,clear_io

               ld  a,(dip_5000)     ; set hardware dips to our defaults
               ld  (&5000),a
               ld  a,(dip_5040)
               ld  (&5040),a
               ld  a,(dip_5080)
               ld  (&5080),a

wait_no_key:   xor a
               in  a,(keyboard)
               cpl
               and %00011111        ; check for any key
               jr  nz,wait_no_key   ; loop until all released, as temporary SpectraNet work-around

               ld  sp,&4c00         ; stack in spare RAM
               jp  0                ; start the ROM!  (and page in DivIDE, if present)

page_rom:      push af
               push bc
               ld  a,%00000001      ; +3 special paging, banks 0/1/2/3
               ld  bc,&1ffd
               out (c),a
               pop bc
               pop af
               ret

page_screen:   push af
               push bc
               ld  a,%00000100      ; +3 normal paging, R3/5/2/7
               ld  bc,&1ffd
               out (c),a
               pop bc
               pop af
               ret


patch_rom:     ld  a,&56            ; ED *56*
               ld  (&233c),a        ; change IM 2 to IM 1

               ld  hl,&47ed
               ld  (&233f),hl       ; change OUT (&00),A to LD I,A
               ld  (&3183),hl

               ld  a,&c3            ; JP nn
               ld  (&0038),a
               ld  hl,do_int_hook   ; interrupt hook
               ld  (&0039),hl

               ld  hl,&04d6         ; SUB 4
               ld  (&3181),hl       ; restore original instruction in patched bootleg ROMs

               ld  a,&01            ; to change &5000 writes to &5001, which is unused
               ld  (&0093),a
               ld  (&01d7),a
               ld  (&2347),a
               ld  (&238a),a
               ld  (&3194),a
               ld  (&3248),a

               ld  a,1              ; start clearing at &5001, to avoid DIP overwrite
               ld  (&230c),a
               ld  (&2353),a
               ld  a,7              ; shorten block clear after start adjustment above
               ld  (&230f),a
               ld  (&2357),a

               ld  a,&41            ; start clearing at &5041, to avoid DIP overwrite
               ld  (&2363),a
               ld  a,&3f            ; shorten block clear after start adjustment above
               ld  (&2366),a

               ld  a,&b0            ; LSB of address in look-up table
               ld  (&3ffa),a        ; skip memory test (actual code starts at &3000)

               ld  hl,&e0f6         ; change AND &1F to OR &E0 so ROM peeks are from unmodified copy of the ROM
               ld  (&2a2d),hl       ; (used as random number source for blue ghost movements)

               ld  a,&cd            ; CALL nn
               ld  (&2c62),a
               ld  hl,text_fix      ; fix bit 7 being set in horizontal text screen writes
               ld  (&2c63),hl

               ld  a,&dc            ; CALL C,nn
               ld  (&0353),a        ; disable 1UP/2UP flashing to save cycles
               ld  (&035e),a

               ret

; The ROM uses bit 7 of the display address to indicate text to the top/bottom lines, which
; is rotated compared to the main screen.  It doesn't clear the bit before using the address,
; but due to RAM mirroring the arcade version still works.  Our version requires a fix.
text_fix:      ld  e,(hl)
               inc hl
               ld  d,(hl)
               res 7,d
               ret


; Do everything we need to update video/sound/input
;
do_int_hook:   ld  (old_stack+1),sp
               ld  sp,new_stack

               push af
               push bc
               push de
               push hl
               ex  af,af'
               push af
               exx
               push bc
               push de
               push hl
               push ix

               call do_flip         ; show last frame, page in new one

               ld  hl,&5062         ; sprite 1 x
               inc (hl)             ; offset 1 pixel left (mirrored)
               ld  hl,&5064         ; sprite 2 x
               inc (hl)

set_border 1
               call do_restore      ; restore under the old sprites
set_border 2
               call do_tiles        ; update a portion of the background tiles
set_border 3
               call flash_maze      ; update maze colour if changed
               call flash_pills     ; flash the power pills
set_border 4
               call do_save         ; save under the new sprite positions
set_border 5
               call do_sprites      ; draw the 6 new masked sprites
set_border 6
               call do_trim         ; trim sprites at screen edge
set_border 7
               call do_input        ; scan the joystick and DIP switches
               call do_sound        ; convert the sound to the AY chip
set_border 0

               ld  hl,&5062         ; sprite 1 x
               dec (hl)             ; reverse change from above
               ld  hl,&5064         ; sprite 2 x
               dec (hl)

               call set_int_chain   ; prepare interrupt handler chain

               pop ix
               pop hl
               pop de
               pop bc
               exx
               pop af
               ex  af,af'
               pop hl
               pop de
               pop bc
               pop af

old_stack:     ld  sp,0             ; self-modified by code above
int_chain:     jp  0                ; address completed by set_int_chain


; Prepare the Pac-Man interrupt handler address for our return - does an IM2-style
; lookup to determine the address for normal Pac-Man interrupt processing
;
set_int_chain: ld  a,i              ; bus value originally written to port &00
               ld  l,a
               ld  h,&3f            ; normal I value
               ld  a,(hl)           ; handler low
               inc hl
               ld  h,(hl)           ; handler high
               ld  l,a
               ld  (int_chain+1),hl ; write into JP in interrupt handler
               ret


; Flip to show the screen prepared during the last frame, and prepare to draw the next
;
do_flip:       push af
               push bc

               ld  a,(scr_page)     ; current screen
               xor %00001000        ; toggle active screen bit
               ld  (scr_page),a
               ld  bc,&7ffd
               out (c),a            ; activate

               sub %00001000        ; set carry if we're viewing the normal screen
               ld  a,0
               rra
               ld  ixh,a            ; b7 holds b15 of drawing screen address

               pop bc
               pop af
               ret

scr_page:      defb %00000111       ; normal screen (page 5), page 7 at &c000


; Set the maze palette colour by detecting the attribute used for the maze white
; We also need to remove the ghost box door, as the real attribute wipe does.
;
flash_maze:    ld  a,(&4440)        ; attribute of maze top-right
               cp  &1f              ; white?
               ld  a,(attr_colour)  ; current attribute setting
               jr  nz,maze_blue     ; if not, draw as normal

               xor %01000000        ; toggle bright
               ld  b,a

               ld  a,&40            ; blank tile
               ld  (&420d),a        ; clear left of ghost box door
               ld  (&41ed),a        ; clear right of ghost box door

               ld  a,b
maze_blue:
               ; fall through...

; Set attributes to value in A
set_attrs:     call page_screen

               ex  af,af'
               ld  a,2              ; 2 screens
attr_scr_lp:   ld  hl,&5800+5
               ld  a,h
               or  ixh
               ld  h,a

               ex  af,af'
               cp  (hl)             ; check current colour
               jr  z,attr_same      ; skip fill if it's the correct colour

               ld  de,32-22
               ld  b,&18            ; 24 lines to fill
attr_fill_lp:  ld  c,22
attr_fill_lp2: ld  (hl),a
               inc l
               dec c
               jr  nz,attr_fill_lp2
               add hl,de
               djnz attr_fill_lp

               call do_flip         ; switch to other screen

               ex  af,af'
               dec a
               jr  nz,attr_scr_lp

attr_same:     jp  page_rom

attr_colour:   defb default_attr


; Set the power pill palette colour to the correct state by reading the 6 known
; pill locations, and reacting to changes in the attribute setting.
;
flash_pills:   ld  a,($4278)        ; bottom attract screen pill location 
               cp  &14              ; power pill?
               jr  z,attract_mode   ; if so, we're in attract mode

               ld  a,(&4384)        ; top-left
               cp  &14
               ld  a,(&4784)
               ld  hl,&4066         ; x=2, y=4
               ld  bc,&878f         ; pre-ORed left byte data
               ld  de,&80c0         ; original right byte data
               call z,pill_2

               ld  a,(&4064)        ; top right
               cp  &14
               ld  a,(&4464)
               ld  hl,&4079         ; x=27, y=4
               call z,pill_1

               ld  a,(&4398)        ; bottom-left
               cp  &14
               ld  a,(&4798)
               ld  hl,&5046         ; x=2, y=24
               ld  bc,&878f
               ld  de,&80c0
               call z,pill_2

               ld  a,(&4078)        ; bottom-right
               cp  &14
               ld  a,(&4478)
               ld  hl,&5059         ; x=27, y=24
               call z,pill_1

               ret

attract_mode:  ld  a,($4678)        ; bottom attract
               ld  hl,&504d         ; x=11, y=24
               call pill_1

               ld  a,(&4332)        ; top attract
               cp  &14
               ld  a,(&4732)
               ld  bc,&0103
               ld  de,&e0f0
               ld  hl,&4ca8         ; x=5, y=18
               call z,pill_2

               ret

; Pill tile in 1 byte
pill_1:        cp  &9f
               jr  z,pill_1_on
               cp  &10
               jr  nz,pill_clear_1
pill_1_on:
               ld  a,%00000100      ; +3 normal paging, R3/5/2/7
               ld  bc,&1ffd
               out (c),a

               ld  a,h
               or  ixh
               ld  h,a

               ld  de,&1e3f         ; pill data for ends and middle

               ld  (hl),d
               inc h
               ld  (hl),e
               inc h
               ld  (hl),e
               inc h
               ld  (hl),e
               inc h
               ld  (hl),e
               inc h
               ld  (hl),d

               ld  a,%00000001      ; +3 special paging, banks 0/1/2/3
               ld  bc,&1ffd
               out (c),a            ; page ROM
               ret
; clear pill
pill_clear_1:  ld  a,%00000100      ; +3 normal paging, R3/5/2/7
               ld  bc,&1ffd
               out (c),a

               ld  a,h
               or  ixh
               ld  h,a

               xor a
               ld  (hl),a
               inc h
               ld  (hl),a
               inc h
               ld  (hl),a
               inc h
               ld  (hl),a
               inc h
               ld  (hl),a
               inc h
               ld  (hl),a

               ld  a,%00000001      ; +3 special paging, banks 0/1/2/3
               ld  bc,&1ffd
               out (c),a            ; page ROM
               ret


; Pill tile spanning 2 bytes
pill_2:        cp  &10
               jr  nz,pill_clear_2

               call page_screen 

               ld  a,h
               or  ixh
               ld  h,a

               ld  (hl),b
               inc l
               ld  a,(hl)
               or  d
               ld  (hl),a
               dec l
               inc h

               ld  (hl),c
               inc l
               ld  a,(hl)
               or  e
               ld  (hl),a
               dec l
               inc h

               ld  (hl),c
               inc l
               ld  a,(hl)
               or  e
               ld  (hl),a
               dec l
               inc h

               ld  (hl),c
               inc l
               ld  a,(hl)
               or  e
               ld  (hl),a
               dec l
               inc h

               ld  a,h
               and %00000111
               call z,blockdown_hl

               ld  (hl),c
               inc l
               ld  a,(hl)
               or  e
               ld  (hl),a
               dec l
               inc h

               ld  (hl),b
               inc l
               ld  a,(hl)
               or  d
               ld  (hl),a
               dec l
               inc h

               ld  a,%00000001      ; +3 special paging, banks 0/1/2/3
               ld  bc,&1ffd
               out (c),a            ; page ROM
               ret

; clear pill
pill_clear_2:  call page_screen

               ld  a,h
               or  ixh
               ld  h,a

               ld  de,&c00f         ; mask

               ld  a,(hl)
               and d
               ld  (hl),a
               inc l
               ld  a,(hl)
               and e
               ld  (hl),a
               dec l
               inc h

               ld  a,(hl)
               and d
               ld  (hl),a
               inc l
               ld  a,(hl)
               and e
               ld  (hl),a
               dec l
               inc h

               ld  a,(hl)
               and d
               ld  (hl),a
               inc l
               ld  a,(hl)
               and e
               ld  (hl),a
               dec l
               inc h

               ld  a,(hl)
               and d
               ld  (hl),a
               inc l
               ld  a,(hl)
               and e
               ld  (hl),a
               dec l
               inc h

               ld  a,h
               and %00000111
               call z,blockdown_hl

               ld  a,(hl)
               and d
               ld  (hl),a
               inc l
               ld  a,(hl)
               and e
               ld  (hl),a
               dec l
               inc h

               ld  a,(hl)
               and d
               ld  (hl),a
               inc l
               ld  a,(hl)
               and e
               ld  (hl),a

               ld  a,%00000001      ; +3 special paging, banks 0/1/2/3
               ld  bc,&1ffd
               out (c),a            ; page ROM
               ret


; Scan the input DIP switches for joystick movement and button presses
;
do_input:      ld  de,&ffff         ; nothing pressed

               ld  a,&7f
               in  a,(keyboard)
               bit 1,a              ; Sym?
               jr  nz,not_colours

               ld  a,&f7
               in  a,(keyboard)
               ld  bc,&0501         ; 5 bits to check, first colour is blue
inp_col_lp:    rra
               jr  nc,got_colour    ; 1-5 = blue/red/magenta/green/cyan
               inc c
               djnz inp_col_lp

               ld  a,&ef
               in  a,(keyboard)
               bit 4,a              ; 6 = yellow?
               jr  z,got_colour
               inc c
               bit 3,a              ; 7 = white?
               jp  nz,input_done

got_colour:    ld  a,&fe
               in  a,(keyboard)
               rra                  ; Shift?
               jr  c,not_bright
               set 6,c              ; use bright version
not_bright:
               ld  a,c
               ld  (attr_colour),a  ; set to be picked up by flash_maze
               ret

not_colours:   ld  a,&f7
               in  a,(keyboard)
               cpl
               and %00000111
               jr  z,not_123
               rra
               jr  nc,not_1
               res 5,e              ; 1 = start 1
not_1:         rra
               jr  nc,not_2
               res 6,e              ; 2 = start 2
not_2:         rra
               jr  nc,not_123
               res 5,d              ; 3 = coin 1
not_123:

               ld  a,&fe
               in  a,(keyboard)
               rra
               jr  c,no_shift

               ld  a,&fb
               in  a,(keyboard)
               bit 4,a
               jr  nz,not_shift_t
               res 4,d              ; shift-t = rack test
not_shift_t:

; Shifted for Cursor keys
               ld  a,&f7
               in  a,(keyboard)
               bit 4,a
               jr  nz,not_shift_5
               res 1,d              ; Shift-5 = left
not_shift_5:
               ld  a,&ef
               in  a,(keyboard)
               cpl
               and %00011100
               jr  z,read_joy
               rra
               rra
               rra
               jr  nc,not_shift_8
               res 2,d              ; Shift-8 = right
not_shift_8:   rra
               jr  nc,not_shift_7
               res 0,d              ; Shift-7 = up
not_shift_7:   rra
               jr  nc,read_qaop
               res 3,d              ; Shift-6 = down
               jr  read_qaop

; Unshifted for Sinclair joystick
no_shift:      ld  a,&ef
               in  a,(keyboard)
               cpl
               and %00011111
               jr  z,not_67890
               rra
               jr  nc,not_0
               res 5,d              ; 0 = coin 1
               res 5,e              ; 0 = start 1
not_0:         rra
               jr  nc,not_9
               res 0,d              ; 9 = up
not_9:         rra
               jr  nc,not_8
               res 3,d              ; 8 = down
not_8:         rra
               jr  nc,not_7
               res 2,d              ; 7 = right
not_7:         rra
               jr  nc,not_67890
               res 1,d              ; 6 = left
not_67890:

read_qaop:     ld  a,&fb
               in  a,(keyboard)
               rra
               jr  c,not_q
               res 0,d              ; Q = up
not_q:
               ld  a,&fd
               in  a,(keyboard)
               rra
               jr  c,not_a
               res 3,d              ; A = down
not_a:
               ld  a,&df
               in  a,(keyboard)
               rra
               jr  c,not_p
               res 2,d              ; P = right
not_p:         rra
               jr  c,not_o
               res 1,d              ; O = left
not_o:
               ld  a,&7f
               in  a,(keyboard)
               rra
               jr  c,not_space
               res 5,d              ; space = coin 1
               res 5,e              ; space = start 1
not_space:

; Kempston joystick
read_joy:      in  a,(kempston)     ; read Kempston joystick
               inc a
               cp  2
               jr  c,not_fire       ; ignore blank or invalid inputs
               dec a
               rra
               jr  nc,not_right
               res 2,d              ; right
not_right:     rra
               jr  nc,not_left
               res 1,d              ; left
not_left:      rra
               jr  nc,not_down
               res 3,d              ; down
not_down:      rra
               jr  nc,not_up
               res 0,d              ; up
not_up:        rra
               jr  nc,not_fire
               res 5,d              ; Fire = coin 1
               res 5,e              ; Fire = start 1
not_fire:

               ld  a,d              ; dip including controls
               cpl                  ; invert so set=pressed
               and %00001111        ; keep only direction bits
               jr  z,joy_done       ; skip if nothing pressed
               ld  c,a
               neg
               and c                ; keep least significant set bit
               cp  c                ; was it the only bit?
               jr  z,joy_done       ; skip if so

               ld  a,(last_controls); last valid (single) controls
               xor c                ; check for differences
               or  %11110000        ; convert to mask
               ld  c,a
               ld  a,d              ; current controls
               or  %00001111        ; release all directions
               and c                ; press the changed key
               jr  joy_multi        ; apply change but don't save

joy_done:      ld  a,d
               ld  (last_controls),a; update last valid controls
input_done:    ld  a,d              ; use original value
joy_multi:     ld  (&5000),a
               ld  a,e
               ld  (&5040),a
               ret

last_controls: defb 0


; Check sprite visibility, returns carry if any visible, no-carry if all hidden
is_visible:    ld  a,&10            ; minimum x/y position to be visible
               ld  b,7              ; 7 sprites to check
               ld  hl,&5062
vis_lp:        cp  (hl)
               ret c
               inc l
               cp  (hl)
               ret c
               inc l
               inc l
               inc l
               djnz vis_lp
               ret


; Draw the background tile changes, in 1-5 steps over the 2 double-buffered screens
do_tiles:      call is_visible      ; set carry state for below

tile_state:    ld  a,ixh
               bit 7,a              ; alt screen? (don't disturb carry!)

               jr  c,tile_strips    ; if any sprites are visible we'll draw in strips
               jr  nz,fulldraw_alt     ; full screen draw (alt)

fulldraw_norm: ld  b,28
               ld  de,pac_chars
               ld  hl,bak_chars1-pac_footer
               add hl,de
               call tile_comp
               ld  hl,bak_chars1
               call do_fruit
               ld  hl,bak_chars1
               call do_lives
               ld  hl,bak_chars1
               call do_score1
               ld  hl,bak_chars1
               jp  do_score2

fulldraw_alt:  ld  b,28
               ld  de,pac_chars
               ld  hl,bak_chars2-pac_footer
               add hl,de
               call tile_comp
               ld  hl,bak_chars2
               call do_fruit
               ld  hl,bak_chars2
               call do_lives
               ld  hl,bak_chars2
               call do_score1
               ld  hl,bak_chars2
               jp  do_score2


tile_strips:   jp  nz,strip_odd
strip_even:    jp  strip_0

strip_0:       ld  b,6
               ld  de,pac_chars
               ld  hl,bak_chars1-pac_footer
               add hl,de
               call tile_comp
               ld  hl,strip_1
               ld  (strip_even+1),hl
               ret

strip_1:       ld  b,5
               ld  de,pac_chars+(32*(6))
               ld  hl,bak_chars1-pac_footer
               add hl,de
               call tile_comp
               ld  hl,bak_chars1
               call do_score1
               ld  hl,bak_chars1
               call do_score2
               ld  hl,strip_2
               ld  (strip_even+1),hl
               ret

strip_2:       ld  b,6
               ld  de,pac_chars+(32*(6+5))
               ld  hl,bak_chars1-pac_footer
               add hl,de
               call tile_comp
               ld  hl,strip_3
               ld  (strip_even+1),hl
               ret

strip_3:       ld  b,5
               ld  de,pac_chars+(32*(6+5+6))
               ld  hl,bak_chars1-pac_footer
               add hl,de
               call tile_comp
               ld  hl,bak_chars1
               call do_fruit
               ld  hl,bak_chars1
               call do_lives
               ld  hl,strip_4
               ld  (strip_even+1),hl
               ret

strip_4:       ld  b,6
               ld  de,pac_chars+(32*(6+5+6+5))
               ld  hl,bak_chars1-pac_footer
               add hl,de
               call tile_comp
               ld  hl,strip_0
               ld  (strip_even+1),hl
               ret

strip_odd:     jp  strip_0_alt

strip_0_alt:   ld  b,6
               ld  de,pac_chars
               ld  hl,bak_chars2-pac_footer
               add hl,de
               call tile_comp
               ld  hl,strip_1_alt
               ld  (strip_odd+1),hl
               ret

strip_1_alt:   ld  b,5
               ld  de,pac_chars+(32*(6))
               ld  hl,bak_chars2-pac_footer
               add hl,de
               call tile_comp
               ld  hl,bak_chars2
               call do_score1
               ld  hl,bak_chars2
               call do_score2
               ld  hl,strip_2_alt
               ld  (strip_odd+1),hl
               ret

strip_2_alt:   ld  b,6
               ld  de,pac_chars+(32*(6+5))
               ld  hl,bak_chars2-pac_footer
               add hl,de
               call tile_comp
               ld  hl,strip_3_alt
               ld  (strip_odd+1),hl
               ret

strip_3_alt:   ld  b,5
               ld  de,pac_chars+(32*(6+5+6))
               ld  hl,bak_chars2-pac_footer
               add hl,de
               call tile_comp
               ld  hl,bak_chars2
               call do_fruit
               ld  hl,bak_chars2
               call do_lives
               ld  hl,strip_4_alt
               ld  (strip_odd+1),hl
               ret

strip_4_alt:   ld  b,6
               ld  de,pac_chars+(32*(6+5+6+5))
               ld  hl,bak_chars2-pac_footer
               add hl,de
               call tile_comp
               ld  hl,strip_0_alt
               ld  (strip_odd+1),hl
               ret


tile_comp:     call find_change     ; scan block for display changes
               dec sp               ; restore the same return address to here
               dec sp

               ld  (hl),a           ; update with new tile value

               cp  144              ; before fruit tiles?
               jr  c,tile_mapped
               sub 63               ; relocate to account for removed fruits
               cp  176-63           ; first ghost tile
               jr  c,tile_mapped
               cp  176-63+6         ; after last ghost tile?
               jr  nc,tile_mapped

               ex  af,af'           ; save tile
               set 2,d              ; switch to attributes
               ld  a,(de)           ; fetch tile attribute
               res 2,d              ; switch back to data
               ld  c,a
               ex  af,af'           ; restore tile
               dec c                ; red ghost?
               jr  nz,tile_mapped
               add a,6              ; offset to Blinky tiles
tile_mapped:
               ex  af,af'           ; save tile for later
               push de
               exx                  ; save to resume find
               pop hl               ; Pac-Man screen address of changed tile

               ld  a,l
               and %00011111        ; column is in bits 0-4
               ld  b,a              ; tile y

               add a,a              ; *2
               add a,a              ; *4
               add a,b              ; *5 (code size to check each byte)
               add a,3              ; skip ld+cp+ret, so we advance pointers
               ld  e,a
               ld  d,find_change/256
               push de              ; return address to resume find

               add hl,hl            ; *2
               add hl,hl            ; *4
               add hl,hl            ; *8 (ignore overflow), H is now mirrored column number
               ld  a,28+2           ; 28 columns wide, offset 2 by additional rows
               sub h                ; unmirror the column
               ld  c,a              ; tile x

draw_tile:     ld  ixl,5            ; offset to centre maze on Speccy display
draw_tile_x:   ld  a,b
               add a,a              ; *2
               ld  b,a
               add a,a              ; *4
               add a,b              ; *6
               ld  l,a
               ld  h,scradtab/256
               ld  e,(hl)
               inc h
               ld  a,(hl)
               or  ixh
               ld  d,a              ; DE holds base addr for screen line

               ld  b,conv_8_6/256
               ld  a,(bc)           ; 4 tiles to 3 byte conversion for tile x
               add a,e              ; add screen LSB
               add a,ixl            ; centre maze on Speccy display
               ld  e,a              ; DE holds addr for tile

               ld  a,c
               ex  af,af'           ; save tile x, restore tile number

               ld  l,a
               ld  h,0
               add hl,hl            ; *2
               ld  b,h
               ld  c,l
               add hl,hl            ; *4
               add hl,bc            ; *6

               ld  a,%00000100      ; +3 normal paging, R3/5/2/7
               ld  bc,&1ffd
               out (c),a

               ex  af,af'
               rra
               jr  c,tile_62
               rra
               jr  c,tile_4

; 11111100
tile_0:        ld  bc,tile_data_0
               add hl,bc
               ld  bc,&0503         ; 5 lines, mask of 00000011
tile_0_lp:     ld  a,(de)
               and c
               or  (hl)
               ld  (de),a
               inc hl
               inc d
               ld  a,d
               and %00000111
               call z,blockdown_de
               djnz tile_0_lp
               ld  a,(de)
               and c
               or  (hl)
               ld  (de),a
               jr  tile_exit

; 00001111 11000000
tile_4:        ld  bc,tile_data_4
               add hl,hl
               add hl,bc
               ld  bc,&05f0         ; 5 lines, mask of 11110000
tile_4_lp:     ld  a,(de)
               and c
               or  (hl)
               ld  (de),a
               inc e
               inc hl
               ld  a,(de)
               and %00111111
               or  (hl)
               ld  (de),a
               dec e
               inc hl
               inc d
               ld  a,d
               and %00000111
               call z,blockdown_de
               djnz tile_4_lp
               ld  a,(de)
               and c
               or  (hl)
               ld  (de),a
               inc e
               inc hl
               ld  a,(de)
               and %00111111
               or  (hl)
               ld  (de),a
               jr  tile_exit

tile_62:       rra
               jr  c,tile_2

; 00000011 11110000
tile_6:        ld  bc,tile_data_6
               add hl,hl
               add hl,bc
               ld  bc,&05fc         ; 5 lines, mask of 11111100
tile_6_lp:     ld  a,(de)
               and c
               or  (hl)
               ld  (de),a
               inc e
               inc hl
               ld  a,(de)
               and %00001111
               or  (hl)
               ld  (de),a
               dec e
               inc hl
               inc d
               ld  a,d
               and %00000111
               call z,blockdown_de
               djnz tile_6_lp
               ld  a,(de)
               and c
               or  (hl)
               ld  (de),a
               inc e
               inc hl
               ld  a,(de)
               and %00001111
               or  (hl)
               ld  (de),a
               jr  tile_exit

; 00111111
tile_2:        ld  bc,tile_data_2
               add hl,bc
               ld  bc,&05c0         ; 5 lines, mask of 11000000
tile_2_lp:     ld  a,(de)
               and c
               or  (hl)
               ld  (de),a
               inc hl
               inc d
               ld  a,d
               and %00000111
               call z,blockdown_de
               djnz tile_2_lp
               ld  a,(de)
               and c
               or  (hl)
               ld  (de),a
               jr  tile_exit

tile_exit:     
               ld  a,%00000001      ; +3 special paging, banks 0/1/2/3
               ld  bc,&1ffd
               out (c),a            ; page ROM

               exx
               ret


; Draw a 12x12 sprite  (H=x, L=y, D=attr)
;
draw_spr:      ld  a,h
               cp  &10              ; off bottom of screen?
               ret c
               ld  a,l
               cp  &10              ; off right of screen?
               ret c

               ld  a,d
               and a                ; sprite palette all black?
               ret z

               call page_screen
               call xy_to_addr
               ld  a,c
               and %00000111        ; shift position

               ex  af,af'
               call map_spr         ; map sprites to the correct orientation/colour

draw_spr2:
               ex  de,hl
               add a,a              ; *2
               ld  l,a
               ld  h,0
               add hl,hl            ; *4
               ld  b,h
               ld  c,l
               add hl,hl            ; *8
               add hl,bc            ; *12

               ex  af,af'
               rra
               jr  c,rot_odd
rot_even:      rra
               jr  c,rot_2_6
rot_0_4:       rra
               ld  bc,spr_data_4    ; rot_4
               jr  c,spr_2
               ld  bc,spr_data_0    ; rot_0
               jp  spr_2

rot_2_6:       rra
               ld  bc,spr_data_6    ; rot_6
               jr  c,spr_3
               ld  bc,spr_data_2    ; rot_2
               jp  spr_2

rot_odd:       rra
               jr  c,rot_3_7
rot_1_5:       rra
               ld  bc,spr_data_5    ; rot_5
               jr  c,spr_3
               ld  bc,spr_data_1    ; rot_1
               jp  spr_2

rot_3_7:       rra
               ld  bc,spr_data_7    ; rot_7
               jr  c,spr_3
               ld  bc,spr_data_3    ; rot_3
               jp  spr_2

; draw a sprite using 2-byte source data (shifts 0-4)
spr_2:         add hl,hl
               add hl,bc
               ld  b,12
rot_0_lp:      ld  a,(de)
               or  (hl)
               ld  (de),a
               inc e
               inc hl
               ld  a,(de)
               or  (hl)
               ld  (de),a
               dec e
               inc hl
               inc d
               ld  a,d
               and %00000111
               call z,blockdown_de
               djnz rot_0_lp
               jp  page_rom

; draw a sprite using 3-byte source data (shifts 5-7)
spr_3:         push bc
               ld  b,h
               ld  c,l
               add hl,hl           ; *24
               add hl,bc           ; *36
               pop bc
               add hl,bc
               ld  b,12
spr_3_lp:      ld  a,(de)
               or  (hl)
               ld  (de),a
               inc e
               inc hl
               ld  a,(de)
               or  (hl)
               ld  (de),a
               inc e
               inc hl
               ld  a,(de)
               or  (hl)
               ld  (de),a
               dec e
               dec e
               inc hl
               inc d
               ld  a,d
               and %00000111
               call z,blockdown_de
               djnz spr_3_lp
               jp  page_rom


; Map an arcade tile number to our tile number, allowing for attribute differences
; and any tiles we've mapped to different locations (we have no fruit tiles)
map_spr:       ld  b,0
               ld  a,e
               srl a
               rl  b                ; b0=flip-y
               rra
               rl  b                ; b1=flip-y, b0=flip-x
               cp  16               ; big pac-man
               ret c                ; anything before is unchanged
               cp  28               ; scared ghost
               jr  c,map_big
               cp  32               ; first ghost
               jr  c,map_scared
               cp  40               ; last ghost + 1
               jr  c,map_ghost
               cp  44               ; first Pac-Man
               ret c
               cp  48               ; last Pac-Man + 1
               ret nc
               inc b
               dec b                ; mirrored?
               ret z                ; no, so right/down
               add a,28             ; offset to left/up
               ret

map_big:       cp  24               ; closed mouth
               ret nc
               add a,48
               bit 0,a              ; mouth segment?
               ret nz
               and %00000010        ; up/down back
               or  %00011000        ; re-use back segments
               ret

map_ghost:     ;jr $ ;01=red 03=pink 05=cyan 07=orange
               sub 16
               dec d                ; red?
               ret z
               add a,16
               bit 3,d              ; transparent colour?
               ret z                ; return if not
               add a,32             ; eyes offset
               and %11111110        ; use only even positions
               ret

map_scared:    bit 1,d              ; check colour
               ret z                ; return if normal colour
               add a,2              ; white flashing offset
               ret


; Trim sprites that overlap the maze edges, as the real hardware does automatically
; Here we trim partial bytes, as full blocks are hidden behind black on black attrs
;
do_trim:       ld  de,&5062         ; start of sprite data
               ld  b,&07            ; 7 sprites to check

trim_lp:       ld  a,(de)           ; sprite x
               inc e
               cp  &10              ; hidden sprite?
               jr  c,no_trim
               cp  &20              ; clipped at right edge?
               jr  c,may_trim
               cp  &f0              ; clipped at left edge?
               jr  nc,may_trim
no_trim:       inc e
               djnz trim_lp
               ret

may_trim:      ld  a,(de)           ; sprite y
               cp  &10              ; hidden sprite?
               jr  c,no_trim

               ld  e,12             ; height of sprite
               ld  hl,&4f45         ; left of tunnel
               cp  &8c              ; sprite y for tunnel?
               jr  z,trim_edge
               ld  e,24             ; height of 2 sprites
               ld  hl,&4d65         ; left of intermission row
trim_edge:
               ld  a,h
               or  ixh              ; offset to current screen
               ld  h,a

               ld  a,%00000100      ; +3 normal paging, R3/5/2/7
               ld  bc,&1ffd
               out (c),a            ; normal paging (screen)

               ld  b,e              ; line count
               ld  de,&03fc         ; masks for below

trim_line_lp:  ld  a,(hl)
               and d                ; clear 6 pixels
               ld  (hl),a
               ld  c,l
               ld  a,l
               add a,&15            ; advance to right edge
               ld  l,a
               ld  a,(hl)
               and e                ; clear 2 pixels
               ld  (hl),a
               ld  l,c              ; restore left position
               inc h                ; next line
               ld  a,h
               and %00000111
               call z,blockdown_hl
               djnz trim_line_lp

               ld  a,%00000001      ; +3 special paging, banks 0/1/2/3
               ld  bc,&1ffd
               out (c),a            ; page ROM

               ret


; Clear a sprite-sized hole, used for blank tiles in our fruit and lives display
;
blank_sprite:  call page_screen

               ld  bc,&0c00         ; 12 lines, zero fill
blank_lp:      ld  (hl),c
               inc l
               ld  (hl),c
               dec l
               inc h
               ld  a,h
               and %00000111
               call z,blockdown_hl
               djnz blank_lp

               jp  page_rom


; Save the background screen behind locations we're about to draw active sprites
;
do_save:       ld  hl,(&5062)       ; pre-fetch position data as we page it out
               push hl
               ld  hl,(&5064)
               push hl
               ld  hl,(&5066)
               push hl
               ld  hl,(&5068)
               push hl
               ld  hl,(&506a)
               push hl
               ld  hl,(&506c) 

               call page_screen

               ld  de,spr_save_7
               call spr_save

               pop hl
               ld  de,spr_save_6
               call spr_save

               pop hl
               ld  de,spr_save_5
               call spr_save

               pop hl
               ld  de,spr_save_4
               call spr_save

               pop hl
               ld  de,spr_save_3
               call spr_save

               pop hl
               ld  de,spr_save_2
               call spr_save

               call page_rom
               ret

; Save a single sprite-sized block, if visible
spr_save:      ld  a,h
               cp  16
               ret c                ; off bottom of screen
               ld  a,l
               cp  16
               ret c                ; off right of screen

               ld  a,d
               or  ixh
               ld  d,a

               call xy_to_addr      ; convert to Speccy display address

               ex  de,hl
               ld  (hl),e           ; save address low
               inc l
               ld  (hl),d           ; save address high
               inc l
               ex  de,hl

               ld  bc,3*12          ; 3 bytes and 12 lines

save_lp:       ld  a,l
               ldi
               ldi
               ldi
               ret po               ; return if done
               ld  l,a
               inc h
               ld  a,h
               and %00000111
               jp  nz,save_lp
               call blockdown_hl
               jp  save_lp

;
; Remove the previous sprites by restoring the image that was underneath them
;
do_restore:
               call page_screen

               ld  hl,spr_save_2
               call spr_restore
               ld  hl,spr_save_3
               call spr_restore
               ld  hl,spr_save_4
               call spr_restore
               ld  hl,spr_save_5
               call spr_restore
               ld  hl,spr_save_6
               call spr_restore
               ld  hl,spr_save_7
               call spr_restore

               jp  page_rom


; Restore a single sprite-sized block, if data was saved
spr_restore:   ld  a,h
               or  ixh
               ld  h,a
               ld  a,(hl)
               and a
               ret z                ; no data saved

               ld  (hl),0           ; flag 'no restore data'

               ld  e,a              ; restore address low
               inc l
               ld  d,(hl)           ; restore address high
               inc l

               ld  bc,3*12          ; 3 bytes of 12 lines

restore_lp:    ld  a,e
               ldi
               ldi
               ldi
               ret po
               ld  e,a
               inc d
               ld  a,d
               and %00000111
               jp  nz,restore_lp
               call blockdown_de
               jp  restore_lp


; Draw the currently visible sprites, in the correct order for overlaps
; Note: sprite order changes depending on mode, so not always as listed!
;
do_sprites:
               ld  hl,(&506c)
               ld  de,(&4ffc)
               call draw_spr        ; fruit

               ld  hl,(&506a)
               ld  de,(&4ffa)
               call draw_spr        ; pacman

               ld  hl,(&5068)
               ld  de,(&4ff8)
               call draw_spr        ; orange ghost

               ld  hl,(&5066)
               ld  de,(&4ff6)
               call draw_spr        ; cyan ghost

               ld  hl,(&5064)
               ld  de,(&4ff4)
               call draw_spr        ; pink ghost

               ld  hl,(&5062)
               ld  de,(&4ff2)
               call draw_spr        ; red ghost

               ret


do_score1:     inc h
               inc h
               inc h                ; advance to header area containing score

               ld  ixl,0            ; screen offset for left edge

               ld  l,&da
               ld  bc,&0003
               call chk_digit       ; 1

               ld  l,&d9
               ld  bc,&0004
               call chk_digit       ; U

               ld  l,&d8
               ld  bc,&0005
               call chk_digit       ; P


               ld  l,&fc
               ld  bc,&0100
               call chk_digit       ; 100,000s

               ld  l,&fb
               ld  bc,&0101
               call chk_digit       ; 10,000s

               ld  l,&fa
               ld  bc,&0102
               call chk_digit       ; 1,000s

               ld  l,&f9
               ld  bc,&0103
               call chk_digit       ; 100s

               ld  l,&f8
               ld  bc,&0104
               call chk_digit       ; 10s

               ld  l,&f7
               ld  bc,&0105
               call chk_digit       ; 1s

               ret

;
do_score2:     inc h
               inc h
               inc h                ; advance to header area containing score

               ld  ixl,26           ; screen offset for left edge

               ld  l,&c7
               ld  bc,&0003
               call chk_digit       ; 2

               ld  l,&c6
               ld  bc,&0004
               call chk_digit       ; U

               ld  l,&c5
               ld  bc,&0005
               call chk_digit       ; P


               ld  l,&e9
               ld  bc,&0100
               call chk_digit       ; 100,000s

               ld  l,&e8
               ld  bc,&0101
               call chk_digit       ; 10,000s

               ld  l,&e7
               ld  bc,&0102
               call chk_digit       ; 1,000s

               ld  l,&e6
               ld  bc,&0103
               call chk_digit       ; 100s

               ld  l,&e5
               ld  bc,&0104
               call chk_digit       ; 10s

               ld  l,&e4
               ld  bc,&0105
               call chk_digit       ; 1s

               ret

chk_digit:     ld  d,&43
               ld  e,l
               ld  a,(de)
               cp  (hl)
               ret z
               ld  (hl),a

               ex  af,af'
               push hl
               call draw_tile_x
               pop hl
               ret


; Draw changes to the fruit display, which is remapped to a vertical layout
; We use the sprite versions of the tiles, for easier drawing
;
do_fruit:      ld  l,5              ; offset to first fruit in display and comparison buffer
               ld  c,&b4
               push hl
               call chk_fruit
               pop hl

               ld  l,7
               ld  c,&a8
               push hl
               call chk_fruit
               pop hl

               ld  l,9
               ld  c,&9c
               push hl
               call chk_fruit
               pop hl

               ld  l,11
               ld  c,&90
               push hl
               call chk_fruit
               pop hl

               ld  l,13
               ld  c,&84
               push hl
               call chk_fruit
               pop hl

               ld  l,15
               ld  c,&78
               push hl
               call chk_fruit
               pop hl

               ld  l,17
               ld  c,&6c
               push hl
               call chk_fruit
               pop hl

               ret

chk_fruit:     ld  b,scradtab/256
               ld  a,(bc)
               add a,&1d            ; Speccy line offset for fruit column
               ld  e,a
               inc b
               ld  a,(bc)
               or  ixh
               ld  d,a

               ld  b,pac_footer/256 ; tile MSB for fruit
               ld  c,l
               ld  a,(bc)           ; live fruit
               cp  (hl)             ; has it changed?
               ret z                ; return if not

               ld  (hl),a           ; update buffered copy
               ex  de,hl
               ex  af,af'

               push hl
               call blank_sprite
               pop hl

               xor a                ; no rotation, for sprite drawing later
               ex  af,af'
               cp  &40              ; blank?
               ret z

               sub &91              ; subtract cherry tile number
               srl a                ; /2
               srl a                ; /4 tiles per fruit, to give fruit sprite number (cherry=0)

               call page_screen
               jp  draw_spr2

;
; Draw changes to the number of remaining lives
;
do_lives:      ld  l,&1b
               ld  c,&b4
               push hl
               call chk_life
               pop hl

               ld  l,&19
               ld  c,&a8
               push hl
               call chk_life
               pop hl

               ld  l,&17
               ld  c,&9c
               push hl
               call chk_life
               pop hl

               ld  l,&15
               ld  c,&90
               push hl
               call chk_life
               pop hl

               ld  l,&13
               ld  c,&84
               push hl
               call chk_life
               pop hl

               ret

; Draw either a blank or a left-facing Pac-Man sprite
chk_life:      ld  b,scradtab/256
               ld  a,(bc)
               add a,&02             ; Speccy line offset for lives column
               ld  e,a
               inc b
               ld  a,(bc)
               or  ixh
               ld  d,a

               ld  b,&40
               ld  c,l
               ld  a,(bc)
               cp  (hl)
               ret z

               ld  (hl),a
               ex  de,hl
               ex  af,af'

               push hl
               call blank_sprite
               pop hl

               xor a                ; no rotation, for sprite drawing later
               ex  af,af'
               cp  &40              ; blank?
               ret z

               call page_screen
               ld  a,72             ; sprite for left-facing Pac-Man
               jp  draw_spr2


; Build the sound table and initialise the AY-3-8912 chip
;
sound_init:    ld  bc,sound_table

sound_lp:      ld  a,b              ; map entry address to freq
               and &3f
               rra
               ld  d,a
               ld  a,c
               rra
               ld  e,a              ; freq (divisor) now in DE

               ld  hl,0
               exx
               ld  de,&da7a         ; dividend in DEHL
               ld  hl,&8000         ; 111861 << 15 = 0xda7a8000 
               ld  b,16
               and a
div_lp:        adc hl,hl            ; shift up for next division
               rl  e
               rl  d
               exx
               adc hl,hl            ; include new bit
               sbc hl,de            ; does it divide?
               jr  nc,div_ok
               add hl,de            ; add back if not, setting carry
div_ok:        exx
               ccf                  ; set carry if it divided
               djnz div_lp
               adc hl,hl            ; include final bit

               ld  a,h
               ex  af,af'
               ld  a,l
               exx
               ld  (bc),a           ; note LSB
               inc c
               ex  af,af'
               ld  (bc),a           ; note MSB
               inc bc               ; freq++
               ex  af,af'

               xor c
               and %00000111
               out (border),a       ; flash the border to show we're busy

               bit 5,b
               jr  z,sound_lp

               xor a
               out (border),a

               ld  hl,sinit_data
               ld  de,&ffbf
               ld  c,&fd
sinit_lp:      ld  a,(hl)
               and a
               ret m
               ld  b,d
               out (c),a
               inc hl
               ld  a,(hl)
               inc hl
               ld  b,e
               out (c),a
               jr sinit_lp

; Sound init: set volumes to zero, enable tones A+B+C, end
sinit_data: defb &08,0, &09,0, &0a,0, &07,%00111000, &ff


; Map the current sound chip frequencies to the AY
;
do_sound:      ld  hl,&5051         ; voice 0 freq and volume
               ld  a,(&5045)        ; voice 0 waveform
               call map_sound
               xor a
               call play_sound

               ld  hl,&5051+5       ; voice 1 freq and volume
               ld  a,(&504a)        ; voice 1 waveform
               call map_sound
               ld  a,1
               call play_sound

               ld  hl,&5051+5+5     ; voice 2 freq and volume
               ld  a,(&504f)        ; voice 2 waveform
               call map_sound
               ld  a,2
               call play_sound

               ret

map_sound:     ld  b,a              ; save waveform

               ld  a,(hl)
               and %00001111
               add a,a
               add a,a
               add a,a
               add a,a
               ld  e,a
               inc hl
               ld  a,(hl)
               and %00001111
               ld  d,a
               inc hl
               ld  a,(hl)
               add a,a
               add a,a
               add a,a
               add a,a
               or  d
               ld  d,a
               or  e                ; check for zero frequency
               inc hl
               inc hl
               ld  a,(hl)           ; volume
               ex  de,hl

               jr  nz,not_silent
               xor a                ; zero frequency gives silence
not_silent:    ex  af,af'           ; save volume for caller

               ld  a,b
               cp  5                ; waveform used when eating ghost?
               jr  z,eat_sound      ; if so, don't divide freq by 8
               srl h
               rr  l
               srl h
               rr  l
               srl h
               rr  l
eat_sound:
               ld  a,h
               or  &c0              ; MSB of sound table
               ld  h,a
               res 0,l

               ld  a,(hl)           ; pick up LSB
               inc hl
               ld  h,(hl)           ; pick up MSB
               ld  l,a

               ret

; Update a single voice, setting the note number and volume
play_sound:    ld  de,&ffbf         ; sound register port MSB
               ld  c,&fd            ; LSB

               add a,a              ; 2 registers per tone
               ld  b,d
               out (c),a            ; tone low
               ld  b,e
               out (c),l

               inc a
               ld  b,d
               out (c),a            ; tone high
               ld  b,e
               out (c),h

               rra
               or  %00001000
               ld  b,d
               out (c),a            ; volume
               ex  af,af'
               ld  b,e
               out (c),a            ; volume data

               ret


; Create the look-up tables used to speed up various calculations
;
make_tables:   ld  hl,conv_8_6
               xor a
conv_86_lp:    ld  (hl),a           ; 0
               inc l
               ld  (hl),a           ; 0
               inc a
               inc l
               ld  (hl),a           ; 1
               inc a
               inc l
               ld  (hl),a           ; 2, etc. (repeating pattern)
               inc a
               inc l
               jr  nz,conv_86_lp

               ; note: HL re-used from above
               ld  de,conv_y
               ld  bc,conv_x
mirror_lp:     xor a
               sub c                ; mirror y-axis
               ld  l,a
               ld  a,(hl)           ; map to Speccy coords
               ld  (de),a
               xor a
               sub e                ; mirror x-axis
               ld  l,a
               ld  a,(hl)           ; map to Speccy coords
               add a,34             ; centre on display
               ld  (bc),a
               inc e
               inc c
               jr  nz,mirror_lp


               ld  hl,tile_data_4
               ld  de,tile_data_6
               exx
               ld  hl,tile_data_0
               ld  de,tile_data_2
               ld  c,192            ; 192 tiles
tilerot_lp:    ld  b,6              ; 6 lines per tile
tilerot_lp2:   ld  a,(hl)
               inc hl
               srl a
               rra
               ld  (de),a           ; >> 6
               inc de
               exx
               ld  c,0
               rra
               rr  c
               rra
               rr  c
               ld  (hl),a           ; >> 4
               inc l
               ld  (hl),c
               inc hl
               ex  de,hl
               rra
               rr  c
               rra
               rr  c
               ld  (hl),a           ; >> 2
               inc l
               ld  (hl),c
               inc hl
               ex  de,hl
               exx
               djnz tilerot_lp2
               dec c
               jr  nz,tilerot_lp


               ld  hl,spr_data_5
               ld  de,76*3*12
               exx
               ld  hl,spr_data_0
               ld  de,76*2*12

               ld  c,76             ; 76 sprites
spr_rot_lp:    push bc
               ld  b,12             ; 12 lines per sprite
spr_rot_lp2:   push bc

               ld  c,(hl)           ; take a line from spr_data_0
               inc hl
               ld  a,(hl)
               dec hl

               push hl              ; save
               ld  b,4              ; four more 2-byte shifted versions
spr_rot_lp3:   add hl,de            ; next shifted copy
               srl c                ; >> 1
               rra
               ld  (hl),c           ; spr_data_1 to spr_data_4
               inc hl
               ld  (hl),a
               dec hl
               djnz spr_rot_lp3

               pop hl               ; restore spr_data_0 position
               inc hl               ; advance to next line
               inc hl

               ex  af,af'           ; preserve A and carry from final rra above
               ld  a,c              ; copy for exx
               exx
               ld  c,a              ; restore C
               ex  af,af'           ; restore A and carry
               ld  b,0              ; extra shift register
               rr  b                ; recover carry
               ex  af,af'

               ld  a,3              ; three 3-byte shifted versions
               push hl              ; save
spr_rot_lp4:   ex  af,af'

               srl c                ; >> 5 to 7
               rra
               rr  b

               ld  (hl),c           ; spr_data_5 to spr_data_7
               inc hl
               ld  (hl),a
               inc hl
               ld  (hl),b
               dec hl
               dec hl
               add hl,de            ; next shifted copy

               ex  af,af'
               dec a
               jr  nz,spr_rot_lp4

               pop hl               ; restore spr_data_5 position
               inc hl               ; advance to next line
               inc hl
               inc hl
               exx                  ; back to spr_data_0

               pop bc
               djnz spr_rot_lp2     ; complete lines

               pop bc
               dec c
               jr  nz,spr_rot_lp    ; complete sprites


               ld hl,scradtab
               ld de,&4000          ; Speccy screen base
               ld b,&c0             ; 192 lines
scrtab_lp:     ld (hl),e
               inc h
               ld (hl),d
               dec h
               inc l
               inc d
               ld  a,d
               and %00000111
               call z,blockdown_de
               djnz scrtab_lp

               ret

; Map a Pac-Man screen coordinate to a Speccy display address, scaling down from 8x8 to 6x6 as we go
;
xy_to_addr:    ld  b,conv_y/256
               ld  c,h
               ld  a,(bc)           ; look up y coord
               ld  h,conv_x/256
               ld  c,(hl)           ; look up x coord

               ld  h,scradtab/256
               ld  l,a
               ld  b,(hl)
               inc h
               ld  a,(hl)
               or  ixh
               ld  h,a
               ld  l,b
               ld  a,c
               and %11111000
               rra
               rra
               rra
               add a,l
               ld  l,a
               ret

blockdown_hl:  ld a,l
               add a,32
               ld l,a
               ret c

               ld a,h
               sub 8
               ld h,a
               ret

blockdown_de:  ld a,e
               add a,32
               ld e,a
               ret c

               ld a,d
               sub 8
               ld d,a
               ret

               defs (-$)%256          ; align to next 256-byte boundary
;
; Scan a 32-byte block for changes, used for fast scanning of the Pac-Man display
; Aligned on a 256-byte boundary for easy resuming of the scanning
find_change:   ld  a,(de)   ; 0
               cp  (hl)
               ret nz
               inc e
               inc l

               ld  a,(de)   ; 1
               cp  (hl)
               ret nz
               inc e
               inc l

               ld  a,(de)   ; 2
               cp  (hl)
               ret nz
               inc e
               inc l

               ld  a,(de)   ; 3
               cp  (hl)
               ret nz
               inc e
               inc l

               ld  a,(de)   ; 4
               cp  (hl)
               ret nz
               inc e
               inc l

               ld  a,(de)   ; 5
               cp  (hl)
               ret nz
               inc e
               inc l

               ld  a,(de)   ; 6
               cp  (hl)
               ret nz
               inc e
               inc l

               ld  a,(de)   ; 7
               cp  (hl)
               ret nz
               inc e
               inc l

               ld  a,(de)   ; 8
               cp  (hl)
               ret nz
               inc e
               inc l

               ld  a,(de)   ; 9
               cp  (hl)
               ret nz
               inc e
               inc l

               ld  a,(de)   ; 10
               cp  (hl)
               ret nz
               inc e
               inc l

               ld  a,(de)   ; 11
               cp  (hl)
               ret nz
               inc e
               inc l

               ld  a,(de)   ; 12
               cp  (hl)
               ret nz
               inc e
               inc l

               ld  a,(de)   ; 13
               cp  (hl)
               ret nz
               inc e
               inc l

               ld  a,(de)   ; 14
               cp  (hl)
               ret nz
               inc e
               inc l

               ld  a,(de)   ; 15
               cp  (hl)
               ret nz
               inc e
               inc l

               ld  a,(de)   ; 16
               cp  (hl)
               ret nz
               inc e
               inc l

               ld  a,(de)   ; 17
               cp  (hl)
               ret nz
               inc e
               inc l

               ld  a,(de)   ; 18
               cp  (hl)
               ret nz
               inc e
               inc l

               ld  a,(de)   ; 19
               cp  (hl)
               ret nz
               inc e
               inc l

               ld  a,(de)   ; 20
               cp  (hl)
               ret nz
               inc e
               inc l

               ld  a,(de)   ; 21
               cp  (hl)
               ret nz
               inc e
               inc l

               ld  a,(de)   ; 22
               cp  (hl)
               ret nz
               inc e
               inc l

               ld  a,(de)   ; 23
               cp  (hl)
               ret nz
               inc e
               inc l

               ld  a,(de)   ; 24
               cp  (hl)
               ret nz
               inc e
               inc l

               ld  a,(de)   ; 25
               cp  (hl)
               ret nz
               inc e
               inc l

               ld  a,(de)   ; 26
               cp  (hl)
               ret nz
               inc e
               inc l

               ld  a,(de)   ; 27
               cp  (hl)
               ret nz
               inc e
               inc l

               ld  a,(de)   ; 28
               cp  (hl)
               ret nz
               inc e
               inc l

               ld  a,(de)   ; 29
               cp  (hl)
               ret nz
               inc e
               inc l

               ld  a,(de)   ; 30
               cp  (hl)
               ret nz
               inc e
               inc l

               ld  a,(de)   ; 31
               cp  (hl)
               ret nz
               inc de       ; 16-bit increment as we may be at 256-byte boundary
               inc hl

               dec b
               jp  nz,find_change   ; jump too big for DJNZ

               pop hl               ; junk return to update
               ret

end_a000:      equ $

new_stack:     equ &b000            ; hangs back into &Axxx

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

               org &b000
; Tables are generated here at run time
conv_8_6:      defs &100
conv_x:        defs &100
conv_y:        defs &100
scradtab:      defs &200
bak_chars1:    defs &400            ; copy of Pac-Man display for normal screen
bak_chars2:    defs &400            ; copy of Pac-Man display for alt screen

end_b000:      equ $

               org &b000
; Graphics are here at load time
load_tiles:    incbin "tiles.bin"      ; 192 tiles * 6 lines * 1 byte per line = 1152 bytes
load_sprites:  incbin "sprites.bin"    ; 76 sprites * 12 lines * 2 byte per line = 1824 bytes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

               org &c000
; 16K of Pac-Man ROMs
               incbin "pacman.6e"
               incbin "pacman.6f"
               incbin "pacman.6h"
               incbin "pacman.6j"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

end start ; auto-run address
