.include "constants.inc"
.include "mmc3-constants.inc"
.include "header.inc"

.feature force_range
.linecont +

; famitone2 config
FT_PAL_SUPPORT=0
FT_NTSC_SUPPORT=1
FT_SFX_ENABLE=1
FT_THREAD=1
FT_DPCM_ENABLE=0
FT_SFX_STREAMS=4
FT_DPCM_OFF=$c000

; music/sfx constants
.enum music_track
  Doom
  InevitableDoom
  Jazz
.endenum

.enum sfx
  Glitchy
  Reglitchy
  Triglitchy
  Click
  Jump
  ButtonOn
  ButtonOff
.endenum

.macro SFX effect, channel
  save_regs
  LDA #sfx::effect
  LDX #.ident ( .concat( "FT_SFX_", .string(channel) ) )
  JSR FamiToneSfxPlay
  restore_regs
.endmacro

.macro GLITCH_SFX
  save_regs
  JSR rand
  AND #%11
  LDX #FT_SFX_CH1
  JSR FamiToneSfxPlay
  restore_regs
.endmacro

.macro PLAY track
.local skip
  save_regs
  LDA #music_track::track
  CMP current_song
  BEQ skip
  STA current_song
  JSR FamiToneMusicPlay
skip:
  restore_regs
.endmacro

.macro STOP_IF_NOT track
.local skip
  save_regs
  LDA #music_track::track
  CMP current_song
  BEQ skip
  JSR FamiToneMusicStop
skip:
  restore_regs
.endmacro

.macro PRINT string, ppuaddr
  save_regs
  LDA #<(string)
  STA addr_ptr
  LDA #>(string)
  STA addr_ptr+1
  LDA #<(ppuaddr)
  STA ppu_addr_ptr
  LDA #>(ppuaddr)
  STA ppu_addr_ptr+1
  JSR write_string
  restore_regs
.endmacro

.macro SCREEN_OFF
  LDA #$00
  STA PPUCTRL ; disable NMI
  STA PPUMASK ; disable rendering
.endmacro

.macro SCREEN_ON
  LDA #%10001000  ; turn on NMIs, sprites use second pattern table
  STA PPUCTRL
  LDA #%00011110  ; turn on screen
  STA PPUMASK
.endmacro

; game config

GRAVITY = $0040 ; 8.8
JUMP_SPEED = -$0570 ; 8.8
CONTROL_ACCELERATION = $0030 ; 9.7
MAX_HORIZONTAL_SPEED = $0400 ; 9.7
MAX_AIR_HORIZONTAL_SPEED = $0100 ; 9.7
GROUND_SPEED_DECAY = $0020 ; 9.7
JUMP_NUDGE = $0100 ; 9.7
LAST_LEVEL = 5

; debug - macros for NintendulatorDX interaction
.ifdef DEBUG
.macro debugOut str
  sta $4040
  jmp :+
      .byte str, 0
:
.endmacro

.macro debugRegs
  STA debug_a
  STX debug_x
  STY debug_y
.endmacro

.define fHex8( addr ) 1, 0, <(addr), >(addr)
.define fDec8( addr ) 1, 1, <(addr), >(addr)
.define fHex16( addr ) 1, 2, <(addr), >(addr)
.define fDec16( addr ) 1, 3, <(addr), >(addr)
.else
.macro debugOut str
.endmacro
.macro debugRegs
.endmacro
.endif

.segment "ZEROPAGE"
FT_TEMP: .res 3
.segment "FAMITONE"
FT_BASE_ADR: .res 186
.segment "CODE"
.include "famitone2.s"

.segment "OAM"
.struct Sprite
  ycoord .byte
  tile .byte
  flag .byte
  xcoord .byte
.endstruct

oam_sprites:
  .repeat 64
    .tag Sprite
  .endrepeat

.zeropage

.enum game_states
  waiting_to_start
  playing
  debugging
  game_over
.endenum

.enum direction
  up = %1000
  down = %0100
  left = %0010
  right = %0001
.endenum

.enum sprite_id
  robot_idle
  robot_walk
  box
  laser
  button_off
  button_on
.endenum

.enum button_type
  none = 0
  respawn_box = 1 ; arg = object index of box
  open_door = 2 ; arg = noop
.endenum

.importzp buttons
.importzp last_frame_buttons
.importzp released_buttons
.importzp pressed_buttons
.importzp rng_seed
.importzp rle_ptr

; zp vars
addr_ptr: .res 2 ; generic address pointer
ppu_addr_ptr: .res 2 ; temporary address for PPU_ADDR
bg_matrix_ptr: .res 2 ; address to current bg matrix

current_song: .res 1

temp_x: .res 1
temp_sx: .res 1
temp_y: .res 1
temp_sy: .res 1
temp_idx: .res 1
temp_dir: .res 1

collision_x: .res 1
collision_sx: .res 1
collision_y: .res 1
collision_top_flags: .res 1 ; for tweaking vertical movement during jumps

.struct Box
  x1 .byte
  sx1 .byte
  y1 .byte
  x2 .byte
  sx2 .byte
  y2 .byte
.endstruct

hitbox_a: .tag Box
hitbox_b: .tag Box

scroll_x: .res 1
scroll_sx: .res 1

nmis: .res 1
old_nmis: .res 1

game_state: .res 1

current_level: .res 1
level_door_x: .res 1
level_door_y: .res 1
level_door_ppu_addr: .res 2
level_door_open: .res 1
glitch_x: .res 1
glitch_y: .res 1
glitch_ppu_addr: .res 2
debugged: .res 1
fixed_flags: .res 1
; 76543210
;     |||+-- can move <-->
;     ||+--- can jump
;     |+---- open door buttons enabled
;     +----- (respawn) buttons enabled
FIXED_MOVE = %0001
FIXED_JUMP = %0010
FIXED_OPEN = %0100
FIXED_RESPAWN = %1000

dialog_counter: .res 1
first_fix_counter: .res 1

anim_offset: .res 1

sprite_counter: .res 1

debug_x: .res 1
debug_y: .res 1
debug_a: .res 1

backup_object_x: .res 1
backup_object_sx: .res 1
backup_object_y: .res 1
backup_object_sy: .res 1

; object coordinates use subpixels for fractional movement
; (object_y, object_sy) = 8.8 number
; (object_x, object_sx) = 9.7 number (in order to cover two screens)
MAX_OBJECTS = 8
objects_length: .res 1
object_x: .res MAX_OBJECTS
object_sx: .res MAX_OBJECTS
object_y: .res MAX_OBJECTS
object_sy: .res MAX_OBJECTS
object_vx: .res MAX_OBJECTS
object_svx: .res MAX_OBJECTS
object_vy: .res MAX_OBJECTS
object_svy: .res MAX_OBJECTS
object_flags: .res MAX_OBJECTS
; flags:
; gmaaaaaf
; |||||||+- flip (0 = face right, 1 = face left)
; ||++++++- anim "index" (* 4 + anim frame = actual anim_sprites index)
; |+------- move flag, tells if object moves
; +-------- grounded
OBJ_ANIM_MASK = %00111111
OBJ_MOVE_FLAG = %01000000
OBJ_GROUNDED_FLAG = %10000000

object_button_command: .res MAX_OBJECTS
object_button_arg1: .res MAX_OBJECTS
object_button_arg2: .res MAX_OBJECTS
object_pressed: .res MAX_OBJECTS

MAX_PIECES = 8
current_piece: .res 1
pieces_length: .res 1
piece_x: .res MAX_PIECES
piece_y: .res MAX_PIECES
piece_target_x: .res MAX_PIECES
piece_target_y: .res MAX_PIECES
piece_index: .res MAX_PIECES

credits_state: .res 1

.segment "BSS"
; non-zp RAM goes here

CREDITS_SCROLL_STRIPES = 80
credits_scroll_x: .res CREDITS_SCROLL_STRIPES
credits_scroll_sx: .res CREDITS_SCROLL_STRIPES
credits_current_stripe: .res 1

.segment "CODE"

.import reset_handler
.import readjoy
.import rand
.import unrle

.import music_data
.import sfx_data

.macro KIL ; pseudo instruction to kill the program
  .byte $12
.endmacro

.macro VBLANK
  .local vblankwait
vblankwait:
  BIT PPUSTATUS
  BPL vblankwait
.endmacro

.macro save_regs
  PHA
  TXA
  PHA
  TYA
  PHA
.endmacro

.macro restore_regs
  PLA
  TAY
  PLA
  TAX
  PLA
.endmacro

; Sets N flag if NUM1 (16 bits) < NUM2 (16 bits)
; From http://www.6502.org/tutorials/compare_beyond.html#5
.macro signed_compare_words NUM1H, NUM1L, NUM2H, NUM2L
  .local exit
  LDA NUM1L ; NUM1-NUM2
  CMP NUM2L
  LDA NUM1H
  SBC NUM2H
  BVC exit ; N eor V
  EOR #$80
exit:
.endmacro

.proc irq_handler
  STA $e000
  save_regs
  LDA credits_state
  BEQ :+

  LDX credits_current_stripe
  INC credits_current_stripe

  LDA PPUSTATUS

  LDA credits_scroll_sx, X
  ASL
  LDA credits_scroll_x, X
  ROL
  STA PPUSCROLL ; horizontal scroll
  LDA #$00
  STA PPUSCROLL

  LDA credits_scroll_x, X
  AND #%10000000
  ROL
  ROL
  ORA #%10001000  ; turn on NMIs, sprites use second pattern table
  STA PPUCTRL
  STA $e000
  LDA #2
  STA $c000
  STA $c001
  STA $e001
  
:
  restore_regs
  RTI
.endproc

.proc nmi_handler
  save_regs
  INC nmis
  LDA credits_state
  BEQ :+
  STA $e000
  LDA #0
  STA credits_current_stripe
  LDA #2
  STA $c000
  STA $c001
  STA $e001  
:
  restore_regs
  RTI
.endproc

.export main
.proc main
  SEI         ; ignore IRQs
  CLD         ; disable decimal mode
  LDX #$40
  STX $4017   ; disable APU frame IRQ
  LDX #$ff
  TXS         ; Set up stack
  INX         ; now X = 0
  STX PPUCTRL ; disable NMI
  STX PPUMASK ; disable rendering
  STX $4010   ; disable DMC IRQs

  LDX #0
clear_ram:
  LDA #$00
  STA $0000,X
  STA $0100,X
  STA $0300,X
  STA $0400,X
  STA $0500,X
  STA $0600,X
  STA $0700,X
  LDA #$fe
  STA $0200,X
  INX
  BNE clear_ram

  SCREEN_ON

  LDX #<music_data
  LDY #>music_data
  LDA #1
  JSR FamiToneInit

  ; init FamiTone SFX
  LDX #<sfx_data
  LDY #>sfx_data
  LDA #1
  JSR FamiToneSfxInit

  ; init rng
  LDA #$a9
  STA rng_seed
  LDA #$73
  STA rng_seed+1

  LDA #$ff
  STA current_song

  CLI ; enable interrupts

  JSR go_to_title

forever:
  LDA nmis
  CMP old_nmis
  BEQ etc
  STA old_nmis
  JSR refresh_oam
  JSR update_tiles
  JSR fix_scroll
.ifdef DEBUG
  LDA #%01011110  ; green tint
  STA PPUMASK
.endif
  ; new frame code
  JSR game_state_handler
.ifdef DEBUG
  LDA #%01111110  ; yellow tint
  STA PPUMASK
.endif
  JSR FamiToneUpdate
.ifdef DEBUG
  LDA #%00111110  ; red tint
  STA PPUMASK
.endif
  JSR reset_pressed_buttons
.ifdef DEBUG
  LDA #%10011110  ; blue tint
  STA PPUMASK
.endif
  JSR physics_update
.ifdef DEBUG
  LDA #%11011110  ; cyan tint
  STA PPUMASK
.endif
  JSR update_pressed_buttons
.ifdef DEBUG
  LDA #%00011110  ; no tint
  STA PPUMASK
.endif
etc:
  JSR rand ; shuffle rng around
  JMP forever
.endproc

.proc fix_scroll
  LDA PPUSTATUS

  LDA scroll_sx
  ASL
  LDA scroll_x
  ROL
  STA PPUSCROLL ; horizontal scroll
  LDA #$00
  STA PPUSCROLL

  LDA scroll_x
  AND #%10000000
  ROL
  ROL
  ORA #%10001000  ; turn on NMIs, sprites use second pattern table
  STA PPUCTRL
  RTS
.endproc

.proc refresh_oam
  ; Refresh OAM
  LDA #$00
  STA OAMADDR
  LDA #$02
  STA OAMDMA

  RTS
.endproc

.proc load_palettes
  ; cobbles Y
  LDY PPUSTATUS
  LDY #$3f
  STY PPUADDR
  LDY #$00
  STY PPUADDR
:
  LDA palettes,Y
  STA PPUDATA
  INY
  CPY #$20
  BNE :-
  RTS
.endproc

.proc load_debug_palettes
  ; cobbles Y
  LDY PPUSTATUS
  LDY #$3f
  STY PPUADDR
  LDY #$00
  STY PPUADDR
:
  LDA debug_palettes,Y
  STA PPUDATA
  INY
  CPY #$20
  BNE :-
  RTS
.endproc

.proc load_default_chr
  ; bg
  LDA #0
  STA BANK_SELECT
  LDA #0
  STA BANK_DATA
  LDA #1
  STA BANK_SELECT
  LDA #2
  STA BANK_DATA

  ; sprites
  LDA #2
  STA BANK_SELECT
  LDA #4
  STA BANK_DATA

  LDA #3
  STA BANK_SELECT
  LDA #5
  STA BANK_DATA

  LDA #4
  STA BANK_SELECT
  LDA #6
  STA BANK_DATA

  LDA #5
  STA BANK_SELECT
  LDA #7
  STA BANK_DATA
  RTS
.endproc

.proc load_debug_chr
  ; bg
  LDA #0
  STA BANK_SELECT
  LDA #8
  STA BANK_DATA
  LDA #1
  STA BANK_SELECT
  LDA #10
  STA BANK_DATA

  ; sprites
  LDA #2
  STA BANK_SELECT
  LDA #12
  STA BANK_DATA

  LDA #3
  STA BANK_SELECT
  LDA #13
  STA BANK_DATA

  LDA #4
  STA BANK_SELECT
  LDA #14
  STA BANK_DATA

  LDA #5
  STA BANK_SELECT
  LDA #15
  STA BANK_DATA
  RTS
.endproc

.proc game_state_handler
  LDX game_state
  LDA game_state_handlers_h, X
  PHA
  LDA game_state_handlers_l, X
  PHA
  RTS
.endproc

.proc go_to_title
  STOP_IF_NOT Doom

  LDA #game_states::waiting_to_start
  STA game_state

  SCREEN_OFF

  JSR load_palettes

  JSR load_default_chr

  LDA PPUSTATUS
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR

  LDA #<nametable_title
  STA rle_ptr
  LDA #>nametable_title
  STA rle_ptr+1
  JSR unrle

  LDA PPUSTATUS
  LDA #$24
  STA PPUADDR
  LDA #$00
  STA PPUADDR

  LDA #<nametable_credits
  STA rle_ptr
  LDA #>nametable_credits
  STA rle_ptr+1
  JSR unrle

  VBLANK

  SCREEN_ON

  LDA #$0
  LDX #0
@loop:
  STA credits_scroll_x, X
  STA credits_scroll_sx, X
  INX
  CPX #CREDITS_SCROLL_STRIPES
  BNE @loop
  STA credits_current_stripe
  STA credits_state

  PLAY Doom

  RTS
.endproc

.proc go_to_playing
  STOP_IF_NOT Jazz
  LDA #game_states::playing
  STA game_state

  SCREEN_OFF

  JSR load_palettes

  JSR load_default_chr

  LDA #$00

  STA scroll_x
  STA scroll_sx

  LDX current_level

  LDA level_data_pointers_l, X
  STA addr_ptr
  LDA level_data_pointers_h, X
  STA addr_ptr+1

  LDY #0 ; Y iterates over level data

  ; read and load nametables

  LDA PPUSTATUS
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR

  LDA (addr_ptr), Y
  INY
  STA rle_ptr
  LDA (addr_ptr), Y
  INY
  STA rle_ptr+1
  save_regs
  JSR unrle
  restore_regs

  LDA #$24
  STA PPUADDR
  LDA #$00
  STA PPUADDR

  LDA (addr_ptr), Y
  INY
  STA rle_ptr
  LDA (addr_ptr), Y
  INY
  STA rle_ptr+1
  save_regs
  JSR unrle
  restore_regs

  LDA (addr_ptr), Y
  INY
  STA level_door_x
  LDA (addr_ptr), Y
  INY
  STA level_door_y

  LDA (addr_ptr), Y
  INY
  STA level_door_ppu_addr
  LDA (addr_ptr), Y
  INY
  STA level_door_ppu_addr+1

  LDA (addr_ptr), Y
  INY
  STA level_door_open

  LDA (addr_ptr), Y
  INY
  STA glitch_x
  LDA (addr_ptr), Y
  INY
  STA glitch_y

  LDA (addr_ptr), Y
  INY
  STA glitch_ppu_addr
  LDA (addr_ptr), Y
  INY
  STA glitch_ppu_addr+1
  BNE :+
  LDA #$1
  STA debugged
:

  LDA (addr_ptr), Y
  INY
  CMP dialog_counter
  BCC :+
  STA dialog_counter
:

  LDX #0

  ; read objects (zero = stop)
  ; first object = robot
@objects_loop:
  LDA (addr_ptr), Y
  BEQ @end_of_objects
  INY
  STA object_x, X
  LDA (addr_ptr), Y
  INY
  STA object_y, X
  LDA (addr_ptr), Y
  INY
  STA object_flags, X
  LDA (addr_ptr), Y
  INY
  STA object_button_command, X

  LDA (addr_ptr), Y
  INY
  STA object_button_arg1, X
  LDA (addr_ptr), Y
  INY
  STA object_button_arg2, X

  LDA #$0
  STA object_sx, X
  STA object_sy, X
  STA object_vx, X
  STA object_svx, X
  STA object_vy, X
  STA object_svy, X
  STA object_pressed, X
  INX
  JMP @objects_loop

@end_of_objects:
  INY

  STX objects_length

  ; read bg matrix pointer
  LDA (addr_ptr), Y
  INY
  STA bg_matrix_ptr

  LDA (addr_ptr), Y
  INY
  STA bg_matrix_ptr+1

  VBLANK

  SCREEN_ON

  PLAY Jazz

  RTS
.endproc

.proc go_to_debugging
  STOP_IF_NOT InevitableDoom

  LDA #game_states::debugging
  STA game_state

  SCREEN_OFF

  JSR load_debug_palettes

  JSR load_debug_chr

  LDX current_level

  LDA debug_level_data_pointers_l, X
  STA addr_ptr
  LDA debug_level_data_pointers_h, X
  STA addr_ptr+1

  LDY #0 ; Y iterates over level data

  ; read and load nametable

  LDA PPUSTATUS
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR

  LDA (addr_ptr), Y
  INY
  STA rle_ptr
  LDA (addr_ptr), Y
  INY
  STA rle_ptr+1
  save_regs
  JSR unrle
  restore_regs

  LDX #0
@pieces_loop:
  LDA (addr_ptr), Y
  BEQ @exit_loop
  INY
  STA piece_x, X
  LDA (addr_ptr), Y
  INY
  STA piece_y, X
  LDA (addr_ptr), Y
  INY
  STA piece_target_x, X
  LDA (addr_ptr), Y
  INY
  STA piece_target_y, X
  LDA (addr_ptr), Y
  INY
  STA piece_index, X
  INX
  JMP @pieces_loop
@exit_loop:
  STX pieces_length

  LDA #$00
  STA current_piece

  LDA #$00
  STA PPUSCROLL
  STA PPUSCROLL
  STA scroll_x
  STA scroll_sx

  VBLANK

  SCREEN_ON

  PLAY InevitableDoom

  RTS
.endproc

.proc go_to_game_over
  JSR FamiToneMusicStop

  LDA #game_states::game_over
  STA game_state

  ; erase sprites
  LDX #$00
  LDA #$F0
:
  STA oam_sprites+Sprite::ycoord, X
  .repeat .sizeof(Sprite)
  INX
  .endrepeat
  BNE :-

  SCREEN_OFF

  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR

  LDA #<nametable_game_over
  STA rle_ptr
  LDA #>nametable_game_over
  STA rle_ptr+1
  JSR unrle

  VBLANK

  SCREEN_ON

  LDA #$00
  STA object_x
  LDA #$80
  STA object_y
  RTS
.endproc

.proc waiting_to_start
  LDA credits_state
  BEQ title
  CMP #1
  BEQ transition_to_credits
  CMP #2
  BEQ credits
  CMP #3
  BEQ transition_to_title
  KIL
title:
  JSR readjoy
  LDA pressed_buttons
  AND #BUTTON_START
  BEQ :+

  LDA #1
  STA current_level
  LDA #$00
  STA debugged
  STA fixed_flags
  STA dialog_counter
  STA first_fix_counter
  JSR go_to_playing
:
  LDA pressed_buttons
  AND #BUTTON_SELECT
  BEQ :+
  INC credits_state
:
  RTS
transition_to_credits:
  JSR scroll_to_credits
  RTS
credits:
  JSR readjoy
  LDA pressed_buttons
  BEQ :+
  INC credits_state
:
  RTS
transition_to_title:
  JSR scroll_to_title
  RTS
.endproc

DELTA_SCROLL = $0100
.proc scroll_to_credits

  LDA credits_scroll_x
  CMP #$80
  BEQ skip_scroll_increment

  CLC
  LDA credits_scroll_sx
  ADC #<(DELTA_SCROLL)
  STA credits_scroll_sx
  STA temp_sx
  LDA credits_scroll_x
  ADC #>(DELTA_SCROLL)
  STA credits_scroll_x
  STA temp_x
skip_scroll_increment:

  LDX #1
@loop:
  LDA credits_scroll_x, X
  CMP #$80
  BEQ @next
  LDA temp_x
  CMP #$80
  BEQ @inc
  SEC
  LDA temp_x
  SBC credits_scroll_x, X
  CMP #$08
  BCS @inc
  JMP @next
@inc:
  CLC
  LDA credits_scroll_sx, X
  ADC #<(DELTA_SCROLL)
  STA credits_scroll_sx, X
  LDA credits_scroll_x, X
  ADC #>(DELTA_SCROLL)
  STA credits_scroll_x, X
@next:
  LDA credits_scroll_x, X
  STA temp_x
  LDA credits_scroll_sx, X
  STA temp_sx
  INX
  CPX #CREDITS_SCROLL_STRIPES
  BNE @loop

  LDA temp_x
  CMP #$80
  BNE :+
  INC credits_state
:
  RTS
.endproc

.proc scroll_to_title
  LDA credits_scroll_x
  BEQ skip_scroll_increment

  CLC
  LDA credits_scroll_sx
  ADC #<(DELTA_SCROLL)
  STA credits_scroll_sx
  STA temp_sx
  LDA credits_scroll_x
  ADC #>(DELTA_SCROLL)
  STA credits_scroll_x
  STA temp_x
skip_scroll_increment:

  LDX #1
@loop:
  LDA credits_scroll_x, X
  BEQ @next
  LDA temp_x
  BEQ @inc
  SEC
  LDA temp_x
  SBC credits_scroll_x, X
  CMP #$08
  BCS @inc
  JMP @next
@inc:
  CLC
  LDA credits_scroll_sx, X
  ADC #<(DELTA_SCROLL)
  STA credits_scroll_sx, X
  LDA credits_scroll_x, X
  ADC #>(DELTA_SCROLL)
  STA credits_scroll_x, X
@next:
  LDA credits_scroll_x, X
  STA temp_x
  LDA credits_scroll_sx, X
  STA temp_sx
  INX
  CPX #CREDITS_SCROLL_STRIPES
  BNE @loop

  LDA temp_x
  BNE :+
  LDA #$00
  STA credits_state
:
  RTS
.endproc

.proc game_over
  LDA object_y
  CMP #$f1
  BNE @still_here
  LDA PPUSTATUS
  JSR rand
  LDA rng_seed
  AND #%11
  CLC
  ADC #$20
  STA PPUADDR
  LDA rng_seed+1
  STA PPUADDR
  JSR rand
  LDA rng_seed
  STA PPUDATA
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR
  RTS
@still_here:
  LDA #$00
  STA sprite_counter
  LDA nmis
  AND #%11000
  LSR
  LSR
  LSR
  STX anim_offset
  LDA anim_sprites_l, X
  STA addr_ptr
  LDA anim_sprites_h, X
  STA addr_ptr+1
  LDA object_x
  STA temp_x
  LDA object_y
  STA temp_y
  JSR display_metasprite

  LDA object_x
  CMP #$90
  BEQ @vertical
  INC object_x
  RTS
@vertical:
  LDA nmis
  AND #%11
  BEQ :+
  RTS
:
  DEC object_y
  LDA object_y
  CMP #$60
  BNE :+
  PRINT thank_you_1, $21e6
  RTS
:
  CMP #$4e
  BNE :+
  PRINT thank_you_2, $2228
  RTS
:
  CMP #$48
  BNE :+
  PRINT thank_you_3, $2230
  RTS
:
  CMP #$40
  BNE :+
  PRINT thank_you_4, $2248
:
  RTS
.endproc

.proc update_horizontal_speed
  LDA object_vx
  BMI @negative_speed
  BEQ @tiebreak
  JMP @positive_speed

@tiebreak:
  LDA object_svx
  BEQ @zero_speed
  JMP @positive_speed
@zero_speed:
  LDA object_flags
  AND #%11000001
  STA object_flags
  JMP @endspeed
@positive_speed:
  LDA object_flags
  AND #%11000000
  ORA #%00000010
  STA object_flags
  AND #OBJ_GROUNDED_FLAG
  BEQ @endspeed

  ; decay positive speed

  CLC
  LDA object_svx
  ADC #<(-GROUND_SPEED_DECAY)
  STA object_svx
  LDA object_vx
  ADC #>(-GROUND_SPEED_DECAY)
  STA object_vx
  BPL @endspeed
  LDA #$0
  STA object_vx
  STA object_svx

  JMP @endspeed
@negative_speed:
  LDA object_flags
  AND #%11000000
  ORA #%00000011
  STA object_flags
  AND #OBJ_GROUNDED_FLAG
  BEQ @endspeed

  ; decay negative speed
  CLC
  LDA object_svx
  ADC #<GROUND_SPEED_DECAY
  STA object_svx
  LDA object_vx
  ADC #>GROUND_SPEED_DECAY
  STA object_vx
  BMI @endspeed
  LDA #$0
  STA object_vx
  STA object_svx

  ; JMP @endspeed
@endspeed:
  RTS
.endproc

.proc first_fix
  JSR readjoy
  LDA buttons
  AND #BUTTON_LEFT
  BEQ :+
  INC first_fix_counter
  LDA first_fix_counter
  CMP #90
  BCC :+
  JSR go_to_debugging
:
  RTS
.endproc

.proc dialog
  JSR readjoy
  LDA pressed_buttons
  BEQ :+
  
  GLITCH_SFX

  LDX dialog_counter
  LDA dialog_ppu_l, X
  STA ppu_addr_ptr
  LDA dialog_ppu_h, X
  STA ppu_addr_ptr+1
  LDA dialog_string_l, X
  STA addr_ptr
  LDA dialog_string_h, X
  STA addr_ptr+1
  save_regs
  JSR write_string
  restore_regs
  INC dialog_counter
:
  RTS
.endproc

.proc platforming_input
  LDX dialog_counter
  LDA dialog_ppu_h, X
  BEQ :+
  JSR dialog
  RTS
:
  LDA fixed_flags
  AND #FIXED_MOVE
  BNE :+
  JSR first_fix
  RTS
:
  JSR update_horizontal_speed
  JSR readjoy
.ifdef DEBUG
  LDA pressed_buttons
  AND #BUTTON_SELECT
  BEQ @noselect
  debugOut { "Coordinates: ", fHex8(object_x), fHex8(object_sx), " (9.7), ", fHex8(object_y), fHex8(object_sy), " (8.8)." }
@noselect:
.endif
  LDA object_flags
  AND #OBJ_GROUNDED_FLAG
  BEQ air_controls
ground_controls:
  LDA buttons
  AND #BUTTON_LEFT
  BEQ :+

  signed_compare_words object_vx, object_svx, #>(-MAX_HORIZONTAL_SPEED), #<(-MAX_HORIZONTAL_SPEED)
  BMI :+

  CLC
  LDA object_svx
  ADC #<(-CONTROL_ACCELERATION)
  STA object_svx

  LDA object_vx
  ADC #>(-CONTROL_ACCELERATION)
  STA object_vx

:
  LDA buttons
  AND #BUTTON_RIGHT
  BEQ :+

  signed_compare_words #>MAX_HORIZONTAL_SPEED, #<MAX_HORIZONTAL_SPEED, object_vx, object_svx
  BMI :+

  CLC
  LDA object_svx
  ADC #<CONTROL_ACCELERATION
  STA object_svx

  LDA object_vx
  ADC #>CONTROL_ACCELERATION
  STA object_vx

:
  LDA pressed_buttons
  AND #BUTTON_UP
  BEQ :+
  LDA fixed_flags
  AND #FIXED_JUMP
  BEQ :+
  LDA object_flags
  AND #(~OBJ_GROUNDED_FLAG)
  STA object_flags
  LDA #<JUMP_SPEED
  STA object_svy
  LDA #>JUMP_SPEED
  STA object_vy
  SFX Jump, CH2
:
  RTS
air_controls:
  LDA buttons
  AND #BUTTON_LEFT
  BEQ :+

  signed_compare_words object_vx, object_svx, #>(-MAX_AIR_HORIZONTAL_SPEED), #<(-MAX_AIR_HORIZONTAL_SPEED)
  BMI :+

  CLC
  LDA object_svx
  ADC #<(-CONTROL_ACCELERATION)
  STA object_svx

  LDA object_vx
  ADC #>(-CONTROL_ACCELERATION)
  STA object_vx

:
  LDA buttons
  AND #BUTTON_RIGHT
  BEQ :+

  signed_compare_words #>MAX_AIR_HORIZONTAL_SPEED, #<MAX_AIR_HORIZONTAL_SPEED, object_vx, object_svx
  BMI :+

  CLC
  LDA object_svx
  ADC #<CONTROL_ACCELERATION
  STA object_svx

  LDA object_vx
  ADC #>CONTROL_ACCELERATION
  STA object_vx
:
  RTS
.endproc

.proc update_tiles
  LDA game_state
  CMP #game_states::playing
  BEQ :+
  RTS
:
  BIT PPUSTATUS
  LDA level_door_open
  BEQ @closed
@open:
  LDA level_door_ppu_addr+1
  STA PPUADDR
  LDA level_door_ppu_addr
  STA PPUADDR
  LDA #$80
  STA PPUDATA
  LDA #$81
  STA PPUDATA
  CLC
  LDA level_door_ppu_addr
  ADC #$20
  PHA
  LDA level_door_ppu_addr+1
  ADC #$00
  STA PPUADDR
  PLA
  STA PPUADDR
  LDA #$90
  STA PPUDATA
  LDA #$91
  STA PPUDATA
  JMP @end_door
@closed:
  LDA level_door_ppu_addr+1
  STA PPUADDR
  LDA level_door_ppu_addr
  STA PPUADDR
  LDA #$8d
  STA PPUDATA
  LDA #$8e
  STA PPUDATA
  CLC
  LDA level_door_ppu_addr
  ADC #$20
  PHA
  LDA level_door_ppu_addr+1
  ADC #$00
  STA PPUADDR
  PLA
  STA PPUADDR
  LDA #$9d
  STA PPUDATA
  LDA #$9e
  STA PPUDATA
@end_door:
  LDA debugged
  BNE @resetaddr
  JSR rand
  LDA glitch_ppu_addr+1
  STA PPUADDR
  LDA glitch_ppu_addr
  STA PPUADDR
  LDA rng_seed
  STA PPUDATA
  LDA rng_seed+1
  STA PPUDATA
  CLC
  LDA glitch_ppu_addr
  ADC #$20
  PHA
  LDA glitch_ppu_addr+1
  ADC #$00
  STA PPUADDR
  PLA
  STA PPUADDR
  JSR rand
  LDA rng_seed
  STA PPUDATA
  LDA rng_seed+1
  STA PPUDATA  
@resetaddr:
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR
  RTS
.endproc

.proc wincon
  LDA level_door_open
  BNE :+
  RTS
:
  LDX #$00
  JSR prepare_object_hitbox
  LDA level_door_x
  STA hitbox_b+Box::x1
  STA hitbox_b+Box::x2
  LDA #$00
  STA hitbox_b+Box::sx1
  STA hitbox_b+Box::sx2
  LDA level_door_y
  STA hitbox_b+Box::y1
  STA hitbox_b+Box::y2
  JSR hitbox_collision
  BNE :+
  RTS
:
  LDA current_level
  CMP #LAST_LEVEL
  BEQ @game_over
  INC current_level
  LDA #$00
  STA debugged
  JSR go_to_playing
  RTS
@game_over:
  JSR go_to_game_over
  RTS
.endproc

.proc check_glitch
  LDA debugged
  BEQ :+
  RTS
:
  LDA fixed_flags ; the first glitch is entered after dialog
  BNE :+
  RTS
:
  LDX #$00
  JSR prepare_object_hitbox
  LDA glitch_x
  STA hitbox_b+Box::x1
  STA hitbox_b+Box::x2
  LDA #$00
  STA hitbox_b+Box::sx1
  STA hitbox_b+Box::sx2
  LDA glitch_y
  STA hitbox_b+Box::y1
  STA hitbox_b+Box::y2
  JSR hitbox_collision
  BNE :+
  RTS
:
  JSR go_to_debugging
  RTS
.endproc

.proc playing
  JSR platforming_input

  ; render objects' sprites
  LDA #0
  STA sprite_counter

  LDA nmis
  AND #%11000
  LSR
  LSR
  LSR
  STA anim_offset

  JSR update_scroll

  LDY #0
@loop:
  ; temp x = x - scroll x
  ; TODO skip if out of current screen
  SEC
  LDA object_sx, Y
  SBC scroll_sx
  STA temp_sx
  LDA object_x, Y
  SBC scroll_x
  STA temp_x
  BMI @next

  ASL temp_sx
  ROL temp_x

  LDA object_y, Y
  STA temp_y
  LDA object_flags, Y
  AND #OBJ_ANIM_MASK
  ASL
  ASL
  CLC
  ADC anim_offset
  TAX
  LDA anim_sprites_l, X
  STA addr_ptr
  LDA anim_sprites_h, X
  STA addr_ptr+1
  save_regs
  JSR display_metasprite
  restore_regs
@next:
  INY
  CPY objects_length
  BNE @loop

  ; erase sprite leftovers
  LDX sprite_counter
  LDA #$f0
:
  STA oam_sprites+Sprite::ycoord, X
  .repeat .sizeof(Sprite)
  INX
  .endrepeat
  BNE :-

  JSR wincon
  JSR check_glitch

  RTS
.endproc

.proc debugging
  LDY #$00
  STY sprite_counter
@draw_loop:
  LDA piece_x, Y
  STA temp_x
  LDA piece_y, Y
  STA temp_y
  LDA piece_index, Y
  TAX
  LDA debug_sprites_l, X
  STA addr_ptr
  LDA debug_sprites_h, X
  STA addr_ptr+1
  save_regs
  JSR display_metasprite
  restore_regs
@next:
  INY
  CPY pieces_length
  BNE @draw_loop

  ; erase sprite leftovers
  LDX sprite_counter
  LDA #$f0
:
  STA oam_sprites+Sprite::ycoord, X
  .repeat .sizeof(Sprite)
  INX
  .endrepeat
  BNE :-

  LDA debugged
  BEQ @still_debugging
  JSR readjoy
  LDA pressed_buttons
  BEQ :+
  JSR go_to_playing
:
  RTS  
@still_debugging:


  LDX current_piece

  JSR readjoy
  LDA pressed_buttons
  AND #BUTTON_B
  BEQ :+
  INC current_piece
  LDA current_piece
  CMP pieces_length
  BNE :+
  LDA #$00
  STA current_piece
:
  LDA pressed_buttons
  AND #BUTTON_A
  BEQ :+
  INC piece_index, X
  LDA piece_index, X
  AND #%11
  BNE :+
  LDA piece_index, X
  SEC
  SBC #%100
  STA piece_index, X
:
  LDA buttons
  AND #BUTTON_RIGHT
  BEQ :+
  INC piece_x, X
:
  LDA buttons
  AND #BUTTON_LEFT
  BEQ :+
  DEC piece_x, X
:
  LDA buttons
  AND #BUTTON_DOWN
  BEQ :+
  INC piece_y, X
:
  LDA buttons
  AND #BUTTON_UP
  BEQ :+
  DEC piece_y, X
:

  JSR check_debug

  RTS
.endproc

.proc check_debug
  LDX #$00
  STX temp_x
@loop:
  LDA piece_index, X
  AND #%11
  BNE @next

  LDA piece_x, X
  SEC
  SBC piece_target_x, X
  BPL :+
  EOR #%11111111
  CLC
  ADC #%1
:
  CMP #$05
  BCS @next

  LDA piece_y, X
  SEC
  SBC piece_target_y, X
  BPL :+
  EOR #%11111111
  CLC
  ADC #%1
:
  CMP #$05
  BCS @next

  LDA piece_x, X
  CMP piece_target_x, X
  BNE :+
  LDA piece_y, X
  CMP piece_target_y, X
  BEQ @no_click
:
  ; x and y are near target, round to target
  LDA piece_target_x, X
  STA piece_x, X
  LDA piece_target_y, X
  STA piece_y, X
  SFX Click, CH2
@no_click:
  INC temp_x

  CPX current_piece
  BNE @next
  INC current_piece
  LDA current_piece
  CMP pieces_length
  BNE @next
  LDA #$00
  STA current_piece
@next:
  INX
  CPX pieces_length
  BNE @loop

  LDA temp_x
  CMP pieces_length
  BEQ @finished
  RTS
@finished:
  LDA #$1
  STA debugged
  SEC
  ROL fixed_flags
  RTS
.endproc

.proc prepare_object_hitbox
  ; fills hitbox buffer with current (indexed by Y) object hitbox
  ; cobbles A
  TYA
  PHA

  LDA object_flags, X
  AND #OBJ_ANIM_MASK
  LSR
  TAY

  CLC
  LDA sprite_hitbox_sx1, Y
  ADC object_sx, X
  STA hitbox_a+Box::sx1
  LDA sprite_hitbox_x1, Y
  ADC object_x, X
  STA hitbox_a+Box::x1

  CLC
  LDA sprite_hitbox_y1, Y
  ADC object_y, X
  STA hitbox_a+Box::y1

  CLC
  LDA sprite_hitbox_sx2, Y
  ADC object_sx, X
  STA hitbox_a+Box::sx2
  LDA sprite_hitbox_x2, Y
  ADC object_x, X
  STA hitbox_a+Box::x2

  CLC
  LDA sprite_hitbox_y2, Y
  ADC object_y, X
  STA hitbox_a+Box::y2

  PLA
  TAY
  RTS
.endproc

.proc hitbox_collision
  ; returns 1 in A if hitbox_a and hitbox_b intersect
  ; Pseudo-code:
  ;  ((a.x1,a.y1),(a.x2,a.y2)) and ((b.x1,b.y1),(b.x2,b.y2))
  ;
  ;  if (a.x2<b.x1 or b.x2<a.x1 or a.y2<b.y1 or b.y2<a.y1):
  ;      don't intersect
  ;  else
  ;      intersect
  ;
  ;  Also: foo < bar ==> foo; CMP bar; carry is clear
  CLC
  LDA hitbox_a+Box::x2
  CMP hitbox_b+Box::x1
  BNE :+
  LDA hitbox_a+Box::sx2
  CMP hitbox_b+Box::sx1
:
  BCS :+
  LDA #$00
  RTS
:
  CLC
  LDA hitbox_b+Box::x2
  CMP hitbox_a+Box::x1
  BNE :+
  LDA hitbox_b+Box::sx2
  CMP hitbox_a+Box::sx1
:
  BCS :+
  LDA #$00
  RTS
:

  CLC
  LDA hitbox_a+Box::y2
  CMP hitbox_b+Box::y1
  BCS :+
  LDA #$00
  RTS
:

  CLC
  LDA hitbox_b+Box::y2
  CMP hitbox_a+Box::y1
  BCS :+
  LDA #$00
  RTS
:
  LDA #$01
  RTS
.endproc

.proc reset_pressed_buttons
  LDX objects_length
  DEX
@loop:
  DEC object_pressed, X
  BPL @next
  INC object_pressed, X
@next:
  DEX
  BPL @loop
  RTS
.endproc

.proc update_pressed_buttons
  LDX objects_length
  DEX
@loop:
  LDA object_button_command, X
  BEQ @next ; not a button

  LDA object_pressed, X
  BEQ @not_pressed
@pressed:
  CMP #$10
  BNE @next

  LDA object_flags, X
  AND #(~OBJ_ANIM_MASK)
  ORA #(sprite_id::button_on<<1)
  STA object_flags, X

  JSR activate_button

  JMP @next

@not_pressed:
  LDA object_flags, X
  AND #(~OBJ_ANIM_MASK)
  ORA #(sprite_id::button_off<<1)
  STA object_flags, X
  JSR deactivate_button
@next:
  DEX
  BPL @loop
  RTS
.endproc

.proc activate_button
  LDA object_button_command, X
  CMP #button_type::respawn_box
  BEQ @respawn_box
  CMP #button_type::open_door
  BEQ @open_door
  RTS
@respawn_box:
  LDA fixed_flags
  AND #FIXED_RESPAWN
  BNE :+
  RTS
:
  LDA object_button_arg1, X
  TAY
  LDA object_button_arg1, Y
  STA object_x, Y
  LDA object_button_arg2, Y
  STA object_y, Y
  LDA #$00
  STA object_sx, Y
  STA object_vy, Y
  STA object_svy, Y
  RTS
@open_door:
  LDA fixed_flags
  AND #FIXED_OPEN
  BNE :+
  RTS
:
  LDA #$01
  STA level_door_open
  RTS
.endproc

.proc deactivate_button
  LDA object_button_command, X
  CMP #button_type::respawn_box
  BEQ @respawn_box
  CMP #button_type::open_door
  BEQ @open_door
  RTS
@respawn_box:
  RTS
@open_door:
  LDA #$00
  STA level_door_open
  RTS
.endproc

.proc physics_update
  LDA game_state
  CMP #game_states::playing
  BEQ :+
  RTS
:
  LDX #0
@loop:
  LDA object_flags, X
  AND #OBJ_MOVE_FLAG
  BEQ @next

  JSR physics_update_single_object_horizontal
  JSR physics_update_single_object_vertical
@next:
  INX
  CPX objects_length
  BNE @loop

  RTS
.endproc

.proc physics_update_single_object_horizontal
  ; compute movement for X-index object, handling collision

  ; horizontal movement
  CLC
  LDA object_sx, X
  STA backup_object_sx
  ADC object_svx, X
  STA object_sx, X
  LDA object_x, X
  STA backup_object_x
  ADC object_vx, X
  STA object_x, X
  ; skip collision if real position didn't change
  CMP backup_object_x
  BNE :+
  LDA object_sx, X
  CMP backup_object_sx
  BNE :+
  RTS
:

  JSR horizontal_bg_collision
  JSR inter_object_collision
  RTS
.endproc

.proc physics_update_single_object_vertical
  ; compute movement for X-index object, handling collision

  ; vertical movement
  ; G
  LDA object_svy, X
  ADC #<GRAVITY
  STA object_svy, X
  LDA object_vy, X
  ADC #>GRAVITY
  STA object_vy, X

  CLC
  LDA object_sy, X
  STA backup_object_sy
  ADC object_svy, X
  STA object_sy, X
  LDA object_y, X
  STA backup_object_y
  ADC object_vy, X
  STA object_y, X
  ; skip collision if real position didn't change
  CMP backup_object_y
  BNE :+
  LDA object_sy, X
  CMP backup_object_sy
  BNE :+
  RTS
:
  JSR vertical_bg_collision
  JSR inter_object_collision
  RTS
.endproc


.proc horizontal_bg_collision
  ; checks if X-index object collides with bg tile
  ; if so, it stops just before collision
  ; input: X index, backup_object_*
  ; cobbles: Y, object_* collision_*

  LDA object_x, X
  CMP backup_object_x
  BCC negative_direction
  BNE positive_direction
  LDA object_sx, X
  CMP backup_object_sx
  BCC negative_direction

positive_direction:
  LDA #direction::right
  STA temp_dir

  LDA object_flags, X
  AND #OBJ_ANIM_MASK
  LSR
  TAY

  CLC
  LDA sprite_hitbox_sx2, Y
  ADC object_sx, X
  STA collision_sx
  LDA sprite_hitbox_x2, Y
  ADC object_x, X
  STA collision_x

  CLC
  LDA sprite_hitbox_y1, Y
  ADC object_y, X
  STA collision_y

  JSR bg_matrix_collision
  BNE round_position_positive

  CLC
  LDA sprite_hitbox_y2, Y
  ADC object_y, X
  STA collision_y

  JSR bg_matrix_collision
  BNE round_position_positive
  RTS

negative_direction:
  LDA #direction::left
  STA temp_dir

  LDA object_flags, X
  AND #OBJ_ANIM_MASK
  LSR
  TAY

  CLC
  LDA sprite_hitbox_sx1, Y
  ADC object_sx, X
  STA collision_sx
  LDA sprite_hitbox_x1, Y
  ADC object_x, X
  STA collision_x

  CLC
  LDA sprite_hitbox_y1, Y
  ADC object_y, X
  STA collision_y

  JSR bg_matrix_collision
  BNE round_position_negative

  CLC
  LDA sprite_hitbox_y2, Y
  ADC object_y, X
  STA collision_y

  JSR bg_matrix_collision
  BNE round_position_negative
  RTS

round_position_positive:
  LDA #$80
  STA collision_sx
  LDA collision_x
  AND #$f8
  STA collision_x
  DEC collision_x
  SEC
  LDA collision_sx
  SBC sprite_hitbox_sx2, Y
  STA object_sx, X
  LDA collision_x
  SBC sprite_hitbox_x2, Y
  STA object_x, X
  ; stop speed
  LDA #$00
  STA object_vx, X
  STA object_svx, X
  RTS

round_position_negative:
  LDA #$00
  STA collision_sx
  LDA collision_x
  AND #$f8
  CLC
  ADC #$08
  STA collision_x
  SEC
  LDA collision_sx
  SBC sprite_hitbox_sx1, Y
  STA object_sx, X
  LDA collision_x
  SBC sprite_hitbox_x1, Y
  STA object_x, X
  ; stop speed
  LDA #$00
  STA object_vx, X
  STA object_svx, X
  RTS
.endproc

.proc vertical_bg_collision
  ; checks if X-index object collides with bg tile
  ; if so, it stops just before collision
  ; input: X index, backup_object_*
  ; cobbles: Y, object_* collision_*
  LDA object_y, X
  CMP backup_object_y
  BCC negative_direction
  BNE positive_direction
  LDA object_sy, X
  CMP backup_object_sy
  BCC negative_direction

positive_direction:
  LDA #direction::down
  STA temp_dir

  LDA object_flags, X
  AND #OBJ_ANIM_MASK
  LSR
  TAY

  CLC
  LDA sprite_hitbox_sx1, Y
  ADC object_sx, X
  STA collision_sx
  LDA sprite_hitbox_x1, Y
  ADC object_x, X
  STA collision_x

  CLC
  LDA sprite_hitbox_y2, Y
  ADC object_y, X
  STA collision_y

  JSR bg_matrix_collision
  BNE round_position_positive

  CLC
  LDA sprite_hitbox_sx2, Y
  ADC object_sx, X
  STA collision_sx
  LDA sprite_hitbox_x2, Y
  ADC object_x, X
  STA collision_x

  JSR bg_matrix_collision
  BNE round_position_positive
  RTS

round_position_positive:
  LDA collision_y
  AND #$f0
  SEC
  SBC #$01
  SEC
  SBC sprite_hitbox_y2
  STA object_y, X
  LDA #$00
  STA object_sy, X
  ; stop speed
  STA object_vy, X
  STA object_svy, X
  LDA object_flags, X
  ORA #OBJ_GROUNDED_FLAG
  STA object_flags, X
  RTS

negative_direction:
  LDA #direction::up
  STA temp_dir

  LDA #%00
  STA collision_top_flags
  LDA object_flags, X
  AND #OBJ_ANIM_MASK
  LSR
  TAY

  CLC
  LDA sprite_hitbox_sx1, Y
  ADC object_sx, X
  STA collision_sx
  LDA sprite_hitbox_x1, Y
  ADC object_x, X
  STA collision_x

  CLC
  LDA sprite_hitbox_y1, Y
  ADC object_y, X
  STA collision_y

  JSR bg_matrix_collision
  BEQ :+
  LDA #%10
  STA collision_top_flags
:

  CLC
  LDA sprite_hitbox_sx2, Y
  ADC object_sx, X
  STA collision_sx
  LDA sprite_hitbox_x2, Y
  ADC object_x, X
  STA collision_x
  JSR bg_matrix_collision
  BEQ :+
  LDA #%01
  ORA collision_top_flags
  STA collision_top_flags
:

  LDA collision_top_flags
  BNE :+
  RTS
:
  CMP #%11
  BEQ round_position_negative
  CMP #%10
  BNE :+
  ; nudge right
  CLC
  LDA object_sx, X
  ADC #<(JUMP_NUDGE)
  STA object_sx, X
  LDA object_x, X
  ADC #>(JUMP_NUDGE)
  STA object_x, X
  RTS
:
  ; nudge left
  SEC
  LDA object_sx, X
  SBC #<(JUMP_NUDGE)
  STA object_sx, X
  LDA object_x, X
  SBC #>(JUMP_NUDGE)
  STA object_x, X
  RTS

round_position_negative:
  LDA collision_y
  AND #$f0
  CLC
  ADC #$10
  SEC
  SBC sprite_hitbox_y1
  STA object_y, X
  LDA #$00
  STA object_sy, X
  ; stopish
  SEC
  ROR object_vy, X
  ROR object_svy, X
  INC object_svy, X
  BNE :+
  INC object_vy, X
:
  RTS
.endproc

.proc inter_object_collision
  ; checks if X-index object collides with any other object
  ; cobbles Y

  STX temp_idx
  LDA object_flags, X
  AND #OBJ_ANIM_MASK
  LSR
  TAY

  CLC
  LDA sprite_hitbox_sx1, Y
  ADC object_sx, X
  STA hitbox_a+Box::sx1
  LDA sprite_hitbox_x1, Y
  ADC object_x, X
  STA hitbox_a+Box::x1

  CLC
  LDA sprite_hitbox_y1, Y
  ADC object_y, X
  STA hitbox_a+Box::y1

  CLC
  LDA sprite_hitbox_sx2, Y
  ADC object_sx, X
  STA hitbox_a+Box::sx2
  LDA sprite_hitbox_x2, Y
  ADC object_x, X
  STA hitbox_a+Box::x2

  CLC
  LDA sprite_hitbox_y2, Y
  ADC object_y, X
  STA hitbox_a+Box::y2

  LDY #0
@loop:
  CPY temp_idx
  BEQ @next

  LDA object_flags, Y
  AND #OBJ_ANIM_MASK
  LSR
  TAX

  CLC
  LDA sprite_hitbox_sx1, X
  ADC object_sx, Y
  STA hitbox_b+Box::sx1
  LDA sprite_hitbox_x1, X
  ADC object_x, Y
  STA hitbox_b+Box::x1

  CLC
  LDA sprite_hitbox_y1, X
  ADC object_y, Y
  STA hitbox_b+Box::y1

  CLC
  LDA sprite_hitbox_sx2, X
  ADC object_sx, Y
  STA hitbox_b+Box::sx2
  LDA sprite_hitbox_x2, X
  ADC object_x, Y
  STA hitbox_b+Box::x2

  CLC
  LDA sprite_hitbox_y2, X
  ADC object_y, Y
  STA hitbox_b+Box::y2

  JSR hitbox_collision
  BEQ @next
  JSR process_x_y_collision
  JMP @end
@next:
  INY
  CPY objects_length
  BNE @loop
@end:
  LDX temp_idx
  RTS
.endproc

.proc process_x_y_collision
  LDX temp_idx
  ; Y = other object index
  LDA temp_dir
  AND #(direction::up | direction::down)
  BEQ @left_right

  LDA object_svy, X
  STA object_svy, Y
  LDA object_vy, X
  STA object_vy, Y
  LDA #$00
  STA object_svy, X
  LDA #$00
  STA object_vy, X

  JMP @collision_fit
@left_right:
  LDA object_svx, X
  STA object_svx, Y
  LDA object_vx, X
  STA object_vx, Y
  LDA #$00
  STA object_svx, X
  LDA #$00
  STA object_vx, X

@collision_fit:
  LDA temp_dir
  CMP #direction::down
  BNE @nopress
  LDA #$10
  STA object_pressed, Y
  STY debug_y
@nopress:

  LDA object_flags, X
  AND #OBJ_ANIM_MASK
  LSR
  TAY
  ; X = object index
  ; Y = sprite hitbox index

  LDA temp_dir
  CMP #direction::up
  BNE :+

  SEC
  LDA hitbox_b+Box::y2
  SBC sprite_hitbox_y1, Y
  STA object_y, X
  CLC
  LDA object_y, X
  ADC #$01
  STA object_y, X

  JMP @rollback
:
  CMP #direction::down
  BNE :+

  LDA object_flags, X
  ORA #OBJ_GROUNDED_FLAG
  STA object_flags, X

  SEC
  LDA hitbox_b+Box::y1
  SBC sprite_hitbox_y2, Y
  STA object_y, X
  SEC
  LDA object_y, X
  SBC #$01
  STA object_y, X

  JMP @rollback
:
  CMP #direction::left
  BNE :+

  SEC
  LDA hitbox_b+Box::sx2
  SBC sprite_hitbox_sx1, Y
  STA object_sx, X
  LDA hitbox_b+Box::x2
  SBC sprite_hitbox_x1, Y
  STA object_x, X
  CLC
  LDA object_sx, X
  ADC #$80
  STA object_sx, X
  LDA object_x, X
  ADC #$00
  STA object_x, X

  JMP @rollback
:
  ; CMP #direction::right
  ; BNE :+

  SEC
  LDA hitbox_b+Box::sx1
  SBC sprite_hitbox_sx2, Y
  STA object_sx, X
  LDA hitbox_b+Box::x1
  SBC sprite_hitbox_x2, Y
  STA object_x, X
  SEC
  LDA object_sx, X
  SBC #$80
  STA object_sx, X
  LDA object_x, X
  SBC #$00
  STA object_x, X

  JMP @rollback

@rollback:
  RTS
.endproc

.proc bg_matrix_collision
  ; check if collision_X, collision_Y corresponds to a solid block in bg matrix

  save_regs

  LDA collision_x
  STA temp_x
  LDA collision_sx
  STA temp_sx
  LDA collision_y
  STA temp_y

  ; byte coordinate:
  ; 4 * (y / 2^4) + (x' / 2^7)
  ;                 (x / 2^6)
  ; bit coordinate:
  ; (x' / 2^4) % 2^3
  ; (x / 2^3) % 2^3

  ; y part
  ; abcdefgh
  ; 0000abcd
  ; 00abcd00
  LSR temp_y
  LSR temp_y
  LDA temp_y
  AND #%00111100
  STA temp_y

  ; x part => temp_x
  ; abcdefgh
  ; 000000ab

  ; x bit part => temp_sx
  ; abcdefgh
  ; 00000cde

  ROL temp_x ; a | bcd...
  ROL temp_x ; b | cde...a
  ROL temp_x ; c | de...ab

  LDA temp_x
  STA temp_sx ; c | de...ab
  ROL temp_sx ; d | e.....c
  ROL temp_sx ; e | f....cd
  ROL temp_sx ; f | ....cde

  LDA temp_x
  AND #%11
  CLC
  ADC temp_y
  STA temp_x
  TAY

  LDA (bg_matrix_ptr), Y
  STA temp_x

  LDA temp_sx
  AND #%111
  STA temp_sx

@loop:
  BMI @exit
  ROL temp_x
  DEC temp_sx
  LDA temp_sx
  JMP @loop
@exit:
  BCS @collision
  restore_regs
  LDA #0
  RTS
@collision:
  restore_regs
  LDA #1
  RTS
.endproc

.proc update_scroll
  ; move scroll according to robot position

  ; $80 = 1000 0000 (8.0) =
  ;       0100 0000  0000 0000 (9.7)

  LDA object_x
  CMP #%01000000
  BCS @far_from_left

  LDA #%00000000
  STA scroll_x
  STA scroll_sx
  RTS

@far_from_left:
  SEC
  LDA object_sx
  SBC #%00000000
  AND #%10000000
  STA scroll_sx
  LDA object_x
  SBC #%01000000
  STA scroll_x
  CMP #%10000000
  BCC @far_from_right

  LDA #%10000000
  STA scroll_x
  LDA #%00000000
  STA scroll_sx
  RTS

@far_from_right:
  STA scroll_x
  LDA object_sx
  AND #%10000000
  STA scroll_sx

  RTS
.endproc

.proc display_metasprite
  ; input: (addr_ptr) = metasprite pointer
  ;        temp_x and temp_y = screen position for metasprite origin
  ; cobbles X, Y
  LDY #0
  LDX sprite_counter
loop:
  LDA (addr_ptr),Y ; delta x
  CMP #128
  BEQ return
  INY
  CLC
  ADC temp_x

  ;  trying to skip offscreen tiles
  BCC :+
  INY
  INY
  INY
  JMP loop
:

  STA oam_sprites+Sprite::xcoord,X
  LDA (addr_ptr),Y ; delta y
  INY
  SEC
  SBC #$01
  CLC
  ADC temp_y
  STA oam_sprites+Sprite::ycoord,X
  LDA (addr_ptr),Y ; tile
  INY
  STA oam_sprites+Sprite::tile,X
  LDA (addr_ptr),Y ; flags
  INY
  STA oam_sprites+Sprite::flag,X
  .repeat .sizeof(Sprite)
  INX
  .endrepeat
  JMP loop
return:
  STX sprite_counter
  RTS
.endproc

.proc write_string
  LDA PPUSTATUS
  LDA ppu_addr_ptr+1
  STA PPUADDR
  LDA ppu_addr_ptr
  STA PPUADDR
  LDY #$00
@loop:
  LDA (addr_ptr), Y
  CMP #$ff
  BEQ @exit
  STA PPUDATA
  INY
  JMP @loop
@exit:
  RTS
.endproc

.segment "VECTORS"
.addr nmi_handler, reset_handler, irq_handler

.segment "RODATA"

.define game_state_handlers waiting_to_start-1, playing-1, debugging-1, game_over-1

game_state_handlers_l: .lobytes game_state_handlers
game_state_handlers_h: .hibytes game_state_handlers

palettes:
.incbin "../assets/bg-palettes.pal"
.incbin "../assets/sprite-palettes.pal"
debug_palettes:
.incbin "../assets/debug-bg-palletes.pal"
.incbin "../assets/debug-sprite-palettes.pal"

strings:
level_01_1: .byte "ME", $0a, "A", $ff
level_01_2: .byte "JUDE", $ff
level_01_3: .byte "CONSERT", $74, $74, "E", $0a, "O", $ff
level_01_4: .byte "JOGO", $ff
level_01_5: .byte "SEGUR",$03, $0a, $82, $0a, "BOT",$04,"O", $0a, "ESQUERDO", $ff

level_02_1: .byte "ME", $a, "AJUDE", $ff
level_02_2: .byte "TERMINE", $75, "O", $8d, "JOGO", $ff

level_03_1: .byte "SOU", $0a, "UM", $0a, "ESPIRITO", $ff
level_03_2: .byte "DE", $0a, "UM", $0a, "PROJET", $b5, $ff
level_03_3: .byte "ABANDON", $40, "DO", $ff

level_04_1: .byte "SE", $0a, "ALGUEM", $0a, "ME", $0a, "JOGAR", $ff
level_04_2: .byte "ATE", $0a, "O", $0a, "FIM", $ff
level_04_3: .byte "TEREI", $0a, "MEU", $0a, "DESCANSO", $ff

level_05_1: .byte "O", $0a, "FIM", $0a, "SE", $0a, "APROXIMA", $ff
level_05_2: .byte "EU", $0a, "ERA", $0a, "UM", $0a, "PROTOT", $76, "PO", $ff
level_05_3: .byte "FIZER", $80, "M", $0a, "APENAS", $0a, $05, $0a, "FASES", $ff
level_05_4: .byte "POR", $0a, "FAVOR", $0a, "N", $6b, "O", $0a, "DESISTA", $ff

thank_you_1: .byte "OBR", $73, $74, "IGADO", $ff
thank_you_2: .byte "FFFINALM", $ff
thank_you_3: .byte "ENT", $82, $9b,"E  DESCANS", $ff
thank_you_4: .byte "AREI", $ff


.define dialog_ppu_table    $0000, \
                            $218e,      $21b2,      $21c9,      $21f0,      $2227,      $0000, \
                            $2188,      $21c8,      $0000, \
                            $2105,      $2129,      $214b,      $0000, \
                            $21c8,      $21e9,      $2249,      $0000, \
                            $21a5,      $2228,      $22a9,      $22c6,      $0000
.define dialog_string_table $0000, \
                            level_01_1, level_01_2, level_01_3, level_01_4, level_01_5, $0000, \
                            level_02_1, level_02_2, $0000, \
                            level_03_1, level_03_2, level_03_3, $0000, \
                            level_04_1, level_04_2, level_04_3, $0000, \
                            level_05_1, level_05_2, level_05_3, level_05_4, $0000

dialog_ppu_l: .lobytes dialog_ppu_table
dialog_ppu_h: .hibytes dialog_ppu_table
dialog_string_l: .lobytes dialog_string_table
dialog_string_h: .hibytes dialog_string_table

sprites:
.include "../assets/metasprites.s"

; each row = 4 frames of animation for a sprite or a sprite flip
.define anim_sprites_table \
        metasprite_0_data, metasprite_1_data, metasprite_0_data, metasprite_1_data, \
        metasprite_2_data, metasprite_3_data, metasprite_2_data, metasprite_3_data, \
        metasprite_0_data, metasprite_4_data, metasprite_5_data, metasprite_6_data, \
        metasprite_2_data, metasprite_7_data, metasprite_8_data, metasprite_9_data, \
        metasprite_10_data, metasprite_10_data, metasprite_10_data, metasprite_10_data, \
        metasprite_10_data, metasprite_10_data, metasprite_10_data, metasprite_10_data, \
        metasprite_11_data, metasprite_11_data, metasprite_11_data, metasprite_11_data, \
        metasprite_11_data, metasprite_11_data, metasprite_11_data, metasprite_11_data, \
        metasprite_12_data, metasprite_12_data, metasprite_12_data, metasprite_12_data, \
        metasprite_12_data, metasprite_12_data, metasprite_12_data, metasprite_12_data, \
        metasprite_13_data, metasprite_13_data, metasprite_13_data, metasprite_13_data, \
        metasprite_13_data, metasprite_13_data, metasprite_13_data, metasprite_13_data

anim_sprites_l: .lobytes anim_sprites_table
anim_sprites_h: .hibytes anim_sprites_table

.define debug_sprites_table \
        metasprite_14_data, metasprite_15_data, metasprite_16_data, metasprite_17_data, \
        metasprite_18_data, metasprite_19_data, metasprite_20_data, metasprite_21_data, \
        metasprite_22_data, metasprite_23_data, metasprite_24_data, metasprite_25_data, \
        metasprite_26_data, metasprite_27_data, metasprite_28_data, metasprite_29_data, \
        metasprite_30_data, metasprite_31_data, metasprite_32_data, metasprite_33_data, \
        metasprite_34_data, metasprite_35_data, metasprite_36_data, metasprite_37_data, \
        metasprite_38_data, metasprite_39_data, metasprite_40_data, metasprite_41_data, \
        metasprite_42_data, metasprite_43_data, metasprite_44_data, metasprite_45_data, \
        metasprite_46_data, metasprite_47_data, metasprite_48_data, metasprite_49_data, \
        metasprite_50_data, metasprite_51_data, metasprite_52_data, metasprite_53_data, \
        metasprite_54_data, metasprite_55_data, metasprite_56_data, metasprite_57_data, \
        metasprite_58_data, metasprite_59_data, metasprite_60_data, metasprite_61_data, \
        metasprite_62_data, metasprite_63_data, metasprite_64_data, metasprite_65_data, \
        metasprite_66_data, metasprite_67_data, metasprite_68_data, metasprite_69_data

debug_sprites_l: .lobytes debug_sprites_table
debug_sprites_h: .hibytes debug_sprites_table

; hitboxes per anim sprite type

sprite_hitbox_x1:
  .byte $00, $00, $00, $01, $00, $00
sprite_hitbox_sx1:
  .byte $00, $00, $80, $00, $08, $08
sprite_hitbox_y1:
  .byte $00, $00, $00, $00, $00, $03
sprite_hitbox_x2:
  .byte $07, $07, $06, $02, $06, $06
sprite_hitbox_sx2:
  .byte $80, $80, $00, $00, $00, $00
sprite_hitbox_y2:
  .byte $0f, $0f, $0f, $07, $07, $07

.define level_data_pointers level_00_data, level_01_data, level_02_data, level_03_data, \
                            level_04_data, level_05_data
level_data_pointers_l: .lobytes level_data_pointers
level_data_pointers_h: .hibytes level_data_pointers

.define debug_level_data_pointers debug_00_data, debug_01_data, debug_02_data, debug_03_data, \
                                  debug_04_data, debug_05_data
debug_level_data_pointers_l: .lobytes debug_level_data_pointers
debug_level_data_pointers_h: .hibytes debug_level_data_pointers


; level data format:
; left and right nametable pointers
; center of door x, y
; door ppu address
; door open status
; center of glitch x, y
; glitch ppu address
; dialog counter
; object x, y, flags (assume subx = 0) (x = 0 means end of objects)
; bg matrix pointer

level_00_data:
  .word level_00_left_nametable, level_00_right_nametable
  .byte $d4, $48
  .word $2594
  .byte $00
  .byte $00, $00
  .byte $248c
  .byte $00
  .byte $30, $c0, (OBJ_MOVE_FLAG | (sprite_id::robot_idle<<1) )
    .byte button_type::none, $00, $00
  .byte $c0, $c8, sprite_id::button_off<<1
    .byte button_type::respawn_box, $03, $00
  .byte $b8, $68, sprite_id::button_off<<1
    .byte button_type::open_door, $00, $00
  .byte $40, $20, (OBJ_MOVE_FLAG | (sprite_id::box<<1))
    .byte button_type::none, $a0, $20
  .byte $00
  .word level_00_bg_matrix

level_00_left_nametable: .incbin "../assets/nametables/level-00-left.rle"
level_00_right_nametable: .incbin "../assets/nametables/level-00-right.rle"

level_00_bg_matrix:
  .byte %11111111, %11111111, %11111111, %11111111
  .byte %10000000, %00000000, %00000000, %00000001
  .byte %10000000, %00000000, %00000000, %00000001
  .byte %10000000, %00000000, %00000000, %00000001
  .byte %10000000, %00000000, %00000000, %00000001
  .byte %10000000, %00000000, %00001110, %00000001
  .byte %10000000, %00000000, %11000000, %00000001
  .byte %10000000, %00000000, %11000001, %11111001
  .byte %10000000, %00000000, %00000000, %00000011
  .byte %10000000, %00000000, %00000000, %00000001
  .byte %10000001, %11110000, %00000000, %00000101
  .byte %10000000, %00000000, %00000000, %00000001
  .byte %10000000, %00000100, %00000000, %01111001
  .byte %10000000, %00000000, %00000011, %11000001
  .byte %11111111, %11111111, %11111111, %11111111

level_01_data:
  .word level_01_left_nametable, level_01_right_nametable
  .byte $c8, $a8
  .word $2690
  .byte $01
  .byte $34, $a8
  .word $228c
  .byte $01
  .byte $30, $a0, (OBJ_MOVE_FLAG | (sprite_id::robot_idle<<1) )
    .byte button_type::none, $00, $00
  .byte $00
  .word level_01_bg_matrix

level_01_left_nametable: .incbin "../assets/nametables/level-01-left.rle"
level_01_right_nametable: .incbin "../assets/nametables/level-01-right.rle"

level_01_bg_matrix:
  .byte %00000000, %00000000, %00000000, %00000000
  .byte %00000000, %00000000, %00000000, %00000000
  .byte %00111111, %11111111, %11111111, %11110000
  .byte %00100000, %00000000, %00000000, %00010000
  .byte %00100000, %00000000, %00000000, %00010000
  .byte %00100000, %00000000, %00000000, %00010000
  .byte %00100000, %00000000, %00000000, %00010000
  .byte %00100000, %00000000, %00000000, %00010000
  .byte %00100000, %00000000, %00000000, %00010000
  .byte %00100000, %00000000, %00000000, %00010000
  .byte %00100000, %00000000, %00000000, %00010000
  .byte %00111111, %11111111, %11111111, %11110000
  .byte %00000000, %00000000, %00000000, %00000000
  .byte %00000000, %00000000, %00000000, %00000000
  .byte %00000000, %00000000, %00000000, %00000000

level_02_data:
  .word level_02_left_nametable, level_02_right_nametable
  .byte $dc, $98
  .word $2656
  .byte $01
  .byte $1c, $a8
  .word $2286
  .byte $07
  .byte $30, $a0, (OBJ_MOVE_FLAG | (sprite_id::robot_idle<<1) )
    .byte button_type::none, $00, $00
  .byte $00
  .word level_02_bg_matrix

level_02_left_nametable: .incbin "../assets/nametables/level-02-left.rle"
level_02_right_nametable: .incbin "../assets/nametables/level-02-right.rle"

level_02_bg_matrix:
  .byte %00000000, %00000000, %00000000, %00000000
  .byte %00000000, %00000000, %00000000, %00000000
  .byte %00000000, %00111111, %11110000, %00111110
  .byte %00000000, %00100000, %00010000, %00100010
  .byte %00111111, %11100000, %00011111, %11100010
  .byte %00100000, %00000000, %00000000, %00000010
  .byte %00100000, %00000000, %00000000, %00000010
  .byte %00100000, %00000000, %00000000, %00000010
  .byte %00100000, %00000000, %00000000, %00000010
  .byte %00100000, %00000110, %00000011, %00000110
  .byte %00100000, %00011110, %00011111, %11111100
  .byte %00111111, %11110011, %11110000, %00000000
  .byte %00000000, %00000000, %00000000, %00000000
  .byte %00000000, %00000000, %00000000, %00000000
  .byte %00000000, %00000000, %00000000, %00000000

level_03_data:
  .word level_03_left_nametable, level_03_right_nametable
  .byte $bc, $68
  .word $258e
  .byte $00
  .byte $2c, $48
  .word $210a
  .byte $0a
  .byte $48, $60, (OBJ_MOVE_FLAG | (sprite_id::robot_idle<<1) )
    .byte button_type::none, $00, $00
  .byte $38, $a8, sprite_id::button_off<<1
    .byte button_type::open_door, $00, $00
  .byte $38, $80, (OBJ_MOVE_FLAG | (sprite_id::box<<1))
    .byte button_type::none, $38, $80
  .byte $00
  .word level_03_bg_matrix

level_03_left_nametable: .incbin "../assets/nametables/level-03-left.rle"
level_03_right_nametable: .incbin "../assets/nametables/level-03-right.rle"

level_03_bg_matrix:
  .byte %00000000, %00000000, %00000000, %00000000
  .byte %00000000, %00000000, %00000000, %00000000
  .byte %00111111, %11111111, %11111111, %11000000
  .byte %00100000, %00000000, %00000000, %01000000
  .byte %00100000, %00000000, %00000000, %01000000
  .byte %00100000, %00000000, %00000000, %01000000
  .byte %00100000, %00000000, %00000000, %01000000
  .byte %00111111, %11111111, %11111111, %11000000
  .byte %00001000, %00100000, %00000000, %00000000
  .byte %00001000, %00100000, %00000000, %00000000
  .byte %00001000, %00100000, %00000000, %00000000
  .byte %00001111, %11100000, %00000000, %00000000
  .byte %00000000, %00000000, %00000000, %00000000
  .byte %00000000, %00000000, %00000000, %00000000
  .byte %00000000, %00000000, %00000000, %00000000

level_04_data:
  .word level_04_left_nametable, level_04_right_nametable
  .byte $a4, $48
  .word $2508
  .byte $00
  .byte $d4, $38
  .word $24d4
  .byte $0e
  .byte $38, $60, (OBJ_MOVE_FLAG | (sprite_id::robot_idle<<1) )
    .byte button_type::none, $00, $00
  .byte $40, $60, (OBJ_MOVE_FLAG | (sprite_id::box<<1))
    .byte button_type::none, $78, $50
  .byte $60, $88, sprite_id::button_off<<1
    .byte button_type::respawn_box, $01, $00
  .byte $a0, $98, sprite_id::button_off<<1
    .byte button_type::open_door, $00, $00
  .byte $00
  .word level_04_bg_matrix

level_04_left_nametable: .incbin "../assets/nametables/level-04-left.rle"
level_04_right_nametable: .incbin "../assets/nametables/level-04-right.rle"

level_04_bg_matrix:
  .byte %00000000, %00000000, %00000000, %00000000
  .byte %00000000, %00000000, %00000000, %00000000
  .byte %01111111, %11111111, %11111111, %11110000
  .byte %01000000, %00000000, %00000000, %00011100
  .byte %01000000, %00000000, %00000000, %00000111
  .byte %01000000, %00000000, %00011100, %00000001
  .byte %01000100, %00100000, %00000001, %11000001
  .byte %01000111, %11100000, %00000000, %00000001
  .byte %01000000, %00000000, %00000000, %00011001
  .byte %01000000, %00001111, %11100111, %00000001
  .byte %01000000, %00000000, %00111111, %11111111
  .byte %01111111, %11111111, %11111110, %00001111
  .byte %00000000, %00000000, %00000000, %00000000
  .byte %00000000, %00000000, %00000000, %00000000
  .byte %00000000, %00000000, %00000000, %00000000

level_05_data:
  .word level_05_left_nametable, level_05_right_nametable
  .byte $14, $38
  .word $20c4
  .byte $00
  .byte $00, $00
  .word $0000
  .byte $12
  .byte $20, $b0, (OBJ_MOVE_FLAG | (sprite_id::robot_idle<<1) )
    .byte button_type::none, $00, $00
  .byte $b0, $20, (OBJ_MOVE_FLAG | (sprite_id::box<<1))
    .byte button_type::none, $b0, $20
  .byte $b0, $b8, sprite_id::button_off<<1
    .byte button_type::respawn_box, $01, $00
  .byte $e0, $68, sprite_id::button_off<<1
    .byte button_type::open_door, $00, $00
  .byte $00
  .word level_05_bg_matrix

level_05_left_nametable: .incbin "../assets/nametables/level-05-left.rle"
level_05_right_nametable: .incbin "../assets/nametables/level-05-right.rle"

level_05_bg_matrix:
  .byte %00000000, %00000000, %00000000, %00000000
  .byte %11111111, %11111111, %11111111, %11111111
  .byte %10000000, %00000000, %00000000, %00000001
  .byte %10000000, %00000000, %00000000, %00000001
  .byte %11111000, %00000000, %00000000, %00000001
  .byte %10000000, %00011100, %00011101, %11100111
  .byte %10000001, %10000001, %00000000, %00000101
  .byte %10000000, %00000001, %11000000, %00011101
  .byte %10000000, %00000000, %00000000, %00000001
  .byte %10000000, %00000000, %00000001, %11000001
  .byte %10000000, %00000000, %00000000, %00000001
  .byte %10000000, %00000000, %00000000, %00000001
  .byte %11111111, %11111111, %11111111, %11111111
  .byte %00000000, %00000000, %00000000, %00000000
  .byte %00000000, %00000000, %00000000, %00000000


; debug level data format:
; - nametable pointer
; - for each piece:
;   - initial x, y
;   - target x, y
;   - piece index / orientation (orientation = 2 lsbits from index)
;   (x = 0 means end of pieces)

debug_00_data:
debug_05_data:

debug_01_data:
  .word debug_01_nametable
  .byte $83, $6b, $38, $20, ($00 << 2 | %10)
  .byte $12, $34, $58, $78, ($01 << 2 | %01)
  .byte $00

debug_01_nametable: .incbin "../assets/nametables/debug-01.rle"

debug_02_data:
  .word debug_02_nametable
  .byte $23, $6b, $50, $90, ($02 << 2 | %10)  
  .byte $47, $24, $30, $50, ($03 << 2 | %10)  
  .byte $a3, $db, $50, $40, ($04 << 2 | %10)
  .byte $00

debug_02_nametable: .incbin "../assets/nametables/debug-02.rle"

debug_03_data:
  .word debug_03_nametable
  .byte $30, $98, $60, $50, ($05 << 2 | %10)
  .byte $a0, $98, $60, $50, ($06 << 2 | %11)
  .byte $40, $50, $60, $98, ($07 << 2 | %01)
  .byte $90, $50, $68, $a0, ($08 << 2 | %10)
  .byte $00

debug_03_nametable: .incbin "../assets/nametables/debug-03.rle"

debug_04_data:
  .word debug_04_nametable
  .byte $30, $30, $60, $78, ($09 << 2 | %10)
  .byte $80, $28, $50, $78, ($0a << 2 | %00)
  .byte $90, $70, $80, $28, ($0b << 2 | %01)
  .byte $30, $40, $a0, $28, ($0c << 2 | %01)
  .byte $a0, $28, $30, $40, ($0d << 2 | %11)
  .byte $00

debug_04_nametable: .incbin "../assets/nametables/debug-04.rle"

nametable_title: .incbin "../assets/nametables/title.rle"
nametable_credits: .incbin "../assets/nametables/credits.rle"
nametable_main: .incbin "../assets/nametables/main.rle"
nametable_game_over: .incbin "../assets/nametables/game_over.rle"

.segment "CHR"
.incbin "../assets/chr/main-bg-2k-1.chr"
.incbin "../assets/chr/main-bg-2k-2.chr"
.incbin "../assets/chr/sprites.chr"
.incbin "../assets/chr/debug-main-bg-4k.chr"
.incbin "../assets/chr/debug-sprites-4k.chr"

; 1k blocks
;  0 : bg
;  1 : bg
;  2 : bg
;  3 : bg
;  4 : sp
;  5 : sp
;  6 : sp
;  7 : sp
;  8 : d bg
;  9 : d bg
; 10 : d bg
; 11 : d bg
; 12 : d sp
; 13 : d sp
; 14 : d sp
; 15 : d sp
;
; banks
; 0 : 2k bg
; 1 : 2k bg
; 2 : 1k sp
; 3 : 1k sp
; 4 : 1k sp
; 5 : 1k sp

