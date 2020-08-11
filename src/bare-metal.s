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
.endenum

.enum sfx
.endenum

.macro SFX effect, channel
  save_regs
  LDA #sfx::effect
  LDX #.ident ( .concat( "FT_SFX_", .string(channel) ) )
  JSR FamiToneSfxPlay
  restore_regs
.endmacro

.macro PLAY track
  save_regs
  LDA #music_track::track
  JSR FamiToneMusicPlay
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
  game_over
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

temp_x: .res 1
temp_sx: .res 1
temp_y: .res 1

.struct Box
  x1 .byte
  y1 .byte
  x2 .byte
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

anim_offset: .res 1

sprite_counter: .res 1

debug_x: .res 1
debug_y: .res 1
debug_a: .res 1

.segment "BSS"
; non-zp RAM goes here

; object coordinates use subpixels for fractional movement
; (object_y, object_sy) = 8.8 number
; (object_x, object_sx) = 9.7 number (in order to cover two screens)
MAX_OBJECTS = 20
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
; ?maaaaaf
; |||||||+- flip (0 = face right, 1 = face left)
; ||++++++- anim "index" (* 4 + anim frame = actual anim_sprites index)
; |+------- move flag, tells if object moves
; +-------- unused (for now)
OBJ_ANIM_MASK = %00111111
OBJ_MOVE_FLAG = %01000000

MAX_WALLS = 12
walls_length: .res 1
wall_x1: .res MAX_WALLS
wall_y1: .res MAX_WALLS
wall_x2: .res MAX_WALLS
wall_y2: .res MAX_WALLS

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

.proc irq_handler
  RTI
.endproc

.proc nmi_handler
  INC nmis
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

  JSR load_palettes

  JSR load_default_chr

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

  ; JSR go_to_title ; TODO: reenable later
  LDA #0
  STA current_level
  JSR go_to_playing

forever:
  LDA nmis
  CMP old_nmis
  BEQ etc
  STA old_nmis
  ; new frame code
  JSR game_state_handler
  JSR screen_stuff
  JSR FamiToneUpdate
  JSR physics_update
etc:
  JSR rand ; shuffle rng around
  JMP forever
.endproc

.proc screen_stuff
  ; Fix Scroll
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

.proc game_state_handler
  LDX game_state
  LDA game_state_handlers_h, X
  PHA
  LDA game_state_handlers_l, X
  PHA
  RTS
.endproc

.proc go_to_title
  LDA #game_states::waiting_to_start
  STA game_state

  SCREEN_OFF

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

  VBLANK

  SCREEN_ON

  RTS
.endproc

.proc go_to_playing
  LDA #game_states::playing
  STA game_state

  SCREEN_OFF

  ; load left/right nametables
  LDX current_level

  LDA PPUSTATUS
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR

  LDA left_nametable_for_level_l, X
  STA rle_ptr
  LDA left_nametable_for_level_h, X
  STA rle_ptr+1
  JSR unrle

  LDA #$24
  STA PPUADDR
  LDA #$00
  STA PPUADDR

  LDA right_nametable_for_level_l, X
  STA rle_ptr
  LDA right_nametable_for_level_h, X
  STA rle_ptr+1
  JSR unrle

  LDA #$00
  STA scroll_x
  STA scroll_sx

  LDA level_data_pointers_l, X
  STA addr_ptr
  LDA level_data_pointers_h, X
  STA addr_ptr+1

  LDY #0 ; Y iterates over level data
  LDX #0

  ; add robot (robot is always the first object)
  ; TODO read from level data

  LDA (addr_ptr), Y ; read robot x
  INY
  STA object_x, X

  LDA (addr_ptr), Y ; read robot y
  INY
  STA object_y, X

  LDA #%01000000
  STA object_flags, X

  LDA #$0
  STA object_sx, X
  STA object_sy, X
  STA object_vx, X
  STA object_svx, X
  STA object_vy, X
  STA object_svy, X
  INX

  STX objects_length

  LDA (addr_ptr), Y ; read number of walls
  INY
  STA walls_length
  LDX #0
@wall_loop:
  LDA (addr_ptr), Y
  INY
  STA wall_x1, X
  LDA (addr_ptr), Y
  INY
  STA wall_y1, X
  LDA (addr_ptr), Y
  INY
  STA wall_x2, X
  LDA (addr_ptr), Y
  INY
  STA wall_y2, X
  INX
  CPX walls_length
  BNE @wall_loop

  VBLANK

  SCREEN_ON

  RTS
.endproc

.proc go_to_game_over
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

  LDA #$22
  STA PPUADDR
  LDA #$40
  STA PPUADDR

  LDA #<nametable_game_over
  STA rle_ptr
  LDA #>nametable_game_over
  STA rle_ptr+1
  JSR unrle

  VBLANK

  SCREEN_ON

  RTS
.endproc

.proc waiting_to_start
  JSR readjoy
  LDA pressed_buttons
  AND #BUTTON_START
  BEQ :+
  JSR go_to_playing
:
  RTS
.endproc

.proc game_over
  JSR readjoy
  LDA pressed_buttons
  AND #BUTTON_START
  BEQ :+
  JSR go_to_title
:
  RTS
.endproc

.proc platforming_input
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

  ; decay positive speed

  CLC
  LDA object_svx
  ADC #%11100000
  STA object_svx
  LDA object_vx
  ADC #%11111111
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

  ; decay negative speed
  CLC
  LDA object_svx
  ADC #%00100000
  STA object_svx
  LDA object_vx
  ADC #%00000000
  STA object_vx
  BMI @endspeed
  LDA #$0
  STA object_vx
  STA object_svx

  ; JMP @endspeed
@endspeed:

  JSR readjoy
  LDA buttons
  AND #BUTTON_LEFT
  BEQ :+

  LDA object_vx
  CMP #%11111100
  BEQ :+

  CLC
  LDA object_svx
  ADC #%11010000
  STA object_svx

  LDA object_vx
  ADC #%11111111
  STA object_vx

:
  LDA buttons
  AND #BUTTON_RIGHT
  BEQ :+

  LDA object_vx
  CMP #%00000100
  BCS :+

  CLC
  LDA object_svx
  ADC #%00110000
  STA object_svx

  LDA object_vx
  ADC #%00000000
  STA object_vx

:
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

  RTS
.endproc

.proc prepare_object_hitbox
  ; fills hitbox buffer with current (indexed by Y) object hitbox
  ; cobbles X, A
  LDA object_flags, Y
  AND #OBJ_ANIM_MASK
  LSR
  TAX

  CLC
  LDA sprite_hitbox_x1, X
  ADC object_x, Y
  STA hitbox_a+Box::x1

  CLC
  LDA sprite_hitbox_y1, X
  ADC object_y, Y
  STA hitbox_a+Box::y1

  CLC
  LDA sprite_hitbox_x2, X
  ADC object_x, Y
  STA hitbox_a+Box::x2

  CLC
  LDA sprite_hitbox_y2, X
  ADC object_y, Y
  STA hitbox_a+Box::y2
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
  BCS :+
  LDA #$00
  RTS
:
  CLC
  LDA hitbox_b+Box::x2
  CMP hitbox_a+Box::x1
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

.proc check_wall_collision
  ; returns 1 in A if hitbox_a intersects with any wall
  LDA #0
  LDX walls_length
  DEX
loop:
  LDA wall_x1, X
  STA hitbox_b+Box::x1
  LDA wall_y1, X
  STA hitbox_b+Box::y1
  LDA wall_x2, X
  STA hitbox_b+Box::x2
  LDA wall_y2, X
  STA hitbox_b+Box::y2
  JSR hitbox_collision
  BEQ next
  LDA #1
  RTS
next:
  DEX
  BPL loop
  LDA #$00
  RTS
.endproc

.proc physics_update
  LDA game_state
  CMP #game_states::playing
  BEQ :+
  RTS
:
  LDY #0
@loop:
  LDA object_flags, Y
  AND #OBJ_MOVE_FLAG
  BEQ @next

  ; (x:sx, y:sy) += (vx:svx, vy:svy)
  CLC
  LDA object_sx, Y
  ADC object_svx, Y
  STA object_sx, Y
  LDA object_x, Y
  ADC object_vx, Y
  STA object_x, Y

  CLC
  LDA object_sy, Y
  ADC object_svy, Y
  STA object_sy, Y
  LDA object_y, Y
  ADC object_vy, Y
  STA object_y, Y

@next:
  INY
  CPY objects_length
  BNE @loop

  JSR update_scroll

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

.segment "VECTORS"
.addr nmi_handler, reset_handler, irq_handler

.segment "RODATA"

game_state_handlers_l:
  .byte <(waiting_to_start-1)
  .byte <(playing-1)
  .byte <(game_over-1)

game_state_handlers_h:
  .byte >(waiting_to_start-1)
  .byte >(playing-1)
  .byte >(game_over-1)

palettes:
.incbin "../assets/bg-palettes.pal"
.incbin "../assets/sprite-palettes.pal"

sprites:
.include "../assets/metasprites.s"

anim_sprites_l:
  ; 00: robot, idle
  .byte <metasprite_0_data
  .byte <metasprite_1_data
  .byte <metasprite_0_data
  .byte <metasprite_1_data
  ; 00': robot, idle, flip
  .byte <metasprite_2_data
  .byte <metasprite_3_data
  .byte <metasprite_2_data
  .byte <metasprite_3_data
  ; 01: robot, walk
  .byte <metasprite_0_data
  .byte <metasprite_4_data
  .byte <metasprite_5_data
  .byte <metasprite_6_data
  ; 01': robot, walk, flip
  .byte <metasprite_2_data
  .byte <metasprite_7_data
  .byte <metasprite_8_data
  .byte <metasprite_9_data

anim_sprites_h:
  ; 00: robot, idle
  .byte >metasprite_0_data
  .byte >metasprite_1_data
  .byte >metasprite_0_data
  .byte >metasprite_1_data
  ; 00': robot, idle, flip
  .byte >metasprite_2_data
  .byte >metasprite_3_data
  .byte >metasprite_2_data
  .byte >metasprite_3_data
  ; 01: robot, walk
  .byte >metasprite_0_data
  .byte >metasprite_4_data
  .byte >metasprite_5_data
  .byte >metasprite_6_data
  ; 01': robot, walk, flip
  .byte >metasprite_2_data
  .byte >metasprite_7_data
  .byte >metasprite_8_data
  .byte >metasprite_9_data

; hitboxes per anim sprite type

sprite_hitbox_x1:
  .byte $00, $00
sprite_hitbox_y1:
  .byte $00, $00
sprite_hitbox_x2:
  .byte $0f, $0f
sprite_hitbox_y2:
  .byte $0f, $0f

left_nametable_for_level_l:
  .byte <(nametable_level_demo_left)
left_nametable_for_level_h:
  .byte >(nametable_level_demo_left)

right_nametable_for_level_l:
  .byte <(nametable_level_demo_right)
right_nametable_for_level_h:
  .byte >(nametable_level_demo_right)

level_data_pointers_l:
  .byte <(level_0_data)
level_data_pointers_h:
  .byte >(level_0_data)

; level data format
; robot x, robot y (assume subx = 0)
; number of walls
; (for each wall) x1 y1 x2 y2


level_0_data:
  .byte $30, $a0


nametable_title: .incbin "../assets/nametables/title.rle"
nametable_main: .incbin "../assets/nametables/main.rle"
nametable_game_over: .incbin "../assets/nametables/game_over.rle"
nametable_level_demo_left: .incbin "../assets/nametables/template-double-level-left.rle"
nametable_level_demo_right: .incbin "../assets/nametables/template-double-level-right.rle"

.segment "CHR"
.incbin "../assets/chr/main-bg-2k-1.chr"
.incbin "../assets/chr/main-bg-2k-2.chr"
.incbin "../assets/chr/sprites.chr"
