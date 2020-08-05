.segment "HEADER"
.byte "NES", $1a ; Magic string that always begins an iNES header

.byte $02        ; Number of 16KB PRG-ROM banks (LSB)
.byte $01        ; Number of 8KB CHR-ROM banks (LSB)

; NES 2.0 Header info from NESDev wiki: https://wiki.nesdev.com/w/index.php/NES_2.0

.byte %01000001  ; Flags 6
;      NNNNFTBM
;      |||||||+--- Hard-wired nametable mirroring type
;      |||||||      0: Horizontal or mapper-controlled
;      |||||||      1: Vertical
;      ||||||+---- "Battery" and other non-volatile memory
;      ||||||       0: Not present
;      ||||||       1: Present
;      |||||+---- 512-byte Trainer
;      |||||       0: Not present
;      |||||       1: Present between Header and PRG-ROM data
;      ||||+----- Hard-wired four-screen mode
;      ||||        0: No
;      ||||        1: Yes
;      ++++------ Mapper Number D0..D3

.byte %00001000  ; Flags 7
;      NNNN10TT
;      ||||||++-- Console type
;      ||||||      0: Nintendo Entertainment System/Family Computer
;      ||||||      1: Nintendo Vs. System
;      ||||||      2: Nintendo Playchoice 10
;      ||||||      3: Extended Console Type
;      ||||++---- NES 2.0 identifier
;      ++++------ Mapper Number D4..D7

.byte %00000000  ; Mapper MSB/Submapper
;      SSSSNNNN
;      ||||++++- Mapper number D8..D11
;      ++++------ Submapper number

.byte %00000000  ; PRG-ROM/CHR-ROM size MSB
;      CCCCPPPP
;      ||||++++-- PRG-ROM size MSB
;      ++++------ CHR-ROM size MSB

.byte %00000000  ; PRG-RAM/EEPROM size
;      ppppPPPP
;      ||||++++-- PRG-RAM (volatile) shift count
;      ++++------ PRG-NVRAM/EEPROM (non-volatile) shift count
;    If the shift count is zero, there is no PRG-(NV)RAM.
;    If the shift count is non-zero, the actual size is
;    "64 << shift count" bytes, i.e. 8192 bytes for a shift count of 7.

.byte %00000000  ; CHR-RAM size
;      ccccCCCC
;      ||||++++-- CHR-RAM size (volatile) shift count
;      ++++------ CHR-NVRAM size (non-volatile) shift count
;    If the shift count is zero, there is no CHR-(NV)RAM.
;    If the shift count is non-zero, the actual size is
;    "64 << shift count" bytes, i.e. 8192 bytes for a shift count of 7.

.byte %00000000  ; CPU/PPU Timing
;      ......VV
;            ++- CPU/PPU timing mode
;                 0: RP2C02 ("NTSC NES")
;                 1: RP2C07 ("Licensed PAL NES")
;                 2: Multiple-region
;                 3: UMC 6527P ("Dendy")

.byte $0 ; only used for Vs. System Type and Extended Console Type (cf. Flags 7)
.byte $0 ; miscelaneous ROMs
.byte $0 ; default expansion device