	metasprite_0_data:

	.byte   0,  0,$00,0
	.byte   8,  0,$01,0
	.byte   0,  8,$04,0
	.byte   8,  8,$05,0
	.byte 128

	metasprite_1_data:

	.byte   0,  0,$02,0
	.byte   8,  0,$03,0
	.byte   0,  8,$06,0
	.byte   8,  8,$07,0
	.byte 128

	metasprite_2_data:

	.byte   0,  0,$02,0
	.byte   8,  0,$19,0
	.byte   8,  0,$03,0
	.byte   0,  8,$08,0
	.byte   8,  8,$09,0
	.byte 128

	metasprite_3_data:

	.byte   0,  0,$00,0
	.byte   8,  0,$01,0
	.byte   0,  8,$0a,0
	.byte   8,  8,$0b,0
	.byte 128

	metasprite_4_data:

	.byte   0,  0,$02,0
	.byte   8,  0,$03,0
	.byte   0,  8,$0c,0
	.byte   8,  8,$0d,0
	.byte 128

	metasprite_pointers:

	.word metasprite_0_data
	.word metasprite_1_data
	.word metasprite_2_data
	.word metasprite_3_data
	.word metasprite_4_data
