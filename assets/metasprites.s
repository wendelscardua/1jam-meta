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

	.byte   8,  0,$00,0|OAM_FLIP_H
	.byte   0,  0,$01,0|OAM_FLIP_H
	.byte   8,  8,$04,0|OAM_FLIP_H
	.byte   0,  8,$05,0|OAM_FLIP_H
	.byte 128

metasprite_3_data:

	.byte   8,  0,$02,0|OAM_FLIP_H
	.byte   0,  0,$03,0|OAM_FLIP_H
	.byte   8,  8,$06,0|OAM_FLIP_H
	.byte   0,  8,$07,0|OAM_FLIP_H
	.byte 128

metasprite_4_data:

	.byte   0,  0,$02,0
	.byte   8,  0,$03,0
	.byte   0,  8,$08,0
	.byte   8,  8,$09,0
	.byte 128

metasprite_5_data:

	.byte   0,  0,$00,0
	.byte   8,  0,$01,0
	.byte   0,  8,$0a,0
	.byte   8,  8,$0b,0
	.byte 128

metasprite_6_data:

	.byte   0,  0,$02,0
	.byte   8,  0,$03,0
	.byte   0,  8,$0c,0
	.byte   8,  8,$0d,0
	.byte 128

metasprite_7_data:

	.byte   8,  0,$02,0|OAM_FLIP_H
	.byte   0,  0,$03,0|OAM_FLIP_H
	.byte   8,  8,$08,0|OAM_FLIP_H
	.byte   0,  8,$09,0|OAM_FLIP_H
	.byte 128

metasprite_8_data:

	.byte   8,  0,$00,0|OAM_FLIP_H
	.byte   0,  0,$01,0|OAM_FLIP_H
	.byte   8,  8,$0a,0|OAM_FLIP_H
	.byte   0,  8,$0b,0|OAM_FLIP_H
	.byte 128

metasprite_9_data:

	.byte   8,  0,$02,0|OAM_FLIP_H
	.byte   0,  0,$03,0|OAM_FLIP_H
	.byte   8,  8,$0c,0|OAM_FLIP_H
	.byte   0,  8,$0d,0|OAM_FLIP_H
	.byte 128

metasprite_10_data:

	.byte   0,  0,$10,1
	.byte   0,  8,$20,1
	.byte   8,  0,$10,1|OAM_FLIP_H
	.byte   8,  8,$20,1|OAM_FLIP_H
	.byte 128

metasprite_11_data:

	.byte   0,  0,$12,2
	.byte 128

metasprite_12_data:

	.byte   0,  0,$13,3
	.byte   8,  0,$13,3|OAM_FLIP_H
	.byte 128

metasprite_13_data:

	.byte   0,  0,$14,3
	.byte   8,  0,$14,3|OAM_FLIP_H
	.byte 128

metasprite_14_data:

	.byte   8,  8,$52,0
	.byte   8, 16,$52,0
	.byte   8, 24,$41,0
	.byte  16, 24,$13,0
	.byte  24, 24,$4f,0
	.byte  24, 32,$0d,0
	.byte 128

metasprite_15_data:

	.byte  24,  8,$72,0
	.byte  16,  8,$72,0
	.byte   8,  8,$61,0
	.byte   8, 16,$13,0
	.byte   8, 24,$6f,0
	.byte   0, 24,$2d,0
	.byte 128

metasprite_16_data:

	.byte  24, 24,$52,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  24, 16,$52,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  24,  8,$41,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  16,  8,$13,0
	.byte   8,  8,$4f,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   8,  0,$0d,0|OAM_FLIP_H|OAM_FLIP_V
	.byte 128

metasprite_17_data:

	.byte   8, 24,$72,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  16, 24,$72,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  24, 24,$61,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  24, 16,$13,0
	.byte  24,  8,$6f,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  32,  8,$2d,0|OAM_FLIP_H|OAM_FLIP_V
	.byte 128

metasprite_18_data:

	.byte   8,  0,$4a,0
	.byte   8,  8,$11,0
	.byte  16,  8,$0c,0
	.byte  24,  8,$41,0
	.byte  24, 16,$56,0
	.byte  24, 24,$4f,0
	.byte  24, 32,$54,0
	.byte 128

metasprite_19_data:

	.byte  32,  8,$6a,0
	.byte  24,  8,$31,0
	.byte  24, 16,$2c,0
	.byte  24, 24,$61,0
	.byte  16, 24,$76,0
	.byte   8, 24,$6f,0
	.byte   0, 24,$74,0
	.byte 128

metasprite_20_data:

	.byte  24, 32,$4a,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  24, 24,$11,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  16, 24,$0c,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   8, 24,$41,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   8, 16,$56,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   8,  8,$4f,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   8,  0,$54,0|OAM_FLIP_H|OAM_FLIP_V
	.byte 128

metasprite_21_data:

	.byte   0, 24,$6a,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   8, 24,$31,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   8, 16,$2c,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   8,  8,$61,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  16,  8,$76,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  24,  8,$6f,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  32,  8,$74,0|OAM_FLIP_H
	.byte 128

metasprite_pointers:

	.word metasprite_0_data
	.word metasprite_1_data
	.word metasprite_2_data
	.word metasprite_3_data
	.word metasprite_4_data
	.word metasprite_5_data
	.word metasprite_6_data
	.word metasprite_7_data
	.word metasprite_8_data
	.word metasprite_9_data
	.word metasprite_10_data
	.word metasprite_11_data
	.word metasprite_12_data
	.word metasprite_13_data
	.word metasprite_14_data
	.word metasprite_15_data
	.word metasprite_16_data
	.word metasprite_17_data
	.word metasprite_18_data
	.word metasprite_19_data
	.word metasprite_20_data
	.word metasprite_21_data

