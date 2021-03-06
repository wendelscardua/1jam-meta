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

metasprite_22_data:

	.byte   0,  0,$0a,0
	.byte   8,  0,$0b,0
	.byte   8,  8,$13,0
	.byte   8, 16,$10,0
	.byte  16, 16,$11,0
	.byte   0, 16,$0d,0
	.byte 128

metasprite_23_data:

	.byte  16,  0,$2a,0
	.byte  16,  8,$2b,0
	.byte   8,  8,$13,0
	.byte   0,  8,$30,0
	.byte   0, 16,$31,0
	.byte   0,  0,$2d,0
	.byte 128

metasprite_24_data:

	.byte  16, 16,$0a,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   8, 16,$0b,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   8,  8,$13,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   8,  0,$10,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   0,  0,$11,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  16,  0,$0d,0|OAM_FLIP_H|OAM_FLIP_V
	.byte 128

metasprite_25_data:

	.byte   0, 16,$2a,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   0,  8,$2b,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   8,  8,$13,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  16,  8,$30,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  16,  0,$31,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  16, 16,$2d,0|OAM_FLIP_H|OAM_FLIP_V
	.byte 128

metasprite_26_data:

	.byte   0,  0,$53,0
	.byte   8,  0,$54,0
	.byte   8,  8,$44,0
	.byte   8, 16,$54,0
	.byte  16, 16,$41,0
	.byte 128

metasprite_27_data:

	.byte  16,  0,$73,0
	.byte  16,  8,$74,0
	.byte   8,  8,$64,0
	.byte   0,  8,$74,0
	.byte   0, 16,$61,0
	.byte 128

metasprite_28_data:

	.byte  16, 16,$53,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   8, 16,$54,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   8,  8,$44,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   8,  0,$54,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   0,  0,$41,0|OAM_FLIP_H|OAM_FLIP_V
	.byte 128

metasprite_29_data:

	.byte   0, 16,$73,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   0,  8,$74,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   8,  8,$64,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  16,  8,$74,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  16,  0,$61,0|OAM_FLIP_H|OAM_FLIP_V
	.byte 128

metasprite_30_data:

	.byte   0,  0,$4f,0
	.byte  16,  0,$4a,0
	.byte   0,  8,$0d,0
	.byte   8,  8,$11,0
	.byte  16,  8,$14,0
	.byte   0, 16,$4f,0
	.byte 128

metasprite_31_data:

	.byte  16,  0,$6f,0
	.byte  16, 16,$6a,0
	.byte   8,  0,$2d,0
	.byte   8,  8,$31,0
	.byte   8, 16,$34,0
	.byte   0,  0,$6f,0
	.byte 128

metasprite_32_data:

	.byte  16, 16,$4f,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   0, 16,$4a,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  16,  8,$0d,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   8,  8,$11,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   0,  8,$14,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  16,  0,$4f,0|OAM_FLIP_H|OAM_FLIP_V
	.byte 128

metasprite_33_data:

	.byte   0, 16,$6f,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   0,  0,$6a,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   8, 16,$2d,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   8,  8,$31,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   8,  0,$34,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  16, 16,$6f,0|OAM_FLIP_H|OAM_FLIP_V
	.byte 128

metasprite_34_data:

	.byte  24,  0,$4c,0
	.byte  24,  8,$4a,0
	.byte  24, 16,$0e,0
	.byte  24, 24,$4c,0
	.byte  16, 24,$46,0
	.byte   8, 24,$0e,0
	.byte   0, 24,$4a,0
	.byte 128

metasprite_35_data:

	.byte  24, 24,$6c,0
	.byte  16, 24,$6a,0
	.byte   8, 24,$2e,0
	.byte   0, 24,$6c,0
	.byte   0, 16,$66,0
	.byte   0,  8,$2e,0
	.byte   0,  0,$6a,0
	.byte 128

metasprite_36_data:

	.byte   0, 24,$4c,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   0, 16,$4a,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   0,  8,$0e,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   0,  0,$4c,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   8,  0,$46,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  16,  0,$0e,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  24,  0,$4a,0|OAM_FLIP_H|OAM_FLIP_V
	.byte 128

metasprite_37_data:

	.byte   0,  0,$6c,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   8,  0,$6a,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  16,  0,$2e,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  24,  0,$6c,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  24,  8,$66,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  24, 16,$2e,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  24, 24,$6a,0|OAM_FLIP_H|OAM_FLIP_V
	.byte 128

metasprite_38_data:

	.byte   0,  0,$4a,0
	.byte   8,  0,$0e,0
	.byte  16,  0,$46,0
	.byte   0,  8,$14,0
	.byte   8,  8,$4f,0
	.byte   0, 16,$42,0
	.byte 128

metasprite_39_data:

	.byte  16,  0,$6a,0
	.byte  16,  8,$2e,0
	.byte  16, 16,$66,0
	.byte   8,  0,$34,0
	.byte   8,  8,$6f,0
	.byte   0,  0,$62,0
	.byte 128

metasprite_40_data:

	.byte  16, 16,$4a,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   8, 16,$0e,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   0, 16,$46,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  16,  8,$14,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   8,  8,$4f,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  16,  0,$42,0|OAM_FLIP_H|OAM_FLIP_V
	.byte 128

metasprite_41_data:

	.byte   0, 16,$6a,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   0,  8,$2e,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   0,  0,$66,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   8, 16,$34,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   8,  8,$6f,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  16, 16,$62,0|OAM_FLIP_H|OAM_FLIP_V
	.byte 128

metasprite_42_data:

	.byte   0,  0,$4a,0
	.byte   8,  0,$0e,0
	.byte  16,  0,$46,0
	.byte  24,  0,$4c,0
	.byte   0,  8,$14,0
	.byte   0, 16,$42,0
	.byte   0, 24,$4a,0
	.byte 128

metasprite_43_data:

	.byte  24,  0,$6a,0
	.byte  24,  8,$2e,0
	.byte  24, 16,$66,0
	.byte  24, 24,$6c,0
	.byte  16,  0,$34,0
	.byte   8,  0,$62,0
	.byte   0,  0,$6a,0
	.byte 128

metasprite_44_data:

	.byte  24, 24,$4a,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  16, 24,$0e,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   8, 24,$46,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   0, 24,$4c,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  24, 16,$14,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  24,  8,$42,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  24,  0,$4a,0|OAM_FLIP_H|OAM_FLIP_V
	.byte 128

metasprite_45_data:

	.byte   0, 24,$6a,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   0, 16,$2e,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   0,  8,$66,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   0,  0,$6c,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   8, 24,$34,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  16, 24,$62,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  24, 24,$6a,0|OAM_FLIP_H|OAM_FLIP_V
	.byte 128

metasprite_46_data:

	.byte  16,  0,$4a,0
	.byte  16,  8,$0e,0
	.byte  16, 16,$4c,0
	.byte   8, 16,$46,0
	.byte   0, 16,$0e,0
	.byte 128

metasprite_47_data:

	.byte  16, 16,$6a,0
	.byte   8, 16,$2e,0
	.byte   0, 16,$6c,0
	.byte   0,  8,$66,0
	.byte   0,  0,$2e,0
	.byte 128

metasprite_48_data:

	.byte   0, 16,$4a,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   0,  8,$0e,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   0,  0,$4c,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   8,  0,$46,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  16,  0,$0e,0|OAM_FLIP_H|OAM_FLIP_V
	.byte 128

metasprite_49_data:

	.byte   0,  0,$6a,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   8,  0,$2e,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  16,  0,$6c,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  16,  8,$66,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  16, 16,$2e,0|OAM_FLIP_H|OAM_FLIP_V
	.byte 128

metasprite_50_data:

	.byte   0,  8,$4a,0
	.byte   8,  8,$0e,0
	.byte  16,  8,$58,0
	.byte  24,  8,$15,0
	.byte  24, 16,$54,0
	.byte  24, 24,$15,0
	.byte 128

metasprite_51_data:

	.byte  24,  0,$6a,0
	.byte  24,  8,$2e,0
	.byte  24, 16,$78,0
	.byte  24, 24,$35,0
	.byte  16, 24,$74,0
	.byte   8, 24,$35,0
	.byte 128

metasprite_52_data:

	.byte  32, 24,$4a,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  24, 24,$0e,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  16, 24,$58,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   8, 24,$15,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   8, 16,$54,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   8,  8,$15,0|OAM_FLIP_H|OAM_FLIP_V
	.byte 128

metasprite_53_data:

	.byte   8, 32,$6a,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   8, 24,$2e,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   8, 16,$78,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   8,  8,$35,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  16,  8,$74,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  24,  8,$35,0|OAM_FLIP_H|OAM_FLIP_V
	.byte 128

metasprite_54_data:

	.byte   8,  0,$42,0
	.byte   8,  8,$42,0
	.byte   8, 16,$42,0
	.byte   8, 24,$42,0
	.byte  16, 24,$4a,0
	.byte 128

metasprite_55_data:

	.byte  32,  8,$62,0
	.byte  24,  8,$62,0
	.byte  16,  8,$62,0
	.byte   8,  8,$62,0
	.byte   8, 16,$6a,0
	.byte 128

metasprite_56_data:

	.byte  24, 32,$42,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  24, 24,$42,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  24, 16,$42,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  24,  8,$42,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  16,  8,$4a,0|OAM_FLIP_H|OAM_FLIP_V
	.byte 128

metasprite_57_data:

	.byte   0, 24,$62,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   8, 24,$62,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  16, 24,$62,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  24, 24,$62,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  24, 16,$6a,0|OAM_FLIP_H|OAM_FLIP_V
	.byte 128

metasprite_58_data:

	.byte   8,  0,$4d,0
	.byte   8,  8,$4e,0
	.byte   8, 16,$4e,0
	.byte   8, 24,$4f,0
	.byte  16,  8,$0e,0
	.byte  16, 24,$4f,0
	.byte  24,  8,$42,0
	.byte 128

metasprite_59_data:

	.byte  32,  8,$6d,0
	.byte  24,  8,$6e,0
	.byte  16,  8,$6e,0
	.byte   8,  8,$6f,0
	.byte  24, 16,$2e,0
	.byte   8, 16,$6f,0
	.byte  24, 24,$62,0
	.byte 128

metasprite_60_data:

	.byte  24, 32,$4d,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  24, 24,$4e,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  24, 16,$4e,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  24,  8,$4f,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  16, 24,$0e,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  16,  8,$4f,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   8, 24,$42,0|OAM_FLIP_H|OAM_FLIP_V
	.byte 128

metasprite_61_data:

	.byte   0, 24,$6d,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   8, 24,$6e,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  16, 24,$6e,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  24, 24,$6f,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   8, 16,$2e,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  24, 16,$6f,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   8,  8,$62,0|OAM_FLIP_H|OAM_FLIP_V
	.byte 128

metasprite_62_data:

	.byte   0,  0,$44,0
	.byte   8,  0,$15,0
	.byte  16,  0,$58,0
	.byte   8,  8,$58,0
	.byte   8, 16,$58,0
	.byte 128

metasprite_63_data:

	.byte  16,  0,$64,0
	.byte  16,  8,$35,0
	.byte  16, 16,$78,0
	.byte   8,  8,$78,0
	.byte   0,  8,$78,0
	.byte 128

metasprite_64_data:

	.byte  16, 16,$44,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   8, 16,$15,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   0, 16,$58,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   8,  8,$58,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   8,  0,$58,0|OAM_FLIP_H|OAM_FLIP_V
	.byte 128

metasprite_65_data:

	.byte   0, 16,$64,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   0,  8,$35,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   0,  0,$78,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   8,  8,$78,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  16,  8,$78,0|OAM_FLIP_H|OAM_FLIP_V
	.byte 128

metasprite_66_data:

	.byte   0,  8,$42,0
	.byte   8,  8,$45,0
	.byte  16,  8,$51,0
	.byte  16,  0,$50,0
	.byte  16, 16,$53,0
	.byte 128

metasprite_67_data:

	.byte   8,  0,$62,0
	.byte   8,  8,$65,0
	.byte   8, 16,$71,0
	.byte  16, 16,$70,0
	.byte   0, 16,$73,0
	.byte 128

metasprite_68_data:

	.byte  16,  8,$42,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   8,  8,$45,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   0,  8,$51,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   0, 16,$50,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   0,  0,$53,0|OAM_FLIP_H|OAM_FLIP_V
	.byte 128

metasprite_69_data:

	.byte   8, 16,$62,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   8,  8,$65,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   8,  0,$71,0|OAM_FLIP_H|OAM_FLIP_V
	.byte   0,  0,$70,0|OAM_FLIP_H|OAM_FLIP_V
	.byte  16,  0,$73,0|OAM_FLIP_H|OAM_FLIP_V
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
	.word metasprite_22_data
	.word metasprite_23_data
	.word metasprite_24_data
	.word metasprite_25_data
	.word metasprite_26_data
	.word metasprite_27_data
	.word metasprite_28_data
	.word metasprite_29_data
	.word metasprite_30_data
	.word metasprite_31_data
	.word metasprite_32_data
	.word metasprite_33_data
	.word metasprite_34_data
	.word metasprite_35_data
	.word metasprite_36_data
	.word metasprite_37_data
	.word metasprite_38_data
	.word metasprite_39_data
	.word metasprite_40_data
	.word metasprite_41_data
	.word metasprite_42_data
	.word metasprite_43_data
	.word metasprite_44_data
	.word metasprite_45_data
	.word metasprite_46_data
	.word metasprite_47_data
	.word metasprite_48_data
	.word metasprite_49_data
	.word metasprite_50_data
	.word metasprite_51_data
	.word metasprite_52_data
	.word metasprite_53_data
	.word metasprite_54_data
	.word metasprite_55_data
	.word metasprite_56_data
	.word metasprite_57_data
	.word metasprite_58_data
	.word metasprite_59_data
	.word metasprite_60_data
	.word metasprite_61_data
	.word metasprite_62_data
	.word metasprite_63_data
	.word metasprite_64_data
	.word metasprite_65_data
	.word metasprite_66_data
	.word metasprite_67_data
	.word metasprite_68_data
	.word metasprite_69_data

