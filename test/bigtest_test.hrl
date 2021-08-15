%%%-------------------------------------------------------------------
%%% @author Matthijs Bakker <matthijs at hypothermic .nl>
%%% @copyright (C) 2021 hypothermic.nl
%%% @doc
%%%     Tests the erl_nbt functionality on the "bigtest.nbt" example
%%%     provided by Notch
%%% @end
%%% Created : 15. Aug 2021 1:40 AM
%%%-------------------------------------------------------------------
-author("Matthijs Bakker <matthijs at hypothermic .nl>").

-include_lib("erl_nbt.hrl").

%%%-------------------------------------------------------------------
%%% A structure of the NBT found in bigtest.nbt
%%%-------------------------------------------------------------------

-define(EXPECTED_NBT,
	#{
		"Level" => #{
			"nested compound test" => #{
				"egg" => #{
					"name" => {?TAG_STRING_TYPE, "Eggbert"},
					"value" => {?TAG_FLOAT_TYPE, 0.5}
				},
				"ham" => #{
					"name" => {?TAG_STRING_TYPE, "Hampus"},
					"value" => {?TAG_FLOAT_TYPE, 0.75}
				}
			},
			"intTest" => {?TAG_INT_TYPE, 2147483647},
			"byteTest" => {?TAG_BYTE_TYPE, 127},
			"stringTest" => {?TAG_STRING_TYPE, "HELLO WORLD THIS IS A TEST STRING AAO!"},
			"listTest (long)" => {?TAG_LIST_TYPE, ?TAG_LONG_TYPE, [
				11, 12, 13, 14, 15
			]},
			"doubleTest" => {?TAG_DOUBLE_TYPE, 0.49312871321823148},
			"floatTest" => {?TAG_FLOAT_TYPE, 0.49823147058486938},
			"longTest" => {?TAG_LONG_TYPE, 9223372036854775807},
			"listTest (compound)" => {?TAG_LIST_TYPE, ?TAG_COMPOUND_TYPE, [
				#{
					"created-on" => {?TAG_LONG_TYPE, 1264099775885},
					"name" => {?TAG_STRING_TYPE, "Compound tag #0"}
				},
				#{
					"created-on" => {?TAG_LONG_TYPE, 1264099775885},
					"name" => {?TAG_STRING_TYPE, "Compound tag #1"}
				}
			]},
			"byteArrayTest (the first 1000 values of (n*n*255+n*7)%100, starting with n=0 (0, 62, 34, 16, 8, ...))"
			=> {?TAG_BYTE_ARRAY_TYPE, ?BYTE_ARRAY_EXAMPLE_DATA},
			"shortTest" => {?TAG_SHORT_TYPE, 32767}
		}
	}).

%%%-------------------------------------------------------------------
%%% The byte array data found tag "byteArrayTest"
%%%-------------------------------------------------------------------

-define(BYTE_ARRAY_EXAMPLE_DATA, [
	0,	62,	34,	16,	8,	10,	22,	44,	76,	18,	70,	32,	4,	86,	78,	80,
	92,	14,	46,	88,	40,	2,	74,	56,	48,	50,	62,	84,	16,	58,	10,	72,
	44,	26,	18,	20,	32,	54,	86,	28,	80,	42,	14,	96,	88,	90,	2,	24,
	56,	98,	50,	12,	84,	66,	58,	60,	72,	94,	26,	68,	20,	82,	54,	36,
	28,	30,	42,	64,	96,	38,	90,	52,	24,	6,	98,	0,	12,	34,	66,	8,
	60,	22,	94,	76,	68,	70,	82,	4,	36,	78,	30,	92,	64,	46,	38,	40,
	52,	74,	6,	48,	0,	62,	34,	16,	8,	10,	22,	44,	76,	18,	70,	32,
	4,	86,	78,	80,	92,	14,	46,	88,	40,	2,	74,	56,	48,	50,	62,	84,
	16,	58,	10,	72,	44,	26,	18,	20,	32,	54,	86,	28,	80,	42,	14,	96,
	88,	90,	2,	24,	56,	98,	50,	12,	84,	66,	58,	60,	72,	94,	26,	68,
	20,	82,	54,	36,	28,	30,	42,	64,	96,	38,	90,	52,	24,	6,	98,	0,
	12,	34,	66,	8,	60,	22,	94,	76,	68,	70,	82,	4,	36,	78,	30,	92,
	64,	46,	38,	40,	52,	74,	6,	48,	0,	62,	34,	16,	8,	10,	22,	44,
	76,	18,	70,	32,	4,	86,	78,	80,	92,	14,	46,	88,	40,	2,	74,	56,
	48,	50,	62,	84,	16,	58,	10,	72,	44,	26,	18,	20,	32,	54,	86,	28,
	80,	42,	14,	96,	88,	90,	2,	24,	56,	98,	50,	12,	84,	66,	58,	60,
	72,	94,	26,	68,	20,	82,	54,	36,	28,	30,	42,	64,	96,	38,	90,	52,
	24,	6,	98,	0,	12,	34,	66,	8,	60,	22,	94,	76,	68,	70,	82,	4,
	36,	78,	30,	92,	64,	46,	38,	40,	52,	74,	6,	48,	0,	62,	34,	16,
	8,	10,	22,	44,	76,	18,	70,	32,	4,	86,	78,	80,	92,	14,	46,	88,
	40,	2,	74,	56,	48,	50,	62,	84,	16,	58,	10,	72,	44,	26,	18,	20,
	32,	54,	86,	28,	80,	42,	14,	96,	88,	90,	2,	24,	56,	98,	50,	12,
	84,	66,	58,	60,	72,	94,	26,	68,	20,	82,	54,	36,	28,	30,	42,	64,
	96,	38,	90,	52,	24,	6,	98,	0,	12,	34,	66,	8,	60,	22,	94,	76,
	68,	70,	82,	4,	36,	78,	30,	92,	64,	46,	38,	40,	52,	74,	6,	48,
	0,	62,	34,	16,	8,	10,	22,	44,	76,	18,	70,	32,	4,	86,	78,	80,
	92,	14,	46,	88,	40,	2,	74,	56,	48,	50,	62,	84,	16,	58,	10,	72,
	44,	26,	18,	20,	32,	54,	86,	28,	80,	42,	14,	96,	88,	90,	2,	24,
	56,	98,	50,	12,	84,	66,	58,	60,	72,	94,	26,	68,	20,	82,	54,	36,
	28,	30,	42,	64,	96,	38,	90,	52,	24,	6,	98,	0,	12,	34,	66,	8,
	60,	22,	94,	76,	68,	70,	82,	4,	36,	78,	30,	92,	64,	46,	38,	40,
	52,	74,	6,	48,	0,	62,	34,	16,	8,	10,	22,	44,	76,	18,	70,	32,
	4,	86,	78,	80,	92,	14,	46,	88,	40,	2,	74,	56,	48,	50,	62,	84,
	16,	58,	10,	72,	44,	26,	18,	20,	32,	54,	86,	28,	80,	42,	14,	96,
	88,	90,	2,	24,	56,	98,	50,	12,	84,	66,	58,	60,	72,	94,	26,	68,
	20,	82,	54,	36,	28,	30,	42,	64,	96,	38,	90,	52,	24,	6,	98,	0,
	12,	34,	66,	8,	60,	22,	94,	76,	68,	70,	82,	4,	36,	78,	30,	92,
	64,	46,	38,	40,	52,	74,	6,	48,	0,	62,	34,	16,	8,	10,	22,	44,
	76,	18,	70,	32,	4,	86,	78,	80,	92,	14,	46,	88,	40,	2,	74,	56,
	48,	50,	62,	84,	16,	58,	10,	72,	44,	26,	18,	20,	32,	54,	86,	28,
	80,	42,	14,	96,	88,	90,	2,	24,	56,	98,	50,	12,	84,	66,	58,	60,
	72,	94,	26,	68,	20,	82,	54,	36,	28,	30,	42,	64,	96,	38,	90,	52,
	24,	6,	98,	0,	12,	34,	66,	8,	60,	22,	94,	76,	68,	70,	82,	4,
	36,	78,	30,	92,	64,	46,	38,	40,	52,	74,	6,	48,	0,	62,	34,	16,
	8,	10,	22,	44,	76,	18,	70,	32,	4,	86,	78,	80,	92,	14,	46,	88,
	40,	2,	74,	56,	48,	50,	62,	84,	16,	58,	10,	72,	44,	26,	18,	20,
	32,	54,	86,	28,	80,	42,	14,	96,	88,	90,	2,	24,	56,	98,	50,	12,
	84,	66,	58,	60,	72,	94,	26,	68,	20,	82,	54,	36,	28,	30,	42,	64,
	96,	38,	90,	52,	24,	6,	98,	0,	12,	34,	66,	8,	60,	22,	94,	76,
	68,	70,	82,	4,	36,	78,	30,	92,	64,	46,	38,	40,	52,	74,	6,	48,
	0,	62,	34,	16,	8,	10,	22,	44,	76,	18,	70,	32,	4,	86,	78,	80,
	92,	14,	46,	88,	40,	2,	74,	56,	48,	50,	62,	84,	16,	58,	10,	72,
	44,	26,	18,	20,	32,	54,	86,	28,	80,	42,	14,	96,	88,	90,	2,	24,
	56,	98,	50,	12,	84,	66,	58,	60,	72,	94,	26,	68,	20,	82,	54,	36,
	28,	30,	42,	64,	96,	38,	90,	52,	24,	6,	98,	0,	12,	34,	66,	8,
	60,	22,	94,	76,	68,	70,	82,	4,	36,	78,	30,	92,	64,	46,	38,	40,
	52,	74,	6,	48,	0,	62,	34,	16,	8,	10,	22,	44,	76,	18,	70,	32,
	4,	86,	78,	80,	92,	14,	46,	88,	40,	2,	74,	56,	48,	50,	62,	84,
	16,	58,	10,	72,	44,	26,	18,	20,	32,	54,	86,	28,	80,	42,	14,	96,
	88,	90,	2,	24,	56,	98,	50,	12,	84,	66,	58,	60,	72,	94,	26,	68,
	20,	82,	54,	36,	28,	30,	42,	64,	96,	38,	90,	52,	24,	6,	98,	0,
	12,	34,	66,	8,	60,	22,	94,	76,	68,	70,	82,	4,	36,	78,	30,	92,
	64,	46,	38,	40,	52,	74,	6,	48
]).
