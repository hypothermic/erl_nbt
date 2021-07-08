%%%-------------------------------------------------------------------
%%% @author Matthijs Bakker <matthijs at hypothermic .nl>
%%% @copyright (C) 2021 hypothermic.nl
%%% @doc
%%%     Tests the erl_nbt functionality on
%%%     the "floating_points.nbt" example
%%% @end
%%% Created : 8. Jul 2021 9:47 PM
%%%-------------------------------------------------------------------
-module(floating_points_test).
-author("Matthijs Bakker <matthijs at hypothermic .nl>").
-copyright("Copyright (C) 2021 hypothermic.nl").

-include_lib("erl_nbt.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TEST_FILE,      "test/floating_points.nbt").
-define(EXPECTED_NBT,
	#{
		"position" => #{
			"x"			=> {?TAG_DOUBLE_TYPE,	11012.8},
			"y"			=> {?TAG_FLOAT_TYPE,	72.0},
			"z"			=> {?TAG_DOUBLE_TYPE,	-799.2},
			"stance"	=> {?TAG_FLOAT_TYPE,	87.0}
		}
	}
).

floating_points_test_() -> [
	{"decode file floating_points.nbt",
		fun () ->
			{ok, InputData}		= file:read_file(?TEST_FILE),
			{ok, DecodedNbt}	= erl_nbt:decode(InputData),

			?assertEqual(?EXPECTED_NBT, DecodedNbt)
		end
	},
	{"encode map and compare with file floating_points.nbt",
		fun () ->
			{ok, EncodedNbt}	= erl_nbt:encode(?EXPECTED_NBT),
			{ok, CorrectData}	= file:read_file(?TEST_FILE),

			% Because child order of compound tags is not guaranteed
			% to be preserved with NBT tags, we can't test for equality.
			% So we only test if the length of these binaries are equal.
			?assertEqual(byte_size(CorrectData), byte_size(EncodedNbt))
		end
	},
	{"encode then decode",
		fun () ->
			{ok, EncodedNbt}	= erl_nbt:encode(?EXPECTED_NBT),
			{ok, DecodedNbt}	= erl_nbt:decode(EncodedNbt),
			?assertEqual(DecodedNbt, ?EXPECTED_NBT)
		end
	}
].