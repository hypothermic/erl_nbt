%%%-------------------------------------------------------------------
%%% @author Matthijs Bakker <matthijs at hypothermic .nl>
%%% @copyright (C) 2021 hypothermic.nl
%%% @doc
%%%     Tests the erl_nbt functionality on the "integers.nbt" example
%%% @end
%%% Created : 8. Jul 2021 10:06 PM
%%%-------------------------------------------------------------------
-module(integers_test).
-author("Matthijs Bakker <matthijs at hypothermic .nl>").
-copyright("Copyright (C) 2021 hypothermic.nl").

-include_lib("erl_nbt.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TEST_FILE,      "test/integers.nbt").
-define(EXPECTED_NBT,
	#{
		"my numbers" => #{
			"my byte"	=> {?TAG_BYTE_TYPE,		79},
			"my short"	=> {?TAG_SHORT_TYPE,	13027},
			"my int"	=> {?TAG_INT_TYPE,		20012318},
			"my long"	=> {?TAG_LONG_TYPE,		91499321421}
		}
	}).

integers_test_() -> [
	{"decode file integers.nbt",
		fun () ->
			{ok, InputData}		= file:read_file(?TEST_FILE),
			{ok, DecodedNbt}	= erl_nbt:decode(InputData),

			?assertEqual(?EXPECTED_NBT, DecodedNbt)
		end
	},
	{"encode map and compare with file integers.nbt",
		fun () ->
			{ok, EncodedNbt}	= erl_nbt:encode(?EXPECTED_NBT),
			{ok, CorrectData}	= file:read_file(?TEST_FILE),

			% Because child order of compound tags is not guaranteed
			% to be preserved with NBT tags, we can't test for equality.
			% So we only test if the length of these binaries are equal.
			?assertEqual(byte_size(CorrectData), byte_size(EncodedNbt))
		end
	}
].