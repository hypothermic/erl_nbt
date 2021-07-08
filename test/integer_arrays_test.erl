%%%-------------------------------------------------------------------
%%% @author Matthijs Bakker <matthijs at hypothermic .nl>
%%% @copyright (C) 2021 hypothermic.nl
%%% @doc
%%%     Tests the erl_nbt functionality on the "integers.nbt" example
%%% @end
%%% Created : 8. Jul 2021 10:25 PM
%%%-------------------------------------------------------------------
-module(integer_arrays_test).
-author("Matthijs Bakker <matthijs at hypothermic .nl>").
-copyright("Copyright (C) 2021 hypothermic.nl").

-include_lib("erl_nbt.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TEST_FILE,      "test/integer_arrays.nbt").
-define(EXPECTED_NBT,
	#{
		"arrays" => #{
			"bunch of bytes" => {?TAG_BYTE_ARRAY_TYPE, [-43, 67, 49, 108]},
			"flock of ints" => {?TAG_INT_ARRAY_TYPE, [420, 2147483646, -82131929]},
			"lots of longs" => {?TAG_LONG_ARRAY_TYPE, [-921312010, 69, 129391203]}
		}
	}
).

integer_arrays_test_() -> [
	{"decode file integer_arrays.nbt",
		fun () ->
			{ok, InputData}		= file:read_file(?TEST_FILE),
			{ok, DecodedNbt}	= erl_nbt:decode(InputData),

			?assertEqual(?EXPECTED_NBT, DecodedNbt)
		end
	},
	{"encode map and compare with file integer_arrays.nbt",
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