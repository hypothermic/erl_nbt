%%%-------------------------------------------------------------------
%%% @author Matthijs Bakker <matthijs at hypothermic .nl>
%%% @copyright (C) 2021 hypothermic.nl
%%% @doc
%%%		Provides encoding functions for NBT data
%%% @end
%%% Created : 23. Jun 2021 2:41 PM
%%%-------------------------------------------------------------------
-module(erl_nbt_encode).
-author("Matthijs Bakker <matthijs at hypothermic .nl>").
-copyright("Copyright (C) 2021 hypothermic.nl").

-include("erl_nbt.hrl").

%%%-------------------------------------------------------------------
%%% Private API
%%%-------------------------------------------------------------------

-export([
	encode/1
]).

-spec encode(Nbt :: erl_nbt:nbt()) ->
	{ok, Binary :: binary()} |
	{error, {invalid_nbt, Details :: string()}}.
encode(Nbt) ->
	{ok, encode_compound(Nbt)}.

encode_tag(Key, Value) when is_list(Value) ->
	<<
		?TAG_STRING_ID:8/unsigned-integer,
		(encode_string(Key))/binary,
		(encode_string(Value))/binary
	>>;

encode_tag(Key, Value) when is_integer(Value) ->
	<<
		?TAG_INT_ID:8/unsigned-integer,
		(encode_string(Key))/binary,
		Value:32/big-signed-integer
	>>;

encode_tag(Key, Value) when is_float(Value) ->
	<<
		?TAG_DOUBLE_ID:8/unsigned-integer,
		(encode_string(Key))/binary,
		Value:64/big-signed-float
	>>;

encode_tag(Key, Value) when is_map(Value) ->
	<<
		?TAG_COMPOUND_ID:8/unsigned-integer,
		(encode_string(Key))/binary,
		(encode_compound(Value))/binary,
		?TAG_END_ID:8/unsigned-integer
	>>.

encode_string(String) ->
	<<
		(length(String)):16/unsigned-integer,
		(list_to_binary(String))/binary
	>>.

encode_compound(Nbt) ->
	maps:fold(fun (Key, Value, Acc) ->
		<<
			Acc/binary,
			(encode_tag(Key, Value))/binary
		>>
		end, <<>>, Nbt).
