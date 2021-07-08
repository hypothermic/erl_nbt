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

%%%-------------------------------------------------------------------
%%% Internal Functions
%%%-------------------------------------------------------------------

encode_tag(Key, {?TAG_BYTE_TYPE, Value}) ->
	<<
		?TAG_BYTE_ID:8/unsigned-integer,
		(encode_string(Key))/binary,
		Value:8/big-signed-integer
	>>;

encode_tag(Key, {?TAG_SHORT_TYPE, Value}) ->
	<<
		?TAG_SHORT_ID:8/unsigned-integer,
		(encode_string(Key))/binary,
		Value:16/big-signed-integer
	>>;

encode_tag(Key, {?TAG_INT_TYPE, Value}) ->
	<<
		?TAG_INT_ID:8/unsigned-integer,
		(encode_string(Key))/binary,
		Value:32/big-signed-integer
	>>;

encode_tag(Key, {?TAG_LONG_TYPE, Value}) ->
	<<
		?TAG_LONG_ID:8/unsigned-integer,
		(encode_string(Key))/binary,
		Value:64/big-signed-integer
	>>;

encode_tag(Key, {?TAG_FLOAT_TYPE, Value}) ->
	<<
		?TAG_FLOAT_ID:8/unsigned-integer,
		(encode_string(Key))/binary,
		Value:32/big-signed-float
	>>;

encode_tag(Key, {?TAG_DOUBLE_TYPE, Value}) ->
	<<
		?TAG_DOUBLE_ID:8/unsigned-integer,
		(encode_string(Key))/binary,
		Value:64/big-signed-float
	>>;

encode_tag(Key, {?TAG_STRING_TYPE, Value}) ->
	<<
		?TAG_STRING_ID:8/unsigned-integer,
		(encode_string(Key))/binary,
		(encode_string(Value))/binary
	>>;

encode_tag(Key, Value) when is_map(Value) ->
	encode_tag(Key, {compound, Value});

encode_tag(Key, {?TAG_COMPOUND_TYPE, Value}) ->
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
