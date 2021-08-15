%%%-------------------------------------------------------------------
%%% @author Matthijs Bakker <matthijs at hypothermic .nl>
%%% @copyright (C) 2021 hypothermic.nl
%%% @doc
%%%		Provides decoding/parsing functions for NBT data
%%% @end
%%% Created : 22. Jun 2021 11:07 PM
%%%-------------------------------------------------------------------
-module(erl_nbt_decode).
-author("Matthijs Bakker <matthijs at hypothermic .nl>").
-copyright("Copyright (C) 2021 hypothermic.nl").

-include("erl_nbt.hrl").

%%%-------------------------------------------------------------------
%%% Private API
%%%-------------------------------------------------------------------

-export([
	decode/2
]).

%%%-------------------------------------------------------------------
%%% @private Use the public equivalent erl_nbt:decode_with_rest/1
%%% @since 0.1.0
%%%
%%% @doc Decodes the first (nested) NBT tag in the specified binary.
%%%
%%% 	This function reads the first NBT tag from a binary.
%%% 	If it is a compound tag, it will read all children (including
%%% 	nested compound tags.)
%%%
%%%		If the given binary has remaining data after the NBT tag *AND*
%%%		the option ?OPTION_RETURN_REMAINDER is given, this remaining
%%%		data will be returned as Rest.
%%%
%%% @end
%%%
%%% @param Binary The binary to read the NBT tag from
%%%
%%% @returns
%%% 	A tuple containing {ok, Nbt, Remainder}
%%%			where NBT is a map (same as type erl_nbt:nbt())
%%%			and Remainder is an (empty) binary
%%%		or an error tuple containing {error, {Reason, Details}}
%%% @end
%%%-------------------------------------------------------------------

-spec decode(Binary :: binary(), Options :: erl_nbt:decode_options()) ->
	{ok, Nbt :: erl_nbt:nbt()} |
	{ok, Nbt :: erl_nbt:nbt(), Rest :: binary()} |
	{error, {max_depth_reached, Depth :: integer()}} |
	{error, {max_count_reached, Count :: integer()}} |
	{error, {unknown_tag_id, EncounteredTagId :: integer()}}.
decode(Binary, Options) ->
	% Use user-defined max depth and max children, or use the defaults
	MaxDepth = case lists:keyfind(?OPTION_MAX_DEPTH, 1, Options) of
				   {?OPTION_MAX_DEPTH, MaxDepthValue} ->
					   MaxDepthValue;
				   _ ->
					   ?DEFAULT_MAX_DEPTH
			   end,
	MaxChildren = case lists:keyfind(?OPTION_MAX_CHILDREN, 1, Options) of
					  {?OPTION_MAX_CHILDREN, MaxChildrenValue} ->
						  MaxChildrenValue;
					  _ ->
						  ?DEFAULT_MAX_CHILDREN
				  end,

	% All NBT files/binaries are inherently compound tags, so read compound.
	{ok, Nbt, Rest} = decode_tag(
		case lists:keyfind(?OPTION_COMPRESSION, 1, Options) of
			% ZLIB decompression
			{?OPTION_COMPRESSION, ?COMPRESSION_ZLIB} ->
				Z = zlib:open(),
				ok = zlib:inflateInit(Z),
				Inflated = zlib:inflate(Z, Binary),
				ok = zlib:inflateEnd(Z),
				ok = zlib:close(Z),
				Inflated;
			% No decompression
			_ ->
				Binary
		end, 0, MaxChildren, 0, MaxDepth, #{}),

	case lists:member(?OPTION_RETURN_REMAINDER, Options) of
		true ->
			{ok, Nbt, Rest};
		false ->
			{ok, Nbt}
	end.

%%%-------------------------------------------------------------------
%%% Internal Functions
%%%-------------------------------------------------------------------

-spec decode_tag(Binary :: binary(), Count :: integer(), MaxChildren :: integer(),
	             Depth :: integer(), MaxDepth :: integer(), Output :: map()) ->
	{ok, Nbt :: erl_nbt:nbt(), Rest :: binary()} |
	{error, {max_depth_reached, Depth :: integer()}} |
	{error, {max_count_reached, Count :: integer()}} |
	{error, {unknown_tag_id, EncounteredTagId :: integer()}}.

% Maximum tag depth has been reached, stop and return error.
decode_tag(_Binary, _Count, _MaxChildren, Depth, MaxDepth, _Output) when Depth >= MaxDepth ->
	{error, {max_depth_reached, Depth}};

% Maximum child count has been reached, stop and return error.
decode_tag(_Binary, Count, MaxChildren, _Depth, _MaxDepth, _Output) when Count >= MaxChildren ->
	{error, {max_count_reached, Count}};

% Reached end of binary, this occurs because the root compound tag doesn't end with 0x00
decode_tag(<<>>, _Count, _MaxChildren, _Depth, _MaxDepth, Output) ->
	{ok, Output, <<>>};

% Check if tag starts with the GZIP magic number
decode_tag(<<31:8/unsigned-integer, 139:8/unsigned-integer, _/binary>> = Binary, Count, _MaxChildren, Depth, _MaxDepth, Output) ->
	decode_tag(zlib:gunzip(Binary), Count, _MaxChildren, Depth, _MaxDepth, Output);

% TAG_End reached, the current compound tag has been fully read. Return all children.
decode_tag(<<Id:8/unsigned-integer, Rest/binary>>, _Count, _MaxChildren, _Depth, _MaxDepth, Output) when Id =:= ?TAG_END_ID ->
	{ok, Output, Rest};

decode_tag(<<Id:8/unsigned-integer, Rest/binary>>, Count, _MaxChildren, Depth, _MaxDepth, Output) when Id =:= ?TAG_BYTE_ID ->
	{ok, Name, Rest2} = decode_string(Rest),
	{ok, Byte, Rest3} = decode_byte(Rest2),

	decode_tag(Rest3, Count + 1, _MaxChildren, Depth, _MaxDepth, Output#{Name => {?TAG_BYTE_TYPE, Byte}});

decode_tag(<<Id:8/unsigned-integer, Rest/binary>>, Count, _MaxChildren, Depth, _MaxDepth, Output) when Id =:= ?TAG_SHORT_ID ->
	{ok, Name, Rest2} = decode_string(Rest),
	{ok, Short, Rest3} = decode_short(Rest2),

	decode_tag(Rest3, Count + 1, _MaxChildren, Depth, _MaxDepth, Output#{Name => {?TAG_SHORT_TYPE, Short}});

decode_tag(<<Id:8/unsigned-integer, Rest/binary>>, Count, _MaxChildren, Depth, _MaxDepth, Output) when Id =:= ?TAG_INT_ID ->
	{ok, Name, Rest2} = decode_string(Rest),
	{ok, Int, Rest3} = decode_int(Rest2),

	decode_tag(Rest3, Count + 1, _MaxChildren, Depth, _MaxDepth, Output#{Name => {?TAG_INT_TYPE, Int}});

decode_tag(<<Id:8/unsigned-integer, Rest/binary>>, Count, _MaxChildren, Depth, _MaxDepth, Output) when Id =:= ?TAG_LONG_ID ->
	{ok, Name, Rest2} = decode_string(Rest),
	{ok, Long, Rest3} = decode_long(Rest2),

	decode_tag(Rest3, Count + 1, _MaxChildren, Depth, _MaxDepth, Output#{Name => {?TAG_LONG_TYPE, Long}});

decode_tag(<<Id:8/unsigned-integer, Rest/binary>>, Count, _MaxChildren, Depth, _MaxDepth, Output) when Id =:= ?TAG_FLOAT_ID ->
	{ok, Name, Rest2} = decode_string(Rest),
	{ok, Float, Rest3} = decode_float(Rest2),

	decode_tag(Rest3, Count + 1, _MaxChildren, Depth, _MaxDepth, Output#{Name => {?TAG_FLOAT_TYPE, Float}});

decode_tag(<<Id:8/unsigned-integer, Rest/binary>>, Count, _MaxChildren, Depth, _MaxDepth, Output) when Id =:= ?TAG_DOUBLE_ID ->
	{ok, Name, Rest2} = decode_string(Rest),
	{ok, Double, Rest3} = decode_double(Rest2),

	decode_tag(Rest3, Count + 1, _MaxChildren, Depth, _MaxDepth, Output#{Name => {?TAG_DOUBLE_TYPE, Double}});

decode_tag(<<Id:8/unsigned-integer, Rest/binary>>, Count, _MaxChildren, Depth, _MaxDepth, Output) when Id =:= ?TAG_BYTE_ARRAY_ID ->
	{ok, Name, Rest2} = decode_string(Rest),
	{ok, ByteArray, Rest3} = decode_byte_array(Rest2),

	decode_tag(Rest3, Count + 1, _MaxChildren, Depth, _MaxDepth, Output#{Name => {?TAG_BYTE_ARRAY_TYPE, ByteArray}});

decode_tag(<<Id:8/unsigned-integer, Rest/binary>>, Count, _MaxChildren, Depth, _MaxDepth, Output) when Id =:= ?TAG_INT_ARRAY_ID ->
	{ok, Name, Rest2} = decode_string(Rest),
	{ok, IntArray, Rest3} = decode_int_array(Rest2),

	decode_tag(Rest3, Count + 1, _MaxChildren, Depth, _MaxDepth, Output#{Name => {?TAG_INT_ARRAY_TYPE, IntArray}});

decode_tag(<<Id:8/unsigned-integer, Rest/binary>>, Count, _MaxChildren, Depth, _MaxDepth, Output) when Id =:= ?TAG_LONG_ARRAY_ID ->
	{ok, Name, Rest2} = decode_string(Rest),
	{ok, LongArray, Rest3} = decode_long_array(Rest2),

	decode_tag(Rest3, Count + 1, _MaxChildren, Depth, _MaxDepth, Output#{Name => {?TAG_LONG_ARRAY_TYPE, LongArray}});

decode_tag(<<Id:8/unsigned-integer, Rest/binary>>, Count, _MaxChildren, Depth, _MaxDepth, Output) when Id =:= ?TAG_STRING_ID ->
	{ok, Name, Rest2} = decode_string(Rest),
	{ok, String, Rest3} = decode_string(Rest2),

	decode_tag(Rest3, Count + 1, _MaxChildren, Depth, _MaxDepth, Output#{Name => {?TAG_STRING_TYPE, String}});

decode_tag(<<Id:8/unsigned-integer, Rest/binary>>, Count, _MaxChildren, Depth, _MaxDepth, Output) when Id =:= ?TAG_LIST_ID ->
	{ok, Name, Rest2} = decode_string(Rest),
	{ok, TypeId, Rest3} = decode_byte(Rest2),
	{ok, Length, Rest4} = decode_int(Rest3),
	{ok, List, Rest5} = decode_element(TypeId, Rest4, Length),

	decode_tag(Rest5, Count + 1, _MaxChildren, Depth, _MaxDepth, Output#{Name => {?TAG_LIST_TYPE, tag_id_to_type(TypeId), List}});

decode_tag(<<Id:8/unsigned-integer, Rest/binary>>, Count, _MaxChildren, Depth, _MaxDepth, Output) when Id =:= ?TAG_COMPOUND_ID ->
	{ok, Name, Rest2} = decode_string(Rest),
	{ok, NestedCompound, Rest3} = decode_tag(Rest2, 0, _MaxChildren, Depth + 1, _MaxDepth, #{}),

	decode_tag(Rest3, Count + 1, _MaxChildren, Depth, _MaxDepth, Output#{Name => NestedCompound});

% Encountered an unknown tag ID, probably a malformed NBT file.
decode_tag(<<Id:8/unsigned-integer, _Rest/binary>>, _Count, _MaxChildren, _Depth, _MaxDepth, _Output) ->
	{error, {unknown_tag_id, Id}}.

decode_element(TypeId, Binary, Length) ->
	decode_element(TypeId, Binary, Length, []).

decode_element(_TypeId, Binary, Remaining, Output) when Remaining =< 0 ->
	{ok, lists:reverse(Output), Binary};

decode_element(TypeId, Binary, Remaining, Output) ->
	{ok, Element, Rest} = case TypeId of
		?TAG_BYTE_ID -> decode_byte(Binary);
		?TAG_SHORT_ID -> decode_short(Binary);
		?TAG_INT_ID -> decode_int(Binary);
		?TAG_LONG_ID -> decode_long(Binary);
		?TAG_FLOAT_ID -> decode_float(Binary);
		?TAG_DOUBLE_ID -> decode_double(Binary);
		?TAG_STRING_ID -> decode_string(Binary);
		?TAG_COMPOUND_ID -> decode_tag(Binary, 0, ?DEFAULT_MAX_CHILDREN, 0, ?DEFAULT_MAX_DEPTH, #{})
	end,
	decode_element(TypeId, Rest, Remaining - 1, [Element | Output]).

decode_byte(<<Byte:8/big-signed-integer, Rest/binary>>) ->
	{ok, Byte, Rest}.

decode_short(<<Short:16/big-signed-integer, Rest/binary>>) ->
	{ok, Short, Rest}.

decode_int(<<Int:32/big-signed-integer, Rest/binary>>) ->
	{ok, Int, Rest}.

decode_long(<<Long:64/big-signed-integer, Rest/binary>>) ->
	{ok, Long, Rest}.

decode_float(<<Float:32/big-signed-float, Rest/binary>>) ->
	{ok, Float, Rest}.

decode_double(<<Double:64/big-signed-float, Rest/binary>>) ->
	{ok, Double, Rest}.

decode_byte_array(<<Length:32/big-signed-integer, Rest/binary>>) ->
	read_next_raw(Rest, 8, Length, []).

decode_int_array(<<Length:32/big-signed-integer, Rest/binary>>) ->
	read_next_raw(Rest, 32, Length, []).

decode_long_array(<<Length:32/big-signed-integer, Rest/binary>>) ->
	read_next_raw(Rest, 64, Length, []).

decode_string(<<Length:16/unsigned-integer, String:Length/binary, Rest/binary>>) ->
	{ok, binary_to_list(String), Rest}.

read_next_raw(Data, UnitLength, Remaining, Out) when Remaining > 0 ->
	<<Value:UnitLength/signed-integer, Rest/binary>> = Data,
	read_next_raw(Rest, UnitLength, Remaining-1, [Value|Out]);

read_next_raw(Data, _UnitLength, _Remaining, Out) ->
	{ok, lists:reverse(Out), Data}.

tag_id_to_type(Id) ->
	case Id of
		?TAG_BYTE_ID			-> ?TAG_BYTE_TYPE;
		?TAG_SHORT_ID			-> ?TAG_SHORT_TYPE;
		?TAG_INT_ID				-> ?TAG_INT_TYPE;
		?TAG_LONG_ID			-> ?TAG_LONG_TYPE;
		?TAG_FLOAT_ID			-> ?TAG_FLOAT_TYPE;
		?TAG_DOUBLE_ID			-> ?TAG_DOUBLE_TYPE;
		?TAG_BYTE_ARRAY_ID		-> ?TAG_BYTE_ARRAY_TYPE;
		?TAG_INT_ARRAY_ID		-> ?TAG_INT_ARRAY_TYPE;
		?TAG_LONG_ARRAY_ID		-> ?TAG_LONG_ARRAY_TYPE;
		?TAG_STRING_ID			-> ?TAG_STRING_TYPE;
		?TAG_LIST_ID			-> ?TAG_LIST_TYPE;
		?TAG_COMPOUND_ID		-> ?TAG_COMPOUND_TYPE
	end.
