%%%-------------------------------------------------------------------
%%% @author Matthijs Bakker <matthijs at hypothermic .nl>
%%% @copyright (C) 2021 hypothermic.nl
%%% @doc
%%%
%%% @end
%%% Created : 22. Jun 2021 11:07 PM
%%%-------------------------------------------------------------------
-module(erl_nbt_decode).
-author("Matthijs Bakker <matthijs at hypothermic .nl>").
-copyright("Copyright (C) 2021 hypothermic.nl").

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

-export([
	decode/1
]).

-spec decode(Binary :: binary()) -> {ok, erl_nbt:nbt(), Rest :: binary()}.
decode(Binary) ->
	% All NBT files/binaries are inherently compound tags, so read compound.
	decode(Binary, 0, 0, []).


%%%-------------------------------------------------------------------
%%% Internal Functions
%%%-------------------------------------------------------------------

decode(Binary, _Depth, tag_end, Output) ->
	{ok, lists:last(lists:reverse(Output)), Binary};

decode(<<>>, _Depth, _Count, Output) ->
	{ok, lists:last(lists:reverse(Output)), []};

decode(_Binary, Depth, _Count, _Output) when Depth >= 10 -> % TODO un-hardcode this
	{error, max_depth_reached};

decode(_Binary, _Depth, Count, _Output) when Count >= 10 -> % TODO un-hardcode this
	{error, max_count_reached};

decode(Binary, Depth, Count, Output) ->
	{ok, Tag, Rest} = decode_tag(Binary, Depth),

	case Tag of
		% End tag, stop processing this compound
		tag_end ->
			decode(Rest, Depth, tag_end, Output);
		% More tags following this one
		_ ->
			decode(Rest, Depth, Count + 1, [Tag | Output])
	end.

decode_tag(<<Type:8/unsigned-integer, Binary/binary>>, Depth) ->
	decode_type(Type, Binary, Depth).

decode_type(0, _Binary, _Depth) ->
	{ok, tag_end, <<>>};

decode_type(3, Binary, _Depth) ->
	{ok, Name, Rest1} = decode_tag_string(Binary),
	{ok, Int, Rest2} = decode_tag_int(Rest1),

	{ok, #{Name => Int}, Rest2};

decode_type(5, Binary, _Depth) ->
	{ok, Name, Rest1} = decode_tag_string(Binary),
	{ok, Float, Rest2} = decode_tag_float(Rest1),

	{ok, #{Name => Float}, Rest2};

decode_type(6, Binary, _Depth) ->
	{ok, Name, Rest1} = decode_tag_string(Binary),
	{ok, Double, Rest2} = decode_tag_double(Rest1),

	{ok, #{Name => Double}, Rest2};

decode_type(8, Binary, _Depth) ->
	{ok, Name, Rest1} = decode_tag_string(Binary),
	{ok, Value, Rest2} = decode_tag_string(Rest1),

	{ok, #{Name => Value}, Rest2};

decode_type(10, Binary, Depth) ->
	{ok, Name, Rest1} = decode_tag_string(Binary),

	{ok, Tags, Rest2} = decode(Rest1, Depth + 1, 0, []),

	{ok, #{Name => Tags}, Rest2};

decode_type(Type, _Binary, _Depth) ->
	{error, {bad_type, Type}}.

decode_tag_string(<<NameLength:16/unsigned-integer, NameBinary:NameLength/binary, Rest/binary>>) ->
	{ok, binary_to_list(NameBinary), Rest}.

decode_tag_int(<<Int:32/signed-big-integer, Rest/binary>>) ->
	{ok, Int, Rest}.

decode_tag_float(<<Float:32/big-float, Rest/binary>>) ->
	{ok, Float, Rest}.

decode_tag_double(<<Double:64/big-float, Rest/binary>>) ->
	{ok, Double, Rest}.
