%%%-------------------------------------------------------------------
%%% @author Matthijs Bakker <matthijs at hypothermic .nl>
%%% @copyright (C) 2021 hypothermic.nl
%%% @doc
%%%		Provides an interface for dealing with NBT data in Erlang
%%% @end
%%% Created : 22. Jun 2021 10:31 PM
%%%-------------------------------------------------------------------
-module(erl_nbt).
-author("Matthijs Bakker <matthijs at hypothermic .nl>").
-copyright("Copyright (C) 2021 hypothermic.nl").

-include("erl_nbt.hrl").

%%%-------------------------------------------------------------------
%%% Types
%%%-------------------------------------------------------------------

-export_type([
	% Structures
	nbt/0, nbt_key/0, nbt_value/0,

	% Integer and floating points
	nbt_byte/0, nbt_short/0, nbt_int/0, nbt_long/0, nbt_float/0, nbt_double/0,

	% Arrays
	nbt_byte_array/0, nbt_int_array/0, nbt_long_array/0,

	% Structure components
	nbt_string/0, nbt_compound/0
]).

-type nbt()				:: #{nbt_key() => {nbt_type(), nbt_value()}}.
% A named NBT tag of any type

-type nbt_key()			:: nbt_string().
% A string containing the tag name

-type nbt_value()		:: nbt_byte() | nbt_short() | nbt_int() | nbt_long() | nbt_float() | nbt_double() |
						   nbt_byte_array() | nbt_int_array() | nbt_long_array() | nbt_string() | nbt_compound().
% The value of a tag. The type of this value depends on the type of the tag

-type nbt_type()		:: byte | short | int | long | float | double |
						    byte_array | int_array | long_array | string | compound.
% An atom describing the type of this tag

-type nbt_byte()		:: byte().
% A value which is guaranteed to be a 8-bit signed integer (from -128 to +127)

-type nbt_short()		:: integer().
% A value which is guaranteed to be a 16-bit signed integer  (from -32768 to +32767)

-type nbt_int()			:: integer().
% A value which is guaranteed to be a 32-bit signed integer (from -2^31 to (+2^31)-1)

-type nbt_long()		:: integer().
% A value which is guaranteed to be a 64-bit signed integer (from -2^63 to (+2^63)-1)

-type nbt_float()		:: float().
% A value which is guaranteed to be a single-precision floating point number (NaN possible)
% Due to the way the Erlang BEAM VM handles IEEE-754 floats, this value can't be SNaN, QNaN, -0/+0 or infinity.

-type nbt_double()		:: float().
% A value which is guaranteed to be a double-precision floating point number (NaN possible)
% Due to the way the Erlang BEAM VM handles IEEE-754 floats, this value can't be SNaN, QNaN, -0/+0 or infinity.

-type nbt_byte_array()	:: list().
% A list of nbt_byte()

-type nbt_int_array()	:: list().
% A list of nbt_int()

-type nbt_long_array()	:: list().
% A list of nbt_long()

-type nbt_string()		:: string().
% A string with a maximum length of nbt_short() (unchecked)

-type nbt_compound()	:: map().
% A collection of named tags. The order that these tags are stored in is not guaranteed.

-type compression()		:: ?COMPRESSION_UNCOMPRESSED | ?COMPRESSION_GZIP.
% The types of compression that may be used in NBT files according to the official spec

-type decode_option()	:: ?OPTION_RETURN_REMAINDER | {?OPTION_COMPRESSION, Compression :: compression()}.
% An option which alters the functionality of the decoding process

-type encode_option()	:: {?OPTION_COMPRESSION, Compression :: compression()}.
% An option which alters the functionality of the encoding process

-type decode_options()	:: [decode_option()].
% A list of decode_option()

-type encode_options()	:: [encode_option()].
% A list of encode_option()

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

-export([
	decode/1,
	decode/2,
	encode/1,
	encode/2
]).

%%%-------------------------------------------------------------------
%%% @since 0.1.0
%%%
%%% @doc Decodes the first (nested) NBT tag in the specified binary.
%%%
%%% 	This function reads the first NBT tag from a binary.
%%% 	If it is a compound tag, it will read all children (including
%%% 	nested compound tags.)
%%%
%%%     Both uncompressed and GZIP-ped binaries are accepted.
%%%
%%% @end
%%%
%%% @param Binary The binary to read the NBT tag from
%%%
%%% @returns
%%% 	A tuple containing {ok, Nbt} where Nbt is a map
%%% @end
%%%-------------------------------------------------------------------

-spec decode(Binary :: binary()) -> {ok, nbt()}.
decode(Binary) ->
	decode(Binary, []).

%%%-------------------------------------------------------------------
%%% @since 0.3.0
%%%
%%% @doc Decodes the first (nested) NBT tag in the specified binary.
%%%
%%% 	This function reads the first NBT tag from a binary.
%%% 	If it is a compound tag, it will read all children (including
%%% 	nested compound tags.)
%%%
%%%     Both uncompressed and GZIP-ped binaries are accepted.
%%%
%%% @end
%%%
%%% @param Binary The binary to read the NBT tag from
%%%
%%% @returns
%%% 	A tuple containing {ok, Nbt} where Nbt is a map
%%% @end
%%%-------------------------------------------------------------------

-spec decode(Binary :: binary(), Options :: decode_options()) -> {ok, nbt()} | {ok, nbt(), Remainder :: binary()}.
decode(Binary, Options) ->
	erl_nbt_decode:decode(Binary, Options).

%%%-------------------------------------------------------------------
%%% @since 0.1.0
%%%
%%% @doc Encodes an NBT tag in Erlang Map form to binary.
%%%
%%% 	This function accepts an NBT tag representation in map-form
%%%		(aka type erl_nbt:nbt()) and encodes it into a binary.
%%%
%%% @end
%%%
%%% @param Nbt The NBT tag to encode
%%%
%%% @returns
%%% 	A tuple containing {ok, Nbt} where Nbt is a map
%%% @end
%%%-------------------------------------------------------------------

-spec encode(Nbt :: nbt()) -> {ok, binary()}.
encode(Nbt) ->
	encode(Nbt, []).

%%%-------------------------------------------------------------------
%%% @since 0.3.0
%%%
%%% @doc Encodes an NBT tag in Erlang Map form to binary.
%%%
%%% 	This function accepts an NBT tag representation in map-form
%%%		(aka type erl_nbt:nbt()) and encodes it into a binary.
%%%
%%% @end
%%%
%%% @param Nbt The NBT tag to encode
%%%
%%% @returns
%%% 	A tuple containing {ok, Nbt} where Nbt is a map
%%% @end
%%%-------------------------------------------------------------------

-spec encode(Nbt :: nbt(), Options :: encode_options()) -> {ok, binary()}.
encode(Nbt, Options) ->
	erl_nbt_encode:encode(Nbt, Options).