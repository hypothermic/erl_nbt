%%%-------------------------------------------------------------------
%%% @author Matthijs Bakker <matthijs at hypothermic .nl>
%%% @copyright (C) 2021 hypothermic.nl
%%% @doc
%%%		Constants and limits for encoding/decoding NBT data
%%% @end
%%% Created : 23. Jun 2021 12:45 PM
%%%-------------------------------------------------------------------
-author("Matthijs Bakker <matthijs at hypothermic .nl>").
-copyright("Copyright (C) 2021 hypothermic.nl").

%%%-------------------------------------------------------------------
%%% Constants
%%%-------------------------------------------------------------------

-define(TAG_END_ID,					0).

-define(TAG_BYTE_ID,				1).
-define(TAG_BYTE_TYPE,				byte).
-define(TAG_SHORT_ID,				2).
-define(TAG_SHORT_TYPE,				short).
-define(TAG_INT_ID,					3).
-define(TAG_INT_TYPE,				int).
-define(TAG_LONG_ID,				4).
-define(TAG_LONG_TYPE,				long).
-define(TAG_FLOAT_ID,				5).
-define(TAG_FLOAT_TYPE,				float).
-define(TAG_DOUBLE_ID,				6).
-define(TAG_DOUBLE_TYPE,			double).

-define(TAG_BYTE_ARRAY_ID,			7).
-define(TAG_BYTE_ARRAY_TYPE,		byte_array).
-define(TAG_INT_ARRAY_ID,			11).
-define(TAG_INT_ARRAY_TYPE,			int_array).
-define(TAG_LONG_ARRAY_ID,			12).
-define(TAG_LONG_ARRAY_TYPE,		long_array).

-define(TAG_STRING_ID,				8).
-define(TAG_STRING_TYPE,			string).
-define(TAG_LIST_ID,				9).
-define(TAG_LIST_TYPE,				list).

-define(TAG_COMPOUND_ID,			10).
-define(TAG_COMPOUND_TYPE,			compound).

%%%-------------------------------------------------------------------
%%% Encoder/Decoder options
%%%-------------------------------------------------------------------

-define(OPTION_RETURN_REMAINDER,	return_remainder).

-define(OPTION_COMPRESSION,			compression).
-define(COMPRESSION_UNCOMPRESSED,	uncompressed).
-define(COMPRESSION_GZIP,			gzip).
-define(COMPRESSION_ZLIB,			zlib).

%%%-------------------------------------------------------------------
%%% Limits according to NBT spec
%%%-------------------------------------------------------------------

-define(MAX_DEPTH,					256).
-define(MAX_COUNT,					1024).