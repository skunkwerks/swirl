%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=4 sw=4 et
%% Licensed under the Apache License, Version 2.0 (the "License"); you may not
%% use this file except in compliance with the License. You may obtain a copy of
%% the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
%% License for the specific language governing permissions and limitations under
%% the License.

%% @doc Library for PPSPP over UDP, aka Swift protocol
%% <p>This module implements a library of functions necessary to
%% handle parsing PPSPP options wire-protocol of PPSPP over UDP, including
%% functions for encoding and decoding.</p>
%% @end

-module(ppspp_options).
-include("swirl.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-spec test() -> term().
-endif.

%% api
-export([unpack/1,
         pack/1,
         get_chunk_addressing_method/1,
         get_content_integrity_check_method/1,
         get_merkle_hash_tree_function/1,
         get_minimum_version/1,
         get_swarm_id/1,
         get_maximum_supported_version/1,
         use_default_options/1]).

%% types largely as defined in PPSPP RFC

-export_type([root_hash/0,
              merkle_tree_hash_function/0,
              content_integrity_protection_method/0,
              options/0,
              option/0]).

-opaque root_hash() :: binary().
-opaque options() :: {options, options_dict()}.
-type options_dict() :: list({ option(), any()}).
-opaque option() :: supported_version
| minimum_version
| swarm_id
| content_integrity_check_method
| merkle_hash_tree_function
| live_signature_algorithm
| ppspp_chunk:addressing_method()
| live_discard_window
| supported_messages
| end_option
| default_chunk_size.

%% matches a subset of types from http://erlang.org/doc/man/crypto.html
%%  digest_type() =  md5 | sha | sha224 | sha256 | sha384 | sha512, and
%%  hash_algorithms() = md5 | ripemd160 | sha | sha224 | sha256 | sha384 | sha512
-opaque merkle_tree_hash_function() :: sha | sha224 | sha256 | sha384 | sha512.

-opaque content_integrity_protection_method() :: no_integrity_protection
| merkle_hash_tree
| sign_all
| unified_merkle_hash_tree.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc unpack PPPSPP options as encoded in wire format into an orddict
%% <p>
%% Unwrap PPSPP option wire format into orddict.
%% </p>
%% @end

-spec unpack(binary()) -> {binary(), options()} | {error, atom()}.
%% options is an orddict.
unpack(Maybe_Options) ->
    {Maybe_Messages, Options} = unpack(Maybe_Options, orddict:new()),
    {Maybe_Messages, {options, Options}}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec unpack(binary(), options_dict()) -> { binary(), options_dict()}.
unpack( <<?PPSPP_SUPPORTED_VERSION,
          Version:?BYTE,
          Maybe_Options/binary >>, Options0) ->
    Options = orddict:store(supported_version, Version, Options0),
    unpack(Maybe_Options, Options);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
unpack( <<?PPSPP_MINIMUM_VERSION,
          Version:?BYTE,
          Maybe_Options/binary >>, Options0) ->
    Options = orddict:store(minimum_version, Version, Options0),
    unpack(Maybe_Options, Options);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
unpack( <<?PPSPP_SWARM_ID_LENGTH,
          Length:?WORD,
          Swarm_ID:Length/binary,
          Maybe_Options/binary >>, Options0) ->
    Options = orddict:store(swarm_id, Swarm_ID, Options0),
    unpack(Maybe_Options, Options);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
unpack( <<?PPSPP_INTEGRITY_CHECK_METHOD,
          Maybe_Method:?BYTE,
          Maybe_Options/binary >>, Options0) ->
    Method = case Maybe_Method of
                 0 -> no_integrity_protection;
                 1 -> merkle_hash_tree;
                 2 -> sign_all;
                 3 -> unified_merkle_hash_tree
             end,
    Options = orddict:store(content_integrity_check_method, Method, Options0),
    unpack(Maybe_Options, Options);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
unpack( <<?PPSPP_MERKLE_HASH_FUNCTION,
          Maybe_Hash_Function:?BYTE,
          Maybe_Options/binary >>, Options0) ->
    Hash_Function = case Maybe_Hash_Function of
                        0 -> sha;
                        1 -> sha224;
                        2 -> sha256;
                        3 -> sha384;
                        4 -> sha512
                    end,
    Options = orddict:store(merkle_hash_function, Hash_Function, Options0),
    unpack(Maybe_Options, Options);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
unpack( <<?PPSPP_LIVE_SIGNATURE_ALGORITHM,
          Maybe_Algorithm:?BYTE,
          Maybe_Options/binary >>, Options0) ->
    Algorithm = case Maybe_Algorithm of
                    %% TODO we can parse this yet but need normative algorithm names
                    0 -> live_signature_algorithm_not_yet_implemented
                end,
    Options = orddict:store(merkle_hash_tree_algorithm, Algorithm, Options0),
    unpack(Maybe_Options, Options);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TODO this should probably move to ppspp_chunk.erl eventually
unpack( <<?PPSPP_CHUNK_ADDRESSING_METHOD,
          Maybe_Method:?BYTE,
          Maybe_Options/binary >>, Options0) ->
    Method = case Maybe_Method of
                 0 -> chunk_32bit_bins;
                 1 -> chunk_64bit_bytes;
                 2 -> chunk_32bit_chunks;
                 3 -> chunk_64bit_bins;
                 4 -> chunk_64bit_chunks
             end,
    Options = orddict:store(chunk_addressing_method, Method, Options0),
    unpack(Maybe_Options, Options);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% discard window is nasty as it has variable size based on pre-conditions:
%% - the chunk addressing method must have been specified already
%% - the window size depends on the addressing method already used
%% so the requirement here is that these are specified in the already-
%% parsed options. Serendipitously we have those in the accumulator.
unpack( <<?PPSPP_LIVE_DISCARD_WINDOW, Maybe_Discard_Window/binary>>,
        Options0) ->
    Length = case orddict:fetch(chunk_addressing_method, Options0) of
                 chunk_32bit_bins   -> 32;
                 chunk_64bit_bytes  -> 64;
                 chunk_32bit_chunks -> 32;
                 chunk_64bit_bins   -> 64;
                 chunk_64bit_chunks -> 64
             end,
    << Window0:Length/big, Maybe_Options/binary >> = Maybe_Discard_Window,
    Window = case Window0 of
                 16#ffffffff         -> ppspp_live_discard_window_keep_all_chunks;
                 16#ffffffffffffffff -> ppspp_live_discard_window_keep_all_chunks;
                 _ -> Window0
             end,
    Options = orddict:store(ppspp_live_discard_window, Window, Options0),
    unpack(Maybe_Options, Options);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
unpack( <<?PPSPP_SUPPORTED_MESSAGES,
          Length:?BYTE,
          Maybe_Supported_Message_Types:Length/big,
          Maybe_Options/binary >>, Options0) ->
    Supported_Message_Types = case Maybe_Supported_Message_Types of
                                  %% TODO probably needs a custom bitmatching parser
                                  _ -> ppspp_supported_messages_not_yet_implemented
                              end,
    Options = orddict:store(ppspp_supported_message_types,
                            Supported_Message_Types, Options0),
    unpack(Maybe_Options, Options);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
unpack( <<?PPSPP_END_OPTION, Maybe_Messages/binary>>, Options) ->
    {Maybe_Messages, Options};
%% if PPPSPP_END_OPTION is not present in the binary, bad things** happen
unpack( <<>>, _Options) -> {error, ppspp_invalid_options}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc get/2 extracts a given parameter from the opaque options
%% <p> Provides a clean interface for other modules to retrieve PPSP options.
%% The options are:
%% <ul>
%% <li>chunk_addressing_method</li>
%% <li>ppspp_content_integrity_check_method</li>
%% <li>ppspp_merkle_hash_function</li>
%% <li>ppspp_minimum_version</li>
%% <li>ppspp_swarm_id</li>
%% <li>ppspp_version</li>
%% <ul>
%% </p>
%% @end
-spec get(option(), options()) -> any().

get(Option, {options, Options_Dict}) ->
    orddict:fetch(Option, Options_Dict).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc get_chunk_addressing_method/1 returns chunking method for the swarm
%% <p>Provides a clean interface for other modules to retrieve PPSP options.</p>
%% @end
-spec get_chunk_addressing_method(options()) -> any().

get_chunk_addressing_method(Options) ->
    get(chunk_addressing_method, Options).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc content_integrity_check_method/1 returns CIPM for the provided swarm
%% <p>Provides a clean interface for other modules to retrieve PPSP options.</p>
%% @end
-spec get_content_integrity_check_method(options()) -> any().

get_content_integrity_check_method(Options) ->
    get(content_integrity_check_method, Options).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc get_merkle_hash_function/1 returns the hash function for the provided swarm
%% <p>Provides a clean interface for other modules to retrieve PPSP options.</p>
%% @end
-spec get_merkle_hash_tree_function(options()) -> any().

get_merkle_hash_tree_function(Options) ->
    get(merkle_hash_tree_function, Options).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc get_minimum_version/1 returns the lowest accepted PPSP version for the swarm
%% <p>Provides a clean interface for other modules to retrieve PPSP options.</p>
%% @end
-spec get_minimum_version(options()) -> any().

get_minimum_version(Options) ->
    get(minimum_version, Options).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc get_swarm_id/1 returns the lowest accepted PPSP version for the swarm
%% <p>Provides a clean interface for other modules to retrieve PPSP options.</p>
%% @end
-spec get_swarm_id(options()) -> root_hash().

get_swarm_id(Options) ->
    get(swarm_id, Options).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc get_maximum_supported_/1 returns the highest accepted PPSP version
%% for the swarm.
%% <p>Provides a clean interface for other modules to retrieve PPSP options.</p>
%% @end
-spec get_maximum_supported_version(options()) -> any().

get_maximum_supported_version(Options) ->
    get(supported_version, Options).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc use_default_options/1 provides the options set within the PPSP draft
%% <p> Provides a clean interface for other modules to retrieve PPSP options.
%% Takes 1 parameter, a binary representing the root hash, or a string in hex.
%% The returned options and values are:
%% <ul>
%% <li>swarm_id: Root_Hash</li>
%% <li>chunk_addressing_method: chunk_32bit_chunks</li>
%% <li>content_integrity_check_method: merkle_hash_tree</li>
%% <li>merkle_hash_tree_function: sha</li>
%% <li>minimum_version: 1</li>
%% <li>version: 1</li>
%% <ul>
%% </p>
%% @end

-spec use_default_options(string() | root_hash()) -> options().
use_default_options(Hex_String) when is_list(Hex_String) ->
    use_default_options(convert:hex_string_to_padded_binary(Hex_String));
use_default_options(Root_Hash) when is_binary(Root_Hash) ->
    {options,
     orddict:from_list( [{swarm_id, Root_Hash},
                         {chunk_addressing_method, chunking_32bit_chunks},
                         {chunk_size, ?PPSPP_DEFAULT_CHUNK_SIZE},
                         {content_integrity_check_method, merkle_hash_tree},
                         {merkle_hash_tree_function, sha},
                         {minimum_version, ?PPSPP_RFC_VERSION},
                         {version, ?PPSPP_RFC_VERSION}])}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc pack an orddict of ppspp erlang terms into a binary PPSPP message segment
%% <p>  Deconstruct PPSPP UDP datagram into multiple erlang terms, including
%% <ul>
%% <li>orddicts for Transport</li>
%% <li>orddicts for Options</li>
%% <li>list of Messages</li>
%% <ul>
%% </p>
%% @end
-spec pack(options()) -> binary().

pack(_) -> <<>>.

-ifdef(TEST).
-spec defaults_test() -> term().
defaults_test() ->
    Hash ="c89800bfc82ed01ed6e3bfd5408c51274491f7d4",
    Root_Hash = convert:hex_string_to_padded_binary(Hash),
    Expected = {options,
                [{chunk_addressing_method,chunking_32bit_chunks},
                 {chunk_size,1024},
                 {content_integrity_check_method,merkle_hash_tree},
                 {merkle_hash_tree_function,sha},
                 {minimum_version,1},
                 {swarm_id, Root_Hash},
                 {version,1}]},
    [?_assertEqual( Expected, use_default_options(Root_Hash) )].
-endif.

