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

%% @doc Provides opaque wrapper around PPSPP options, including wire encoding
%% and decoding.
%%
%% This module implements opaque types and accessor functions necessary to
%% handle parsing PPSPP options wire-protocol of PPSPP over UDP, including
%% functions for encoding and decoding.

%% This is by and large a translation from the PPSPP RFC into Erlang.
%% @end

-module(ppspp_options).
-include("swirl.hrl").

%% api
-export([unpack/1,
         pack/1,
         get_chunk_addressing_method/1,
         get_content_integrity_check_method/1,
         get_merkle_hash_tree_function/1,
         get_minimum_version/1,
         get_chunk_size/1,
         get_swarm_id/1,
         get_maximum_supported_version/1,
         get_options/1,
         use_minimum_options/0,
         use_default_options/0,
         use_default_options/1]).

%% types largely as defined in PPSPP RFC

-export_type([swarm_id/0,
              merkle_tree_hash_function/0,
              content_integrity_protection_method/0,
              options/0,
              option/0]).

%% @type swarm_id() = binary(). TODO could be restricted further.
-opaque swarm_id() :: binary().

%% @type options() = #{ option() => any()}. TODO could be restricted further.
-opaque options() :: #{ option() => any()}.

%% @type option() = supported_version
%% | minimum_version
%% | swarm_id
%% | content_integrity_check_method
%% | merkle_hash_tree_function
%% | live_signature_algorithm
%% | live_discard_window
%% | supported_messages
%% | end_option
%% | default_chunk_size
%% | chunk_addressing_method.
-opaque option() :: supported_version
| minimum_version
| swarm_id
| content_integrity_check_method
| merkle_hash_tree_function
| live_signature_algorithm
| chunk_addressing_method
| live_discard_window
| supported_messages
| end_option
| default_chunk_size
| chunk_addressing_method.

%% Matches a subset of types from http://erlang.org/doc/man/crypto.html
%%  digest_type() =  md5 | sha | sha224 | sha256 | sha384 | sha512, and
%%  hash_algorithms() = md5 | ripemd160 | sha | sha224 | sha256 | sha384 | sha512
%% @type merkle_tree_hash_function() = sha | sha224 | sha256 | sha384 | sha512.
-opaque merkle_tree_hash_function() :: sha | sha224 | sha256 | sha384 | sha512.

%% @type content_integrity_protection_method() = no_integrity_protection
%% | merkle_hash_tree
%% | sign_all
%% | unified_merkle_hash_tree.
-opaque content_integrity_protection_method() :: no_integrity_protection
| merkle_hash_tree
| sign_all
| unified_merkle_hash_tree.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Unpack PPPSPP options as encoded in wire format into erlang terms.
-spec unpack(binary()) -> {binary(), options()} | {error, any()}.
unpack(Maybe_Options) ->
    {Maybe_Messages, Options} = unpack(Maybe_Options, #{}),
    {Maybe_Messages, Options}.
-spec unpack(binary(), options()) -> { binary(), options()}.
unpack( <<?PPSPP_SUPPORTED_VERSION,
          Version:?BYTE,
          Maybe_Options/binary >>, Options) ->
    unpack(Maybe_Options, Options#{supported_version => Version});
unpack( <<?PPSPP_MINIMUM_VERSION,
          Version:?BYTE,
          Maybe_Options/binary >>, Options) ->
    unpack(Maybe_Options, Options#{minimum_version => Version});
unpack( <<?PPSPP_SWARM_ID_LENGTH,
          Length:?WORD,
          Swarm_ID:Length/binary,
          Maybe_Options/binary >>, Options) ->
    unpack(Maybe_Options, Options#{swarm_id => Swarm_ID});
unpack( <<?PPSPP_INTEGRITY_CHECK_METHOD,
          Maybe_Method:?BYTE,
          Maybe_Options/binary >>, Options) ->
    Method = case Maybe_Method of
                 0 -> no_integrity_protection;
                 1 -> merkle_hash_tree;
                 2 -> sign_all;
                 3 -> unified_merkle_hash_tree
             end,
    unpack(Maybe_Options, Options#{content_integrity_check_method => Method});
unpack( <<?PPSPP_MERKLE_HASH_FUNCTION,
          Maybe_Hash_Function:?BYTE,
          Maybe_Options/binary >>, Options) ->
    Hash_Function = case Maybe_Hash_Function of
                        0 -> sha;
                        1 -> sha224;
                        2 -> sha256;
                        3 -> sha384;
                        4 -> sha512
                    end,
    unpack(Maybe_Options, Options#{merkle_hash_function => Hash_Function});
unpack( <<?PPSPP_LIVE_SIGNATURE_ALGORITHM,
          Maybe_Algorithm:?BYTE,
          Maybe_Options/binary >>, Options) ->
    Algorithm = case Maybe_Algorithm of
                    %% TODO we can parse this yet but need normative algorithm names
                    0 -> live_signature_algorithm_not_yet_implemented
                end,
    unpack(Maybe_Options, Options#{merkle_hash_tree_algorithm => Algorithm});
%% TODO this should probably move to ppspp_chunk.erl eventually
unpack( <<?PPSPP_CHUNK_ADDRESSING_METHOD,
          Maybe_Method:?BYTE,
          Maybe_Options/binary >>, Options) ->
    Method = case Maybe_Method of
                 0 -> chunk_32bit_bins;
                 1 -> chunk_64bit_bytes;
                 2 -> chunk_32bit_chunks;
                 3 -> chunk_64bit_bins;
                 4 -> chunk_64bit_chunks
             end,
    unpack(Maybe_Options, Options#{chunk_addressing_method => Method});
%% discard window is nasty as it has variable size based on pre-conditions:
%% - the chunk addressing method must have been specified already
%% - the window size depends on the addressing method already used
%% so the requirement here is that these are specified in the already-
%% parsed options. Serendipitously we have those in the accumulator.
unpack( <<?PPSPP_LIVE_DISCARD_WINDOW, Maybe_Discard_Window/binary>>,
        #{chunk_addressing_method := Chunking_Method} = Options) ->
    Length = case Chunking_Method of
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
    unpack(Maybe_Options, Options#{ppspp_live_discard_window => Window});
unpack( <<?PPSPP_SUPPORTED_MESSAGES,
          Length:?BYTE,
          Maybe_Supported_Message_Types:Length/big,
          Maybe_Options/binary >>, Options) ->
    Supported_Message_Types = case Maybe_Supported_Message_Types of
                                  %% TODO probably needs a custom bitmatching parser
                                  _ -> ppspp_supported_messages_not_yet_implemented
                              end,
    unpack(Maybe_Options,
           Options#{ppspp_supported_message_types => Supported_Message_Types});
unpack( <<?PPSPP_END_OPTION, Maybe_Messages/binary>>, Options) ->
    {Maybe_Messages, Options};
%% if PPPSPP_END_OPTION is not present in the binary, bad things** happen as
%% we run over the end of the buffer consuming whatever else is after options.
unpack( <<>>, _Options) -> {error, ppspp_invalid_options}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Returns chunking method for the swarm
-spec get_chunk_addressing_method(options()) -> any().
get_chunk_addressing_method(Options) ->
    maps:get(chunk_addressing_method, Options).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Returns CIPM for the provided swarm
-spec get_content_integrity_check_method(options()) -> any().
get_content_integrity_check_method(Options) ->
    maps:get(content_integrity_check_method, Options).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Returns the hash function for the provided swarm
-spec get_merkle_hash_tree_function(options()) -> any().
get_merkle_hash_tree_function(Options) ->
    maps:get(merkle_hash_tree_function, Options).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Returns the lowest accepted PPSP version for the swarm.
-spec get_minimum_version(options()) -> any().
get_minimum_version(Options) ->
    maps:get(minimum_version, Options).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Returns the embedded swarm_id.
-spec get_swarm_id(options()) -> swarm_id().
get_swarm_id(Options) ->
    maps:get(swarm_id, Options).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Returns the swarm chunk size
-spec get_chunk_size(options()) -> pos_integer().
get_chunk_size(Options) ->
    maps:get(chunk_size, Options).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Returns the highest accepted PPSP version for the swarm.
-spec get_maximum_supported_version(options()) -> any().
get_maximum_supported_version(Options) ->
    maps:get(supported_version, Options).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc helper unwrapper to pull out components from a datagram map
%% @end
%% FIXME this can probably be deleted in its entirety
-spec get_options(#{}) -> options().
get_options(#{options := Options}) -> Options.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Provides standard options from the PPSPP RFC as erlang terms.
%%
%% Most users and programmers will take the standard options from the RFC.
%% Takes 1 optional parameter, a binary representing the root hash, or a string
%% in hex. Blank swarm options are used for starting peer_workers that may
%% handle many peer swarms concurrently on different channels. It returns:
%% <pre lang="erlang">
%% options => #{
%%        chunk_addressing_method => chunking_32bit_chunks,
%%        chunk_size => 1024,
%%        content_integrity_check_method => merkle_hash_tree,
%%        merkle_hash_tree_function => sha256,
%%        minimum_version => 1,
%%        supported_version => 1,
%%        swarm_id => << ... >>}.
%% </pre>
-spec use_default_options() -> options().
use_default_options() ->
    use_default_options(<<>>).

%% @doc Provides standard options from the PPSPP RFC as erlang terms.
-spec use_default_options(string() | swarm_id()) -> options().
use_default_options(Hex_String) when is_list(Hex_String) ->
    use_default_options(convert:hex_string_to_padded_binary(Hex_String));
use_default_options(Swarm_id) when is_binary(Swarm_id) ->
    #{swarm_id                       => Swarm_id,
      chunk_addressing_method        => ?PPSPP_DEFAULT_CHUNK_METHOD,
      chunk_size                     => ?PPSPP_DEFAULT_CHUNK_SIZE,
      content_integrity_check_method => ?PPSPP_DEFAULT_INTEGRITY_METHOD,
      merkle_hash_tree_function      => ?PPSPP_DEFAULT_MERKLE_HASH_FUN,
      minimum_version                => ?PPSPP_RFC_VERSION,
      supported_version              => ?PPSPP_RFC_VERSION}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Provides standard options from the PPSPP RFC as erlang terms.
%% This function provides the minimum set of ppspp options to use during
%% parsing of handshake messages. In all other message types, one can
%% expect a full set of valid options to be retrieved from the mutually
%% agreed swarm options that are assigned to this particular swarm / channel.
-spec use_minimum_options() -> options().
use_minimum_options() ->
    #{minimum_version => ?PPSPP_RFC_VERSION}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% <ul>
%% @doc Pack a map of erlang terms into a binary PPSPP message segment
-spec pack(options()) -> binary().

pack(_) -> <<>>.
