%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=4 sw=4 et
% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

%% @doc Library for PPSPP over UDP, aka Swift protocol
%% <p>This module implements a library of functions necessary to
%% handle parsing PPSPP options wire-protocol of PPSPP over UDP, including
%% functions for encoding and decoding.</p>
%% @end

-module(ppspp_options).

-include("ppspp.hrl").

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([unpack/1,
         pack/1,
         defaults/1,
         get/2]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc unpack a UDP packet into a PPSPP datagram using erlang term format
%% <p>  Deconstruct PPSPP UDP datagram into multiple erlang terms, including
%% <ul>
%% <li>orddicts for Transport</li>
%% <li>orddicts for Options</li>
%% <li>list of Messages</li>
%% <ul>
%% A single datagram MAY contain multiple PPSPP messages; these will be handled
%% recursively as needed.
%% TODO unpack supported messages.
%% </p>
%% @end

%% packet() = [
%% TODO revisit specs
%% {transport, ppspp_transport()},
%% {options, ppspp_options()},
%% {messages, ppspp_messages()}
%% ].
%% TODO add semantic checks Version =< ?SWIRL_MAX_PPSPP_VERSION and similar

%%-spec unpack(binary() -> ppspp_options()).
%% ppspp_options is an orddict.
%% [{ok, Options}, Maybe_Messages] = ppspp_options:unpack(Maybe_Options),
%% unpack ( <<more_options, Rest>>, orddict) -> [{ok, Options}, More_Options].

unpack(Maybe_Options) ->
    [Options, Maybe_Messages] = unpack(Maybe_Options, orddict:new()),
    Options_in_a_Dict = orddict:store(options, Options, orddict:new()),
    [{ok, Options_in_a_Dict }, Maybe_Messages].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
unpack( <<?PPSPP_VERSION,
          Version:?BYTE,
          Maybe_Options/binary >>, Options0) ->
    Options = orddict:store(ppspp_version, Version, Options0),
    unpack(Maybe_Options, Options);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
unpack( <<?PPSPP_MINIMUM_VERSION,
          Version:?BYTE,
          Maybe_Options/binary >>, Options0) ->
    Options = orddict:store(ppspp_minimum_version, Version, Options0),
    unpack(Maybe_Options, Options);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
unpack( <<?PPSPP_SWARM_ID_LENGTH,
          Length:?WORD,
          Swarm_ID:Length/binary,
          Maybe_Options/binary >>, Options0) ->
    Options = orddict:store(ppspp_swarm_id, Swarm_ID, Options0),
    unpack(Maybe_Options, Options);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
unpack( <<?PPSPP_INTEGRITY_CHECK_METHOD,
          Maybe_Method:?BYTE,
          Maybe_Options/binary >>, Options0) ->
    Method = case Maybe_Method of
                 0 -> ppspp_no_integrity_protection;
                 1 -> ppspp_merkle_hash_tree;
                 2 -> ppspp_sign_all;
                 3 -> ppspp_unified_merkle_hash_tree;
                 _ -> ppspp_invalid_content_integrity_protection_method
             end,
    Options = orddict:store(ppspp_content_integrity_check_method, Method, Options0),
    unpack(Maybe_Options, Options);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
unpack( <<?PPSPP_MERKLE_HASH_FUNCTION,
          Maybe_Hash_Function:?BYTE,
          Maybe_Options/binary >>, Options0) ->
    Hash_Function = case Maybe_Hash_Function of
                        0 -> ppspp_sha1;
                        1 -> ppspp_sha224;
                        2 -> ppspp_sha256;
                        3 -> ppspp_sha384;
                        4 -> ppspp_sha512;
                        _ -> ppspp_merkle_hash_function_invalid
                    end,
    Options = orddict:store(ppspp_merkle_hash_function, Hash_Function, Options0),
    unpack(Maybe_Options, Options);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
unpack( <<?PPSPP_LIVE_SIGNATURE_ALGORITHM,
          Maybe_Algorithm:?BYTE,
          Maybe_Options/binary >>, Options0) ->
    Algorithm = case Maybe_Algorithm of
                    %% TODO we can parse this yet but need normative algorithm names
                    0 -> ppspp_live_signature_algorithm_not_yet_implemented;
                    _ -> ppspp_live_signature_algorithm_invalid
                end,
    Options = orddict:store(ppspp_merkle_hash_algorithm, Algorithm, Options0),
    unpack(Maybe_Options, Options);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
unpack( <<?PPSPP_CHUNK_ADDRESSING_METHOD,
          Maybe_Method:?BYTE,
          Maybe_Options/binary >>, Options0) ->
    Method = case Maybe_Method of
                 0 -> ppspp_chunking_32bit_bins;
                 1 -> ppspp_chunking_64bit_bytes;
                 2 -> ppspp_chunking_32bit_chunks;
                 3 -> ppspp_chunking_64bit_bins;
                 4 -> ppspp_chunking_64bit_chunks;
                 _ -> ppspp_chunking_invalid
             end,
    Options = orddict:store(ppspp_chunking_method, Method, Options0),
    unpack(Maybe_Options, Options);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% discard window is nasty as it has variable size based on pre-conditions:
%% - the chunk addressing method must have been specified already
%% - the window size depends on the addressing method already used
%% so the requirement here is that these are specified in the already-
%% parsed options. Serendipitously we have those in the accumulator.
unpack( <<?PPSPP_LIVE_DISCARD_WINDOW, Maybe_Discard_Window/binary>>,
        Options0) ->
    Length = case orddict:fetch(ppspp_chunking_method, Options0) of
                 ppspp_chunking_32bit_bins   -> 32;
                 ppspp_chunking_64bit_bytes  -> 64;
                 ppspp_chunking_32bit_chunks -> 32;
                 ppspp_chunking_64bit_bins   -> 64;
                 ppspp_chunking_64bit_chunks -> 64;
                 _ -> ppspp_live_discard_window_missing_or_invalid_chunking_method
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
    [Options, Maybe_Messages];
%% if PPPSPP_END_OPTION is not present in the binary, bad things** happen
unpack( <<>>, _Options) -> {error, ppspp_options_invalid}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc get/2 extracts a given parameter from the opaque options
%% <p> Provide a clean interface for other modules to retrieve PPSP options.
%% The options are:
%% <ul>
%% <li>ppspp_chunking_method</li>
%% <li>ppspp_content_integrity_check_method</li>
%% <li>ppspp_merkle_hash_function</li>
%% <li>ppspp_minimum_version</li>
%% <li>ppspp_swarm_id</li>
%% <li>ppspp_version</li>
%% <ul>
%% </p>
%% @end
%%-spec get(ppspp_options(), orddict()) -> ppspp_options()).
get(Option, Options) ->
    orddict:fetch(Option, Options).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc defaults/1 provides the options set within the PPSP draft
%% <p> Provide a clean interface for other modules to retrieve PPSP options.
%% Takes 1 parameter, a binary representing the root hash.
%% The returned options and values are:
%% <ul>
%% <li>ppspp_swarm_id: Root_Hash</li>
%% <li>ppspp_chunking_method: ppspp_chunking_32bit_chunks</li>
%% <li>ppspp_content_integrity_check_method: ppspp_merkle_hash_tree</li>
%% <li>ppspp_merkle_hash_function: ppspp_sha1</li>
%% <li>ppspp_minimum_version: 1</li>
%% <li>ppspp_version: 1</li>
%% <ul>
%% </p>
%% @end
%%-spec defaults(ppspp_hash()) -> orddict()).

defaults(Root_Hash) when is_binary(Root_Hash) ->
    orddict:from_list( [{ppspp_swarm_id, Root_Hash},
                        {ppspp_chunking_method, ppspp_chunking_32bit_chunks},
                        {ppspp_chunk_size, ?PPSPP_DEFAULT_CHUNK_SIZE},
                        {ppspp_content_integrity_check_method, ppspp_merkle_hash_tree},
                        {ppspp_merkle_hash_function, ppspp_sha1},
                        {ppspp_minimum_version, ?PPSPP_CURRENT_VERSION},
                        {ppspp_version, ?PPSPP_CURRENT_VERSION}]).

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
%%-spec unpack(binary() -> ppspp_options()).
pack(_) -> ok.
