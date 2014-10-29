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
%% handle the wire-protocol of PPSPP over UDP, including
%% functions for encoding and decoding messages.</p>
%% @end

-module(ppspp_chunk).
-include("swirl.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-spec test() -> term().
-endif.

% api
-export([unpack/2,
         pack/2]).

-export_type([addressing_method/0,
              spec/0,
              chunk_range/0,
              byte_range/0,
              bin_number/0]).

-type addressing_method() ::
chunk_32bit_bins
| chunk_64bit_bins
| chunk_64bit_bytes
| chunk_32bit_chunks
| chunk_64bit_chunks.

-opaque spec() :: {chunk_spec, chunk_range() | byte_range() | bin_number()}.
-opaque chunk_range() ::
{ chunk_32bit_chunks, uint_32bit(), uint_32bit()}
| { chunk_64bit_chunks, uint_64bit(), uint_64bit()}.
-opaque byte_range() ::
{ chunk_64bit_bytes,  uint_64bit(), uint_64bit()}.
-opaque bin_number() ::
{ chunk_32bit_bins, uint_32bit()}
| { chunk_64bit_bins, uint_64bit()}.

-type uint_32bit() :: 0..16#ffffffff.
-type uint_64bit() :: 0..16#ffffffffffffffff.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% api
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc unpack a chunk spec from a message, typically a have message
%% <p>  Deconstruct PPSPP UDP datagram into multiple erlang terms, including
%% parsing any additional data within the same segment. Any parsing failure
%% is fatal & will propagate back to the attempted datagram unpacking.
%% </p>
%% @end

%% TODO add uint32/64 0x7fffffff / 0xffffffff indicators for all/no chunks

-spec unpack(addressing_method(), binary()) -> {spec(), binary()}.
%% bin numbers use a single field
unpack(chunk_32bit_bins, <<Bin_Number:?DWORD, Rest/binary>>) ->
    {{chunk_spec, {chunk_32bit_bins, Bin_Number}}, Rest};
unpack(chunk_64bit_bins, <<Bin_Number:?QWORD, Rest/binary>>) ->
    {{chunk_spec, {chunk_64bit_bins, Bin_Number}}, Rest};
% others have a start and end range
unpack(chunk_64bit_bytes, Range) ->
    {Start, End, Rest} = unpack_uint_64bits(Range),
    {{chunk_spec, {chunk_64bit_bytes, Start, End}}, Rest};
%% now the small and large chunk schemes
unpack(chunk_64bit_chunks, Range) ->
    {Start, End, Rest} = unpack_uint_64bits(Range),
    {{chunk_spec, {chunk_64bit_chunks, Start, End}}, Rest};
unpack(chunk_32bit_chunks, Range) ->
    {Start, End, Rest} = unpack_uint_32bits(Range),
    {{chunk_spec, {chunk_32bit_chunks, Start, End}}, Rest}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TODO pack
-spec pack(spec(), addressing_method()) -> binary().
pack(_Chunk_Spec, _Addressing_Method) -> <<>>.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal

-spec unpack_uint_32bits(binary()) -> {uint_32bit(), uint_32bit(), binary()}.
unpack_uint_32bits(<<Uint1:?DWORD, Uint2:?DWORD, Rest/binary>>) ->
    { Uint1, Uint2, Rest}.

-spec unpack_uint_64bits(binary()) -> {uint_64bit(), uint_64bit(), binary()}.
unpack_uint_64bits(<<Uint1:?QWORD, Uint2:?QWORD, Rest/binary>>) ->
    { Uint1, Uint2, Rest}.
