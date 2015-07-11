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

%% @doc Conversion Helper Functions
%%
%% This module implements a set of helper functions to convert various
%% erlang or binary formats to human-readable text.
%% @end

-module(convert).
-include("swirl.hrl").

%% api
-export([bin_to_hex/1,
         int_to_hex/1,
         bin_to_string/1,
         hex_string_to_padded_binary/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% api

-spec bin_to_hex(binary()) -> string().
bin_to_hex(Binary) when is_binary(Binary) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || <<N>> <= Binary]).

-spec int_to_hex(non_neg_integer()) -> string().
int_to_hex(Int) when is_integer(Int), Int >=0, Int =< 16#ffffffff ->
    lists:flatten([io_lib:format("0x~8.16.0b",[Int])]).

-spec bin_to_string(binary()) -> string().
bin_to_string(Binary) when is_binary(Binary) ->
    lists:flatten(["0x", bin_to_hex(Binary)]).

-spec hex_string_to_padded_binary(string()) -> binary().
hex_string_to_padded_binary(String) when is_list(String) ->
    Bytes_Length = (length(String) + 1) div 2,
    {ok, [Int], []} = io_lib:fread("~16u", String),
    <<Int:Bytes_Length/big-unsigned-integer-unit:8>>.
