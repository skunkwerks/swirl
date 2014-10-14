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

%% @doc conversion library
%% <p>This module implements a set of functions to convert various
%% erlang or binary formats to human-readable text .</p>
%% @end

-module(convert).
-include("swirl.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% api
-export([bin_to_hex/1,
         bin_to_string/1,
         hex_string_to_padded_binary/1,
         port_to_atom/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% api

bin_to_hex(Binary) when is_binary(Binary) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || <<N>> <= Binary]).

bin_to_string(Binary) when is_binary(Binary) ->
    lists:flatten(["0x", bin_to_hex(Binary)]).

hex_string_to_padded_binary(String) when is_list(String) ->
    Bytes_Length = (length(String) + 1) div 2,
    {ok, [Int], []} = io_lib:fread("~16u", String),
    <<Int:Bytes_Length/big-unsigned-integer-unit:8>>.

port_to_atom(Port) when is_integer(Port), Port > 0, Port < 65535 ->
    Port_as_string = lists:flatten("swirl_peer_" ++
                                   io_lib:format("~4.16.0b", [Port])),
    list_to_atom(Port_as_string).
