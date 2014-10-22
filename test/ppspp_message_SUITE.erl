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

-module(ppspp_message_SUITE).
-include("swirl.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([handshake/1]).

-spec all() -> [atom()].
all() -> [handshake].

-spec handshake(any()) -> true.
handshake(_Config) ->
    ct:comment("ensure handshake wire format matches erlang term based format"),
    {ok, [Traces]} = file:consult("../../test/data/handshakes.trace"),
    Root_Hash = <<200,152,0,191,200,46,208,30,214,227,191,213,64,140,81,
                  39,68,145,247,212>>,
    Test = fun({Raw, Expected}) ->
                   Expected = ppspp_message:unpack(Raw, Root_Hash) end,
    lists:map(Test, Traces).

