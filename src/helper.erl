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

%% @author Dave Cottlehuber <dch@jsonified.com>
%% @doc Library for PPSPP over UDP
%% <p>This module implements a library of functions necessary to
%% handle the wire-protocol of PPSPP over UDP, including
%% functions for encoding and decoding.</p>
%% @end

-module(helper).
-include("swirl.hrl").

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([binary_to_hex/1, binary_to_string/1]).

binary_to_hex(Binary) when is_binary(Binary) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || <<N>> <= Binary]).

binary_to_string(Binary) when is_binary(Binary) ->
    lists:flatten(["0x", binary_to_hex(Binary)]).

