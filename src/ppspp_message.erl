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
%% @doc Library for PPSPP over UDP, aka Swift protocol
%% <p>This module implements a library of functions necessary to
%% handle the wire-protocol of PPSPP over UDP, including
%% functions for encoding and decoding.</p>
%% @end

-module(ppspp_message).
-include("swerl.hrl").

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([unpack/1, pack/1,validate_message_type/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% external API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc unpack a datagram segment into a PPSPP message using erlang term format
%% <p>  Deconstruct PPSPP UDP datagram into multiple erlang terms, including
%% parsing any additional data within the same segment. Any parsing failure
%% is fatal & will propagate back to the attempted datagram unpacking.
%% <ul>
%% <li>Message type</li>
%% <li>orddict for Options</li>
%% <ul>
%% </p>
%% @end

%% message() = [
%% TODO revisit specs
%% {options, ppspp_options()},
%% {message_type, ppspp_message_type()}
%% ].

%%-spec unpack(binary() -> ppspp_message()).

unpack(Maybe_Messages) when is_binary(Maybe_Messages) ->
    unpack(Maybe_Messages, []).

%% if the binary is empty, all messages were parsed successfully
unpack( <<>>, Parsed_Messages) ->
    {ok, lists:reverse(Parsed_Messages)};
%% otherwise try to unpack another valid message, peeling off and parsing
%% recursively the remainder, accumulating valid (parsed) messages.
%% A failure anywhere in a message ultimately causes the entire datagram
%% to be rejected.
unpack(<<Maybe_Message_Type:?PPSPP_MESSAGE_SIZE, Rest/binary>>, Parsed_Messages) ->
    {ok, Type} = validate_message_type(Maybe_Message_Type),
    [{Type, Parsed_Message}, Maybe_More_Messages] = parse(Type, Rest),
    unpack(Maybe_More_Messages, [Parsed_Message | Parsed_Messages]);
unpack(_Maybe_Messages, _Rest) -> {error, ppspp_invalid_message}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle(_) -> {ok, state}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pack(_) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%-spec unpack(binary() -> ppspp_message_type()).
validate_message_type(Maybe_Message_Type) when is_integer(Maybe_Message_Type), Maybe_Message_Type < ?PPSPP_MAXIMUM_MESSAGE_TYPE ->
    %% message types in the current spec version
    Message_Type = case <<Maybe_Message_Type:?PPSPP_MESSAGE_SIZE>> of
        ?HANDSHAKE -> handshake;
        ?DATA -> data;
        ?ACK -> ack;
        ?HAVE -> have;
        ?INTEGRITY -> integrity;
        %?PEX_RESv4 -> pex_resv4;
        %?PEX_REQ -> pex_req;
        %?SIGNED_INTEGRITY -> signed_integrity;
        ?REQUEST -> request;
        ?CANCEL -> cancel;
        ?CHOKE -> choke;
        ?UNCHOKE -> unchoke;
        %?PEX_RESv6 -> pex_resv6;
        %?PEX_REScert -> pex_rescert;
        _  -> ppspp_message_type_not_yet_implemented
    end,
    ?DEBUG_SWERL("parser: valid message type ", Message_Type),
    {ok, Message_Type};
%% message types that are not acceptable eg peer is using more recent spec
validate_message_type(_Maybe_Message_Type) ->
    ?DEBUG_SWERL("parser: invalid message type ", _Maybe_Message_Type),
    {error, ppspp_message_type_not_recognised}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%-spec ... parse takes a msg_type, _data, and returns
%%    {error, something} or tagged tuple for the unpacked message
%%    [{Type, Parsed_Message}, Maybe_More_Messages]
%% TODO parse should probably be unpack/2 and then drop validate_message_type/1
parse(handshake, <<Channel:?PPSPP_CHANNEL_SIZE, Maybe_Options/binary>>) ->
    [{ok, Options}, Maybe_Messages] = ppspp_options:unpack(Maybe_Options),
    [{handshake, orddict:store(channel, Channel, Options) }, Maybe_Messages];

parse(data, _Rest) ->
    [{data, parsed_msg}, _Rest];
parse(ack, _Rest) ->
    [{ack, parsed_msg}, _Rest];
parse(have, _Rest) ->
    [{have, parsed_msg}, _Rest];
parse(integrity, _Rest) ->
    [{integrity, parsed_msg}, _Rest];
parse(pex_resv4, _Rest) ->
    [{pex_resv4, parsed_msg}, _Rest];
parse(pex_req, _Rest) ->
    [{pex_req, parsed_msg}, _Rest];
parse(signed_integrity, _Rest) ->
    [{signed_integrity, parsed_msg}, _Rest];
parse(request, _Rest) ->
    [{request, parsed_msg}, _Rest];
parse(cancel, _Rest) ->
    [{cancel, parsed_msg}, _Rest];
parse(choke, _Rest) ->
    [{choke, parsed_msg}, _Rest];
parse(unchoke, _Rest) ->
    [{unchoke, parsed_msg}, _Rest];
parse(pex_resv6, _Rest) ->
    [{pex_resv6, parsed_msg}, _Rest];
parse(pex_rescert, _Rest) ->
    [{pex_rescert, parsed_msg}, _Rest];
parse(ppspp_message_type_not_yet_implemented, _Rest) ->
    [{ppspp_message_parser_not_implemented, parsed_msg}, _Rest];
%% TODO confirm we shouldn't be able to get here by using -spec()
parse(_, _Rest) ->
    {error, ppspp_message_type_not_parsable}.
