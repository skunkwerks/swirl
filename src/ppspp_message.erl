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

-module(ppspp_message).
%-include("ppspp.hrl").
-include("ppspp_records.hrl").
-include("swirl.hrl").

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

%% api
-export([unpack/1, 
         pack/1,
         validate_message_type/1, 
         handle/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% api
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
    [{ok, Parsed_Message}, Maybe_More_Messages] = parse(Type, Rest),
    unpack(Maybe_More_Messages, [Parsed_Message | Parsed_Messages]);
unpack(_Maybe_Messages, _Rest) -> {error, ppspp_invalid_message}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pack(_) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% private
%%-spec unpack(binary() -> ppspp_message_type()).
validate_message_type(Maybe_Message_Type)
  when is_integer(Maybe_Message_Type),
       Maybe_Message_Type < ?PPSPP_MAXIMUM_MESSAGE_TYPE ->
    %% message types in the current spec version
    Message_Type = case <<Maybe_Message_Type:?PPSPP_MESSAGE_SIZE>> of
                       ?HANDSHAKE -> handshake;
                       ?DATA -> data;
                       ?ACK -> ack;
                       ?HAVE -> have;
                       ?INTEGRITY -> integrity;
                       ?PEX_RESv4 -> pex_resv4;
                       ?PEX_REQ -> pex_req;
                       ?SIGNED_INTEGRITY -> signed_integrity;
                       ?REQUEST -> request;
                       ?CANCEL -> cancel;
                       ?CHOKE -> choke;
                       ?UNCHOKE -> unchoke;
                       ?PEX_RESv6 -> pex_resv6;
                       ?PEX_REScert -> pex_rescert;
                       _  -> ppspp_message_type_not_yet_implemented
                   end,
    ?DEBUG("message: parser got valid message type ~p~n", [Message_Type]),
    {ok, Message_Type};
%% message types that are not acceptable eg peer is using more recent spec
validate_message_type(_Maybe_Message_Type) ->
    ?DEBUG("message: parser got invalid message type ~p~n", [_Maybe_Message_Type]),
    {error, ppspp_message_type_not_recognised}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%-spec ... parse takes a msg_type, _data, and returns
%%    {error, something} or {ok, {key, orddict}} for the unpacked message
%%    [{Type, Parsed_Message}, Maybe_More_Messages]
%% TODO parse should probably be unpack/2 and then drop validate_message_type/1
parse(handshake, <<Channel:?PPSPP_CHANNEL_SIZE, Maybe_Options/binary>>) ->
    [{ok, Options}, Maybe_Messages] = ppspp_options:unpack(Maybe_Options),
    [{ok, {handshake, orddict:store(channel, Channel, Options) }}, Maybe_Messages];

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%-spec ... handle takes a tuple of {type, message_body} where body is a
%%    parsed orddict message and returns either
%%    {error, something} or tagged tuple for the unpacked message
%%    {ok, reply} where reply is probably an orddict to be sent to the
%%    alternate peer.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  The payload of the HANDSHAKE message is a channel ID (see
%  Section 3.11) and a sequence of protocol options.  Example options
%  are the content integrity protection scheme used and an option to
%  specify the swarm identifier.  The complete set of protocol options
%  are specified in Section 7.
handle(Type, {handshake,_Payload}, State) ->
    {ok, HANDSHAKE} = prepare(Type, {handshake, orddict:new()}, State),
    {ok, HAVE}      = prepare(Type, {have, orddict:new()}, State),
    {ok, [HANDSHAKE, HAVE]};

%% handles HANDSHAKE received by leecher.
%% TODO leecher type not implemented in the record yet
handle(leecher, {handshake,_Payload},_State) ->
    ok;

handle(_Type, {ack,_Payload},_State) ->
    %% TODO store the ACKed packets SOMEWHERE !
    ok;

handle(_Type, {have, _Payload},_State) ->
    %% TODO figure out what range to request.
    ok;

handle(_Type, {integrity, _Payload},_State) ->
    %% TODO figure out how to handle integrity messages. The leecher will
    %% recevive integrity message which cn spn multiple chunks so we need to
    %% piggybck integrity messages untill the DATA the right data packet
    %% arrives 
    ok;

handle(live, {signed_integrity, _Payload},_State) ->

    ok;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% REQUEST
handle(static, {request, [_Start, _End]},_State) ->
    %handle_REQ_List({static, State}, REQ_List, []);
    ok;
handle(live,   {request, [Start, End]}, State) ->
    Start_Munro    = mtree_core:get_munro_root(Start),
    End_Munro      = mtree_core:get_munro_root(End),
    {ok, REQ_List} = get_bin_list(Start_Munro, End_Munro, {Start, End}, []),
    %% REQ_List : [ Munro_Root1, bins ... , Munro_Root2, bins ..]
    %% the bins that follow the Munro_Root lie in the range of the Munro_Root.
    {ok, handle_REQ_List({live, State}, REQ_List, [])};

handle(_Type, Message,_State) ->
    ?DEBUG("message: handler not yet implemented ~p~n", [Message]),
    {ok, ppspp_message_handler_not_yet_implemented}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL INTERNAL FUNCTION
%% handle_REQ_List/3 : REQ_List and return value will be as follows 
%% REQ_List (live): [Munro_Root1, Leaf_bin ... , Munro_Root2, Leaf_bins ..] 
%% Returns (live) : [INTEGRITY, SIGNED_INTEGRITY, INTEGRITY, DATA, ...] messages
%% REQ_List (static) : [Leaf_bin, ...] 
%% Returns (static)  : [INTEGRITY, DATA, INTEGRITY, DATA, ...] messages
handle_REQ_List(_, [], Acc) ->
    lists:reverse(Acc);
handle_REQ_List({_Type, State}, [Leaf | Rest], Acc) when Leaf rem 2 =:=0 ->
    {ok, INTEGRITY} = prepare(live, {integrity, Leaf, orddict:new()}, State),
    {ok, DATA}      = prepare(live, {data, Leaf}, State), 
    handle_REQ_List({live, State}, Rest, [DATA, INTEGRITY | Acc]);
handle_REQ_List({live, State}, [Munro_Root | Rest], Acc) ->
    {ok, INTEGRITY} = prepare(live, {integrity, Munro_Root,
                                     orddict:new()}, State),
    {ok, SIGNED_INTEGRITY} = prepare(live, {signed_integrity, Munro_Root,
                                            orddict:new()}, State),
    handle_REQ_List({live, State}, Rest, [SIGNED_INTEGRITY, INTEGRITY | Acc]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL INTERNAL FUNCTION
%% return list of bins : [Munro_Root1, bins ... , Munro_Root2, bins ..]
%% the bins that follow the Munro_Root lie in the range of the Munro_Root.
get_bin_list(End_Munro, End_Munro,   {Start, End}, Acc) ->
    {ok, lists:concat([Acc, [End_Munro | lists:seq(Start, End, 2)]])};
get_bin_list(Start_Munro, End_Munro, {Start, End}, Acc) ->
    [_, Last] = mtree_core:bin_to_range(Start_Munro),
    New_Acc   = lists:concat([Acc, [Start_Munro | lists:seq(Start, Last, 2)]]),
    {ok, Munro_Root} = mtree:get_next_munro(Start_Munro),
    get_bin_list(Munro_Root, End_Munro, {Last+2, End}, New_Acc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% HANDSHAKE message is of the sort :
%% {handshake, [{channel, }, {options, [{}, {}, ...]}] 
prepare(Type, {handshake,_Payload}, State) ->
    Sub_Options =
      [ {ppspp_swarm_id, State#state.ppspp_swarm_id},
        {ppspp_version, State#state.ppspp_version},
        {ppspp_minimum_version, State#state.ppspp_minimum_version},
        {ppspp_chunking_method, State#state.ppspp_chunking_method},
        {ppspp_integrity_check_method,
         State#state.ppspp_integrity_check_method},
        {ppspp_merkle_hash_function, State#state.ppspp_merkle_hash_function}],

    %% Add LIVE streaming options incase the seeder is in live stream swarm.
    Options_List =
    if
        Type =:= live orelse Type =:= injector ->
            [ {ppspp_live_signature_algorithm,
               State#state.ppspp_live_signature_algorithm },
              {ppspp_live_disard_window,
               State#state.ppspp_live_discard_window} | Sub_Options];
        true -> Sub_Options 
    end,

    Options = orddict:from_list(Options_List),
    %% TODO : allocate free channel & store it in Payload as source channel id
    %% TODO : discuss how to get the free Channel id
    Channel  = not_implemented,
    {ok, {handshake, orddict:from_list([{channel, Channel},
                                        {options, Options}])}};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ACK msg 
%% the ACK message is of the sort : {ack, [{range, [{Start,End}]}] 
prepare(_Type, {ack, [Start, End]}, _State) ->
    {ok, {ack, orddict:from_list(range, [{Start, End}])}};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% REQUEST msg 
%% the REQUEST message is of the sort : {request, [{range, [{Start,End}]}] 
prepare(_Type, {request, [Start, End]}, _State) ->
    {ok, {request, orddict:from_list([{range, [{Start, End}]}])}};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HAVE
%% Prepre HAVE message for sttic and live stream
%% the HAVE message is of the sort : {have, [{range, [{Start,End}]}] 
prepare(static, {have,_Payload}, State) ->
    %% NOTE : incase the data range is not continous get_data_range will return
    %% wrong output
    {ok, Start, End} = mtree:get_data_range(State#state.mtree),
    {ok, {have, orddict:from_list(range, [{Start, End}])}};
prepare(_Live,  {have,_Payload}, State) ->
    {ok, Munro_Root, _} = mtree:get_latest_munro(State#state.mtree),
    [Start, End]        = mtree_core:bin_to_range(Munro_Root),
    {ok, {have, orddict:from_list(range, [{Start, End}])}};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% REQUEST msg 
%% the REQUEST message is of the sort : {request, [{range, [{Start,End}]}] 
prepare(_Type, {request, [Start, End]}, _State) ->
    {ok, {request, orddict:from_list(range, [{Start, End}])}};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% DATA msg : {data, [{range, }, {timestamp, int}, {data, binary}]} 
prepare(_Type, {data, Chunk_ID}, State) ->
    {ok, _Hash, Data} = mtree_store:lookup(State#state.mtree, Chunk_ID),
    %% TODO : figure out what the Start and End range mean in the specs.
    Range = not_implemented,
    %% DATA msg must contain the Chunk_ID of the chunk.
    %% TODO : add ntp module and filter out the timestamp.
    Ntp_Timestamp = time, %% ntp:ask(), 
    {ok, {data, orddict:from_list([{range, Range},
                                   {timestamp, Ntp_Timestamp},
                                   {data, Data}])}};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INTEGRITY msg : {integrity, [{range, }, {hash, binary}] 
%% TODO : decide if instead of Bin we need to use Range !
prepare(_Type, {integrity, Bin}, State) ->
    {ok, Hash, _Data} = mtree_store:lookup(State#state.mtree, Bin),
    Range = not_implemented,
    {ok, {integrity, orddict:from_list([{range, Range}, {hash, Hash}])}};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SIGNED_INTEGRITY msg : {signed_integrity, [{range, },
%%                                            {timestamp, Time},
%%                                            {signature, binary}] 
prepare(live, {signed_integrity, Munro_Root}, State) ->
    {ok, _Hash, _Data} = mtree_store:lookup(State#state.mtree, Munro_Root),
    Range = not_implemented,
    %% TODO : add ntp module and filter out the timestamp.
    Ntp_Timestamp = time, %% ntp:ask(), 
    Signture      = not_implemented,
    {ok, {data, orddict:from_list([{range, Range},
                                   {timestamp, Ntp_Timestamp},
                                   {signature, Signture}])}}.

