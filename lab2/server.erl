-module(server).
-export([start/1,stop/1, handler/2, channelStart/1, handlerChannel/2]).

-record(server_st, {
    channels
}).

initial_state() ->
    #server_st{
        channels = []
    }.

-record(channelstate, {
    users
}).

channel_init() ->
    #channelstate{
        users = []
    }.

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    genserver:start(ServerAtom, initial_state(), fun handler/2).

handler(State, {join, Channel, Nick, Pid}) ->

    case lists:member(Channel, State#server_st.channels) of
        false -> 
            ChannelPID = channelStart(Channel),
            genserver:request(ChannelPID, {join, Channel, Nick, Pid}),
            NewCh = lists:append(Channel, State#server_st.channels),
            {reply, ok, State#server_st{channels = NewCh}};
        true -> 
           genserver:request(list_to_atom(Channel), {join, Channel, Nick, Pid}),
           {reply, ok, State}
    end.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    genserver:stop(ServerAtom).

channelStart(ChannelName) ->
    genserver:start(list_to_atom(ChannelName), channel_init(), fun handlerChannel/2).

handlerChannel(State, {join, Channel, Nick, Pid}) ->
    InChannel = lists:member(Pid, State#channelstate.users),

    case InChannel of
        true -> {reply, {error, user_already_joined, "User already in channel"}, State};
        false -> 
            NewUs = lists:append([Pid], State#channelstate.users),
            {reply, ok, State#channelstate{users = NewUs}}
    end;

handlerChannel(State, {leave, Channel, Nick, Pid}) ->
    InChannel = lists:member(Pid, State#channelstate.users),

    case InChannel of
        true -> 
            NewUs = [User || User <- State#channelstate.users, User /= Pid],
            {reply, ok, State#channelstate{users = NewUs}};
        false -> {reply, {error, user_not_joined, "User not in channel"}, State}
    end;

handlerChannel(State, {message_send, Channel, Msg, Nick, Pid}) ->
    Data = {request, self(), make_ref(), {message_receive, Channel, Nick, Msg}},
    [Member ! Data || Member <- State#channelstate.users, Member /= Pid],
    io:fwrite(lists:length(State#channelstate.users)),
    {reply, ok, State}.
