-module(restler_user_resource).
-export([
    init/1,
    allowed_methods/2,
    service_available/2,
    finish_request/2,
    delete_resource/2,
    content_types_accepted/2,
    put_user/2,
    to_html/2
]).

-include_lib("webmachine/include/webmachine.hrl").

-record(context, {riakconn, username}).

-spec init(list()) -> {ok, term()}.
init([]) ->
    {{trace, "/tmp"}, #context{}}.
    %% {ok, #context{}}.

allowed_methods(ReqData, State) ->
    {['GET', 'PUT', 'DELETE'], ReqData, State}.

service_available(ReqData, State) ->
    case pooler:take_member(riak8087) of
        error_no_members ->
            {false, ReqData, State};
        Pid ->
            {true, ReqData,
             State#context{riakconn=Pid,
                           username=proplists:get_value(username, wrq:path_info(ReqData))}}
    end.

finish_request(ReqData, #context{riakconn=undefined} = State) ->
    {ok, ReqData, State};
finish_request(ReqData, #context{riakconn=Pid} = State) ->
    pooler:return_member(riak8087, Pid, ok),
    {ok, ReqData, State#context{riakconn=undefined}}.

delete_resource(ReqData, #context{riakconn=RiakPid, username=Username} = State) ->
    ok = riakc_pb_socket:delete(RiakPid, {<<"default">>, <<"users">>}, list_to_binary(Username)),
    {true, ReqData, State}.

content_types_accepted(ReqData, State) ->
    {[{"application/json", put_user}], ReqData, State}.

-spec to_html(wrq:reqdata(), term()) -> {iodata(), wrq:reqdata(), term()}.
to_html(ReqData, #context{riakconn=RiakPid, username=Username} = State) ->
    case riakc_pb_socket:get(RiakPid, {<<"default">>, <<"users">>}, list_to_binary(Username)) of
        {error, notfound} ->
            {{halt, 404}, ReqData, State};
        {ok, Object} ->
            {"<html><body>User resource loaded.<br>Username: " ++
                Username ++ "<pre>" ++
                binary_to_list(riakc_obj:get_value(Object)) ++
                "</pre></body></html>",
             ReqData, State}
    end.

put_user(ReqData, #context{riakconn=RiakPid, username=Username} = State) ->
    case verify_user_data(ReqData) of
        undefined ->
            error_logger:info_msg("Unparsable input~n"),
            {{halt, 409}, ReqData, State};
        Body ->
            error_logger:info_msg("Putting user. Path tokens:~p~nData:~p~n",
                                  [wrq:path_tokens(ReqData), Body]),
            {store_user(RiakPid, Username,
                        wrq:req_body(ReqData)),
             ReqData, State}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private

verify_user_data(ReqData) ->
    try
        mochijson2:decode(wrq:req_body(ReqData))
    catch
        _:_ -> undefined
    end.

store_user(_RiakPid, undefined, _Document) ->
    error_logger:info_msg("Error. No user name~n"),
    {halt, 409};
store_user(RiakPid, Username, Document) ->
    case riakc_pb_socket:get(RiakPid, {<<"default">>, <<"users">>}, list_to_binary(Username)) of
        {error, notfound} ->
            error_logger:info_msg("New User name: ~p  Data:~n~p~n", [Username, Document]),
            Object = riakc_obj:new({<<"default">>, <<"users">>},
                                   list_to_binary(Username),
                                   Document,
                                   <<"application/json">>),
            riakc_pb_socket:put(RiakPid, Object);
        {ok, Object} ->
            error_logger:info_msg("Updating User name: ~p  Data:~n~p~n", [Username, Document]),
            UpdatedObj = riakc_obj:update_value(Object, Document),
            riakc_pb_socket:put(RiakPid, UpdatedObj)
    end,
    true.
