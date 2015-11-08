-module(restler_user_resource).
-export([
    init/1,
    allowed_methods/2,
    delete_resource/2,
    content_types_accepted/2,
    put_user/2,
    to_html/2
]).

-include_lib("webmachine/include/webmachine.hrl").

-record(context, {riakconn}).

-spec init(list()) -> {ok, term()}.
init([]) ->
    {{trace, "/tmp"}, undefined}.
    %% {ok, undefined}.

allowed_methods(ReqData, State) ->
    {['GET', 'PUT', 'DELETE'], ReqData, State}.

delete_resource(ReqData, State) ->
    error_logger:info_msg("Deleting resource. Path tokens:~p~n", [wrq:path_tokens(ReqData)]),
    {delete_user(wrq:path_tokens(ReqData)), ReqData, State}.

content_types_accepted(ReqData, State) ->
    {[{"application/json", put_user}], ReqData, State}.

-spec to_html(wrq:reqdata(), term()) -> {iodata(), wrq:reqdata(), term()}.
to_html(ReqData, State) ->
    error_logger:info_msg("User resource. Path tokens:~p~n", [wrq:path_tokens(ReqData)]),
    {"<html><body>User resource loaded.</body></html>", ReqData, State}.

put_user(ReqData, State) ->
    case verify_user_data(ReqData) of
        undefined ->
            error_logger:info_msg("Unparsable input~n"),
            {{halt, 409}, ReqData, State};
        Body ->
            error_logger:info_msg("Putting user. Path tokens:~p~nData:~p~n",
                                  [wrq:path_tokens(ReqData), Body]),
            {store_user(wrq:path_tokens(ReqData), wrq:req_body(ReqData)), ReqData, State}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private

verify_user_data(ReqData) ->
    try
        mochijson2:decode(wrq:req_body(ReqData))
    catch
        _:_ -> undefined
    end.

store_user([Username | _], Document) ->
    Pid = pooler:take_member(riak8087),
    case riakc_pb_socket:get(Pid, {<<"default">>, <<"users">>}, list_to_binary(Username)) of
        {error, notfound} ->
            error_logger:info_msg("New User name: ~p  Data:~n~p~n", [Username, Document]),
            Object = riakc_obj:new({<<"default">>, <<"users">>},
                                   list_to_binary(Username),
                                   Document,
                                   <<"application/json">>),
            riakc_pb_socket:put(Pid, Object);
        {ok, Object} ->
            error_logger:info_msg("Updating User name: ~p  Data:~n~p~n", [Username, Document]),
            UpdatedObj = riakc_obj:update_value(Object, Document),
            riakc_pb_socket:put(Pid, UpdatedObj)
    end,
    pooler:return_member(riak8087, Pid, ok),
    true;
store_user(_, _Document) ->
    error_logger:info_msg("Error. No user name~n"),
    {halt, 409}.


delete_user([Username | _]) ->
    Pid = pooler:take_member(riak8087),
    ok = riakc_pb_socket:delete(Pid, {<<"default">>, <<"users">>}, list_to_binary(Username)),
    pooler:return_member(riak8087, Pid, ok),
    true;
delete_user(_) ->
    {halt, 409}.
