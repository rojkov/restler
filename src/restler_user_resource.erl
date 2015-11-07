-module(restler_user_resource).
-export([
    init/1,
    allowed_methods/2,
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
    {['GET', 'PUT'], ReqData, State}.

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
            {false, ReqData, State};
        Body ->
            error_logger:info_msg("Putting user. Path tokens:~p~nData:~p~n",
                                  [wrq:path_tokens(ReqData), Body]),
            {true, ReqData, State}
    end.

verify_user_data(ReqData) ->
    try
        mochijson2:decode(wrq:req_body(ReqData))
    catch
        _:_ -> undefined
    end.
