-module(restler_sensors_resource).
-export([
    init/1,
    to_html/2
]).

-include_lib("webmachine/include/webmachine.hrl").

-spec init(list()) -> {ok, term()}.
init([]) ->
    {{trace, "/tmp"}, undefined}.
    %% {ok, undefined}.

-spec to_html(wrq:reqdata(), term()) -> {iodata(), wrq:reqdata(), term()}.
to_html(ReqData, State) ->
    error_logger:info_msg("Path tokens:~p~n", [wrq:path_tokens(ReqData)]),
    {"<html><body>Sensors resource loaded.</body></html>", ReqData, State}.
