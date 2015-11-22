-module(restler_sensor_history_resource).
-export([
    init/1,
    allowed_methods/2,
    service_available/2,
    resource_exists/2,
    finish_request/2,
    to_html/2
]).

-include_lib("webmachine/include/webmachine.hrl").

% TODO: define username and sensorid as binaries
-record(context, {riakconn, username, sensorid}).

-spec init(list()) -> {ok, term()}.
init([]) ->
    {{trace, "/tmp"}, #context{}}.
    %% {ok, #context{}}.

allowed_methods(ReqData, State) ->
    {['GET'], ReqData, State}.

service_available(ReqData, State) ->
    error_logger:info_msg("[D] ~p~n", [wrq:path_info(ReqData)]),
    case pooler:take_group_member(riak8087) of
        error_no_members ->
            {false, ReqData, State};
        Pid ->
            {true, ReqData,
             State#context{riakconn=Pid,
                           username=proplists:get_value(username, wrq:path_info(ReqData)),
                           sensorid=proplists:get_value(sensorid, wrq:path_info(ReqData))}}
    end.

finish_request(ReqData, #context{riakconn=undefined} = State) ->
    {ok, ReqData, State};
finish_request(ReqData, #context{riakconn=Pid} = State) ->
    pooler:return_group_member(riak8087, Pid, ok),
    {ok, ReqData, State#context{riakconn=undefined}}.

resource_exists(ReqData, #context{riakconn=RiakPid, username=Username, sensorid=SID} = State) ->
    case riakc_pb_socket:get(RiakPid, {<<"default">>, <<"sensors">>},
                             list_to_binary(Username ++ "/" ++ SID)) of
        {error, notfound} ->
            {{halt, 404}, ReqData, State};
        {ok, _Object} ->
            {true, ReqData, State}
    end.

-spec to_html(wrq:reqdata(), term()) -> {iodata(), wrq:reqdata(), term()}.
to_html(ReqData, #context{riakconn=_RiakPid, username=Username, sensorid=SID} = State) ->
    {"<html><body>Sensors resource " ++ Username ++ "/" ++ SID ++ " History:<br><pre>" ++
     "</pre></body></html>", ReqData, State}.
