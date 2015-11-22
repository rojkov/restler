-module(restler_sensor_resource).
-export([
    init/1,
    allowed_methods/2,
    service_available/2,
    resource_exists/2,
    finish_request/2,
    delete_resource/2,
    content_types_accepted/2,
    process_post/2,
    put_sensor/2,
    to_html/2
]).

-include_lib("webmachine/include/webmachine.hrl").

% TODO: define username and sensorid as binaries
-record(context, {riakconn, username, sensorid, sensor}).

-spec init(list()) -> {ok, term()}.
init([]) ->
    {{trace, "/tmp"}, #context{}}.
    %% {ok, #context{}}.

allowed_methods(ReqData, State) ->
    {['GET', 'PUT', 'DELETE', 'POST'], ReqData, State}.

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
    case riakc_pb_socket:get(RiakPid, {<<"default">>, <<"users">>}, list_to_binary(Username)) of
        {error, notfound} ->
            {{halt, 404}, ReqData, State};
        {ok, _Object} ->
            case riakc_pb_socket:get(RiakPid,
                                     {<<"default">>, <<"sensors">>},
                                     list_to_binary(Username ++ "/" ++ SID)) of
                {error, notfound} ->
                    {false, ReqData, State};
                {ok, Sensor} ->
                    {true, ReqData, State#context{sensor=Sensor}}
            end
    end.

delete_resource(ReqData, #context{riakconn=RiakPid, username=Username, sensorid=SID} = State) ->
    ok = riakc_pb_socket:delete(RiakPid, {<<"default">>, <<"sensors">>},
                                list_to_binary(Username ++ "/" ++ SID)),
    {true, ReqData, State}.

content_types_accepted(ReqData, State) ->
    {[{"application/json", put_sensor}], ReqData, State}.

-spec to_html(wrq:reqdata(), term()) -> {iodata(), wrq:reqdata(), term()}.
to_html(ReqData, #context{riakconn=_RiakPid, username=Username, sensorid=SID, sensor=Sensor} = State) ->
    error_logger:info_msg("Path tokens:~p~n", [wrq:path_tokens(ReqData)]),
    {"<html><body>Sensors resource " ++ Username ++ "/" ++ SID ++ " Data:<br><pre>" ++
     binary_to_list(riakc_obj:get_value(Sensor)) ++
     "</pre></body></html>", ReqData, State}.

process_post(ReqData, #context{sensor=undefined} = State) ->
    error_logger:info_msg("Sensor not found. Path tokens:~p~n", [wrq:path_tokens(ReqData)]),
    {{halt, 404}, ReqData, State};
process_post(ReqData, #context{riakconn=RiakPid, username=Username, sensorid=SID, sensor=Sensor} = State) ->
    {struct, OldSensorData} = mochijson2:decode(riakc_obj:get_value(Sensor)),
    CleanedSensorData = proplists:delete(<<"last_update">>,
                                         proplists:delete(<<"last_update_time">>, OldSensorData)),
    CurrentTime = get_current_time_key(),
    NewSensorData = [{<<"last_update">>, mochijson2:decode(wrq:req_body(ReqData))},
                     {<<"last_update_time">>, list_to_binary(CurrentTime)} | CleanedSensorData],
    UpdatedObj = riakc_obj:update_value(Sensor, iolist_to_binary(mochijson2:encode({struct, NewSensorData}))),
    riakc_pb_socket:put(RiakPid, UpdatedObj),

    UpdateValueObj = riakc_obj:new({<<"default">>, <<"sensor_updates">>},
                                   list_to_binary(Username ++ "/" ++ SID ++ "/" ++ CurrentTime),
                                   wrq:req_body(ReqData),
                                   <<"application/json">>),
    riakc_pb_socket:put(RiakPid, UpdateValueObj),
    {true, ReqData, State}.

put_sensor(ReqData, #context{riakconn=RiakPid, username=Username, sensorid=SID, sensor=undefined} = State) ->
    Object = riakc_obj:new({<<"default">>, <<"sensors">>},
                           list_to_binary(Username ++ "/" ++ SID),
                           wrq:req_body(ReqData),
                           <<"application/json">>),
    riakc_pb_socket:put(RiakPid, Object),
    {true, ReqData, State};
put_sensor(ReqData, #context{riakconn=RiakPid, sensor=Sensor} = State) ->
    UpdatedObj = riakc_obj:update_value(Sensor, wrq:req_body(ReqData)),
    riakc_pb_socket:put(RiakPid, UpdatedObj),
    {true, ReqData, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private

get_current_time_key() ->
    DateTime = erlang:now(),
    {{Year,Month,Day},{Hour,Min,Sec}} = calendar:now_to_universal_time(DateTime),
    lists:flatten(
        io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B",
                      [Year, Month, Day, Hour, Min, Sec])).
