-module(chessms_server).
-behaviour(application).
-export([start/0, start/2, stop/1]).
-define(DEFAULT_PORT, 8084).

start() ->
    application:start(sasl),
    application:start(ranch),
    application:start(crypto),
    application:start(cowboy),
    application:start(xmerl),
    application:start(mnesia),
    application:start(mimetypes),
    application:start(chessms_server).

start(_Type, _Args) ->
    chessms_store:init(),
    Port = case application:get_env(chessms_server, port) of
        {ok, P} ->
            P;
        _ ->
            ?DEFAULT_PORT
    end,

    Dispatch = [
        {'_', [
                    {[<<"twiml">>, <<"sms">>], chessms_handler, []},
                    {[], cowboy_static, [
                        {directory, "priv/static/"},
                        {file, <<"index.html">>},
                        {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
                    ]},
                    {['...'], cowboy_static, [
                        {directory, "priv/static/"},
                        {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
                    ]}
        ]}
    ],
    cowboy:start_http(http, 10, [{port, Port}], [{dispatch, Dispatch}]),
    chessms_server_sup:start_link().

stop(_State) ->
    ok.
