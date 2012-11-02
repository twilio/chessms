ChesSMS is an application that lets you play Chess over SMS, powered by
[Twilio](http://www.twilio.com/).


## Dependencies
Erlang/OTP R15B01 (may work on older versions)

## Building 

ChesSMS uses [rebar](https://github.com/basho/rebar). To build it...

    ./rebar get-deps
    ./rebar compile

## Choosing a Chess Engine
ChesSMS should in theory work with any UCI compatible chess engine. It is known to
work with [Stockfish](https://github.com/mcostalba/Stockfish).

If using stockfish, compile it according to your machine and copy the
binary to `priv/bin/stockfish`.

If using another Chess Engine, you can put the binary where ever you want
but you'll have to modify `src/chessms_server.app.src` and change the
`engine_path` configuration parameter.

## Running
If everything has been built correctly, you should be able to run:

    erl -pa ebin deps/*/ebin -s chessms_server -chessms_server port 7000

## Connecting to SMS
To connect ChesSMS to an SMS enabled Twilio number, sign up for Twilio
and buy a number (if you haven't already). Configure the SMS URL of the
phone number to point where your ChesSMS server is running.

Now, you should be able to text "play" to your Twilio phone number to
start a chess game!

## Contributing
Contributions are welcome. If you're looking for things to add, checkout
the Issues page.
