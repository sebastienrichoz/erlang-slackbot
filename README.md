# HEIG-MCS-sentibot
Slack bot developed in Erlang to analyze sentiments

An OTP application

## Configuration

Rename `priv/slack.config_example` into `priv/slack.config` and set your bot's token there.

## Build

    $ rebar3 compile

## Run 

    $ rebar3 auto
    ...
    Eshell V8.3  (abort with ^G)
    1> application:start(sentibot).
    2> observer:start().  // Tool to observe supervision tree

## Group members
* Combremont Rosanne
* Djomo Patrick
* Ponce Kevin
* Richoz Sébastien
