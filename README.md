# Sentimental Slack bot in Erlang

*Project made in june 2017 for HEIG-VD courses*

## Introduction
This Bot was developped as an Erlang 19.0 OTP Application with [rebar3](https://www.rebar3.org/) during our bachelor's degree in HEIG-VD.

It is capable to connect to any slack team and channel in order to operate simple sentiments detections.

![System architecture](/doc/screens/screen1.png)   

It is also provided with other features you can display with `sentibot: help` message.

![System architecture](/doc/screens/screen2.png)   

## Configuration

As this is a [Slack Custom bot](https://api.slack.com/bot-users#custom_bot_users), you'll have to manually add it to your team. This can be done in a few steps:

1. Create a new Slack bot configuration on [https://my.slack.com/services/new/bot](https://my.slack.com/services/new/bot)
2. Copy to clipboard the token assigned to your newly created bot
3. Copy the `priv/slack.config_example` into `priv/slack.config` and set your bot's token there.

## Build

Once you have properly configured your bot, run  the following command in your terminal (assuming you have [rebar3](https://www.rebar3.org/) installed).

    $ rebar3 compile

This will fetch all the dependencies needed for this project.

## Run

After successfully build the application, run these commands to start the Erlang Shell and the app.

    $ rebar3 auto
    ...
    Eshell V8.3  (abort with ^G)
    1> application:start(sentibot).
    2> observer:start().  // Tool to observe supervision tree. Not necessary.
    
### Alternative Build & Run with Intellij

Open the project with Intellij (version used: 2017.1.4) and make the `Rebar -> Path` in `Preferences -> Other Settings -> Erlang External Tools` point to your `rebar3` local installation.

After that you'll be able to compile the project (`rebar compile`) and run it (`rebar auto`) with the configurations provided. Once the project is run, type the command `application:start(sentibot).` in Erlang shell (should appear in Intellij).

## Bot Commands

Invite the bot to any slack channel of your team and try one of the following commands !

- `sentibot: help` display help.    
- `sentibot: add <emoji> <feeling>` Add a new feeling. Example: `sentibot: add :scream: scared`.     
- `I am <sentiment>` Save your feeling. sentiment is happy | sad | sleeping | strong | tired.      
- `sentibot: sentiments` display all the channel feelings.               
- `sentibot: rename <newname>` Rename sentibot into newname.                     
- `sentibot: clear` Remove all the channel stored feelings.

## Architecture

At initialization, the application creates the following supervision tree.

![System architecture](/doc/architecture.png)

- kvs (`src/sentibot_kvs.erl`) stands for key-value store. It stores the association of user => feeling, and feeling => ascii_representation using hard-coded maps.
- ctl (`src/sentibot_ctl.erl`) is the controller of the app. It contains the business logic and is responsible to orchestrate the application.
- slack (`src/sentibot_slack.erl`) is a module using [Erlang slacker plugin](https://github.com/julienXX/slacker) to communicate with the [Slacker REST API](https://api.slack.com/web) of Slack.
- wss (`src/sentibot_wss.erl`) is a websocket catching all the incoming messages from channels where the bot is invited to.

## Sequence diagrams

At initialization, the application starts the supervisor, which starts sequentially the other Erlang processes.

*Note that RTM_slack corresponds to the Slack API and is not part of this project.*

![Sequence diagram init](/doc/diagram_sequence_init.png)

This diagram shows how a message with format "I am SENTIMENT" is handled by the children processes of the supervisor.

![Sequence diagram "I am X"](/doc/diagram_sequence_I_AM_X.png)

The socket filters event of type "message" and immediately sends them asynchronously to the controller to keep a high response state for other incoming messages.                           

## Improvements

The sentiments could be stored in a persistent database such as sqlite to keep history even if server crashs.

## Dependencies

- Slacker: https://github.com/julienXX/slacker.git
- websocket_client: https://github.com/jeremyong/websocket_client
- lager: https://github.com/erlang-lager/lager
- jsone: https://github.com/sile/jsone

## Authors
* Combremont Rosanne
* Djomo Patrick
* Ponce Kevin
* Richoz Sébastien
