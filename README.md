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

## Architecture
![System architecture](/doc/architecture.png)   

## Sequence diagram
![Sequence diagram init](/doc/diagram_sequence_init.png)   
![Sequence diagram "I am X"](/doc/diagram_sequence_I_AM_X.png)   

## Command
`sentibot: help` display help.    
`sentibot: add <emoji> <feeling>` Add a new feeling. Example: `sentibot: add :scream: scared`.     
`I am <sentiment>` Save your feeling. sentiment is happy | sad | sleeping | strong | tired.      
`sentibot: sentiments` display all the channel feelings.               
`sentibot: rename <newname>` Rename sentibot into newname.                     
`sentibot: clear` Remove all the channel stored feelings.                            

## Group members
* Combremont Rosanne
* Djomo Patrick
* Ponce Kevin
* Richoz Sébastien
