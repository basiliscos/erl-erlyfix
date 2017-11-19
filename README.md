erlyfix
=====

[![Travis](https://img.shields.io/travis/basiliscos/erl-erlyfix.svg)](https://travis-ci.org/basiliscos/erl-erlyfix)
[![license](https://img.shields.io/github/license/basiliscos/erl-erlyfix.svg)](https://github.com/basiliscos/erl-erlyfix/blob/master/LICENSE)

FIX (Foreign Information eXchange) protocol implementation in erlang.

Versions
-----

Versions supported: R18 and up

Description
-----

This FIX protocol implementation based on XML FIX-protocol definitions provided by [quickfixengine.org](http://quickfixengine.org/). It is possible to have own/proprietary extenstion XML, which extends the quickfix's definition. 

The library provides only serialization, deserialization and basic validation of FIX-messages and does not provides network layer. You have to build your own FIX-client/server. 

Synopsis
-----

```erlang
% loading protocol
{ok, Protocol} = erlyfix_protocol:load("test/FIX44.xml"),
{ok, Protocol} = erlyfix_protocol:load("test/FIX44.xml", "test/extension-sample.xml"),

% message serialization
{ok, IoList} = erlyfix_protocol:serialize(Protocol, 'MarketDataSnapshotFullRefresh', [
    {'SenderCompID', <<"me">>},                         % field
    {'TargetCompID', <<"you">>},
    {'MsgSeqNum', 1},
    {'SendingTime', <<"20171109-16:19:07.541">>},
    {'Instrument', [{'Symbol', <<"EURCHF">>}] },        % component
    {'MDReqID', <<"31955:1510225047.01637:EURCHF">>},
    {'MDFullGrp', [{'NoMDEntries', [                    % group
        [{'MDEntryType', <<"BID">>}]
    ]}]}
]),

% message deserialization
BinaryMessage = <<"8=FIX.4.4", 1, "9=102", 1, "35=A", 1,
    "212=1", 1, "213=", 1, 1,"49=me", 1, "56=you", 1,
    "34=1", 1, "52=20090107-18:15:16", 1, "98=0", 1, "108=60", 1, "384=2", 1,
    "372=abc", 1, "385=S", 1, "372=def", 1, "385=R", 1, "10=232", 1
>>,
{ok, 'Logon', TagsMarkup, <<>>} = erlyfix_parser:parse(BinaryMessage, Protocol),

```


Build
-----

    $ rebar3 compile
