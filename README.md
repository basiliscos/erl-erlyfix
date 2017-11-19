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



Build
-----

    $ rebar3 compile
