#!/usr/bin/env zsh
erl -make
clear
erl -sname france@localhost -eval 'routherd:france().'