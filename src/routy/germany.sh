#!/usr/bin/env zsh
erl -make
clear
erl -sname germany@localhost -eval 'routherd:germany().'