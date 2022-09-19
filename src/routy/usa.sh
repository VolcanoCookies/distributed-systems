#!/usr/bin/env zsh
erl -make
clear
erl -sname usa@localhost -eval 'routherd:usa().'