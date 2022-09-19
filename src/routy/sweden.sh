#!/usr/bin/env zsh
erl -make
clear
erl -sname sweden@localhost -eval 'routherd:sweden(), routherd:international().'