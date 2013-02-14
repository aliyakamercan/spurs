#!/bin/sh
erl -pa ebin deps/*/ebin -s spurs_ex \
    -eval "io:format(\"Point your browser at http://localhost:8080/1~n\")."
    -eval "io:format(\"Point your browser at http://localhost:8080/2~n\")."
