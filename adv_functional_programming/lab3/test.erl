-module(test).
-export ([run/0,simple_hash/1]).

run() -> io:format("hello").

simple_hash(X) -> X*2.