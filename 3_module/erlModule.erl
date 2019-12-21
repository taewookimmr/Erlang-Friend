-module(erlModule).
-author("Kirk"). % author is an attribute of this module
-version("1.0"). % also  is version
-export([printgo/0]). % export is also an attribute, exactly, pre-built attribute 
% import is also pre-built attribute.


printgo() ->
    io:fwrite("printgo Function").
