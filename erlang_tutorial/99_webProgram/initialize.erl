-module(initialize). 
-export([start/0]). 

start() ->
   inets:start(), 
   Pid = inets:start(httpd, [{port, 8081}, {server_name,"httpd_test"}, 
   {server_root,"C://tmp"},{document_root,"C://tmp/htdocs"},
   {bind_address, "localhost"}]), io:fwrite("~p",[Pid]).