{application, chat,
 [{description, "Chat"},
  {vsn, "0.0.1"},
  {modules, [chat_app,
	     chat_sup,
	     chat_server]},
  {registered, [chat_server]},
  {applications, [kernel, stdlib]},
  {mod, {chat_app, []}}]
}.	      		
