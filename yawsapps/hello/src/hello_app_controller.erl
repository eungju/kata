-module(hello_app_controller).
-export([hook/1]).

hook(A) ->
	{phased, {ewc, A},
		fun(_Ewc, Data) ->
			{ewc, html_container, index, [A, {data, Data}]}
		end}.