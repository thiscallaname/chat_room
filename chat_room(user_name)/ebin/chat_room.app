{application, chat_room,
[
{description, "chat room for Erlang and OTP in action"},
{vsn, "0.1.0"},
{modules,[listen,
			m,
			server,
			listen]},
{application, [kernel, stdlib]},
{mod, {listen,[]}}
]}.