{application, multip_chat_room,
[{description, "a multip_chatroom for client use"},
{vsn, "0.1.0"},
{modules,[client, server,listen]},
{application, [kernel, stdlib]},
{mod, {listen,[]}}
]}.