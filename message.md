Message Doc
==========

From Client
----------
 - "connect"
 - "disconnect"
 - "player", "move", X, Y, Dx, Dy, Timestamp
 - "bomb", "set", X, Y, Timestamp
 - "name", Name, Timestamp
 - "time", Timestamp

From Server
----------
 - "connect"
 - "disconnect", Id
 - "player", "move", Id, X, Y, Dx, Dy, Timestamp
 - "player", "init", Id, Timestamp
 - "bomb", "set", Id, X, Y, Range, Timestamp
 - "bomb", "bang", Id, Timestamp
 - "time", ClientTimestamp, ServerTimestamp

From Bot
----------
 - "connect"
 - "disconnect"
 - "player", "move", Dx, Dy
 - "bomb", "set"
 - "name", Name

