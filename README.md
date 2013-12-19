Bomber
=================
It's a classical web-game.

Introduction
-----------------
Server side use erlang based on [cowboy](https://github.com/extend/cowboy).

Client side use html5 canvas to fixed multi-platform.

Message transform with websocket.

Getting Start
-----------------
To run this game, you need GNU `make` and `git` in your PATH.

To build the game, run the following command:

    $ make

To start the release in the foreground:

    $ ./_rel/bin/websocket_example console

Then point your browser at [http://localhost:2333](http://localhost:2333).

You can also try online demo on [http://bomber.kilnyy.org](http://bomber.kilnyy.org).
