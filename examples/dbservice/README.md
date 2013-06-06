# DB Service

## Overview

This is a simple example of a service oriented approach to a Redis client.

The service is represented by the `dbservice` module.

The module provides a front-end to a Redis connection, exposing two application
functions: `get_value` and `set_value`. It's started as a registered service
when the application is started.

`dbservice` performs the following functions:

- Asynchronously connects to a local Redis server, reconnecting if the
  connection is lost

- If the connection is established, passes `get_value` and `set_value` through
  to the client's `get` and `set` functions respectively

- If the connection is not established, generates an error for calls to
  `get_value` and `set_value`

## Usage

Run the application by changing to its directory and running:

    $ make shell

If a local Redis server is running, the dbservice will connect immediately and
you can use its functions:

    > dbservice:set_value("msg", "Hello!").
	ok
	> dbservice:get_value("msg").
	{ok, <<"Hello!">>}

If the service cannot connect to a local Redis server, it will retry in 5
second intervals.

If the connection to the Redis server is lost, the service will terminate
immediately and then be restarted by the application supervisor. It will then
be available, but calls to `get_value` and `set_value` will fail until a new
connection is established:

    > dbsevice:get_value("name").
	** exception error: not_connected
	     in function  dbservice:handle_db_call/1
