Overview
========
The NextGen MMO architecture is a proof of concept that high scalability for a online game can be achieved rather easily when choosing Erlang as programming language.

The NextGen code based is separated into 5 different servers and a test suite:
* Connection Server
* Account Server
* Character Server
* Area Server
* Monitor Server
* TestSuite

Compiling
=========

For each server, go into its directory and run:

	erl -make

Running
=======
For each server, go into the ebin directory and copy the servername.app.example file to servername.app (where servername is connsrv, accsrv, charsrv, areasrv or monsrv).

Edit those files to make the node name suite your system. You can see your node name by running:

	erl -sname test

Connection Server
-----------------
Go into the ConnectionServer directory and run:

	erl -pa ebin -sname conn1

You should be given an Erlang shell in which you can start the application:

	application:start(connsrv).

Account Server
--------------
Go into the AccountServer directory and run:

	erl -pa ebin -sname accsrv

You should be given an Erlang shell in which you can start the application:

	application:start(accsrv).

Character Server
----------------
Go into the CharacterServer directory and run:

	erl -pa ebin -sname charsrv

You should be given an Erlang shell in which you can start the application:

	application:start(charsrv).

Area Server
-----------
Go into the AreaServer directory and run:

	erl -pa ebin -sname start_area 

You should be given an Erlang shell in which you can start the application:

	application:start(areasrv).

Monitor Server
-----------
Go into the MonServer directory and run:

	erl -pa ebin -sname monsrv

You should be given an Erlang shell in which you can start the application:

	application:start(monsrv).

Testing
=======
Go into the TestSuite directory and compile:

	erl -make

Then start a shell:

	erl -pa ebin

From the shell, you can execute a test with 100 simulated clients by running this function:

	testsuite:run_test(100).

The argument is the number of clients and can be tweaked according to how powerful system you have.

You can also specify the hostname if you are connecting to a remote connection server:

	testsuite:run_test("10.0.0.1", 14000).

When the test is done, a report file will be written, this make take some time for larger tests.

Notes
=====
* The largest amount of clients on a successful test (response times below 50 ms) is currenlty 14,000. This was tested on a AMD Quad Core with 3Ghz CPU and 8 GB of RAM.
* When increasing the clients to larger number, be mindful about the system limit for max open files (On linux check the ulimit -n).
* The different servers may run on different computers, you just have to edit the app files accordingly.
* When running large tests, the testsuite should run on a separate node not to interfer with the server performance.
* During a test, you can monitor the number of connected clients and the CPU and memory consumption for all nodes by running the monsrv:start_log() and monsrv:stop_log() functions (Currenlty this only works on Linux systems).
