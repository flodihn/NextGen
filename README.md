Overview
========
The NextGen MMO architecture is a proof of concept that high scalability for a online game can be achieved rather easily when choosing Erlang as programming language.

The NextGen code based is separated into 5 different servers and a test suite:
* Connection Server
* Account Server
* Character Server
* Area Server
* Monitor Server (Only necessary for when area servers are distributed across two or more machines).
* TestSuite

Compiling
=========

For each server, go into its directory and run:

	erl -make

Running
=======
For each server, go into the ebin directory and remove the .example extension on the servername.app.example file.

Edit those files to make the host name suite your system. You can see your node name by running:

	erl -sname test

* Note: Only change the host name, the node name (connsrv, charsrv etc) must remain the same. Do not change the node name unless you know what you are doing.

Running a testing with TCP clients.
===================================
Start all the server by executing the startall.sh script (You can also go into the ConnectionServer, AccountServer, CharacterServer and AreaServer directories and run start.sh from there):

	./startall.sh 

Now enter the TestSuite directory and start an Erlang node:

	cd TestSuite
	erl -pa ebin/

Before running a test, you need to initialise the test suite (in the Erlang shell):

	testsuite:init().

Now you are ready to run a test with a number of simulated clients:
	testsuite:run_test(1000).


You can also specify the hostname if you are connecting to a remote connection server:

	testsuite:run_test("10.0.0.1", 1000).

When the test is done, a report file will be written, this make take some time for larger tests.

Notes
=====
* The largest amount of clients on a successful test (response times below 50 ms) is currenlty 14,000. This was tested on a AMD Quad Core with 3Ghz CPU and 8 GB of RAM.
* When increasing the clients to larger number, be mindful about the system limit for max open files (On linux check the ulimit -n).
* The different servers may run on different computers, you just have to edit the app files accordingly.
* When running large tests, the testsuite should run on a separate node on a different machine not to interfere with the server performance.
* During a test, you can monitor the number of connected clients and the CPU and memory consumption for all nodes by running the monsrv:start_log() and monsrv:stop_log() functions (Currenlty this only works on Linux systems).
