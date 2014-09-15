sudo erl -pa ebin -pa deps/elli/ebin -sname policyserver -eval "application:start(http_api)" $1
