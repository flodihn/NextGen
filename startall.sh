killall -9 beam.smp 

cd ConnectionServer
sh start.sh -noshell &
cd ..

cd AccountServer 
sh start.sh -noshell &
cd ..

cd CharacterServer 
sh start.sh -noshell &
cd ..

cd AreaServer 
sh start.sh areasrv -noshell &
cd ..


