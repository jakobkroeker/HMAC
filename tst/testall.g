
LoadPackage("hmac");;

SetInfoLevel(InfoHMAC,1);


SetP1Points( MPC, 600 ); # modify @FR.isc. Assume that @hmac.isc is 'IsMPCFloat'
#SetP1Points( @hmac.isc, 600 ); # modify @FR.isc.
# MAKEP1EPS@FR(); 

dirs := DirectoriesPackageLibrary("hmac","tst");;

Print("test 'utils.tst'\n");
ReadTest(Filename(dirs,"utils.tst"));;
Print("test 'padicLift.tst'\n");
ReadTest(Filename(dirs,"padicLift.tst"));;
Print("test 'hurwitz.tst'\n");
ReadTest(Filename(dirs,"hurwitz.tst"));;

