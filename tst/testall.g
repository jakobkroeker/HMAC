
LoadPackage("hmac");

SetInfoLevel(InfoHMAC,0);

dirs := DirectoriesPackageLibrary("hmac","tst");

ReadTest(Filename(dirs,"utils.tst"));
ReadTest(Filename(dirs,"padicLift.tst"));
ReadTest(Filename(dirs,"hurwitz.tst"));

