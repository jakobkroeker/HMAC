
BindGlobal("PATH@HMAC", PackageInfo("HMAC")[1].InstallationPath);

BindGlobal("TSTPATH@HMAC",Concatenation(PATH@HMAC,"/tst") );



VERSION@ := Filename(DirectoriesPackageLibrary("hmac",""),".version");
if VERSION@<>fail then
    VERSION@ := ReadLine(InputTextFile(VERSION@));
    Remove(VERSION@); # remove \n
fi;
MakeReadOnlyGlobal("VERSION@");

