#############################################################################
##
#W hmacdoc.gi                                                   Jakob Kroeker
##
#H   @(#)$Id$
##
#Y Copyright (C) 2012, Jakob Kroeker
##
#############################################################################


BindGlobal("HMACDOC", function() 
#SetGAPDocTextTheme("ColorPrompt");
MakeGAPDocDoc(Concatenation(PATH@HMAC,"/doc"),"hmac.xml",
  [ "../gap/utils.gi","../gap/padicLift.gi","../gap/hurwitz.gi",
   "../PackageInfo.g"],"hmac", "MathJax");
#    "MathML" fails...
#"MathJax"
end);

 BindGlobal("IHMACDOC", function() 
#SetGAPDocTextTheme("ColorPrompt");
MakeGAPDocDoc(Concatenation(PATH@HMAC,"/doc/internal"),"ihmac.xml",
  [ "../../gap/utils.gi","../../gap/padicLift.gi","../../gap/hurwitz.gi",
   "../../PackageInfo.g"],"hmac", "MathJax");
#    "MathML" fails...
#"MathJax"
end);
