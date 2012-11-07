#############################################################################
##
#W init.g                                                   Jakob Kroeker
##
#H   @(#)$Id$
##
#Y Copyright (C) 2012, Jakob Kroeker
##
#############################################################################
##
##  This file reads the declarations of the packages' new objects
##
#############################################################################

POSTHOOK@HMC := []; # to be processed at the end

BindGlobal("@hmac", rec()); # a record to store locals in the package

#############################################################################
##
#I Create info class to be able to debug loading
##
InfoHMAC := NewInfoClass("InfoHMAC");
SetInfoLevel(InfoHMAC, 1);

InfoHMACRootPairing := NewInfoClass("InfoHMACRootPairing");
SetInfoLevel(InfoHMACRootPairing, 0);

#############################################################################

#############################################################################
##
#R Read the declaration files.
##
ReadPackage("hmac","gap/namespaceEmulation.g");
ReadPackage("hmac","gap/helpers.gd");
ReadPackage("hmac","gap/hmacdoc.gd");
ReadPackage("hmac","gap/utils.gd");
ReadPackage("hmac","gap/padicLift.gd");
ReadPackage("hmac","gap/hurwitz.gd");


#############################################################################

#E init.g . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
