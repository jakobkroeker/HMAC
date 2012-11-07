#############################################################################
##
#W read.g                                                   Jakob Kroeker
##
#H   @(#)$Id$
##
#Y Copyright (C) 2012, Jakob Kroeker
##
#############################################################################
##
##  This file reads the implementations, and in principle could be reloaded
##  during a GAP session.
#############################################################################

#############################################################################
##
#R Read the install files.
##

ReadPackage("hmac","gap/helpers.gi");
ReadPackage("hmac","gap/hmacdoc.gi");
ReadPackage("hmac","gap/utils.gi");
ReadPackage("hmac","gap/padicLift.gi");
ReadPackage("hmac","gap/hurwitz.gi");

#############################################################################


#############################################################################


#E read.g . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
