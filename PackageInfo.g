#############################################################################
##
##  PackageInfo.g for the package `HMaC'                       Jakob Kroeker
##							   Laurent Bartholdi
##     				                   Hans-Christian v. Bothmer
##
## $Id$
##
SetPackageInfo( rec(
PackageName := "Hmac",
Subtitle := "Hurwitz map algebraic construction",
Version := "1.0.0",
## <#GAPDoc Label="Version">
## 1.0.0
## <#/GAPDoc>
Date := "11/10/2012",
ArchiveURL := Concatenation("",~.Version),
ArchiveFormats := ".tar.gz",
Persons := [
  rec(
    LastName      := "Kroeker",
    FirstNames    := "Jakob",
    IsAuthor      := true,
    IsMaintainer  := true,
    Email         := "kroeker@math.uni-hannover.de",
    WWWHome       := "http://www.crcg.de/wiki/User:Kroeker",
    PostalAddress := Concatenation( [
                       "Institut für algebraische Geometrie\n",
                       "Welfenschloss 1\n",
                       "D-30167 Hannover\n",
                       "Germany" ] ),
    Place         := "Hannover",
    Institution   := "Leibniz Universität Hannover"
  ),
   rec(
    LastName      := "Bartholdi",
    FirstNames    := "Laurent",
    IsAuthor      := true,
    IsMaintainer  := false,
    Email         := "laurent.bartholdi@gmail.com",
    WWWHome       := "http://www.uni-math.gwdg.de/laurent",
    PostalAddress := Concatenation( [
                       "Mathematisches Institut\n",
                       "Bunsenstraße 3-5\n",
                       "D-37073 Göttingen\n",
                       "Germany" ] ),
    Place         := "Göttingen",
    Institution   := "Georg-August Universität zu Göttingen"
  ),
 rec(
    LastName      := "Graf von Bothmer",
    FirstNames    := "Hans-Christian",
    IsAuthor      := false,
    IsMaintainer  := false,
    Email         := "bothmer@math.uni-hannover.de",
    WWWHome       := "http://www.crcg.de/wiki/index.php5?title=User:Bothmer",
    PostalAddress := Concatenation( [
                       "Fakultät für Mathematik, Informatik und Naturwissenschaften\n",
		      "Fachbereich Mathematik\n",
                       "Bundesstraße 55\n",
                       "D-20146 Hamburg\n",
                       "Germany" ] ),
    Place         := "Hamburg",
    Institution   := "Universität Hamburg"
  )
],

Status := " ",
CommunicatedBy := " ",
AcceptDate := "",

README_URL := " ",
PackageInfoURL := " ",
AbstractHTML := "The <span class=\"pkgname\">HMaC</span> package allows \
   GAP to construct Hurwitz map approximations or lift smooth isolated points on a variety over a finite field to \
   extensions of rational number field",
PackageWWWHome := "",

PackageDoc := rec(
  BookName  := "HMaC",
  HTMLStart := "doc/chap0.html",
  PDFFile   := "doc/manual.pdf",
  SixFile   := "doc/manual.six",
  LongTitle := "Hurwitz map algebraic construction",
  ArchiveURLSubset := ["doc"],
  Autoload  := true
),

Dependencies := rec(
  GAP := ">=4.5.0",
  NeededOtherPackages := [["Float",">=0.4"],
                      ["FR",">=1.2.7"] ],
  SuggestedOtherPackages := [],
  # additional desired packages: graphviz, display

  # for compilation of the external module, one needs:
  # gcc, gfortran, libcblas, libgsl, javac, appletviewer.
  ExternalConditions := []
),

AvailabilityTest := ReturnTrue,
                    
BannerString := Concatenation("Loading HMaC ", String( ~.Version ), " ...\n"),

Autoload := false,
TestFile := "tst/testall.g",
Keywords := ["Hurwitz map construction", "Hensel lifting"]
));
