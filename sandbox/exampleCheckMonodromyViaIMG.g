x := P1z;

num := 50176 * (x^4 + 11*x^3 - 29*x^2 + 11*x + 1)^5 *
    (x^5 - 215/98*x^4 + 485/196*x^3 - 235/196*x^2 + 15/98*x + 1/49) *
    (x^5 - 25/16*x^4 + 75/32*x^3 - 25/64*x^2 + 5/64*x + 1/64)^5;

den := 9765625 * x^7 * (x+1)^7 * (x^2 - 3/2*x + 1)^7 * (x^3 - 4*x^2 + 5/4*x
+ 1/8)^7;

rat := num / den;

downsphere := NewMarkedSphere([P1zero,P1one,P1infinity]);

upsphere := NewMarkedSphere(Concatenation([
  P1PreImages(x^4 + 11*x^3 - 29*x^2 + 11*x + 1, P1zero), # above 0
  P1PreImages(x^5 - 25/16*x^4 + 75/32*x^3 - 25/64*x^2 + 5/64*x + 1/64,
P1zero),
  P1PreImages(x, P1zero), # above infinity
  P1PreImages(x+1, P1zero),
  P1PreImages(x^2 - 3/2*x + 1, P1zero),
  P1PreImages(x^3 - 4*x^2 + 5/4*x + 1/8, P1zero),

P1PreImages(28672*x^20-2114560*x^19+13722240*x^18-65614080*x^17+245351840*x^16-660267008*x^15+1248458280*x^14-1700835920*x^13+1704958640*x^12-1267574420*x^11+690436992*x^10-257110380*x^9+52736995*x^8-948040*x^7-1171555*x^6-246148*x^5+86660*x^4+11060*x^3-1520*x^2-240*x-8,
P1zero)])); # above 1

 SetInfoLevel( InfoIMG , 3 );

biset := SphereMachineOfBranchedCovering(downsphere, upsphere, rat);

Display(biset);
