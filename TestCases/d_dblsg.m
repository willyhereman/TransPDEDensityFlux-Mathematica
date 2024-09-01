(* data file d_dblsg.m *)

debug = False;
debugLevel = 1;

(* Double sine-Gordon Equation *)
eq[1][x,t] = D[D[u[1][x,t],t],x]-4*a*Sin[u[1][x,t]/2]-4*b*Sin[u[1][x,t]];

noeqs = 1;
name = "Double sine-Gordon Equation";
parameters = {};
weightpars = {a,b};

(**** user can supply the rhorank and/or the name for the output file  ****)
(* rhorank = 4; *)
(* myfile = "sineGR4.o"; *) 

(**** user can supply the form of rho ****)
formrho[x,t] = {};

(* end of data file d_dblsg.m *)
