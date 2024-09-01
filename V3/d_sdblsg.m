(* data file d_sdblsg.m *)

debug = False;
debugLevel = 1;

(* Double sine-Gordon System *)
eq[1][x,t] = D[u[1][x,t],t]-u[2][x,t]
eq[2][x,t] = D[u[2][x,t],t]-D[u[1][x,t],{x,2}]-4*a*Sin[u[1][x,t]/2]-4*b*Sin[u[1][x,t]];

noeqs = 2;
name = "Double sine-Gordon System";
parameters = {};
weightpars = {a,b};

(**** user can supply the rhorank and/or the name for the output file  ****)
(* rhorank = 4; *)
(* myfile = "sineGR4.o"; *) 

(**** user can supply the form of rho ****)
formrho[x,t] = {};

(* end of data file d_sdblsg.m *)
