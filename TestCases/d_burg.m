
(* d_burg.m *)

debug = False;

(* Burgers' Equation *)

eq[1][x,t] = D[u[1][x,t],t]+u[1][x,t]*D[u[1][x,t],x]-vv*D[u[1][x,t],{x,2}];

noeqs = 1;
name = "Burgers' Equation";
parameters = {vv};
weightpars = {};

formrho[x,t] = {};

(* end of d_burg.m *)

