
(* d_mkdv.m *)

debug = False;

(* modified Korteweg-de Vries Equation *)

eq[1][x,t] = D[u[1][x,t],t]+6*u[1][x,t]^2*D[u[1][x,t],x]+D[u[1][x,t],{x,3}];

noeqs = 1;
name = "Modified KdV Equation";
parameters = {};
weightpars = {};

formrho[x,t] = {};

(* d_mkdv.m *)
