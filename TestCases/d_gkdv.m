(* d_gkdv.m *)

debug = False;

(* Generalized Korteweg-de Vries Equation with degree 3 on u[x,t] *)

eq[1][x,t] = D[u[1][x,t],t]+u[1][x,t]^3*D[u[1][x,t],x]+D[u[1][x,t],{x,3}];

noeqs = 1;
name = "Generalized KdV Equation";
parameters = {};
weightpars = {};

formrho[x,t] = {};

(* d_gkdv.m *)
