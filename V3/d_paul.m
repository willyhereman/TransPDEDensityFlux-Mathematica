
(* d_paul.m *)

debug = False;

(* Boussinesq System *)

eq[1][x,t] = D[u[1][x,t],t]+cc*D[u[2][x,t],x];

eq[2][x,t] = D[u[2][x,t],t]+D[bb*u[1][x,t]-3/2*dd*u[1][x,t]^2-
             aa*D[u[1][x,t],{x,2}],x];

noeqs = 2;
name = "Boussinesq System";
parameters = {};
weightpars = {aa,bb,cc,dd};

formrho[x,t] = {};

(* d_paul.m *)
