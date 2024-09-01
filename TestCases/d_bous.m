
(* d_bous.m *)

debug = False;

(* Boussinesq System *)

eq[1][x,t] = D[u[1][x,t],t]+D[u[2][x,t],x];

eq[2][x,t] = D[u[2][x,t],t]+D[beta*u[1][x,t]-3/2*u[1][x,t]^2-
             aa*D[u[1][x,t],{x,2}],x];

noeqs = 2;
name = "Boussinesq System";
parameters = {aa};
weightpars = {beta};

formrho[x,t] = {};

(* d_bous.m *)
