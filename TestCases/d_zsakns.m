(* d_zsakns.m *)

debug = False;

(* ZS-AKNS System *)

eq[1][x,t] = D[u[1][x,t],t]-D[u[1][x,t],{x,2}]-2*u[1][x,t]^2*u[2][x,t];

eq[2][x,t] = D[u[2][x,t],t]+D[u[2][x,t],{x,2}]+2*u[2][x,t]^2*u[1][x,t];

noeqs = 2;
name = "ZS-AKNS System";
parameters = {};
weightpars = {}; 

formrho[x,t] = {};

(* d_zsakns.m *)
