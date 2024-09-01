(* d_kaup.m *)

debug = False;

(* Kaup System *)

eq[1][x,t] = D[u[1][x,t],t]+D[D[u[1][x,t],{x,2}]-
             3*u[2][x,t]*D[u[1][x,t],x]+3*u[1][x,t]*
             u[2][x,t]^2-3*u[1][x,t]^2,x];

eq[2][x,t] = D[u[2][x,t],t]+D[D[u[2][x,t],{x,2}]+
             3*u[2][x,t]*D[u[2][x,t],x]+u[2][x,t]^3-
             6*u[1][x,t]*u[2][x,t],x];
noeqs = 2;
name="Kaup System";
parameters = {};
weightpars = {};

formrho[x,t] = {};

(* d_kaup.m *)
