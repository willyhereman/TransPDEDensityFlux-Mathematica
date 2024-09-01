(* d_3ckdv.m *)

debug = False;

(* 3 Component Korteweg-de Vries Equation *)

eq[1][x,t] = D[u[1][x,t],t]-6*u[1][x,t]*D[u[1][x,t],x]-D[u[1][x,t],{x,3}]+
             D[u[2][x,t]^2+u[3][x,t]^2,x];
eq[2][x,t] = D[u[2][x,t],t]-2*D[u[1][x,t]*u[2][x,t],x];
eq[3][x,t] = D[u[3][x,t],t]-2*D[u[1][x,t]*u[3][x,t],x];

noeqs = 3;
name = "3-Compenent KdV Equation System";
parameters = {};
weightpars = {};

formrho[x,t] = {};

(* d_3ckdv.m *)
