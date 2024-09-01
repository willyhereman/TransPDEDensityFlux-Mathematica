
(* d_nls.m *)

debug = False;

(* NLS eqs. *)

eq[1][x,t] = D[u[1][x,t],t]-D[u[2][x,t],{x,2}]+
             aa*u[2][x,t]*(u[1][x,t]^2+u[2][x,t]^2);
eq[2][x,t] = D[u[2][x,t],t]+D[u[1][x,t],{x,2}]-
             aa*u[1][x,t]*(u[1][x,t]^2+u[2][x,t]^2);
noeqs = 2;
name = "NLS System: real and imaginary parts";
aa = 2; 
parameters = {};
weightpars = {};

formrho[x,t] = {};

(* d_nls.m *)
