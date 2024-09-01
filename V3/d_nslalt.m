
(* d_nlsalt.m *)

debug = False;

(* NLS eqs.: original & complex conjagate, after i is absorbed in scale on t *)

eq[1][x,t] = D[u[1][x,t],t]-D[u[1][x,t],{x,2}]+
             2*u[1][x,t]^2*u[2][x,t];
eq[2][x,t] = D[u[2][x,t],t]+D[u[2][x,t],{x,2}]-
             2*u[2][x,t]^2*u[1][x,t];

noeqs = 2;
name = "NLS System: equation and conjugate";
parameters = {};
weightpars = {};

formrho[x,t] = {};
(*
formrho[x,t] = {c[1]*u[1][x,t]*D[u[1][x,t],x]*u[2][x,t]^2+
                c[2]*1/3*D[u[1][x,t],{x,2}]*D[u[2][x,t],x]+
                c[3]*u[2][x,t]*D[u[2][x,t],x]*u[2][x,t]^2+
                c[4]*1/3*D[u[2][x,t],{x,2}]*D[u[1][x,t],x]};
*)

(* d_nlsalt.m *)

