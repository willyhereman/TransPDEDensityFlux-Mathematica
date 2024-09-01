
(* d_5sk.m *)

debug = False;

(* Fifth order KdV-Sawada-Kotera Case *)

eq[1][x,t] = D[u[1][x,t],t]+aa*u[1][x,t]^2*D[u[1][x,t],x]+bb*D[u[1][x,t],x]*
             D[u[1][x,t],{x,2}]+cc*u[1][x,t]*D[u[1][x,t],{x,3}]+
             D[u[1][x,t],{x,5}];
noeqs = 1;
name = "Fifth order KdV-Sawada-Kotera Equation";
aa = 5;
bb = 5;
cc = 5;
parameters = {};
weightpars = {};

formrho[x,t] = {};

(* d_5sk.m *)
