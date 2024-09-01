(* d_5ito.m *)

debug = False;

(* Fifth order KdV-Ito Case *)

eq[1][x,t] = D[u[1][x,t],t]+aa*u[1][x,t]^2*D[u[1][x,t],x]+bb*D[u[1][x,t],x]*
             D[u[1][x,t],{x,2}]+cc*u[1][x,t]*D[u[1][x,t],{x,3}]+
             D[u[1][x,t],{x,5}];
noeqs = 1;
name = "Fifth order KdV-Ito Equation";
aa = 2;
bb = 6;
cc = 3;
parameters = {};
weightpars = {};

formrho[x,t] = {};

(* d_5ito.m *) 
