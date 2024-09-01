
(* d_5kdv.m *)

debug = False;

(* Fifth order KdV equation *)

eq[1][x,t] = D[u[1][x,t],t]+aa*u[1][x,t]^2*D[u[1][x,t],x]+bb*D[u[1][x,t],x]*
	     D[u[1][x,t],{x,2}]+cc*u[1][x,t]*D[u[1][x,t],{x,3}]+
             D[u[1][x,t],{x,5}];

(* aa, bb and cc are constant parameters *)

noeqs = 1;
name = "Fifth-order KdV Equation (parameterized)";

parameters = {aa,bb,cc};
weightpars = {};

formrho[x,t] = {};

(* d_5kdv.m *)
