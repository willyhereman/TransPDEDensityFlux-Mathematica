
(* d_7ski.m *)

debug = False;

(* Seventh order KdV-SK-Ito Case *)

eq[1][x,t] = D[u[1][x,t],t]+aa*u[1][x,t]^3*D[u[1][x,t],x]+bb*D[u[1][x,t],x]^3+
             cc*u[1][x,t]*D[u[1][x,t],x]*D[u[1][x,t],{x,2}]+
             dd*u[1][x,t]^2*D[u[1][x,t],{x,3}]+
             ee*D[u[1][x,t],{x,2}]*D[u[1][x,t],{x,3}]+
             ff*D[u[1][x,t],x]*D[u[1][x,t],{x,4}]+
             gg*u[1][x,t]*D[u[1][x,t],{x,5}]+D[u[1][x,t],{x,7}]; 
noeqs = 1;
name = "Seventh order KdV-SK-Ito Equation";
aa = 252; bb = 63; cc = 378; dd = 126; ee = 63; ff = 42; gg = 21;
parameters = {};
weightpars = {};

formrho[x,t] = {};

(* d_7ski.m *)

