
(* d_ito.m *)

debug = False;

(* Ito System *)

eq[1][x,t] = D[u[1][x,t],t]+aa*D[u[1][x,t],{x,3}]+bb*u[1][x,t]*D[u[1][x,t],x]+
             cc*u[2][x,t]*D[u[2][x,t],x]; 
eq[2][x,t] = D[u[2][x,t],t]+dd*D[(u[1][x,t]*u[2][x,t]),x];
noeqs = 2;
name = "Ito System";
aa = 1;
bb = 6;
cc = 2;
dd = 2;
parameters = {};
weightpars = {};

formrho[x,t] = {};

(* d_ito.m *)
