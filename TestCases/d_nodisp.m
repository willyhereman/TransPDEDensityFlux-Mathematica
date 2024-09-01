(* d_nodisp.m *)

debug = False;

(* Non-dispersive Long Wave Equations *)

eq[1][x,t] = D[u[1][x,t],t]+D[u[1][x,t]*u[2][x,t],x];

eq[2][x,t] = D[u[2][x,t],t]+D[u[2][x,t],x]*u[2][x,t]+D[u[1][x,t],x];

noeqs = 2;
name = "Non-dispersive Long Wave System";
parameters = {};
weightpars = {}; 

(* weightu[2] = 1/2; *)

formrho[x,t] = {};

(* d_nodisp.m *)
