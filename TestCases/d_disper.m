(* d_disper.m *)

debug = False;

(* Dispersive Long Wave Equations *)
 
eq[1][x,t] = D[u[1][x,t],t]+D[u[1][x,t]*u[2][x,t],x]+
             aa*D[u[2][x,t],x]+bb*D[u[2][x,t],{x,3}];

eq[2][x,t] = D[u[2][x,t],t]+u[2][x,t]*D[u[2][x,t],x]+D[u[1][x,t],x];

noeqs = 2;
name = "Dispersive Long Wave System";
parameters = {bb};
weightpars = {aa}; 

formrho[x,t] = {};

(* d_disper.m *)
