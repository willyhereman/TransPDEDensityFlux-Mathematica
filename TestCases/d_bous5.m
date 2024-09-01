(* d_bous5.m *)

debug = False;

(* Boussinesq System *)

eq[1][x,t] = D[u[1][x,t],t]+cc*D[u[2][x,t],x];

eq[2][x,t] = D[u[2][x,t],t]+bb*D[u[1][x,t],x]-3*dd*u[1][x,t]*D[u[1][x,t],x]-
             aa*D[u[1][x,t],{x,3}];

noeqs = 2;
name = "Boussinesq System";
parameters = {aa,cc};
weightpars = {bb,dd};

formrho[x,t] = {};

(* d_bous5.m *)

