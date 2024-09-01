
(* start of data file d_hirsat.m *)

debug = False;

(* Hirota-Satsuma System *)

eq[1][x,t]=D[u[1][x,t],t]-aa*D[u[1][x,t],{x,3}]-
           6*aa*u[1][x,t]*D[u[1][x,t],x]+6*u[2][x,t]*D[u[2][x,t],x];

eq[2][x,t]=D[u[2][x,t],t]+D[u[2][x,t],{x,3}]+3*u[1][x,t]*D[u[2][x,t],x];

noeqs = 2;
name = "Hirota-Satsuma System";
aa = 1/2;
parameters = {};
weightpars = {};

formrho[x,t] = {};

(* end of data file d_hirsat.m *)
