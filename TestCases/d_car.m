
(* d_car.m *)

debug = False;

(* Carleman System *)

eq[1][x,t] = D[u[1][x,t],t]+D[u[1][x,t],x]+aa*u[1][x,t]*u[2][x,t];

eq[2][x,t] = D[u[2][x,t],t]-D[u[2][x,t],x]+bb*u[1][x,t]*u[2][x,t];

noeqs = 2;
name="Carleman System";
aa = -1; bb = 1;
parameters = {};
weightpars = {};

formrho[x,t] = {};

(* d_car.m *)
