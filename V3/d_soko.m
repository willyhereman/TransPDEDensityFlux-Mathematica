(* d_soko.m *)

debug = False;

(* Drinfel'd-Sokolov System *)

aa = 3; bb = 2; cc = 2; dd = 1;

eq[1][x,t] = D[u[1][x,t],t]+aa*u[2][x,t]*D[u[2][x,t],x];

eq[2][x,t] = D[u[2][x,t],t]+bb*D[u[2][x,t],{x,3}]+
             cc*u[1][x,t]*D[u[2][x,t],x]+
             dd*D[u[1][x,t],x]*u[2][x,t];

noeqs = 2;
name = "Drinfel'd-Sokolov System";
parameters = {};
weightpars = {};

formrho[x,t] = {}; 

(* d_soko.m *)
