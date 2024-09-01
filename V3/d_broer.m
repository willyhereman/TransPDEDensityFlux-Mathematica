(* d_broer.m *)

debug = False;

(* Kaup-Broer System *)

eq[1][x,t]= D[u[1][x,t],t]-u[1][x,t]*D[u[1][x,t],x]-
            D[u[2][x,t],x]+1/2*D[u[1][x,t],{x,2}];

eq[2][x,t]= D[u[2][x,t],t]-D[u[1][x,t],x]*u[2][x,t]-u[1][x,t]*D[u[2][x,t],x]-
            1/2*D[u[2][x,t],{x,2}];
noeqs = 2;
name="Kaup-Broer System";
parameters = {};
weightpars = {};

formrho[x,t] = {};

(* end of data file d_broer.m *)
