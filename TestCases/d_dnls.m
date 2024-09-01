

(* d_dnls.m *)

debug = False;

(* DNLS eqs. *)

eq[1][x,t]= D[u[1][x,t],t]+3*u[1][x,t]^2*D[u[1][x,t],x]+
            u[2][x,t]^2*D[u[1][x,t],x]+2*u[1][x,t]*u[2][x,t]*D[u[2][x,t],x]-
            D[u[2][x,t],{x,2}]; 
eq[2][x,t]= D[u[2][x,t],t]+2*u[1][x,t]*u[2][x,t]*D[u[1][x,t],x]+
            u[1][x,t]^2*D[u[2][x,t],x]+3*u[2][x,t]^2*D[u[2][x,t],x]+
            D[u[1][x,t],{x,2}];
noeqs = 2;
name = "DNLS System";
parameters = {};
weightpars = {};

formrho[x,t] = {};

(* d_dnls.m *)
