(* d_verhee.m *)

debug = False;

(* Verheest eqs *)

eq[1][x,t]= D[u[1][x,t],t]+D[u[1][x,t]*(u[1][x,t]^2+
            u[2][x,t]^2),x]+D[u[1][x,t],{x,3}];
eq[2][x,t]= D[u[2][x,t],t]+D[u[2][x,t]*(u[1][x,t]^2+u[2][x,t]^2),x]+
            D[u[2][x,t],{x,3}];
noeqs = 2;
name = "Verheest System";
parameters = {};
weightpars = {};

formrho[x,t] = {};

(* d_verhee.m *)
