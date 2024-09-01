
(* d_scham.m *)

debug = False;

(* Schamel equation-original *)
(* 16*D[u[x,t],t]+30*Sqrt[u[x,t]]*D[u[x,t],x]+D[u[x,t],{x,3}] == 0; *)

(* Schamel equation-after transformation u^(1/2) -> u *)

eq[1][x,t] = 16*u[1][x,t]*D[u[1][x,t],t]+30*u[1][x,t]^2*D[u[1][x,t],x]+
             3*D[u[1][x,t],x]*
             D[u[1][x,t],{x,2}]+u[1][x,t]*D[u[1][x,t],{x,3}];

noeqs = 1;
name = "Schamel Equation";
parameters = {};
weightpars = {};

formrho[x,t] = {};

(* d_scham.m *) 
