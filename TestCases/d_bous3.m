
(* d_bous3.m *)

debug = True ;

(* Boussinesq System *)

eq[1][x,t] = D[u[1][x,t],t]+D[u[2][x,t],x] ;

eq[2][x,t] = D[u[2][x,t],t]+bb*D[u[1][x,t],x]-u[1][x,t]*D[u[1][x,t],x]-
             aa*D[u[1][x,t],{x,3}];

noeqs = 2 ;
name = "Boussinesq System-3";
parameters = {aa} ;
dimparams = {bb} ;

formrho[x,t] = {} ;

(* end of d_bous3.m *)
