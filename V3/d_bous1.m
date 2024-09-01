
(* d_bous1.m *)

debug = True ;

(* Boussinesq System *)

eq[1][x,t] = D[u[1][x,t],t]+D[aa*u[1][x,t]*u[2][x,t],x]+bb*D[u[2][x,t],{x,3}] ;

eq[2][x,t] = D[u[2][x,t],t]+D[u[1][x,t],x]+cc*u[2][x,t]*D[u[2][x,t],x] ;

noeqs = 2 ;
name = "Boussinesq System-1";
parameters = {aa,bb,cc} ;
dimparams = {} ;

formrho[x,t] = {} ;

(* end of d_bous1.m *)
