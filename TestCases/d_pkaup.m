

(* d_pkaup.m *)

debug = False;

(* Kaup Syatem *)

eq[1][x,t] = D[u[1][x,t],t]+D[gg*D[u[1][x,t],{x,2}]+
             aa*u[2][x,t]*D[u[1][x,t],x]+bb*u[1][x,t]*
             u[2][x,t]^2+cc*u[1][x,t]^2,x];

eq[2][x,t] = D[u[2][x,t],t]+D[hh*D[u[2][x,t],{x,2}]+dd*u[2][x,t]*D[u[2][x,t],x]+
             ee*u[2][x,t]^3+ff*u[1][x,t]*u[2][x,t],x];
noeqs = 2;
name="Kaup System (parameterized)";
parameters = {aa,bb,cc,dd,ee,ff,gg,hh};  
weightpars = {};

formrho[x,t] = {};

(* d_pkaup.m *)
