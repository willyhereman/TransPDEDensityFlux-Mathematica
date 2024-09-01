(* d_pito.m *)

debug = False;

(* Ito System *)

eq[1][x,t] = D[u[1][x,t],t]-aa*D[u[1][x,t],{x,3}]-bb*u[1][x,t]*D[u[1][x,t],x]-
             cc*u[2][x,t]*D[u[2][x,t],x]; 

eq[2][x,t] = D[u[2][x,t],t]-dd*D[(u[1][x,t]*u[2][x,t]),x];
noeqs = 2;
name="Ito System (parameterized)";
parameters = {aa,bb,cc,dd};
weightpars = {};

formrho[x,t] = {};

(* end of data file d_pito.m *)
