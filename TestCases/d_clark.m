
(* d_clark.m *)

debug = False;

(* Third order equation in paper by Clarkson, Mansfield and Priestly *)

eq[1][x,t] = D[u[1][x,t],t]+2*kk*D[u[1][x,t],x]-u[1][x,t]*D[u[1][x,t],{x,3}]-
       aa*u[1][x,t]*D[u[1][x,t],x]-bb*D[u[1][x,t],x]*D[u[1][x,t],{x,2}];

(* aa, bb and kk are constant parameters *)

noeqs = 1;
name = "Third-Order Equation (Clarkson et.al.-parameterized)";
parameters = {bb};
weightpars = {aa,kk};

formrho[x,t] = {};

(* d_clark.m *)
