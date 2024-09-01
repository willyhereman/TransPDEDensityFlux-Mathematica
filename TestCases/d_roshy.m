
(* d_roshy.m *)

debug = False;

(* Rosenau-Hyman Equation *)

eq[1][x,t] = D[u[1][x,t],t]+u[1][x,t]*D[u[1][x,t],{x,3}]+
             aa*u[1][x,t]*D[u[1][x,t],x]+3*D[u[1][x,t],x]*D[u[1][x,t],{x,2}];

noeqs = 1;
name = "Rosenau-Hyman Equation";
parameters = {};
weightpars = {aa};

formrho[x,t] = {};  

(* d_roshy.m *)
