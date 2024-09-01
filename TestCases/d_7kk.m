(* d_7kk.m *)

debug = False;

(* Seventh order KK Case *)

eq[1][x,t] = D[u[1][x,t],t]+aa*u[1][x,t]^3*D[u[1][x,t],x]+bb*D[u[1][x,t],x]^3+
             cc*u[1][x,t]*D[u[1][x,t],x]*D[u[1][x,t],{x,2}]+
             dd*u[1][x,t]^2*D[u[1][x,t],{x,3}]+
             ee*D[u[1][x,t],{x,2}]*D[u[1][x,t],{x,3}]+
             ff*D[u[1][x,t],x]*D[u[1][x,t],{x,4}]+
             gg*u[1][x,t]*D[u[1][x,t],{x,5}]+D[u[1][x,t],{x,7}]; 
noeqs = 1;
name = "Seventh order KK Equation";
aa = 2016; bb = 630; cc = 2268; dd = 504; ee = 252; ff = 147; gg = 42;
parameters = {};
weightpars = {};

formrho[x,t] = {};

(* d_7kk.m *)
