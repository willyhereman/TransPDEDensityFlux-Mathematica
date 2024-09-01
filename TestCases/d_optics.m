(* d_optics.m *)

debug = False;

(* Banerjee's equations for cascading optics *)

eq[1][x,t]= D[u[1][x,t],t]-aa1*D[u[2][x,t],{x,2}]+dd1*u[1][x,t]+ 
            bb1*(u[1][x,t]*u[3][x,t]+u[2][x,t]*u[4][x,t]);

eq[2][x,t]= D[u[2][x,t],t]+aa1*D[u[1][x,t],{x,2}]+dd1*u[2][x,t]+
            bb1*(u[1][x,t]*u[4][x,t]-u[2][x,t]*u[3][x,t]);

eq[3][x,t]= D[u[3][x,t],t]-aa2*D[u[4][x,t],{x,2}]+dd2*u[3][x,t]+
            bb2*(u[1][x,t]*u[1][x,t]-u[2][x,t]*u[2][x,t]);

eq[4][x,t]= D[u[4][x,t],t]+aa2*D[u[3][x,t],{x,2}]+dd2*u[3][x,t]+
            bb2*(u[1][x,t]*u[2][x,t]+u[2][x,t]*u[1][x,t]);

noeqs = 4;

name = "Optical System";

parameters = {aa1,aa2,bb1,bb2};
weightpars = {dd1,dd2};

formrho[x,t] = {};

(* d_optics.m *)
