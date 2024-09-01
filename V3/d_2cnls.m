(* d_2cnls.m *)

debug = False;

(* 2-Compenent Nonlinear Schrodinger Equation *)

F = 2*Sum[u[i][x,t]^2,{i,1,4}];

eq[1][x,t] = D[u[1][x,t],t]+D[u[3][x,t],{x,2}]+F*u[3][x,t]; 
eq[2][x,t] = D[u[2][x,t],t]+D[u[4][x,t],{x,2}]+F*u[4][x,t];
eq[3][x,t] = D[u[3][x,t],t]-D[u[1][x,t],{x,2}]-F*u[1][x,t];
eq[4][x,t] = D[u[4][x,t],t]-D[u[2][x,t],{x,2}]-F*u[2][x,t];

noeqs = 4;
name = "2-Compenent Nonlinear Schrodinger Equation";
parameters = {};
weightpars = {};

formrho[x,t] = {};

(* d_2cnls.m *)
