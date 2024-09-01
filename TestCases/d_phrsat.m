(* start of data file d_phrsat.m *)

debug = False;

(* Hirota-Satsuma System with one parameter *)

eq[1][x,t]=D[u[1][x,t],t]-aa*D[u[1][x,t],{x,3}]-
           6*aa*u[1][x,t]*D[u[1][x,t],x]+6*u[2][x,t]*D[u[2][x,t],x];

eq[2][x,t]=D[u[2][x,t],t]+D[u[2][x,t],{x,3}]+3*u[1][x,t]*D[u[2][x,t],x];

noeqs = 2;
name = "Hirota-Satsuma System (parameterized)";
parameters = {aa};
weightpars = {};

(**** user can supply the rhorank and/or the name for the output file  ****)

(* rhorank = 6; *)
(* myfile = "phrsatr6.o"; *) 

(**** user can give the weights of u[1], u[2], and partial t, make  ****)
(**** sure they are correct! If not, you will see!                  ****)
(* weightu[1] = 2; *)
(* weightu[2] = 2; *)
(* weight[t]  = 3; *)  

formrho[x,t] = {};  

(**** user can supply the form of rho ****)
(* for rank 6: *)
(*
formrho[x,t]={c[1]*u[1][x,t]^3+c[2]*u[1][x,t]*u[2][x,t]^2+
              c[3]*D[u[1][x,t],x]^2+c[4]*D[u[2][x,t],x]^2}; 
*)

(* end of data file d_phrsat.m *)
