(* data file d_malf.m *)

debug = False;

(* Some Korteweg-de Vries Type Equation *)

eq[1][x,t] = D[u[1][x,t],t]-3*(D[u[1][x,t],x])^2+D[u[1][x,t],{x,3}];

noeqs = 1;
name = "Malfliet Equation";
parameters = {};
weightpars = {};

(**** user can supply the rhorank and/or the name for the output file  ****)

(* rhorank = 6; *)
(* myfile = "kdvr6.o"; *) 

(**** user can give the weights of u[1] and partial t, make  ****)
(**** sure they are correct! If not, you will see!           ****)

(* weightu[1] = 2; *)
(* weight[t] = 3;  *)  

formrho[x,t] = {};  

(**** user can supply the form of rho ****)

(* for rank 6: *)
(* 
formrho[x,t] = {c[1]*u[1][x,t]^3+c[2]*D[u[1][x,t],x]^2}; 
*) 
(* for rank 8: *)
(*
formrho[x,t] = {c[1]*u[1][x,t]^4+c[2]*u[1][x,t]*D[u[1][x,t],x]^2+
                c[3]*D[u[1][x,t],{x,2}]^2+
                c[4]*D[u[1][x,t],x]*D[u[1][x,t],{x,3}]}; 
*)

(* end of data file d_malf.m *)
