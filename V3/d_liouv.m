(* data file d_liouvnop.m *)

debug = False;
debugLevel = 1;

(* Liouville Equation, no parameter *)
eq[1][x,t] = D[D[u[1][x,t],t],x] - Exp[u[1][x,t]];

noeqs = 1;
name = "Liouville Equation, no parameter";
parameters = {};
weightpars = {};

(**** user can supply the rhorank and/or the name for the output file  ****)
(* rhorank = 4; *)
(* myfile = "liouvR6.o"; *) 

(**** user can give the weights of u[1] and partial t, make  ****)
(**** sure they are correct! If not, you will see!           ****)
(* 
weightu[1] = 1;    (* note: this u[1] is really D[u[1][x,t],x]   *)
weight[t] = -1;  

givenscalerules = {weight[d/dt] -> -1, weightu[1] -> 1}; 
*)

(**** user can supply the form of rho ****)
formrho[x,t] = {};

(* formrho[x,t] = {(1/16)*u[1][x,t]^4-(1/4)*D[u[1][x,t],{x,1}]^2};*)  (* already modified for trick  *)
 
(*  end of data file d_liouvnop.m *)
