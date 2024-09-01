(* data file d_ss.m *)

debug = False;

(* sine-Gordon Equation *)

eq[1][x,t] = D[u[1][x,t],t]-u[2][x,t];
eq[2][x,t] = D[u[2][x,t],t]-D[u[1][x,t],{x,2}]+alpha*Sin[u[1][x,t]];

noeqs = 2;
name = "System Sine-Gordon Equation";
parameters = {};
weightpars = {alpha};

(**** user can supply the rhorank and/or the name for the output file  ****)

(* rhorank = 6; *)
(* myfile = "kdvr6.o"; *) 

(**** user can give the weights of u[1] and partial t, make  ****)
(**** sure they are correct! If not, you will see!           ****)

(*
givenscalerules = {weight[d/dt] -> 1, weight[alpha] -> 2, 
                   weightu[2] -> 1, weightu[1] -> 0}; 
*)

(* note: this u[1] is really D[u[1][x,t],x]   *)
(*
weight[d/dt] = 1; 
weight[alpha] = 2; 
weightu[2] = 1;
weightu[1] = 0; 
*) 

formrho[x,t] = {};

(**** user can supply the form of rho ****)

(* already modified for trick  *)
(* 
formrho[x,t] = {(1/16)*u[1][x,t]^4-(1/4)*D[u[1][x,t],{x,1}]^2};
*)  

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

(* end of data file d_ss.m *)
