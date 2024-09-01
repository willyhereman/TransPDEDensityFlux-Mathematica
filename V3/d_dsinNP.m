(* Data file d_dsinNP.m *)

(* Double Sine-Gordon Equation *)
eq[1][x,t] = D[D[u[1][x,t],t],x] - Sin[u[1][x,t]] - Sin[2*u[1][x,t]];

noeqs = 1;
name = "Double Sine-Gordon Equation";
parameters = {};
weightpars = {};

(**** User can supply the rhorank and/or the name for the output file.  ****)
(* rhorank = 2; *)
(* Only has solution for rank 2 *)
(* myfile = "filename.o"; *) 

(**** User can give the weights of u[1] and partial t.     ****)
(**** Make sure they are correct!  If not, you will see!   ****)
(*
givenscalerules = {weight[d/dt] -> -1, weightu[1] -> 0}; 
*)

(**** User can supply the form of rho. ****)
formrho[x,t] = {};

(* End of data file d_dsinNP.m *)
