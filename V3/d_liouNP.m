(* Data file d_liouNP.m *)

(* Liouville Equation *)
eq[1][x,t] = D[D[u[1][x,t],t],x] - Exp[u[1][x,t]];

noeqs = 1;
name = "Liouville Equation";
parameters = {};
weightpars = {};

(**** User can supply the rhorank and/or the name for the output file.  ****)
(* rhorank = 4; *)
(* myfile = "filename.o"; *) 

(**** User can give the weights of u[1] and partial t.     ****)
(**** Make sure they are correct!  If not, you will see!   ****)
(*
givenscalerules = {weight[d/dt] -> -1, weightu[1] -> 0}; 
*)

(**** User can supply the form of rho. ****)
formrho[x,t] = {};

(* End of data file d_liouNP.m *)
