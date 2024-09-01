(* Data file d_tzetP.m *)

(* Tzetzeica Equation w/ weighted parameter *)
eq[1][x,t] = D[D[u[1][x,t],t],x] - alpha*Exp[u[1][x,t]] + alpha*Exp[-2*u[1][x,t]];

noeqs = 1;
name = "Tzetzeica Equation w/ weighted parameter";
parameters = {};
weightpars = {alpha};

(**** User can supply the rhorank and/or the name for the output file.  ****)
(* rhorank = 4; *)
(* myfile = "filename.o"; *) 

(**** User can give the weights of u[1] and partial t.     ****)
(**** Make sure they are correct!  If not, you will see!   ****)
(*
givenscalerules = {weight[d/dt] -> 1, weightu[1] -> 0}; 
*)

(**** User can supply the form of rho. ****)
formrho[x,t] = {};

(* End of data file d_tzetP.m *)