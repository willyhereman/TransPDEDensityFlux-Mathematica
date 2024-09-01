(* Data file d_mikhSY.m *)

(* Mikhailov System *)
eq[1][x,t] = D[u[1][x,t],t] - u[2][x,t];
eq[2][x,t] = D[u[2][x,t],t] - D[u[1][x,t],{x,2}] + 
              alpha*Exp[u[1][x,t]] + alpha*Exp[-2*u[1][x,t]];

noeqs = 2;
name = "Mikhailov System";
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

(* End of data file d_mikhSY.m *)
