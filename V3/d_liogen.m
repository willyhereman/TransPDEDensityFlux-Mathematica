(* data file d_liogen.m *)

debug = False;
debugLevel = 1;

(* Liouville Equation, generalized *)
eq[1][x,t] = D[D[u[1][x,t],t],x] - CC*Exp[aa*u[1][x,t]] - DD*Exp[-aa*u[1][x,t]];

noeqs = 1;
name = "Liouville Equation, no parameter";
parameters = {aa};
weightpars = {CC,DD};

(**** user can supply the rhorank and/or the name for the output file  ****)
formrho[x,t]={}; 

(**** user can give the weights of u[1] and partial t, make  ****)
(**** sure they are correct! If not, you will see!           ****)


(*  end of data file d_liogen.m *)
