

(* d_mvdnls.m *)

debug = False;

(* MVDNLS eqs, note the extra term with `beta' *)

eq[1][x,t]= D[u[1][x,t],t]+D[u[1][x,t]*(u[1][x,t]^2+u[2][x,t]^2)+
            beta*u[1][x,t]-D[u[2][x,t],x],x];
eq[2][x,t]= D[u[2][x,t],t]+D[u[2][x,t]*(u[1][x,t]^2+u[2][x,t]^2)+
            D[u[1][x,t],x],x];
noeqs = 2;
name = "MVDNLS System";
parameters = {};
weightpars = {beta};

formrho[x,t] = {};

(* d_mvdnls.m *)
