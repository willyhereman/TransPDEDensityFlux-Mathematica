
(* File: TransPDEDensityFluxV3.m *)
(* Third version of TransPDEDensityFlux.m *)

(* Last updated: Saturday, October 4, 2003 at 24:00, by WH in Boulder *)
(* Added code of Eklund to keep track of all rhos and js in a lists *)
(* All changes signed by WH 10/04/2003 and WH 10/05/2003 *)

(* Based on TransPDEDensityFluxV2.m and TransPDEDensityFluxPaul0812.m *)
(* TransPDEDensityFluxV2.m: *)
(* Includes the latest version of the continuous homotopy operator *)
(* Code inserted in this file, see HOMOTOPY OPERATOR *)
(* Code works under Mathematica 5.0 and earlier versions *)
(* Also minor changes in the print statements *)
(* All changes signed by WH 10/04/2003 *)

(* TransPDEDensityFluxPaul0812.m *)
(* Last updated, 08/07/2003 by P.Adams *)

(* History:                                                                  *)
(* Time-stamp: <Mon Sep 22 21:58:49 MDT 2003>                                *)
(* TransPDEDensityFlux.m version Paul Adams. Version March 7, 2003           *)
(* Last Updated: July 1, 2003 by Mike C.                                     *)
(* Integration of Homotopy Operator                                          *)
(* search for "homotopyOperator", "Mike C.", and "integration" for changes   *)
(* Note:                                                                     *)
(* The homotopyOperator code must be in a special place.                     *)
(* If this file is in, say,                                                  *)
(* d:\iii\jjj\kkk\TransPDE\TransPDEDensityFlux.m                             *)
(* then the Homotopy Operator code must be in:                               *)
(* d:\iii\jjj\kkk\PDE1DFlx\homotopyOperator.m                                *)
(* That is, put the PDE1DFlx directory at the same level as the directory    *)
(* containing this file, but don't put this code and the Homotopy Operator   *)
(* code in the same directory.                                               *)

(*****************************************************************************)
(*                                                                           *)
(*          *** M A T H E M A T I C A   P R O G R A M ***                    *)
(*                                                                           *)
(*      SYMBOLIC COMPUTATION of CONSERVED DENSITIES for SYSTEMS of           *)
(*                  NONLINEAR EVOLUTION EQUATIONS                            *)
(*                                                                           *)
(* program name: TransPDEDensityFluxV3.m                                     *)
(*                                                                           *)
(* purpose: computation of the conserved densities and corresponding flux    *)
(*          with possible compatibility conditions, verification of the      *)
(*          conservation law.                                                *)
(*                                                                           *)
(* input to condens.m : system of nonlinear evolution equations of any order,*)
(*                      any degree, polynomial type, in variables x and t,   *)
(*                      only constant parameters, functions in x and t are   *)
(*                      NOT allowed as parameters                            *)
(*                                                                           *)
(*                      u[i]_t = f(u[1],...,u[N],u[1]_{nx},...,u[N]_{nx})    *)
(*                      with i=1,...,N; and n=1,2,...                        *)
(*                                                                           *)
(* output : density and flux of desired rank (if it exists),                 *)
(*          and compatibility conditions on parameters, if applicable        *)
(*                                                                           *)
(* tested on : IBM RISC 6000, IBM Compatible PC PIII, SGI Indigo2 XL         *)
(*                                                                           *)
(* language : Mathematica 5.0, 4.0, 3.0, and 2.2 (also versions 2.0 and 2.1) *)
(*                                                                           *)
(* authors: Unal Goktas, Paul Adams, and Willy Hereman                       *)
(*          Department of Mathematical and Computer Sciences                 *)
(*          Colorado School of Mines                                         *)
(*          Golden, CO 80401-1887, USA                                       *)
(*                                                                           *)
(* Version 3.0: October 5, 2003                                              *)
(*                                                                           *)
(* Copyright 2003                                                            *)
(*                                                                           *)
(*****************************************************************************)

Clear["Global`*"]; 

(* ------------------------------------------------------------------------- *)

(*****************************************************************************)
(* commentinter[]: prints a welcome message                                  *)
(*****************************************************************************)
(* WH 10/04/2003 Version is now 2.0, dated October 4, 2003 *)
commentinter[] := Block[{},
Print["*********************************************************"];
Print["        WELCOME TO THE MATHEMATICA PROGRAM               "];
Print["  by UNAL GOKTAS, PAUL ADAMS, and WILLY HEREMAN          "];
Print["    FOR THE COMPUTATION OF CONSERVED DENSITIES           "];
Print["  WITH OR WITHOUT TRANSCENDENTAL NONLINEARITIES          "];
Print["     Version 3.0 released on October 5, 2003             "];
Print["                 Copyright 2003                          "];
Print["*********************************************************"]
]; (* end commentinter *)

(*****************************************************************************)
(* cls: clears the screen                                                    *)
(*****************************************************************************)
cls := Print["\n\n\n\n\n\n\n\n\n\n\n\n\n"];

(*****************************************************************************)
(* printpage[ , ]: a subroutine for menu                                     *)
(*****************************************************************************)
printpage[n_,page_] := Module[
{i,choice,lenpage},
(* WH 10/04/2003 *)
(* cls; *)
lenpage = Length[page[n]];
Print[" "];
Print["  *** MENU INTERFACE ***  (page: ",n,")"];
Print["-------------------------------------------"];
For[i=1,i <= lenpage,i++,
   Print[Part[Part[page[n],i],1]]];
Print[" nn) Next Page"];
Print[" tt) Your System"];
Print[" qq) Exit the Program"];
Print["------------------------------------------"];
If[Not[NumberQ[choice]],
  choice = Input["ENTER YOUR CHOICE: "];
];  
Return[choice]; ]; (* end printpage *)

(*****************************************************************************)
(* menu: creates the menu                                                    *)
(*****************************************************************************)
menu := Module[
{counterpage = 1,menulist,numpages,page,
choice1,control,choice2,choice3,lenmenulist,i},
menulist = {
{"  1) KdV Equation (d_kdv.m)"},
{"  2) Modified KdV Equation (d_mkdv.m)"},
{"  3) Fifth Order KdV Equation-parameterized (d_5kdv.m)"},
{"  4) Fifth Order KdV Equation-Lax Case (d_5lax.m)"},
{"  5) Fifth Order KdV Equation-SK Case (d_5sk.m)"},
{"  6) Fifth Order KdV Equation-KK Case (d_5kk.m)"},
{"  7) Fifth Order KdV Equation-Ito Case (d_5ito.m)"},
{"  8) Seventh Order KdV Equation-parameterized (d_7kdv.m)"},
{"  9) Seventh Order KdV Equation-Lax Case (d_7lax.m)"},
{" 10) Seventh Order KdV Equation-SK-Ito Case (d_7ski.m)"},
{" 11) Seventh Order KdV Equation KK Case (d_7kk.m)"},
{" 12) Schamel Equation (d_scham.m)"},
{" 13) Rosenau-Hyman Equation (d_roshy.m)"},
{" 14) Hirota-Satsuma System-parameterized (d_phrsat.m)"},
{" 15) Hirota-Satsuma System (d_hirsat.m)"},
{" 16) Ito System-parameterized (d_pito.m)"},
{" 17) Ito System (d_ito.m)"},
{" 18) NLS System (real and imaginary parts) (d_nls.m)"},
{" 19) NLS System (equation and conjugate) (d_nlsalt.m)"},
{" 20) DNLS System (d_dnls.m)"},
{" 21) MVDNLS System (d_mvdnls.m)"},
{" 22) Kaup System-parameterized (d_pkaup.m)"},
{" 23) Kaup System (d_kaup.m)"},
{" 24) Kaup-Broer System (d_broer.m)"},
{" 25) Drinfel'd-Sokolov System (d_soko.m)"},
{" 26) Dispersive Long Wave System (d_disper.m)"}, 
{" 27) Non-dispersive Long Wave System (d_nodisp.m)"}, 
{" 28) 3-Component KdV System (d_3ckdv.m)"},
{" 29) 2-Component Nonlinear Schrodinger Equation (d_2cnls.m)"},
{" 30) Boussinesq System (d_bous.m)"},
{" 31) Sine-Gordon Equation (d_sineNP.m)"},
{" 32) Sine-Gordon Equation w/ weighted parameter (d_sineP.m)"},
{" 33) Sine-Gordon System (d_sineSY.m)"},
{" 34) Hyperbolic Sine-Gordon Equation (d_sinhNP.m)"},
{" 35) Hyperbolic Sine-Gordon Equation w/ weighted parameter (d_sinhP.m)"},
{" 36) Hyperbolic Sine-Gordon System (d_sinhSY.m)"},
{" 37) Liouville Equation (d_liouNP.m)"},
{" 38) Liouville Equation w/ weighted parameter (d_liouP.m)"},
{" 39) Liouville System (d_liouSY.m)"},
{" 40) Double Sine-Gordon Equation (d_dsinNP.m)"},
{" 41) Multiple Sine-Gordon Equation (d_msinNP.m)"},
{" 42) Tzetzeica Equation (d_tzetNP.m)"},
{" 43) Tzetzeica Equation w/ weighted parameter (d_tzetP.m)"},
{" 44) Mikhailov System (d_mikhSY.m)"},
{" 45) Double-Liouville System (d_dlioSY.m)"}
}; (* closes menulist *)

menupagelen = 15;
lenmenulist = Length[menulist];
numpages = Ceiling[lenmenulist/menupagelen];
For[i = 1,i <= numpages,i++,
   page[i] = If[lenmenulist >= (i*menupagelen),
               menulist[[Table[k,{k,(i-1)*menupagelen+1,i*menupagelen}]]],
               menulist[[Table[k,{k,(i-1)*menupagelen+1,lenmenulist}]]]]];

choice1 = printpage[counterpage,page];

control := (
Switch[choice1,
tt,Print["Make sure that you have prepared the data file for the system"];
   Print["you want to test (similar to the data files we supplied)."];
(* WH 10/04/2003 *)
   choice2 = Input["Is your file is ready? (0 = no, 1 = yes): "];
   If[choice2 === 1,
    choice3 = InputString["Enter the name of your data file: "];
    Check[Get[choice3], 
          Print["ERROR opening data file.  Aborting."];
          Abort[]],
    Print["Aborting the computations!"];
    Print["Prepare your data file first, then start over."];
    Abort[] 
     ], 
nn,If[counterpage < numpages,counterpage++;
     choice1 = printpage[counterpage,page]; control,
     counterpage = 1; choice1 = printpage[1,page]; control],
qq,Print["Aborting the computations!"];Abort[],
1,<<d_kdv.m,2,<<d_mkdv.m,3,<<d_5kdv.m,4,<<d_5lax.m,5,<<d_5sk.m,
6,<<d_5kk.m,7,<<d_5ito.m,8,<<d_7kdv.m,9,<<d_7lax.m,10,<<d_7ski.m,
11,<<d_7kk.m,12,<<d_scham.m,13,<<d_roshy.m,14,<<d_phrsat.m,15,<<d_hirsat.m,
16,<<d_pito.m,17,<<d_ito.m,18,<<d_nls.m,19,<<d_nlsalt.m,20,<<d_dnls.m,
21,<<d_mvdnls.m,22,<<d_pkaup.m,23,<<d_kaup.m,24,<<d_broer.m,25,<<d_soko.m,
26,<<d_disper.m,27,<<d_nodisp.m,28,<<d_3ckdv.m,29,<<d_2cnls.m,30,<<d_bous.m,
31,<<d_sineNP.m,32,<<d_sineP.m,33,<<d_sineSY.m,34,<<d_sinhNP.m,35,<<d_sinhP.m,
36,<<d_sinhSY.m,37,<<d_liouNP.m,38,<<d_liouP.m,39,<<d_liouSY.m,40,
<<d_dsinNP.m,41,<<d_msinNP.m,42,<<d_tzetNP.m,43,<<d_tzetP.m,44,
<<d_mikhSY.m,45,<<d_dlioSY.m,_,Print["Aborting the computations!"];Abort[]
]; (* closes Switch *)
); (* end control *)

control; 

If[debugT,
   debugLevel = Input["Enter the level (0-4) of debug (0 = no debug): "]];
If[Not[NumberQ[storeLog]], 
  storeLog = Input["Save transcript in log file? (0 = no, 1 = yes): "];
];
If[storeLog==1,storeLog=True,storeLog=False];
If[storeLog,
   If[Not[StringQ[myfile]],
      myfile = InputString["Enter the name of the output file: "]]
   ];
]; (* end menu *)

(*****************************************************************************)
(* EulerD[  ]: applies the Euler operator for variational derivatives.       *)
(*             Written by Yu He in 1992, simplified by W. Hereman            *)
(*****************************************************************************)
EulerD[f_, (y_)[x_, r___], w:{x_, r___}] := 
  Module[{Dfuncs, Dtimes, dummyfunc}, 
    Dfuncs = Union[Cases[{f}, Derivative[__][y][__], Infinity]]; 
    Dtimes = (Head[Head[#1]] & ) /@ Dfuncs /. Derivative -> List; 
    Expand[D[f, y[x, r]] + (ReleaseHold[Thread[dummyfunc[(D[f, 
                 #1] & ) /@ Dfuncs, 
             (Hold[Apply[Sequence, #1]] & ) /@ 
              (Thread[{w, #1}] & ) /@ Dtimes]]] /. dummyfunc -> D) . 
        ((-1)^#1 & ) /@ (Apply[Plus, #1] & ) /@ Dtimes] ]; (* end EulerD *)
 
EulerD[f_, v:{(y_)[x_, r___], ___}, w:{x_, r___}] := 
   (EulerD[f, #1, w] & ) /@ v /; 
   If[Apply[And, (MatchQ[#1, _[Apply[Sequence, w]]] & ) /@ v], 
   True, Message[EulerD::argx, w]];

EulerD[f_, (y_)[x_], x_] := EulerD[f, y[x], {x}];
EulerD[f_, v:{(y_)[x_], ___}, x_] := EulerD[f, v, {x}];

(*****************************************************************************)
(* pdeform[]: takes an expression and prints it with subscripted derivatives *)
(*            and removes the functional dependence (i.e. f[x,t] -> f)       *)
(*            Written by Tracy Otto & Tony Miller (CSM-1995).                *)
(*            Modified by U. Goktas.                                         *)
(*****************************************************************************)
pdeform[expres_] := expres /. {
        Derivative[n__][u[k_]][x__] :>
                SequenceForm[u, Subscript[k],Subscript[","],
                        Subscript[
                         SequenceForm @@ Flatten[Table[#[[1]], {#[[2]]}]& /@
                                        Transpose[{{x}, {n}}]]]],
        u[n_][x__] :> SequenceForm[u,Subscript[n]] }; (* end pdeform *)

(*****************************************************************************)
(* length[]: a modified length function                                      *)
(*****************************************************************************)
length[a_] := Module[{l},
If[Head[a] === Plus,l = Length[a],
  If[Head[a] === Times,l = Length[a],l = 1 ] ] ;Return[l] ]; (* end length *)

(*****************************************************************************)
(* split[]: if free coefficients c[i] remain in the density, the density     *)
(*          is further split in independent pieces corresponding to each     *)
(*          coefficient c[i]                                                 *)
(*****************************************************************************)
split[expr_] := Module[{listofc,lenlistofc,ci,rhoi,rest}, 
    rest=Expand[expr];
    If[FreeQ[rest,c[__]], 
    Print[" "];
    Print["The density has no free coefficients."], 
    listofc=Union[Cases[rest,c[__],12]];
    lenlistofc=Length[listofc];
    Print[" "];
    Print["There is/are ", lenlistofc, " free coefficient(s) in the density."];
    Print["These free coefficients are: ", listofc, "."];
    Print["Splitting the density in independent pieces."];
     Do[
       ci=Part[listofc,i];
       rhoi=Expand[Coefficient[rest,ci]]; 
       rest=Expand[rest-ci*rhoi];
       Print[" "];
       Print["Part of the density with coefficient ", ci, ":"];
       Print[" "];
       Print[rhoi],{i,1,lenlistofc}]; (* end do *)
     Print[" "];
     Print["Part of the density that has no free coefficient:"];
     Print[" "];
     Print[rest];
     Print[" "] ] (* end if *) ]; (* end module *)

(*****************************************************************************)
(* normal[]: normalizes the density according to the coefficient of the      *)
(*           first term in rho[x,t], returns normalized forms of rho[x,t]    *)
(*           and J[x,t]. Does not divide by the coefficient if the           *)
(*           coefficient is not a number. Returns a list with rho in the     *)
(*           first place, and j in the second place. To be used on the list  *)
(*           that comes out of stripper[rho[x,t]]                            *)
(*****************************************************************************)
normal[mylist_List] := Module[{leadingcoef,facexpr,denrho,rhojlist}, 
(* Cancel common denominators first, keep track of these with parameters *)
facexpr = Factor[rho[x,t]];
denrho = Denominator[facexpr];
If[Not[NumberQ[denrho]], denrho = Factor[denrho]];
If[Not[FreeQ[Head[denrho],Times]],
   If[NumberQ[Part[denrho,1]] && Part[denrho,1] =!= 0, 
     denrho = denrho/Part[denrho,1]]];
If[Not[NumberQ[denrho]],  
  Print["WARNING!"];
(*  Print[" "]; *)
  Print["Both rho[x,t] and J[x,t] were multiplied by: ",denrho,"."]; 
  Print[" "];
  rho[x,t] = Expand[Factor[rho[x,t]*denrho]]; 
  J[x,t] = Expand[Factor[J[x,t]*denrho]]];
leadingcoef = Coefficient[rho[x,t],Part[mylist,1]];
If[NumberQ[leadingcoef] && leadingcoef =!= 0,
  rho[x,t] = Expand[rho[x,t]/leadingcoef];
  J[x,t] = Expand[J[x,t]/leadingcoef]]; 
rhojlist = {rho[x,t],J[x,t]};
Return[rhojlist]; ]; (* end normal *)

(*****************************************************************************)
(* stripper[]: cancels the numerical factors of each term of the argument    *)
(*             and puts these into a list after cancellation                 *)
(*****************************************************************************)
stripper[givenexpr_] := Module[
{lenexpr1,list = {},expr1,expr2,expr3,expr3s,lenexpr3,i,k,s},
expr1 = Expand[givenexpr];
If[Head[expr1] === Plus,lenexpr1 = Length[expr1],lenexpr1 = 1];
For[k = 1,k <= lenexpr1,k++,
   If[lenexpr1 == 1,expr2 = expr1,expr2 = Part[expr1,k]];
   expr2 = expr2 /. c[i_] :> 1;
   expr3 = FactorList[expr2];
   lenexpr3 = Length[expr3];
   For[s = 1,s <= lenexpr3,s++,
      expr3s = Part[expr3,s];
      If[FreeQ[expr3s,u],
        If[FreeQ[weightpars,Part[expr3s,1]],
          expr2 = expr2/Part[expr3s,1]^Part[expr3s,2];
          ],
        s = lenexpr3+1;
        ];
      ];
   list = Append[list,expr2];
   ];
Return[list]; ]; (* end stripper *)

(*****************************************************************************)
(* highestpair[]: a subroutine for highest order function                    *)
(*****************************************************************************)
highestpair[pairlist_] := Module[
{length,k,highestlist},
If[pairlist != {},
  length = Length[pairlist];
  highestlist = {Part[pairlist,1]};
  If[length >= 2,
    For[k = 2,k <= length,k++,
      If[Part[pairlist,k][[1]] > Part[highestlist,Length[highestlist]][[1]],
        highestlist = {Part[pairlist,k]},
        If[Part[pairlist,k][[1]]===Part[highestlist,Length[highestlist]][[1]],
	  highestlist = Union[Append[highestlist,pairlist[[k]]]]
          ];
        ];
      ];
   ];
 Return[highestlist],
 If[debug,
   Print["The expression (given to the function that computes the"]; 
   Print["highest order) does not have any derivatives!"]];
 Return[{{0,0}}];
 ]  ]; (* end highestpair *)

(*****************************************************************************)
(* highestorder[]: returns the highest order (or the term with the highest   *)
(*                 order) of the argument                                    *)
(*****************************************************************************)
highestorder[gexpr_,debug11_,debug12_,debug21_,debug22_,debug31_,debug32_] := 
Module[
{length1,i,newexpr1,length2,newexpr6,temporder1,temporder2,newexpr5,k,
newexpr2,newexpr3,temporder3,newexpr4,orderlist1 ={},orderlist2 = {},
orderlist3 = {},highestlist1,highestlist2,highestlist3,expr},
expr = Expand[gexpr];
If[Not[FreeQ[expr,Derivative]], 
  length1 = length[expr];
  temporder1 = {}; temporder2 = {}; temporder3 = {}; newexpr5 = 1;
  For[i = 1,i <= length1,i++,
     If[Not[FreeQ[expr,Plus]],temporder1 = {}; temporder2 = {};
            temporder3 = {};newexpr5 = 1];
     If[length1 != 1,newexpr1 = expr[[i]],newexpr1 = expr]; 
       If[Not[FreeQ[newexpr1,Derivative]],  
	 length2 = length[newexpr1];
	 For[k = 1,k <= length2,k++,
	    If[length2 != 1,newexpr2 = newexpr1[[k]],newexpr2 = newexpr1];
	    If[Not[FreeQ[newexpr2,Derivative]],
	      If[FreeQ[newexpr2,Power],
		newexpr3 = FullForm[newexpr2];
                newexpr6 = newexpr3[[1]][[0]][[0]];
		temporder1 = Append[temporder1,newexpr6[[1]]];
		temporder2 = Append[temporder2,newexpr6[[2]]];
		temporder3 = Append[temporder3,newexpr6[[1]]+newexpr6[[2]]];
		newexpr4 = newexpr2;
		newexpr5 = newexpr5*newexpr2, 
		newexpr3 = FullForm[newexpr2];
                newexpr6 = newexpr3[[1]][[1]][[0]][[0]];  
		temporder1 = Append[temporder1,newexpr6[[1]]]; 
		temporder2 = Append[temporder2,newexpr6[[2]]];
		temporder3 = Append[temporder3,newexpr6[[1]]+newexpr6[[2]]];
		newexpr4 = newexpr3[[1]];
		newexpr5 = newexpr5*newexpr3[[1]];
		];  
	      ];  
	    ];  
         temporder1 = Max[temporder1];
         temporder2 = Max[temporder2];
         temporder3 = Max[temporder3];
	 orderlist1 = Append[orderlist1,{temporder1,newexpr4}];
	 orderlist2 = Append[orderlist2,{temporder2,newexpr4}];
	 orderlist3 = Append[orderlist3,{temporder3,newexpr5}];
	 ];  
     ];        
  ];                  
Which[
debug11,highestlist1 = highestpair[orderlist1];
  If[debug12,Return[Table[highestlist1[[k]][[2]],{k,1,Length[highestlist1]}]],
     Return[highestlist1[[1]][[1]]]], 
debug21,highestlist2 = highestpair[orderlist2];
  If[debug22,Return[Table[highestlist2[[k]][[2]],{k,1,Length[highestlist2]}]],
      Return[highestlist2[[1]][[1]]]],
debug31,highestlist3 = highestpair[orderlist3];
  If[debug32,Return[Table[highestlist3[[k]][[2]],{k,1,Length[highestlist3]}]],
      Return[highestlist3[[1]][[1]]]] ]; ]; (* end highestorder *)

(*****************************************************************************)
(* setuniformrank[]: a subroutine for the scaling function                   *)
(*****************************************************************************)
setuniformrank[list_] := Module[
{i,lengthlist,syslist={}},
lengthlist = Length[list];
For[i = 1,i < lengthlist,i++,
   syslist = Append[syslist,Part[list,i] == Part[list,lengthlist]]; 
   ];
Return[syslist]; ]; (* end setuniformrank *)

(*****************************************************************************)
(* scaling[]: determines the scaling properties, if the equations are        *)
(*            incompatible it will print the appropriate messages            *)
(*****************************************************************************)
scaling[eqlist_List] := Module[
{pointsym,expr,list,syslist,msyslist = {},i,j,k,scalesol,
lenmsyslist,tempscalesol = {},tempmsyslist,trouble,troubleleft,
troubleright,posleft,posright,troublelist},
If[weightpars =!= {},
  pointsym = Union[Table[weightpars[[i]] -> E^weight[weightpars[[i]]],
                     {i,1,Length[weightpars]}],
       {u[i_Integer][x,t] :> E^weightu[i],
        D[u[i_Integer][x,t],{x,j_Integer}] :> E^(weightu[i]+j),
        D[u[i_Integer][x,t],{t,j_Integer}] :> E^(weightu[i]+j*weight[d/dt]),
        x -> E, t -> E^weight[d/dt]}],
  pointsym = {u[i_Integer][x,t] :> E^weightu[i],
        D[u[i_Integer][x,t],{x,j_Integer}] :> E^(weightu[i]+j),
        D[u[i_Integer][x,t],{t,j_Integer}] :> E^(weightu[i]+j*weight[d/dt]),
        x -> E, t -> E^weight[d/dt]};
  ];
For[i = 1, i <= noeqs, i++,
   expr[i] = Part[eqlist,i];
   list[i] = stripper[expr[i]];
   list[i] = list[i] /. pointsym;
   list[i] = PowerExpand[Log[list[i]]];
   syslist[i] = setuniformrank[list[i]];
   msyslist = Union[msyslist,syslist[i]];
   ];
scalesol = Flatten[Solve[msyslist]];
If[MemberQ[msyslist,False],
  Print[" "];
  Print["Fatal Error! Weights are incorrect."];
  Print["Check your choice(s) for the weights in the data file."];
  Print["Aborting the computations!"];
  CloseLog[]; Abort[],
  If[scalesol === {} && msyslist =!= {True},
    Print[" "];
    Print["In the given system there is at least one equation with terms"];
    Print["of unequal rank. Scaling properties can not be determined for"];
    Print["this system. The program will try to find the conflict, and,"];
    Print["if successful, provide suggestions to help resolve the conflict."];
    lenmsyslist = Length[msyslist];
    i = 1;
    While[tempscalesol === {} && i <= lenmsyslist,
         tempmsyslist = Complement[msyslist,{Part[msyslist,i]}];
         tempscalesol = Flatten[Solve[tempmsyslist]];
         i++;
         ];
    i--;
    If[tempscalesol =!= {},
      trouble = Part[msyslist,i];
      troubleleft = Part[trouble,1];
      troubleright = Part[trouble,2];
      For[j = 1, j <= noeqs, j++,
         posleft = Flatten[Position[list[j],troubleleft,{1}]];
         posright = Flatten[Position[list[j],troubleright,{1}]];
         If[posleft =!= {} && posright =!= {},
           troublelist = Union[
             Table[Part[Expand[eq[j][x,t]],posright[[k]]],
                     {k,1,Length[posright]}],
             Table[Part[Expand[eq[j][x,t]],posleft[[k]]],
                     {k,1,Length[posleft]}]
                ];
           Print["The terms"];
           Print[troublelist];
           Print["in equation ",j," are incompatible. Try to introduce an"];
           Print["auxiliary parameter with weight as coefficient of one of"];
           Print["these terms. Aborting the computations! "];
           ];
         ],
    Print["Try to introduce auxiliary parameters with weight as coefficients"];
    Print["into the system. Aborting the computations!"];
    ];
    CloseLog[];
    Abort[],
    Return[scalesol];
    ];
  ]; ]; (* end scaling *)

(*****************************************************************************)
(* subroutine4[]: a subroutine for the function evaluate                     *)
(*****************************************************************************)
subroutine4[givenexpr_] := Module[
{expr1,nexpr1,freerule = {},part1,lennexpr1,i},
expr1 = Expand[givenexpr];
nexpr1 = Numerator[Factor[expr1]];
lennexpr1 = length[nexpr1];
For[i = 1,i<= lennexpr1,i++,
   part1 = Part[nexpr1,i];
   If[MemberQ[unknownlist,part1],
     freerule = Union[freerule,{part1 -> 1}]];
   ];
If[freerule =!= {},
  Print[" "];
  Print["Caution! There is a common (free) factor in the density."];
  Print[" "];
  Print["Setting ",freerule,"."];
  ]; 
Return[freerule]; ]; (* end subroutine4 *)

(*****************************************************************************)
(* subroutine5[]: a subroutine for the function constructform                *)
(*****************************************************************************)
subroutine5[nodims_,givenlist_] := Module[
{m,boundary,scale,var,len,tempwei,temppar,list,pair,ctm = 0,k,s},
list[0] = {};
For[m = 1,m <= nodims,m++,
   pair = Part[givenlist,m];
   var = Part[pair,1];
   scale = Part[pair,2];
   If[scale =!= 0,
     If[m === 1,
       boundary = Floor[rhorank/scale];
       list[m] = Table[{i*scale,var^i},{i,0,boundary}];
       ctm = m,
       len = Length[list[m-1]];
       list[m] = {};
       For[k = 1,k <= len,k++,
          tempwei = Part[Part[list[m-1],k],1];
          temppar = Part[Part[list[m-1],k],2];
          boundary = Floor[(rhorank-tempwei)/scale];
          For[s = 0,s <= boundary,s++,
             list[m] = Union[list[m],{{tempwei+s*scale,temppar*var^s}}];
             ctm = m;
             ];
          ];
       ];
     ];
   ];
Return[list[ctm]]; ]; (* end subroutine5 *)

(*****************************************************************************)
(* constructform[]: returns the form of rho or J                             *)
(*****************************************************************************)
constructform[nodims_,varscalelist_,rank_,ifrho_] := Module[
{list,len,tempwei,temppar,inttest,formlist = {},triviallist = {},
difflist1,difflist2,lendifflist1,i,j}, 
Print[" "];
Print["Starting the construction of the form of the density."];
list = subroutine5[nodims,varscalelist];
len = Length[list];

For[i = 1,i <= len,i++,
   tempwei = Part[Part[list,i],1];
   temppar = Part[Part[list,i],2];
   If[varsweightzero=={},
      notfreeqtemppar = Not[FreeQ[temppar,u]],
      notfreeqtemppar = True
     ];
   inttest = rank-tempwei;
   If[IntegerQ[inttest] && notfreeqtemppar, 
     If[inttest === 0, 
       formlist = Union[formlist,{temppar}],
       dtemppar = D[temppar,{x,inttest-1}];

       If[Not[FreeQ[dtemppar,Plus]],
          difflist1 = Map[strip, dtemppar /. Plus->List],
          difflist1 = Map[strip, {dtemppar}]];

       lendifflist1 = Length[difflist1];
       For[j = 1, j <= lendifflist1,j++,
            dpartdif = D[Part[difflist1,j],x];
            If[Not[FreeQ[dpartdif,Plus]],
                difflist2 = Map[strip, Union[Flatten[dpartdif /. Plus->List]]],
                difflist2 = Map[strip, Union[Flatten[{dpartdif}]]]];
          triviallist = Union[triviallist,{Last[difflist2]}];

   If[debugLevel>=3,
      Print[" In constructform, formlist (before) = ",formlist];
      Print["     and the difflist2 = ",difflist2]
     ];
   If[ifrho,
      formlist = Union[formlist,Complement[difflist2,triviallist]],
      formlist = Union[formlist,difflist2];
      formlist = Complement[formlist,{0}]
     ];
   If[debugLevel>=3,
      Print[" In constructform, triviallist = ",triviallist];
      Print["      and formlist (after) = ",formlist]
];

          ];
       ];
     ];
   ];

lenform = Length[formlist];
form[x,t] = 0;

(* Test for mixed derivatives, sets flag mixedDeriv accordingly *)
derivList = 
  Select[Flatten[eqlist /. {Plus -> List}], 
      Not[FreeQ[#, Derivative]] &] /. {Derivative[a_, b_][__][__] -> {a, b}};
mixedDeriv = Length[Cases[derivList, {a_ /; a > 0, b_ /; b > 0}]] > 0;

For[i = 1,i <= lenform,i++,
   unknownlist = Table[c[i],{i,1,lenform}];
   form[x,t] = form[x,t]+c[i]*Part[formlist,i];
   ];

construles={};

(* This part replaces the constant coefficients with functional coefficients
   if there are variables with weight of zero *)
If[ifrho,
   If[varsweightzero=!={} && !mixedDeriv, 
       construles={c[n_]->h[n][wtzvar]}
     ], (* begin else, when working on J *)
   If[varsweightzero=!={},   
       construles={c[n_]->h[n][wtzvar]}
     ]
   ];
form[x,t] = form[x,t]/.construles;
Print[" "];
If[ifrho,
    Print["For RANK = ",rank,", this is the form of density rho[x,t]: "],
    Print["Form of corresponding J[x,t]: "]];
Print[" "];
Print[pdeform[Expand[form[x,t]]]];
Print[" "];

If[form[x,t] === 0  &&  ifrho,
  Print["The only density is the trivial density rho[x,t] = 0."];
  Print[" "];
  Print["Aborting the computations! "]; 
  CloseLog[]; Abort[] ];
Return[form[x,t]] 
]; (* end constructform *)               

(*****************************************************************************)
(* evaluate[]: computes the density and flux, with verification              *)
(* * Last modified: July 1, 2003 at 4:13 am by Mike C.                       *)
(*                  Integration of homotopyOperator function call            *)
(*****************************************************************************)
evaluate[expr1_,expr2_,solc_,rules_] := Module[
{test,factortest,newrhot,newformj,freerule,rhojlist}, 
rho[x,t] = Expand[expr1 /. solc];
newrhot[x,t] = Expand[ expr2 /. Flatten[{rules,solc}] ];

(* WH 10/05/2003 *)
debugevaluate = False; (* set to False later *)

(* OLD J DETERMINATION *) 
(*  newformJ[x,t] = (-1)*Integrate[newrhot[x,t],x]; *)
(* OLD J *)

(* NEW J *)
(* Ryan Sayers, May 29, 2003 *)

(* Integration checked by Mike C., July 1, 2003 *)
(* The variable "noeqs" is defined in each data file, and it specifies *)
(* the number of dependent variables of the form u[i][x, t] *)

(* WH 10/04/2003 *)
(* For cases without transcendental functions *)
If[debugLevel >= 1, 
   Print["HEREMAN 3, computing newformJ[x,t] with homotopy operator code!!!"]
   ];
Print[" "];
Print["Starting computation of J[x,t] with the continuous homotopy operator."];

newformJ[x,t] = (-1)*continuousHomotopyOperator[newrhot[x,t], u, noeqs, x]; 

Print["Finished computation of J[x,t] with the continuous homotopy operator."];

If[debugLevel >= 1, 
   Print["HEREMAN 4, newformJ[x,t] from homotopy operator code: "];
   Print[newformJ[x,t]]
  ];

(* End of homotopyOperator integration in this function *)

J[x,t] = Expand[newformJ[x,t]];
Print[" "];
If[FreeQ[J[x,t],Integrate] && rho[x,t] =!= 0,
  If[rho[x,t] =!= 0,freerule = subroutine4[rho[x,t]],freerule = {}];
  rho[x,t] = rho[x,t] /. freerule;
  J[x,t] = J[x,t] /. freerule;
  test = Expand[D[rho[x,t],t]+D[J[x,t],x] /. rules];
  factortest = Expand[test];
  If[factortest =!= 0,factortest = Factor[factortest]];
  If[factortest =!= 0,Print[" "];
    Print["Automatic verification of the conservation law FAILED!"]];
  Print[" "];
  rhojlist = normal[stripper[rho[x,t]]];
  rho[x,t] = Part[rhojlist,1];
  rho[x,t] = Factor[rho[x,t]];
  rho[x,t] = Expand[rho[x,t]]; 
(* HERE HERE HERE *)
(* 10/05/2003 capturing the rho in list *)
  If[rho[x,t] =!= 0, listallrhos = Append[listallrhos,rho[x,t]] ];
  If[debugevaluate,
    Print["At Pt. EVAL1NEW, listallrhos = "];
    Print[listallrhos]
    ];
(* WH 10/05/2003 changed the name *)
(* Tell Paul !!! *)
(* Modified the code here because sortIt does not work properly *)
Print["Sorting rho"];
If[
  rho[x,t]=!=0, sortedrho = sortIt[rho[x,t]];
  If[sortedrho == {}, sortedrho = {rho[x,t]}]
  ];

(* HERE HERE HERE *)
  J[x,t] = Part[rhojlist,2];
  J[x,t] = Factor[J[x,t]];
  J[x,t] = Expand[J[x,t]];
(* HERE HERE HERE *)
(* 10/05/2003 capturing the fluxes in list *)
  If[J[x,t] =!= 0, listallfluxes = Append[listallfluxes,J[x,t]]];
  If[debugevaluate,
    Print["At Pt. EVAL2NEW, listallfluxes = "];
    Print[listallfluxes]
    ];
(* HERE HERE HERE *)
(* WH 10/05/2003 changed the name *)
Print["Sorting J"];
(* WH 10/05/2003 changed the name *)
(* Tell Paul !!! *)
(* Modified the code here because sortIt does not work properly *)
If[
   J[x,t]=!=0, sortedflux = sortIt[J[x,t]];
   If[sortedflux == {}, sortedflux = {J[x,t]}]
  ];
  Print["*******************************************************"];
  Print[" "];
  Print["The normalized density rho[x,t] is:"];
  Print[" "];
  Print[pdeform[rho[x,t]]];
  split[pdeform[rho[x,t]]];
  Print[" "];
(* WH 10/04/2003 *)
  If[Length[J[x,t]] <= 20 || debug,
    Print["*******************************************************"];
    Print[" "];
    Print["The corresponding flux J[x,t] is:"];
    Print[" "];
    Print[pdeform[J[x,t]]],
    Print["*******************************************************"];
    Print[" "];
    Print["Flux J[x,t] has been computed. It has ",Length[J[x,t]]," terms."];
    Print["Since there are more than 20 terms, J[x,t] will not be shown."];
    Print["To see the last computed flux at the end, type: pdeform[J[x,t]]"];
    Print["or J[x,t]"]
    ];
  Print[" "];
  Print["*******************************************************"];
  Print["Result of explicit verification: (rho_t + J_x) = ",factortest];
(* WH 10/04/2003 *)
If[factortest==0,
   Print["Succes!"], (* else *)
   Print["Failure!"]
   ];
  Print["*******************************************************"], 
  Print["*******************************************************"];
  Print["There is NO DENSITY of this form!"]; 
  Print["*******************************************************"];
  ] ]; (* end evaluate *) 

(*****************************************************************************)
(* constructeqlist[]: forms the system of equations for coefficients c[i]    *)
(*****************************************************************************)
constructeqlist[expr_] := Module[
{expr1,rules1,rules2,eqlist = {},tempexpr,expr2,templist,lentemplist,s,
tempcoef,parttemplist,expr3},
rules1 = D[u[i_][x,t],{x,j_}] :> 0;
rules2 = u[i_][x,t] :> 0;
expr1 = Expand[expr];
tempexpr = expr1 //. rules1;
tempexpr = tempexpr //. rules2;
eqlist = Union[eqlist,{tempexpr}]; 
expr2 = expr1-tempexpr;
If[expr2 =!= 0,
  templist = Union[stripper[expr2]];
  lentemplist = Length[templist];
  expr3 = expr2;
  For[s = 1,s <= lentemplist,s++,
     parttemplist = Part[templist,s];
     tempcoef = Coefficient[Expand[expr2],parttemplist];
     tempcoef = tempcoef //. rules1;
     tempcoef = tempcoef //. rules2;
     If[debug,expr3 = expr3 - (tempcoef*parttemplist)];
     eqlist = Union[eqlist,{tempcoef}];
     ];
  If[debug,
    expr3 = Factor[expr3];
    Print["Remainder of the expression during the construction of"];
    Print["the system for the coefficients c[i]: "];
(* WH 10/04/2003 *)
    Print[expr3]];
  If[debug && expr3 =!= 0,
    Print["Error in the construction of the system for the c[i]!"];
    Print["Aborting the computations!"];
    CloseLog[]; Abort[] ];
  ];
Return[eqlist]; ]; (* end constructeqlist *)

(*****************************************************************************)
(* mysimplify[]: simplifies the system of equations for the c[i]             *)
(*****************************************************************************)
mysimplify[list_] := Module[
{newlist,lennewlist,k,fterm,simplelist = {}},
Print["Starting the simplification of the system."];
Print[" "];
newlist = Complement[list,{0}];
lennewlist = Length[newlist];
For[k = 1 ,k <= lennewlist,k++,
   fterm = Factor[newlist[[k]]];
   If[Part[fterm,0] === Times && NumberQ[Part[fterm,1]],  
     fterm = fterm/Part[fterm,1] ];
   If[Intersection[Expand[simplelist],Expand[{fterm*(-1)}]] === {},
     simplelist = Union[simplelist,{fterm}]];
   ];
Return[simplelist]; ]; (* end mysimplify *)

(*****************************************************************************)
(* analyzer[]: determines the coefficients c[i] that must be zero, used in   *)
(*             the search for compatibility conditions                       *)
(*****************************************************************************)
analyzer[list_,len_] := Module[
{i,partlist,analyzelist = {}},
Print[" "];
Print["Starting the analysis of the system."];
Print[" "];
For[i = 1,i <= len,i++,
   partlist = Part[list,i];
   If[Length[partlist] === 1 && MemberQ[unknownlist,partlist], 
     analyzelist = Union[analyzelist,{partlist}],
     If[Length[partlist] === 2 && MemberQ[unknownlist,partlist[[2]]] &&
      (MemberQ[parameters,partlist[[1]]] || MemberQ[weightpars,partlist[[1]]]),
      analyzelist = Union[analyzelist,{partlist[[2]]}] ] ];
   ];
Return[analyzelist]; ]; (* end analyzer *)

(*****************************************************************************)
(* picklhs[]: makes a system, setting LHS == 0                               *)
(*****************************************************************************)
picklhs[list_,k_] := Part[list,k] == 0;

(*****************************************************************************)
(* coefmat[]: creates the coefficient matrix of the system for the c[i]      *)
(*****************************************************************************)
coefmat[system_List,unknowns_List] := Module[
{i,mat = {},lensys}, 
If[system === {} || unknowns === {}, Print["Fatal Error"],
lensys = Length[system];
For[i = 1,i <= lensys,i++, 
   mat = Append[mat,Table[Coefficient[Expand[Part[Part[system,i],1]],
   Part[unknowns,k]],{k,1,Length[unknowns]}]]]];
Return[mat]; ]; (* end coefmat *)

(*****************************************************************************)
(* main[]: solves the system for the c[i], computes the compatibility        *)
(*         conditions (if applicable)                                        *)
(*****************************************************************************)
main[utlist_] := Module[
{rules = {},trules,ressubst,formj,tformj,dtformj,comcond,k,comcondfactab,
myeqlist,maineqlist,lengthpar,syscondlist1={},syscond,inputval,inputrule,sol1,
lengthsol1,nrules1,par,newmaineqlist,solc,nrules2,nsolc,funclist,j,seqlist,
lenseqlist,analyzelist,inputpart,inputlist,i,iii,complexanalysis,comcondfac},
rhseq[i_] := D[u[i][x,t],t] /. Part[utlist,i];
rule[i_,n_] := D[D[u[i][x,t],{x,n}],{t,1}] -> D[rhseq[i],{x,n}];
For[i = 1,i <= noeqs,i++,
   trules[i] = Table[rule[i,n],{n,0,highestorderrho}];
   rules = Union[rules,trules[i]];
   ];
If[debug,
(* WH 10/04/2003 *)
  Print["These are the replacement rules from the given system: "];
  Print[rules]];
(* Computation of j *)
rhot[x,t] = D[formrho[x,t],t];
ressubst = rhot[x,t] /. rules;
ressubst = Expand[-ressubst]; 
(* WH 10/04/2003 *)
If[debug,Print["The Euler operator will be applied to: "];
  Print[pdeform[ressubst]]]; 
funclist = Table[u[i][x,t],{i,1,noeqs}];
Print[" "];
Print["Applying the Euler operator (the variational derivative)."];
formj = EulerD[ressubst,funclist,{x,t}];
Print[" "];
Print["Finished with variational differentiation."];
tformj = formj;
(* begin If point 1 *)
If[Length[Cases[tformj,x_0]] === noeqs,
  Print[" "];
  Print["End of the computations."];
  evaluate[formrho[x,t],rhot[x,t],{},rules], (* else point 1 *)
  dtformj = tformj; 
  dtformj = Expand[dtformj];
  Print[" "];
  Print["Starting the construction of the system for the c[i]."];
  Print[" "];
  myeqlist = {};
  For[j = 1,j <= noeqs,j ++,
     If[Part[dtformj,j] =!= 0,
       myeqlist = Union[myeqlist,constructeqlist[Part[dtformj,j]]]];
     ];
 seqlist = mysimplify[myeqlist];
 lenseqlist = Length[seqlist];
 maineqlist = Flatten[Table[picklhs[seqlist,i],{i,1,lenseqlist}]];
 system = MapAll[Factor,maineqlist];
If[
(Not[debug] && Length[maineqlist]>50) && (parameters==={} && weightpars==={}), 
 Print["Since there are no parameters, and the linear system"];
 Print["for the c[i] has ",Length[maineqlist],
       " equations, which is more than 50,"];
 Print["the system will not be shown."];
 Print["To see the system at the end of the computations, type: system"];
 Print[" "],
 Print["This is the system for the c[i]:"]; 
 Print[" "];
 Print[system];
 Print[" "];
 Print["List of unknown coefficients c[i]:"];
 Print[" "];
 Print[unknownlist];
 Print[" "] 
  ];
 Print["Starting the solution of this system with ",
 Length[maineqlist], " equations."];
 If[parameters =!= {}, (* then start for parameter test *)
   analyzelist = analyzer[seqlist,lenseqlist];
   lengthpar = Length[parameters];
   Print["The system for the coefficients c[i] has ",
          lengthpar," parameter(s)."];
   complexanalysis=lengthpar+Length[maineqlist]+Length[unknownlist];
   If[complexanalysis > 20, 
   coefmatrix = coefmat[system,unknownlist];
   Print["The system of PDEs, rank and form of rho, the system for"];
   Print["c[i], its coefficient matrix, and the lists of unknowns and"]; 
   Print["parameters, will all be saved in the file worklog.m for"];
   Print["further analysis. For analysis use the Mathematica functions"]; 
   Print["Reduce, Solve, etc. Load the file worklog.m with the command"]; 
   Print["<<worklog.m ."];
   Save["worklog.m",myfile,eqlist,rhorank,formrho,system,coefmatrix,
   unknownlist,parameters];
   Clear[eqlist,system,coefmatrix];
   ];
 For[i=1,i <= lengthpar,i++,
    syscondlist1 = Append[syscondlist1,parameters[[i]] != 0];
    ];
   syscond = Apply[And,syscondlist1];
   Print[" "];
   Print["Starting the search for compatibility conditions."];
   Print[" "];
   Print["The program will try all possible choices for non-vanishing"]; 
   Print["c[i]."];
   Print[" "];
   inputlist = Complement[unknownlist,analyzelist];
   If[inputlist === {},
     Print["All of the coefficients c[i] in the density have to vanish."];
     Print["The search for compatibility conditions ends."]; 
     ];
   While[inputlist =!= {},
    inputpart = Part[inputlist,1];
    inputval = inputpart == 1;
    inputlist = Complement[inputlist,{inputpart}];
    inputrule = ToRules[inputval];
    Print[" "];
    Print["-------------------------------------------------------"];
    Print[" "];
    Print["* Setting ",inputrule[[1]],":"];
    Print[" "];
    Print["Computation of the compatibility conditions."];
    comcond = Eliminate[maineqlist /. inputrule,unknownlist];
    Print[" "];
    comcondfac = MapAll[Factor,comcond];
    (* Remove possible duplicates *)
    If[Head[comcondfac] === Or, 
      comcondfactab = Table[Part[comcondfac,k], {k,1,Length[comcondfac]}];
      comcondfac = Union[comcondfactab]
      ];
    Print["This is the compatibility condition:"];
    Print[" "];
    Print[comcondfac];
    sol1 = Reduce[comcond && syscond,parameters];
    If[comcondfac === True,   
      Print[" "];
      Print["The compatibility condition is satisfied without constraints"];
      Print["on the parameters."] 
      ];
    If[sol1 =!= False,
      If[!FreeQ[sol1,Or],
        lengthsol1 = Length[sol1];
        For[iii = 1,iii <= lengthsol1,iii++,
           nrules1 = ToRules[Part[sol1,iii]];
           If[comcondfac=!=True, 
             For[k=1,k <= lengthpar,k++,
                par[k]=parameters[[k]] /. nrules1;   
                Print[" "];
                Print["For ",parameters[[k]]," = ",par[k] ];
                ] 
             ];
           newmaineqlist = maineqlist /.nrules1;
           solc = Flatten[Solve[newmaineqlist /. inputrule,unknownlist]];
           solc = Union[solc,inputrule];
           Print[" "];
           Print["The solution of the system is:"];
           Print[" "];
           Print[solc];
           nrules2 = rules /. nrules1;
           evaluate[formrho[x,t],rhot[x,t],solc,nrules2];
           inputlist = Intersection[inputlist,
             Union[solc[[Flatten[Position[solc,_->0]]]] /. x:(c[a_]->0)->c[a],
                   Complement[inputlist,First[#]& /@ solc]]];
           Clear[nrules1,nrules2,solc,newmaineqlist];
           ] (* for *),
        nrules1 = ToRules[sol1];
        If[comcondfac=!=True, 
           For[k = 1,k <= lengthpar,k++,
              par[k]=parameters[[k]] /. nrules1;
              Print[" "];
              Print["For ",parameters[[k]]," = ", par[k]];
              ]
           ];
        newmaineqlist = maineqlist /. nrules1;
        solc = Flatten[Solve[newmaineqlist /. inputrule,unknownlist]];
        solc = Union[solc,inputrule];
        Print[" "];
        Print["The solution of the system is "];
        Print[" "];
        Print[solc];
        nrules2 = rules /. nrules1;
        evaluate[formrho[x,t],rhot[x,t],solc,nrules2];
        inputlist = Intersection[inputlist,
             Union[solc[[Flatten[Position[solc,_->0]]]] /. x:(c[a_]->0)->c[a],
                   Complement[inputlist,First[#]& /@ solc]]];
        Clear[nrules1,nrules2,solc,newmaineqlist];
        ] (* if *),
      Print[" "];
      Print["The system becomes inconsistent, or the compatibility ",
            "conditions require that one or more of the parameters ",
            "are zero."];
      Print["Not acceptable!"];
      ] ] (* closes While *),   
   nsolc = Flatten[Solve[maineqlist,unknownlist]];
   Print[" "];
   Print["Solution of the system: "];
   Print[" "];
   Print[nsolc];
   evaluate[formrho[x,t],rhot[x,t],nsolc,rules];
   Clear[solc1,solc2,solc3,nsolc];
   ];
  ]; (* end point 1 *) ]; (* end main *)
 
(* ------------------------------------------------------------------------- *)

(* set-up for collecting the data in a log file, transcript of computations *)
logfile="";
OpenLog[filename_String] := (logfile = OpenWrite[filename];
  If[logfile === $Failed, Return[]];
  AppendTo[$Echo,logfile]; AppendTo[$Output,logfile];);
CloseLog[] := ($Echo = Complement[$Echo,{logfile}];
  $Output = Complement[$Output,{logfile}];
  Close[logfile];);

(* ************************************************************************ *)
(* **     These functions (below) created by P.Adams, to allow for       ** *)
(* **     systems with transcendental nonlinearities.   03/07/2003       ** *)
(* ************************************************************************ *)

(*****************************************************************************)
(* alternateFindJ[]: given an un-integrable (using Integrate) Jx[x,t], this  *)
(*          function will attempt to construct a form of J[x,t], then        *)
(*          determine the actual J[x,t]                                      *)
(*      * Input:  expression, Jx[x,t]                                        *)
(*      * Output: final J[x,t] and constants rules                           *)
(*      * Format: finalJ[x,t] = alternateFindJ[myJx[x,t]];                   *)
(*****************************************************************************)
alternateFindJ[calculatedJx_]:=Module[{Jrank,candJ,candJx,setToZero,Jxterms,
             solveUsJx,resultJ,linearListJx,linearRulesJx,allSolveRulesJx,
             calcJx},
    If[debugLevel>=2,Print["Entering the alternateFindJ function."]];
    (* No need to check to see if Jx[x,t] is integrable, done previously *)
    Jrank = rhorank + weight[d/dt] - 1;
    If[debugLevel>=3,
       Print["   nodims: ",nodims,"  Jrank=",Jrank,
                          "  and varscalelist: ",varscalelist]];
    candJ[x,t] = constructform[nodims,varscalelist,Jrank,False];
    If[debugLevel>=3, Print["   candJ[x,t] = "];
(* WH 10/04/2003 *)
       Print[candJ[x,t]]]; 
    JnumFns = Length[candJ[x,t]]; (* used in solver *)
    candJ[x,t] = candJ[x,t] /. {h[n_][u[m_]] -> h[n][u[m][x,t]]};
    If[debugLevel>=3, Print["   candJ[x,t] = "];
(* WH 10/04/2003 *)
       Print[candJ[x,t]]];   
    candJx[x,t] = D[candJ[x,t],x];
    calcJx = calculatedJx;
    If[debugLevel>=1,
       Print["  The candidate for the Jx[x,t] = ",candJx[x,t]];
       Print["  Trying to match with Jx[x,t]= "];
(* WH 10/04/2003 *)
       Print[Expand[TrigExpand[TrigReduce[calcJx]]]]  ];  
    setToZero[x,t] = Expand[TrigExpand[TrigReduce[candJx[x,t] - calcJx]]];
    simpleDerivRules = Table[
        Derivative[p_][h[q_]][u[i][x, t]] -> 
               Derivative[p][h[q]][simpleIndepVars[[i]]], {i,noeqs}];
    If[debugLevel >= 3,Print["   simpleDerivRules = ",simpleDerivRules]];
    If[debugLevel >= 3,Print["   setToZero[x,t] = ",setToZero[x,t]]];
    Jxterms = findTerms[setToZero[x,t]];
    If[debugLevel >= 3,Print["   Jxterms[x,t] = ",Jxterms]];
    solveUsJx = findCoefList[setToZero[x,t],Jxterms,noeqs+1];
    solveUsJx = solveUsJx /. findSolveListRules /. simpleDerivRules;
    If[debugLevel >= 3,
               Print["   (after rules) solveUsJx: ",solveUsJx]];
    If[debugT,mysolveJx=solveUsJx];  (* globalized, for checking *)
    {solveUsJx,linearListJx,allSolveRulesJx}=systemSolver[solveUsJx,JnumFns];
    If[debugLevel>=3,
        Print["   Length of solveUsJx, after systemSolver = ",
        Length[solveUsJx], " and should be equal to zero."];
        Print["   solveUsAll after systemSolver = ",
              TableForm[solveUsAll], " and should be {}."];
        Print["   and linearListJx, going into solveLinear = ",linearListJx]];
    If[debugT,mylinJx=linearListJx];  (* globalized, for checking *)
    linearRulesJx = solveLinear[linearListJx];
    allSolveRulesJx = Union[Flatten[Append[allSolveRulesJx,linearRulesJx]]];
    If[debugLevel>=3,Print["   allSolveRulesJx: ",allSolveRulesJx]];
    If[debugLevel>=3,Print["   setToZero /. rules: ",
        Expand[TrigExpand[TrigReduce[setToZero[x,t] //. allSolveRulesJx]]]]];
    resultJ = Expand[candJ[x,t] //. allSolveRulesJx];
    resultJ = Expand[TrigExpand[TrigReduce[
            resultJ /. Map[Reverse,simpleUsRules]]]];
    If[debugLevel>=3,Print["   After u1 change, resultJ = ",resultJ]];
    Print["   Checking solution..."];
    shouldbezero=TrigExpand[TrigReduce[
           D[resultJ,x]-calculatedJx /. linearRulesJx]];
    If[shouldbezero==0,Print["   Calculated J[x,t] is correct. \n"],
             Print["   Error.  The calculated J[x,t] may be unreliable."]];
    If[debugLevel>=2,Print["Leaving the alternateFindJ function."]];
    Return[{resultJ,linearRulesJx}]];  (* end alternateFindJ *)

(*****************************************************************************)
(* checkJx[]: checks the given expression for integrability using the EulerD *)
(*          function, reports on the results, then returns J[x,t]            *)
(*      * Input:  expression, usually Jx[x,t] (J_x)                          *)
(*      * Output: results of checking (messages), J[x,t] (Null if no sol'n)  *)
(*      * Format: myJ[x,t] = checkJx[myJx[x,t]];                             *)
(*      * Last modified: July 1, 2003 at 4:13 am by Mike C.                  *)
(*                       Integration of homotopyOperator function call       *)
(*****************************************************************************)
checkJx[expr_]:=Module[{fullJ,intlistofeqs,testresults,result},
    intlistofeqs = Table[u[i][x,t],{i,noeqs}];
    testresults = Expand[TrigReduce[EulerD[expr,intlistofeqs,{x,t}]] 
                     //. allSolveRules];
    If[(Union[testresults]=!={0}), (* begin if ### *)
       Print["ERROR!"];
(* WH 10/04/2003 *)
       Print["Variational derivative is not equal to zero.  Aborting."];
       (* solveUsAll=findSolveList[expr]; *)
       result=Null;
       Abort[],
(* WH 10/04/2003 *)
       Print["Testing D[J[x,t],x]. Variational derivative is equal to zero."];
(* Added If statement on 11/04/2002, as a 'safety-valve' to keep the 
   computations from taking too long on expressions that we know the 
   built-in Integrate will have no chance of solving.  PJA *)
(* WH 10/04/2003 Note, no longer integrating with built in functions *)
      (*  If [Length[expr]>20,
          Print["Jx[x,t] is too long to use the 'Integrate' function;"];
          Print["   Must use alternate method."];
          Return[Null]];
       *)

(* BEGIN INTEGRATE HERE!!!!!!!!!!!!!!!!!!!!!!!! *)
(* Ryan Sayers, May 29, 2003 *)

(*        fullJ[x,t]=Integrate[expr,x] //. allSolveRules;    *)

(* Integration checked by Mike C., July 1, 2003 *)
(* The variable "noeqs" is defined in each data file, and it specifies *)
(* the number of dependent variables of the form u[i][x, t] *)
(* WH 10/04/2003 Application of the homotopy operator happens below! *)

(* WH 10/04/2003 *)
(* For cases with transcendental functions *)
If[debugLevel >=1, 
   Print["HEREMAN 1, computing fullJ[x,t] with homotopy operator code!!!"]
   ];
Print[" "];
Print["Starting computation of J[x,t] with the continuous homotopy operator."];

fullJ[x,t] = continuousHomotopyOperator[expr, u, noeqs, x];

Print["Finished computation of J[x,t] with the continuous homotopy operator."];
If[debugLevel >=1, 
   Print["HEREMAN 2, fullJ[x,t] after homotopy operator code:"];
   Print[fullJ[x,t]]
  ];

(* END INTEGRATE HERE!!!!!!!!!!!!!!!!!!!!!!!!!! *)

(* End of homotopyOperator integration in this function *)
(* WH 10/04/2003 *)
(* *) 
       If[Not[FreeQ[fullJ[x,t],Integrate]],
          result=Null;
          Print["Will need to integrate Jx[x,t] in an alternate method."],
          result=fullJ[x,t]
         ];
(* *)
       ]; (* end if ### *)
    Return[result]];  (* end checkJx *)

(*****************************************************************************)
(* checkSolution[]: checks the solution of the "solveUsAll" system of        *)
(*          coefficient h-functions                                          *)
(*      * Input:  solveUsAll, linearRules, allSolveRules                     *)
(*      * Output: finalized allSolveRules (and error messages)               *)
(*      * Format: allSolveRules = checkSolution[solveUsAll, linearRules,     *)
(*                                                     allSolveRules];       *)
(*      * Comment: needs a "savedSolveUsAll", the starting state for         *)
(*                 the list of equations needing solution to get rho         *)
(*****************************************************************************)
checkSolution[system_, linrules_, rules_] := Module[
        {intrules, intsystem, testedsystem, leftoverHfncs,
           leftoverCs, solveleftover},
    (* This simplifies the allSolveRules:  *)
    intrules = rules //. linrules;
    intrules = Map[ReplacePart[#, ReplacePart[#[[2]], 
                       Expand[#[[2]][[2]]], 2], 2] &, intrules];
    (* Now combining linearRules and allSolveRules:  *)
    intrules = Union[Flatten[Append[intrules, linrules]]];
    (* Testing for any remaining "glitches" in the list of solve rules *)
    testedsystem = Union[Expand[TrigExpand[system //. intrules]]];
    leftoverHfncs = Select[Union[Flatten[Map[strip, testedsystem] /. 
                       {Times -> List, Plus -> List}]], Not[FreeQ[#, h]] &];
    leftoverCs = Select[Union[Flatten[Map[strip, testedsystem] /. 
                       {Times -> List, Plus -> List}]], Not[FreeQ[#, C]] &];
    If[Length[leftoverHfncs]>0,
        Print["There is still h-function freedom in the system."];
        Print["Full solution not properly calculated."];
        Print["Aborting."];
        Abort[]
      ];
    If[Length[leftoverCs]>0,
        Print["After solving linear system, too much freedom still exists."];
        Print["Attempting to remove excess freedom in constants."];
        solveleftover = Map[# == 0 &, Complement[testedsystem, {0}]];
        intrules = Union[intrules, Flatten[Solve[solveleftover, leftoverCs]]]
      ];
    Return[intrules]];  (* end checkSolution *)

(*****************************************************************************)
(* countHs[]: counts the number of functional coefficients (for the purposes *)
(*          of these calculations, only functions of the form h[#][_] will   *)
(*          be used as coefficients)                                         *)
(*      * Input:  expression (e.g., h[2][u1]+h[3][u2] )                      *)
(*      * Output: number of h[#][u1] functions and derivatives (e.g., 2)     *)
(*      * Format: numberOfHs = countHs[myexpr];                              *)
(*****************************************************************************)
countHs[term_] := Count[{term}, h[_][_], {1, Infinity}] + 
    Count[{term}, Derivative[_][h[_]][_], {1, Infinity}]; (* end countHs *)

(*****************************************************************************)
(* decomposeForWeights[]: takes in an expression, decomposes each piece into *)
(*          its weight-pieces, to allow for other functions to calculate     *)
(*          the weights for each piece, after assembling the system and      *)
(*          solving for each weight piece                                    *)
(*      * Input:  list of expressions (e.g.,{D[u[1][x,t],t]+Cos[u[2][x,t]]}) *)
(*      * Output: counted weights of each piece (u[1],d/dt,etc.) in each     *)
(*                    term of the expression                                 *)
(*               (e.g., {{weightu[1],weight[d/dt]},{weightu[2]}} )           *)
(*      * Format: mylistofweights = decomposeForWeights[myexpr];             *)
(*****************************************************************************)
decomposeForWeights[expr_] := Module[{result},
    result = nestListOf[expr];
    weightzerorules = findZeroRules[result];
    varsweightzero = weightzerorules /. (weightu[n_] -> 0) -> u[n];
    result = result //. transrules /. weightRules /. weightzerorules /. 
        weightparRules /. noweightparRules;
    result = Map[ReplaceAll[#, List -> Plus] &, result];
    Return[result]];    (* end decomposeForWeights *)

(*****************************************************************************)
(* deFraction[]: (overloaded) this will remove the fractional coefficients   *)
(*          from the given expression(s) selecting the largest multiplier    *)
(*          between the two functions (if two are given) to give the least   *)
(*          possibility of fractions                                         *)
(*      * Input:  one or two expr's from which to remove fractional coefs    *)
(*      * Output: equivalent expression(s), multiplied by a scalar           *)
(*      * Format: myexpr = deFraction[myexpr];                               *)
(*            or: {myexpr1, myexpr2} = deFraction[myexpr1, myexpr2];         *)
(*****************************************************************************)
deFraction[expr_] := Module[{multiplier1, tempexpr},
    If[debugLevel>=2,Print["Entering the deFraction function."]];
    tempexpr = expr;
    multiplier1 = Factor[tempexpr][[1]];
    If[Not[NumberQ[multiplier1]], 
        multiplier1 = 1,
        multiplier1 = Expand[1/multiplier1]];
(* WH 10/04/2003 *)
If[debugLevel >= 1,
    Print["   To remove fractions, multiplying results by ",multiplier1]
  ];
    tempexpr = Expand[multiplier1*tempexpr];
    If[debugLevel >= 3,
        Print["   New expression after deFraction = ", tempexpr]];
    If[debugLevel>=2,Print["Leaving the deFraction function."]];
    Return[tempexpr]];
deFraction[expr1_, expr2_] := Module[
    {multiplier1, multiplier2, tempexpr1, tempexpr2},
    If[debugLevel>=2,Print["Entering the deFraction function."]];
    tempexpr1 = expr1; tempexpr2 = expr2;
    multiplier1 = Factor[tempexpr1][[1]];
    If[Not[NumberQ[multiplier1]], 
        multiplier1 = 1,
        multiplier1 = Expand[1/multiplier1]];
    multiplier2 = Factor[tempexpr2][[1]];
    If[Not[NumberQ[multiplier2]], 
        multiplier2 = 1,
        multiplier2 = Expand[1/multiplier2]];
    multiplier1 = LCM[multiplier1,multiplier2];
    nofrac = (multiplier1 == 1);
    tempexpr1 = Expand[multiplier1*tempexpr1];
    tempexpr2 = Expand[multiplier1*tempexpr2];
    (* If the first term of expr1 is negative, switch sign of both *)
    If[TrueQ[Head[tempexpr1[[1]]]==Times],
        If[TrueQ[Negative[tempexpr1[[1]][[1]]]], 
             tempexpr1=Expand[-1*tempexpr1];
             tempexpr2=Expand[-1*tempexpr2];
             Print["   To remove fractions, multiplying results by ",
                      Expand[-1*multiplier1]],  (* else *)
             If[nofrac,
                 Print["   No fractions to remove."],
                 Print["   To remove fractions, multiplying results by ",
                      multiplier1]] ],
        If[nofrac,
            Print["   No fractions to remove."],
            Print["   To remove fractions, multiplying results by ",
                      multiplier1]]  ]; 
    If[debugLevel >= 3,
      Print["   New expression #1 after deFraction = ", tempexpr1];
      Print["   New expression #2 after deFraction = ", tempexpr2]];
    If[debugLevel>=2,Print["Leaving the deFraction function."]];
    Return[{tempexpr1, tempexpr2}]]; (* end deFraction *)

(*****************************************************************************)
(* findCoefList[]: takes in an expression, a list of "building block" terms, *)
(*          and an indexing number, and produces a list of gathered          *)
(*          coefficients (of each "building block" term), which will be used *)
(*          to determine the system of equations to be solved                *)
(*      * Input:  {expr with terms of the (general) form: h[]u1_x u2_xx,     *)
(*                 list of terms (e.g., {u1_xx, u1_x u2_x, u1 u2_3x},        *)
(*                 counter-number (used by the calling-function) }           *)
(*      * Output: list of (functional) coefficients                          *)
(*      * Format: mytcoeflist = findCoefList[myexpr,myterms,counter];        *)
(*****************************************************************************)
findCoefList[expr_, termList_, solveListNum_] :=
    Module[{s, tempexpr, numEqs, resultList, i},
      If[debugLevel>=2,Print["Entering the findCoefList function."]];
      s = solveListNum;
      tempexpr = expr;
      numEqs = Length[termList];
      resultList = {};
      For[i = 1, i <= numEqs, i++, 
        f[s][i] = Coefficient[tempexpr, termList[[i]]]; 
        tempexpr = Expand[tempexpr - (termList[[i]]*f[s][i])]; 
        If[debugLevel>=4, Print["   term", i, " = ", termList[[i]], 
            "   Coef", " = ", f[s][i], "   tempexpr= ", tempexpr]]];
      If[Not[TrueQ[tempexpr == 0]], ++numEqs; f[s][numEqs] = tempexpr];
      resultList = Table[f[s][j], {j, numEqs}];
      resultList = Map[strip, resultList]; 
      If[debugLevel>=3,Print["   Result of findCoefList = ", resultList]];
      If[debugLevel>=2,Print["Leaving the findCoefList function."]];
      Return[resultList]]; (* end findCoefList *)

(*****************************************************************************)
(* findConstants[]: this finds the list of constant terms in the expression, *)
(*          i.e., those parts that are not any u[#] term (that is, u[#] by   *)
(*          itself or as the argument of a (transcendental) function) or any *)
(*          member of the weightpars list, taken to powers that would be     *)
(*          found for the given rank                                         *)
(*      * Input:  expression with constant and non-constant components       *)
(*      * Output: list of constant terms                                     *)
(*      * Format: myconstants = findConstants[myexpr];                       *)
(*****************************************************************************)
findConstants[expr_] := Module[{intexpr, result1, result2},
    If[debugLevel>=2,Print["Entering the findConstants function."]];
    intexpr = Flatten[expr /. {Plus -> List, Times -> List}]
           /. Derivative[_][ff[n_]][p_] ->ff[n][p] ;
    result1 = Cases[intexpr, C[_][_]];
    result2 = Union[Cases[intexpr, ff[_][_]]];
    result1 = Union[result1, Cases[intexpr, c[_]]];
    If[debugLevel>=3,
        Print["   Result1 of findConstants function = ", result1];
        Print["   Result2 of findConstants function = ", result2]];
    If[debugLevel>=2,Print["Leaving the findConstants function."]];
    Return[{result1,result2}]
    ];  (* end findConstants *)

(*****************************************************************************)
(* findMainsolforlist[]: Simply goes through the equations from the system,  *)
(*          as found in the data input file, and selects the terms that      *)
(*          contain a derivative in t.                                       *)
(*      * Input:  (none)                                                     *)
(*      * Output: list of terms with t-derivatives (e.g.,                    *)
(*           {D[u[1][x,t],t], D[D[u[2][x,t],{x,2}],t]} )                     *)
(*      * Format: mainsolforlist = findMainsolforlist;                       *)
(*****************************************************************************)
findMainsolforlist:=Module[{result,derivList,templist,lowestVars},
    If[debugLevel>=2,Print["Entering the findMainsolforlist function."]];
    derivList = 
        Flatten[Table[listOf[Select[eq[j][x, t], 
                      Not[FreeQ[#, Derivative]] &]], {j, noeqs}]]; 
    templist = derivList /. {Derivative[_, 0][u[_]][x, t] -> 0,
            Derivative[a_, 1][u[b_]][x, t] -> Derivative[a, 0][u[b]][x, t]};
    lowestVars = Complement[Union[templist], {0}];
    result = D[lowestVars, t];
    If[debugLevel>=3,
            Print["   Result of the findMainsolforlist function: ",result]];
    If[debugLevel>=2,Print["Leaving the findMainsolforlist function."]];
    Return[result]];

(*****************************************************************************)
(* finddTrules[]:                                                            *)
(*      * Input:  equations brought in from input file                       *)
(*      * Output: list of (functional) coefficients                          *)
(*      * Format: mytcoeflist = findCoefList[myexpr,myterms,counter];        *)
(*****************************************************************************)
finddTrules[expr_] := Module[{i, maxTder, tempexpr, temprules},
    If[debugLevel>=2, Print["Entering the finddTrules function."]];
    i = -1; maxTder = 1; tempexpr = expr; temprules = utlist;
    While[maxTder > 0,
      i++;
      temprules = Union[temprules, D[temprules, {x, i}]];
      tempexpr = tempexpr /. temprules;
      partslist = 
        Union[Flatten[tempexpr /. {Plus -> List, Times -> List}]];
      maxTder = Last[Union[Select[partslist, Not[FreeQ[#, 
           Derivative[_, _]]] &] /. {Derivative[a_, b_][_][x,t] -> b}]]];
    temprules=MapAll[Expand,temprules];
    If[debugLevel>=3, Print["   Result of finddTrules: ", temprules]];
    If[debugLevel>=2, Print["Leaving the findSolveList function."]];
    Return[temprules]
    ];  (* end of finddTrules function *)

(*****************************************************************************)
(* findSolveList[]: this uses sub-functions to generate the list of eqns     *)
(*          that are needed to be solved to make the given expression have   *)
(*          an Euler derivative equal to zero (and thus be integrable)       *)
(*      * Input:  expression to be made integrable                           *)
(*      * Output: list of equations to solve                                 *)
(*      * Format: myeqns = findSolveList[myexpr];                            *)
(*****************************************************************************)
findSolveList[expr_] := Module[
    {utermList, solveUsList, finalSolveList},
    If[debugLevel>=2,Print["Entering the findSolveList function."]];
    Print["Now applying the Euler operator."];
    For[i = 1, i <= noeqs, i++,
      LuE[i][x, t] = EulerD[expr, u[i][x, t], {x, t}];
      If[debugLevel>=1,Print["   LuE[",i,"]= ",LuE[i][x,t]]];
      utermList[i] = findTerms[LuE[i][x, t]];
      solveUsList[i] = findCoefList[LuE[i][x, t], utermList[i], i]];
    (* findSolveListRules made global for other functions *)
    findSolveListRules = Union[simpleUsRules, # -> 1 & /@ weightpars];
    If[debugLevel >= 3, 
      Print["   findSolveListRules in findSolveList= ",findSolveListRules]];
    finalSolveList = Complement[Union[Flatten[Table[solveUsList[i],
          {i, noeqs}]]] /. findSolveListRules, {True}];
    finalSolveList = Sort[finalSolveList, (Length[#1] < Length[#2]) &];
    If[debugLevel>=3,
            Print["   Result of findSolveList = ", finalSolveList]];
    If[debugLevel>=2,Print["Leaving the findSolveList function."]];
    Return[finalSolveList]];  (* end findSolveList *)

(*****************************************************************************)
(* findTerms[]: looks through an expression and creates a list of all        *)
(*          "building block" combinations of u[#][x,t] (and derivatives) --  *)
(*          this is done to be able to collect all terms with identical      *)
(*          "building blocks" in order to create a list of (coefficient)     *)
(*          functions that would need to be solved to make the Euler         *)
(*          derivative equal to zero after all substitutions were complete   *)
(*      * Input:  expression with terms of the (general) form: h[]u1_x u2_xx *)
(*      * Output: list of terms from expr, made of only u-terms (u1_x u2_xx) *)
(*      * Format: mytermslist = findTerms[myexpr];                           *)
(*****************************************************************************)
findTerms[expr_] := Module[{terms},
    If[debugLevel>=2,Print["Entering the findTerms function."]];
    terms = listOf[expr];
    terms = Complement[Union[Table[Select[
        Table[terms[[j]][[i]], {i,Length[terms[[j]]]}],
          (! FreeQ[#, u[_]] && FreeQ[#, h[_]] && FreeQ[#, ff[_]] &&   
           ! MemberQ[transfns, #]) || (MemberQ[weightpars,#]) &], 
                {j,Length[terms]}]], {{}}];
    If[debugLevel>=4,Print["terms=",terms]];
    terms = Reverse[Apply[Times, terms, {1}]];
    If[debugLevel>=3,Print["   Result of findTerms function,",
        " list of (u[#]) building blocks= ", terms]];
    If[debugLevel>=2,Print["Leaving the findTerms function."]];
    Return[terms]]; (* end findTerms *)

(*****************************************************************************)
(* findTransFnsList[]: takes a list of expressions, determines what          *)
(*          transcendental functions are found in that list (of the form     *)
(*          Sin[u[#][x, t]]) and outputs the list of those transcendental    *)
(*          functions and their "brothers"                                   *)
(*      * Input:  list of expressions (e.g., {u[1][x,t]+Cos[u[2][x,t]]} )    *)
(*      * Output: list of transcendental functions found in the list         *)
(*               (e.g., {Sin[u[2][x,t]], Cos[u[2][x,t]]} )                   *)
(*      * Format: mytransfnslist = findTransFnsList[mylist];                 *)
(*****************************************************************************)
findTransFnsList[eqnlist_] := Module[{tempeqnlist,tempresult, fnlist},
    If[debugLevel>=2, Print["Entering the findTransFnsList function."]];
    If[debugLevel>=4, Print["   eqnlist=", eqnlist]];
    tempeqnlist = MapAll[Expand, Table[eqnlist[[i]][[2]], 
                                               {i, Length[eqnlist]}]];
    If[debugLevel>=4, Print["   tempeqnlist=", tempeqnlist]];
    tempresult = Union[Flatten[Map[nestListOf, tempeqnlist]]];
    If[debugLevel>=4, Print["   tempresult1=", tempresult]];
    tempresult = Union[Flatten[Table[Cases[tempresult, listoftrans[[ii]]], 
          {ii, Length[listoftrans]}]]];
    If[debugLevel>=4, Print["   tempresult2=", tempresult]];
    fnlist = Flatten[tempresult /. pairRules];
    Off[Table::iterb];
    fnlist =Flatten[fnlist /. Power[x_,n_Integer] -> Table[x^i,{i, n}]];
    On[Table::iterb];
    If[debugLevel>=3, 
                 Print["   Result of findTransFnsList, fnlist = ", fnlist]];
    If[debugLevel>=2, Print["Leaving the findTransFnsList function."]];
    Return[fnlist]]; (* end findTransFnsList *)

(*****************************************************************************)
(* findWeightSystem[]: determines the system of weights that needs to be     *)
(*          solved                                                           *)
(*      * Input:  original equation list                                     *)
(*      * Output: system of weights that needs to be solved to properly      *)
(*                    determine scaling parameters                           *)
(*      * Format: myweightsystem = findWeightSystem[myeqlist];               *)
(*****************************************************************************)
findWeightSystem[eqlist_] := Module[
    {tempres, solvelist, pieces, solveForList, result, overdetrule, 
          weightszero},
    If[debugLevel>=2,Print["Entering the findWeightSystem function."]];
    solvelist = {};
    For[ii = 1, ii <= noeqs, ii++,
      pieces = decomposeForWeights[eqlist[[ii]]];
      If[debugLevel >= 2, Print["pieces = ", pieces]];
      tempres = 
        Table[pieces[[1]] == pieces[[jj]], {jj, 2, Length[pieces]}];
      solvelist = Union[solvelist, tempres] /. weightzerorules];
    weightszero = weightzerorules /. (weightu[n_] -> 0) -> weightu[n];
    solveForList = 
      Complement[
        Append[Union[Table[weightu[i], {i, noeqs}], 
            weightpars /. weightparRules], weight[d/dt]], weightszero];
    solvelist=ExpandAll[solvelist];
    If[debugLevel>=4, Print["   solveForList = ", solveForList]];
    If[debugLevel>=4, Print["   solvelist = ", solvelist]];
    If[Length[solveForList]>Length[solvelist],
         overdetrule={Last[solveForList]->1},
         overdetrule={}];
    result = Union[Flatten[Solve[solvelist, solveForList]], 
                  weightzerorules];
    result = Union[result/.overdetrule,overdetrule];
    If[result=={},
       If[debugLevel>=4, 
              Print["   Adjusting size of solvelist, resolving."]];
       solvelist=Take[solvelist,-1*Length[solveForList]];
       result =
         Union[Flatten[Solve[solvelist, solveForList]], weightzerorules]];
    If[debugLevel>=3, 
       Print["   Result of findWeightSystem is = ", result]];
    If[debugLevel>=2,Print["Leaving the findWeightSystem function."]];
    Return[result]];   (* end findWeightSystem *)

(*****************************************************************************)
(* findZeroRules[]: determines the variables that have weight zero, given    *)
(*          the list of equations/expressions involving them                 *)
(*      * Input:  list of expressions                                        *)
(*      * Output: list of rules setting any zero-weight vars = 0             *)
(*      * Format: myzerorules = findZeroRules[myexprlist];                   *)
(*****************************************************************************)
findZeroRules[expr_] := Module[{tempexpr, transparts, transargs, result},
    tempexpr=Map[strip, Flatten[expr /. Plus -> List]];
    transparts = Flatten[Table[Cases[
         Flatten[tempexpr], listoftrans[[i]]], {i, Length[listoftrans]}]];
    transargs = 
      Union[Flatten[Cases[transparts, u[_Integer][x, t], Infinity]]];
    result = transargs /. {u[a_][x, t] -> (weightu[a] -> 0)};
    Return[result]];   (* end findZeroRules *)

(*****************************************************************************)
(* functionizeRules[]: takes a list of rules and makes them functions, so    *)
(*          when they are applied to the list of rules, derivatives of       *)
(*          functions will evaluate correctly (e.g., the replacement         *)
(*          f[x] -> 2x when applied to f'[x] would not produce 2 (the        *)
(*          derivative of 2x wrt x) unless this function is used             *)
(*      * Input:  list of rules (e.g., {h[2][x]-> 2 x}                       *)
(*      * Output: same list, in Function form                                *)
(*               (e.g., {h[2]->Function[{x},2 x]})                           *)
(*      * Format: myrules = functionizeRules[myrules];                       *)
(*****************************************************************************)
functionizeRules[list_] := Module[{intlist, seqi, ulisti, rule1},
    If[debugLevel>=2,Print["Entering the functionizeRules function."]];
    intlist = list;
    rule1={h[i_][xx__]->xx};
    If[debugLevel>=4,Print["   rule1 = ",rule1]];
    For[i = 1, i <= Length[intlist], i++,
       seqi=intlist[[i]][[1]]/.rule1;
       If[debugLevel>=4,Print["   seqi at i = ",i," is ",seqi]];
          ulisti=List[seqi];
       If[debugLevel>=4,Print["   ulisti at i = ",i," is ",ulisti]];
       If[Not[FreeQ[intlist[[i]], h]], 
          intlist[[i]] = Head[intlist[[i]][[1]]] -> 
              myFunction[ulisti, Evaluate[intlist[[i]][[2]]]]]];
    intlist=intlist/.{myFunction->Function};
    If[debugLevel>=3,
          Print["   Result of the functionizeRules function: ",intlist]];
    If[debugLevel>=2,Print["Leaving the functionizeRules function."]];
          Return[intlist]]; (* end functionizeRules *)

(*****************************************************************************)
(* listOf[]: puts an expression into list format                             *)
(*      * Input:  expression (e.g., 4x-3y+2yz)                               *)
(*      * Output: terms of expression in list form (e.g., {x, y, yz}         *)
(*      * Format: mylist=listOf[myexpr];                                     *)
(*****************************************************************************)
listOf[expr_] := If[TrueQ[Head[expr]==Plus]||TrueQ[Head[expr]==Times],
                    If[Length[expr] == 1, {expr}, Apply[List,expr]],
                    {expr}]; (* end listOf *)

(*****************************************************************************)
(* nestListOf[]: puts an expression into a nested list, where the original   *)
(*               expression comes from first multiplying the internal lists  *)
(*               and then adding the now-multiplied sub-lists (see example)  *)
(*      * Input:  expression (e.g., 4x-3y+2yz)                               *)
(*      * Output: terms of expression in nested list form, stripped of       *)
(*                numeric coefficients        (e.g., {{x},{y},{y,z}}         *)
(*      * Format: mynestlist=nestListOf[myexpr];                             *)
(*****************************************************************************)
nestListOf[expr_] := (Map[strip, expr, Infinity] /. Plus -> List) /. 
                       Times -> List; (* end nestListOf *)

(*****************************************************************************)
(* removeConseq[]: searches through a list of expressions (all assumed ==0)  *)
(*          and removes those that are derivative consequences of other      *)
(*          members of the list (e.g., if f'[x] and f''[x] are both in the   *)
(*          list, f''[x] will be removed, since if f'[x] == 0, then          *)
(*          f''[x] == 0 must necessarily be true, and is thus not needed     *)
(*      * Input:  list of expressions                                        *)
(*                (e.g., {f[x], f'[x], f''[x], g[x], g''[x], h'[x]})         *)
(*      * Output: same list, w/ deriv consequences removed                   *)
(*                         (e.g., {f[x], g[x], h'[x]} )                      *)
(*      * Format: mylist = removeConseq[mylist];                             *)
(*****************************************************************************)
removeConseq[eqlist_List, level_Integer] := Module[
    {intlist, removelist},
    If[debugLevel>=2,Print["Entering the removeConseq function."]];
    intlist = eqlist;
    removelist = {};
    For[i = 1, i <= Length[intlist], i++,
        For[j = 1, j <= noeqs, j++, 
            removelist = Union[removelist, Table[D[intlist[[i]], 
                      {simpleIndepVars[[j]], k}], {k, level}]]];
        If[debugLevel>=4, 
           Print["   removelist in loop of removeConseq = ", removelist]]];
    removelist = Complement[removelist, {0}];
    removelist = Map[superStrip, removelist];
    removelist = Intersection[removelist,intlist];
    removelist = Sort[removelist, (countHs[#1] < countHs[#2]) &]; 
    If[Length[removelist]==Length[intlist],
        removelist=Drop[removelist,1]];
    If[debugLevel>=3, Print["   In removeConseq, removelist = ", 
        TableForm[removelist]]];
    intlist = Complement[intlist, removelist];
    intlist = Sort[intlist, (countHs[#1] < countHs[#2]) &];
    If[debugLevel>=3, 
           Print["   Result of removeConseq function (system of eqns): ",
               TableForm[intlist]]];
    If[debugLevel>=2,Print["Leaving the removeConseq function."]];
    Return[intlist]]; (* end removeConseq *)

(*****************************************************************************)
(* removePrevSoln[]: included as a post-calculations processing tool only.   *)
(*          Does not necessarily remove lower form, particularly if terms    *)
(*          old or new have coefficients longer than one term                *)
(*         (e.g., (c[1] + 3*c[2]) )                                          *)
(*      * Input:  previous solution and its rank, new solution and its rank, *)
(*                weighted parameter and its weight                          *)
(*      * Output: new solution with old solution removed, if possible        *)
(*      * Format: newsolution = removePrevSoln[oldrho, ..., alpha, 2];       *)
(*****************************************************************************)
removePrevSoln[prevrho_, prevJ_, prevRank_Integer, newrho_, newJ_, 
      newRank_Integer, parm_, weightOfParm_Integer] := Module[
    {dbg}, dbg = False;
    rules1 = {c[_] -> 1, f[_][_] -> 1, Derivative[_][f[_]][_] -> 1};
    rules2 = {c[m_]->CC[m],f[n_][p_]->FF[n][p], 
        Derivative[q_][f[r_]][s_] -> Derivative[q][FF[r]][s]};
    rules3 = {Exp[_] ->0,Cos[_] ->0,Sin[_] ->0, Sinh[_] ->0,Cosh[_] ->0};
    rules4 = {FF[t_] -> f[t], CC[v_] -> c[v]};
    constMult = parm^((newRank - prevRank)/weightOfParm);
    intprevrho = Flatten[{(prevrho /. Plus -> List)}*constMult];
    intprevJ = Flatten[{(prevJ /. Plus -> List)}*constMult];
    intnewrho = newrho /. Plus -> List;
    intnewJ = newJ /. Plus -> List;
    int1 = Intersection[Map[strip, intprevrho] /. rules1, 
        Map[strip, intnewrho] /. rules1];
    int2 = Intersection[Map[strip, intprevJ] /. rules1, 
        Map[strip, intnewJ] /. rules1];
    If[dbg, Print["int1 = ", int1]; Print["int2 = ", int2]];
    tablePrevrho = Table[Plus @@ Coefficient[intprevrho, int1[[i]]] 
            /. rules3, {i, Length[int1]}] /. rules2;
    tableNewrho = Table[Plus @@ Coefficient[intnewrho, int1[[i]]] 
            /. rules3, {i, Length[int1]}];
    tablePrevJ = Table[Plus @@ Coefficient[intprevJ, int2[[i]]] 
            /. rules3, {i, Length[int2]}] /. rules2;
    tableNewJ = Table[Plus @@ Coefficient[intnewJ, int2[[i]]] 
            /. rules3, {i, Length[int2]}];
    If[dbg, Print["tablePrevrho=", tablePrevrho]; 
      Print["tableNewrho=", tableNewrho]; 
      Print["tablePrevJ=", tablePrevJ]; Print["tableNewJ=", tableNewJ]]; 
    fnlrules = Union[
      Flatten[Solve[Table[tablePrevrho[[i]]-tableNewrho[[i]]==0, 
          {i,Length[tableNewrho]}],Union[Map[strip, tablePrevrho]]]],
      Flatten[Solve[Table[tablePrevJ[[i]] - tableNewJ[[i]] == 0, 
          {i,Length[tableNewJ]}], Union[Map[strip, tablePrevJ]]]]  ];
    result1 = Expand[newrho - 
          ((constMult*prevrho /. rules2) /. fnlrules /. rules4)];
    If[(Length[newrho] - Length[prevrho]) < (Length[result1]),
      Print["New result of wrong length!"]];
    result2 = Expand[newJ - 
          ((constMult*prevJ /. rules2) /. fnlrules /. rules4)];
    If[dbg, Print["   fnlrules=", fnlrules]];
    Return[{result1, result2}] ];  (* end removePrevSoln *)

(*****************************************************************************)
(* replaceConstants[]: (overloaded) this replaces the complex constants      *)
(*          (of the form: C[x][y]) in the given expression(s) with more      *)
(*          simple constants (of the form: a, b, c, etc.)                    *)
(*      * Input:  expressions with complex constants                         *)
(*      * Output: same expression, with simpler constants                    *)
(*      * Format: myexpr = replaceConstants[myexpr];                         *)
(*            or: {myexpr1, myexpr2} = replaceConstants[myexpr1, myexpr2]    *)
(*****************************************************************************)
replaceConstants[expr1_] := Module[
    {complexConstantsList, result1, freeFnsList, simpleConstantsList},
    (* constantsReplaceRules made global *)
    If[debugLevel>=2,Print["Entering the replaceConstants function."]];
    {complexConstantsList, freeFnsList} = findConstants[expr1];
    constantsReplaceRules = Table[complexConstantsList[[i]] -> c[i], 
                                    {i, Length[complexConstantsList]}];
    freeFnsList=Table[Head[freeFnsList[[i]]], {i, Length[freeFnsList]}];
    fnsReplaceRules = Table[freeFnsList[[i]] -> f[i],
                                    {i, Length[freeFnsList]}];
    simpleConstantsList=Table[c[i],{i,Length[complexConstantsList]}];
    If[debugLevel>=4,
        Print["   In replaceConstants, simpleConstantsList = ",
                simpleConstantsList];
        Print["        freeFnsList = ", freeFnsList];
        Print["   constantsReplaceRules = ", constantsReplaceRules];
        Print["   fnsReplaceRules = ",fnsReplaceRules]
      ];
    result1 = expr1 /. constantsReplaceRules /. fnsReplaceRules;
    If[debugLevel>=3,
(* WH 10/04/2003 *)
         Print["   Result of replaceConstants, result1 = "];
         Print[result1]];
    If[debugLevel>=2,Print["Leaving the replaceConstants function."] ];
    Return[result1]];
(* Function is overloaded, to work with either one or two input expressions *)
replaceConstants[expr1_, expr2_] := Module[
    {complexConstantsList, result1, result2, freeFnsList, simpleConstantsList},
    If[debugLevel>=2,Print["Entering the replaceConstants function."]];
    {complexConstantsList, freeFnsList} = Table[Union[
        findConstants[expr1][[i]], findConstants[expr2][[i]]],{i,2}];
    If[debugLevel>=4,
      Print["   In replaceConstants, complexConstantsList=",
                                            complexConstantsList];
      Print["        and freeFnsList=",freeFnsList]];
    constantsReplaceRules = Table[complexConstantsList[[i]] -> c[i], 
                                       {i, Length[complexConstantsList]}];
    freeFnsList=Table[Head[freeFnsList[[i]]], {i, Length[freeFnsList]}];
    fnsReplaceRules = Table[freeFnsList[[i]] -> f[i],
                                       {i, Length[freeFnsList]}];
    simpleConstantsList=Table[c[i],{i,Length[complexConstantsList]}];
    If[debugLevel >= 4,
        Print["   In replaceConstants, simpleConstantsList = ",
                simpleConstantsList];
        Print["        freeFnsList = ", freeFnsList];
        Print["   constantsReplaceRules = ", constantsReplaceRules];
        Print["   fnsReplaceRules = ",fnsReplaceRules]
      ];
    result1 = expr1 /. constantsReplaceRules /. fnsReplaceRules;
    result2 = expr2 /. constantsReplaceRules /. fnsReplaceRules;
    If[debugLevel>=3,
(* WH 10/04/2003 *)
         Print["   Result of replaceConstants, result1 = "];
         Print[result1]];
    If[debugLevel>=3,Print["  and result2 = ", result2]];
    If[debugLevel>=2,Print["Leaving the replaceConstants function."]];
    Return[{result1, result2}]]; (* end replaceConstants *)

(*****************************************************************************)
(* setup[]: setup creates some generic lists and elements used later         *)
(*      * Input:  none                                                       *)
(*      * Output: none                                                       *)
(*      * Format: setup;                                                     *)
(*****************************************************************************)
setup:=( 
   time1start=SessionTime[];
   simpleIndepVars = {u1, u2, u3, u4, u5};
   complexIndepVars = Table[u[i][x, t], {i, noeqs}];
   simpleUsRules = 
     Table[complexIndepVars[[i]] -> simpleIndepVars[[i]], 
                                   {i, Length[complexIndepVars]}];
   weightRules = { 
      Power[E, u[h_][x, t]] -> weightu[h], 
      Derivative[a_, b_][u[c_]][x, t] -> weightu[c]+a+b*weight[d/dt],
      u[d_][x, t] -> weightu[d], 
      Power[u[e_][x, t], f_] -> f*weightu[e]};
(*   listoftrans = {Sin[__], Cos[__], Sinh[__], Cosh[__], Exp[__]};  *)
   listoftrans = {Sin[__], Sin[__]^__, Cos[__], Cos[__]^__, Sinh[__], 
        Sinh[__]^__, Cosh[__], Cosh[__]^__, Exp[__], Power[__,_?Negative]};
   pairRules = {Sin[a__] -> {Sin[a], Cos[a]}, Cos[b__] -> {Sin[b], Cos[b]}, 
      Sinh[c__] -> {Sinh[c], Cosh[c]}, Cosh[d__] -> {Sinh[d], Cosh[d]}};
   weightparRules = Map[# -> weight[#] &, weightpars];
   transrules = {Sin[a__] -> a, Sin[b__]^c__ -> c*b, Cos[d__] -> d, 
       Cos[e__]^f__ -> f*e, Sinh[g__] -> g, Sinh[h__]^j__ -> j*h, 
       Cosh[k__] -> k, Cosh[m__]^n__ -> n*m, Exp[p__] -> p};
   noweightparRules = Map[# -> 0 &, parameters];
   derivConstCtr = 1  (* used in systemSolver *)
   );   (* end setup *)

(*****************************************************************************)
(* solveLinear[]: this will solve a linear system of equations, after first  *)
(*          grouping like terms (some have transcendental functions          *)
(*          multiplied by them) and generating a new system                  *)
(*      * Input:  list (system) of equations to solve                        *)
(*      * Output: list of replacement rules                                  *)
(*      * Format: myrules = solveLinear[mylinearsystem];                     *)
(*****************************************************************************)
solveLinear[linlist_] := Module[
    {linrules, linearSolveUs, linearSolveUsFns, zerorules1, 
        zerorules2, linterms, onerules1, freefncs},
    zerorules1=Map[# -> 0 &, transfns /. simpleUsRules] ;
    onerules1 =Map[# -> 1 &, transfns /. simpleUsRules] ;
    zerorules2=Map[# -> 0 &, simpleIndepVars];
    If[debugLevel>=2,Print["Entering the solveLinear function."]];
    linearSolveUs = {};
    If[debugLevel>=4,Print["   In solveLinear, linlist=",linlist]];
    linterms = Complement[Union[Map[strip, Union[Flatten[
        linlist /. Plus -> List]]] /. {C[_][_] -> 1, c[_] -> 1}], {1}];
    freefncs = Union[Map[ReplaceAll[#, List -> Times] &, Select[Table[
        Flatten[{linterms[[i]] /. Times -> List}], {i, Length[linterms]}], 
          MemberQ[#, ff[_][_]] &]] /. onerules1];
    linterms = Complement[Union[linterms /. ff[_][_] -> 0], {0}];
    linearSolveUs=Union[Flatten[Table[Union[Map[superStrip, 
            Coefficient[linlist, linterms[[i]]] /. zerorules1 /. zerorules2
            ]], {i, Length[linterms]}]]];
(* Need to add a line here to solve the freefncs, TODO PJA 08/10/2003 *)
    linearSolveUs = Complement[Union[linearSolveUs, 
         Map[superStrip, linlist/.zerorules1 /. zerorules2 ]],{0}];
    linearSolveUs = Map[# == 0 &, linearSolveUs];
    If[debugLevel>=2,
          Print["   In solveLinear, linearSolveUs= ", linearSolveUs]];
    linearSolveUsFns = Union[Flatten[Table[If[
              Length[linearSolveUs[[i]][[1]]] == 
                1, {strip[linearSolveUs[[i]][[1]]]}, 
              Table[strip[linearSolveUs[[i]][[1]][[j]]], {j, 
                  Length[linearSolveUs[[i]][[1]]]}]], {i, 
              Length[linearSolveUs]}]]];
    If[debugLevel>=2, 
        Print["   linearSolveUsFns = ", linearSolveUsFns]];
    linrules = Flatten[Solve[linearSolveUs, linearSolveUsFns]];
    If[debugLevel>=2,
        Print["   Result of solveLinear function, linrules = ", linrules]];
    If[debugLevel>=2,Print["Leaving the solveLinear function."]];
    Return[linrules]]; (* end solveLinear *)

(* Tell Paul about this *)
(* WH 10/05/2003 *)
(* Change sortIt function so it returns the original expr is there are *)
(* no free constants in expr. Does not seem to work properly *)
(*****************************************************************************)
(* sortIt[]: sorts the given expression according to the                     *)
(*          given sort-list, and puts it into list-form                      *)
(*      * Input:  expression and list of pieces to sort by                   *)
(*      * Output: expression, sorted, and put into a list format             *)
(*      * Format: myexprSorted = sortIt[myexpr, mysortlist];                 *)
(*****************************************************************************)
sortIt[expr_] := Module[{result, intsort}, 
    If[TrueQ[Head[expr]=!=Times],
       intsort = Select[Union[Flatten[expr /. {Plus -> List, Times -> List}]], 
           Not[FreeQ[#, c[_]]] &];
       intsort = Union[Select[Union[Flatten[expr /. 
           {Plus -> List, Times -> List}]], Not[FreeQ[#, f[_]]] &],intsort];
       If[debugLevel>=3,Print["Sorting by: ",intsort]];
       result = Table[Select[expr/.Plus->List, 
            Not[FreeQ[#, intsort[[i]]]] &], {i, Length[intsort]}];
       result = Map[Apply[Plus, #] &, result],  (* else *)  
       result = {expr}];
   If[debugLevel>=3, Print["   Result of sortIt: ",TableForm[result]]];
   (* WH 10/05/2003 Quick fix on next line ! *)
   If[result == {}, result = {expr}];
   Return[result] ]; (* end sortIt *)

(*****************************************************************************)
(* sortStrip[]: takes in a list, superStrips it, then sorts it by length     *)
(*      * Input:  list of expressions (e.g., {3x, 2x-4y, 2yz})               *)
(*      * Output: same list, stripped and sorted (e.g., {x, yz, x-2y})       *)
(*      * Format: mylist = sortStrip[mylist];                                *)
(*****************************************************************************)
sortStrip[expr_] := Module[{intexpr},
      intexpr = expr;
      intexpr = Sort[Complement[Union[Expand[superStrip /@ intexpr]], {0}], 
             (Length[#1] < Length[#2]) &];
      If[debugLevel >= 3,Print["   After sortStrip, length of expression = ",
                               Length[intexpr]];
          Print["   After sortStrip, expression = ", TableForm[intexpr]]];
      Return[intexpr]]; (* end sortStrip *)

(*****************************************************************************)
(* strip[]: removes the numeric coefficient from a single term               *)
(*      * Input:  single term (e.g., 4x, -3y, 2yz)                           *)
(*      * Output: term with numeric coef removed (e.g., x, y, yz )           *)
(*      * Format: myterm=strip[myterm];                                      *)
(*****************************************************************************)
strip[term_] :=
    If[TrueQ[Head[term] == Times] && NumberQ[term[[1]]], Drop[term, 1], 
      term]; (* end strip *)

(*****************************************************************************)
(* superStrip[]: removes the largest numeric coefficient from all terms of   *)
(*               an expression; will also handle a leading negative sign     *)
(*      * Input:  expression with multiple terms (e.g., 4x+2y-8z)            *)
(*      * Output: expr with largest numeric coef removed (e.g., 2x+y-4z)     *)
(*      * Format: myexpr=superStrip[myexpr];                                 *)
(*****************************************************************************)
superStrip[term_] := Module[{stTerm},
    stTerm = strip[Factor[term]];
    If[(Length[stTerm] > 1) && (Length[stTerm[[1]]] > 1),
        If[TrueQ[Negative[stTerm[[1]][[1]]]], 
              Return[-1*stTerm], 
              Return[stTerm]],
        Return[stTerm]]]; (* end superStrip *)

(*****************************************************************************)
(* systemSolver[]: this will solve a system of equations with functions,     *)
(*          derivatives of functions, and constants, taking place of old     *)
(*          "solveIt" function and related subfunctions, outputting the      *)
(*          resulting list (which should be empty at the end, signalling     *)
(*          that all equations were solved), a linear system to be solved,   *)
(*          and the replacement rules derived from the solving of the        *)
(*          given system                                                     *)
(*      * Input:  list of equations to solve                                 *)
(*      * Output: {list of equations to solve, list of linear function to    *)
(*                solve, list of existing replacement rules}                 *)
(*      * Format: {myeqs, mylinear, myrules} = solveIt[myeqs];               *)
(*****************************************************************************)
systemSolver[eqlist_List, numFns_Integer] := Module[
    {inteqs, intlinear, intrules, lastLengthOfSolveList, solveList, 
        solveFns, fewHlist, linearTemp, singleResult},
    If[debugLevel>=2, Print["Entering systemSolver function"]];
    inteqs = eqlist; (* internal copy of the list of equations to solve *)
    intlinear = {};
    intrules = {};
    lastLengthOfSolveList = Length[inteqs] + 1;
    inteqs = removeConseq[inteqs, 1];
    inteqs = ExpandAll[Union[Map[superStrip, inteqs]]];
    While[Length[inteqs] > 0,
       linearTemp = Select[inteqs, FreeQ[#, h] &];
          linearTemp=ExpandAll[linearTemp]; 
       If[debugLevel>=4, Print["   linearTemp = ", linearTemp]];
       inteqs = Complement[inteqs, linearTemp];
       intlinear = Union[Flatten[Append[intlinear, linearTemp]]];
       If[debugLevel>=4, Print["   intlinear = ", intlinear]];
       If[Length[inteqs]>1,inteqs = removeConseq[inteqs, 2]];  
       inteqs = ExpandAll[inteqs];  
       (* Next line checks to see if inteqs (list of eqns to solve) has been
          reduced to nothing after last two lines, Breaks out of While loop *)
       If[Length[inteqs]==0, 
           Print[" Length of eqn list = 0, exiting solver."];
           Break[]];   
       inteqs = Sort[inteqs, (countHs[#1] < countHs[#2]) &];
       fewHlist = Select[inteqs,countHs[#]==countHs[inteqs[[1]]]&];
       fewHlist = Sort[fewHlist, (Length[#1] < Length[#2]) &];
       If[debugLevel>=4, Print["   fewHlist = ",fewHlist]];
       If[lastLengthOfSolveList <= Length[inteqs], 
          Print["ERROR in system solver!!! Aborting!"]; Abort[]];
       lastLengthOfSolveList = Length[inteqs];
       If[debugLevel>=3, 
           Print["  List of eqs to solve: "];
(* WH 10/04/2003 *)
           Print[TableForm[inteqs]]];
       Print["    Number of eqs left to solve: ", Length[inteqs]];
       solveList = {fewHlist[[1]] == 0};  
       If[debugLevel>=2, Print["  Now solving: ", solveList]];
       solveFns = {};
       For[i = 1, i <= numFns, i++, 
           If[Not[FreeQ[solveList, h[i]]], 
             AppendTo[solveFns, h[i][ altwtzvar ]]]]; 
       solveFns = Union[Flatten[solveFns]];
       If[Length[solveFns]>1,solveFns={solveFns[[1]]}]; 
          If[debugLevel>=2, Print["  For function(s): ", solveFns]];
(* For Mathematica 5.0 *)
(* WH 10/04/2003 *)
(* Adjustment for Mathematica 5.0 *)
(* Mike C. and W.H. October 1, 2003 *)
If[$VersionNumber >=5., 
          If[Not[FreeQ[solveList, Derivative]],
             singleResult = MapAll[Simplify, 
                Flatten[DSolve[solveList, solveFns, altwtzvar, 
                    GeneratedParameters -> C[derivConstCtr]]]];
           derivConstCtr++,
           singleResult = Union[Flatten[Solve[solveList, solveFns, 
                      altwtzvar ]]]
           ], (* else for Mathematica version 4 or lower *)
          If[Not[FreeQ[solveList, Derivative]],
             singleResult = MapAll[Simplify, 
                Flatten[DSolve[solveList, solveFns, altwtzvar, 
                    DSolveConstants -> C[derivConstCtr]]]];
           derivConstCtr++,
           singleResult = Union[Flatten[Solve[solveList, solveFns, 
                      altwtzvar ]]]
            ]
  ];
       singleResult = Expand //@ (TrigExpand //@ singleResult);
       If[debugLevel>=2, 
          Print["  Solution for this iteration: ", singleResult]];
       singleResult = functionizeRules[singleResult];
       intrules = Union[singleResult, intrules];
       If[debugLevel>=4, Print["  intrules so far: ", TableForm[intrules]]];
       inteqs = Union[Map[superStrip, inteqs /. intrules]];
       inteqs = Sort[Complement[Union[Expand[TrigExpand[
           TrigReduce[inteqs]]]], {0}], (countHs[#1] < countHs[#2]) &];
       ];(* end While loop *)
    If[debugLevel>=3, 
         Print["   At end of systemSolver, after expansion, intrules= ",
                 intrules]];
    If[debugLevel>=2, Print["Leaving the systemSolver function"]];
    Print["System solved."];
    Return[{inteqs, intlinear, intrules}] ];  (* end systemSolver *)

(*****************************************************************************)
(* maintrans[]: this is the calling function, executing the given routines   *)
(*          appropriately.  This function assumes that the main program has  *)
(*          already done some of the work finding certain items (or items    *)
(*          have been determined by the data file, namely:                   *)
(*              utlist, formrho[x,t], debugLevel, noeqs, weights, rhorank.   *)
(*          This function will be called from the condens code as an         *)
(*          alternative to the existing "main" function                      *)
(*      * Input:  no inputs (does use global information)                    *)
(*      * Output: no output (does store in global variables)                 *)
(*      * Format: called instead of the "standard" main function             *)
(*      * Last modified: July 1, 2003 at 4:57 am by Mike C.                  *)
(*                  Integration of homotopyOperator function call            *)
(*****************************************************************************)

maintrans:=(
rhonumFns=Length[formrho[x,t]];
If[debugLevel>=3,Print["   At start of maintrans, rhonumFns=",rhonumFns]]; 
altwtzvar = Part[simpleIndepVars,wtzvar/.u[n_]->n]; 
transfns = findTransFnsList[utlist];
formrho[x,t]=formrho[x,t]/.{h[a_][u[b_]] -> h[a][u[b][x, t]]};
(* Print["The form of rho, before substitution from system, is: "];
Print[formrho[x,t]]; *)
(* Can use these next 3 lines anywhere we want a break point -- only active
   if debugLevel is set with a non-Integer (i.e., 2.1, 1.5, etc.)  *)
If[Mod[debugLevel,1]>0,
(* WH 10/04/2003 *)
   abortnow = Input["Abort now? (0 = no, 1 = yes): "];
   If[abortnow==1,Abort[]]];
dTrules = finddTrules[ D[formrho[x,t],t] ];
formrhot[x,t]=(D[formrho[x,t],t])/.dTrules;
formrhot[x,t]=Expand[formrhot[x,t]];
(* WH 10/04/2003 *)
If[debugLevel>=2, 
   Print["After taking the t-derivative and substitution from the system,",
        " rho_t is: "];
   Print[formrhot[x,t]]
  ];
(* Print[" "]; *)
(* WH 10/04/2003 *)
(* Print["This must be completely integrable w.r.t. x"]; *)
(* Print[" "]; *)
(* WH 10/04/2003 *)
Print["Now deriving the list of equations to solve (from coefficients)."];
solveUsAll = findSolveList[formrhot[x, t]];
Print["Length of system to solve: ",Length[solveUsAll]];
(* WH 10/04/2003 *)
If[debugLevel>=1,
   Print["System to solve:"];
   Print[TableForm[solveUsAll]] 
   ];
derivConstCtr = 0;
(* WH 10/04/2003 *)
(* Print[" "]; *)
(* WH 10/04/2003 *)
Print["Now solving the coefficient functions for formrho."];
If[Mod[debugLevel,1]>0,
(* WH 10/04/2003 *)
   abortnow = Input["Abort now? (0 = no, 1 = yes): "];
   If[abortnow==1,Abort[]]];
savedsolveUsAll = solveUsAll;  (* globalized *)
Print["There are ",Length[savedsolveUsAll],
                            " equations in the ODE system to solve."];

If[Length[solveUsAll] > 0,  (* start Then (++++) *)
   {solveUsAll,linearList,allSolveRules}=systemSolver[solveUsAll,rhonumFns];
   Print["Finished with solving for coefficient functions for formrho."];
   If[debugLevel>=2,Print["Length of solveUsAll, after solveIt = ",
                       Length[solveUsAll], " and should be equal to zero."];
                    Print["solveUsAll after solveIt= ",
                       TableForm[solveUsAll], " and should be {}."]];
   If[Mod[debugLevel,1]>0,
(* WH 10/04/2003 *)
      abortnow = Input["Abort now? (0 = no, 1 = yes): "];
      If[abortnow==1,Abort[]]];
(* WH 10/04/2003 *)
   Print["Now solving the remaining linear system for constant coefficients."];
   linearList = ExpandAll[Complement[linearList,{0}]];
   If[debugLevel>=3,Print["   Before solving, linearList = ",linearList]];
   linearRules = solveLinear[linearList];
   (* This simplifies the allSolveRules:  *)
   allSolveRules = allSolveRules //. linearRules;
   allSolveRules = 
       Map[ReplacePart[#, ReplacePart[#[[2]], 
                            Expand[#[[2]][[2]]], 2], 2] &, allSolveRules];
   (* Now combining linearRules and allSolveRules:  *)
   allSolveRules = Union[Flatten[Append[allSolveRules, linearRules]]];
(* WH 10/04/2003 *)
   Print["Now checking for completeness of the solution."];
   unsolvedEqs = Complement[Union[Expand[TrigExpand[Union[
                   savedsolveUsAll /. allSolveRules ]]]],{0}];
   While[Length[unsolvedEqs]>0,
       Print["There are still unsolved equations.  Completing solution."];
       {unsolvedEqs,tempLinList,tempRules} = 
                            systemSolver[unsolvedEqs,rhonumFns];
       templinRules = solveLinear[tempLinList];
       allSolveRules = Union[allSolveRules,tempRules,templinRules];
       unsolvedEqs = Complement[Union[Expand[TrigExpand[Union[
                          savedsolveUsAll //. allSolveRules ]]]],{0}];
       Print["After another round of solving, the unsolved equations are: ",
                          unsolvedEqs];
        ], (* start Else (++++) *)
   allSolveRules={}
  ];  (* end If (++++)  *)

If[debugLevel>=3,
    Print["After solving linear system, and removing freedoms,",
              " allSolveRules = ", allSolveRules];
    Print["   and rho is: ",formrho[x, t] //. allSolveRules]];
Print["Now creating the final form of the rho."];
finalrho[x, t] = TrigExpand[Expand[(formrho[x, t] //. allSolveRules)]];

(* In case of acceptable freedom in the system, this ensures the 
      alternate J-finder will not be confused, if it is needed at all  *)
If[Not[FreeQ[finalrho[x,t],h[_]]],
   Print[" There will be free functions in the solution, of the form: ff[#]"];
   finalrho[x, t] = finalrho[x, t] /. {h[a_][b_] -> ff[a][b], 
      Derivative[c_][h[d_]][e_] -> Derivative[c][ff[d]][e]}];

(* WH 10/05/2003 *)
Print["After substitution, the solution for rho is: "];
Print[finalrho[x,t]];
Print[" "];
(* Form of conservation law is: D_t(rho) + D_x(J) = 0 *)
Jx[x, t] = Expand[-1*D[finalrho[x, t], t] /. dTrules]; 
(* WH 10/04/2003 *)
If[debugLevel >= 1,
   Print["From finalrho, Jx[x,t] = "];
   Print[Jx[x,t]]
  ];
(* WH 10/04/2003 *) 
(* Print["Now integrating for J using built-in functions."]; *)
(* Print["Computing J with the HOMOTOPY OPERATOR!"]; *)
finalJ[x,t]=checkJx[Jx[x,t]];
If[debugLevel >= 1,
   Print["HEREMAN 5! Coming out of checkJx routine, finalJ[x,t] ="];
   Print[finalJ[x,t]]
  ];
If[debugLevel>=1,
(* WH 10/03/2003 *)
(*   Print["   After attempt to use Integrate, finalJ[x,t] = "]; *)
(* WH 10/04/2003 *)
  Print["After integration with homotopy operator, finalJ[x,t] = "]; 
  Print[finalJ[x,t]]
  ];
If[Mod[debugLevel,1]>0,
(* WH 10/04/2003 *)
   abortnow = Input["Abort now? (0 = no, 1 = yes): "];
   If[abortnow==1,Abort[]]
  ];
finalJ[x,t]=Expand[Simplify[finalJ[x,t]]];
(* Finalizing the form, making constants easier to read and removing
   fractions by multiplying flux and density by same factor  *)
If[TrueQ[finalJ[x,t]==Null],
     Print[" "];
     Print["Built-in functions not able to integrate; ",
           "solving for J by another means."];
     {finalJ[x,t],constJxrules} = alternateFindJ[Jx[x,t]];
     finalJ[x,t]   = finalJ[x,t] /. constJxrules;
     finalrho[x,t] = finalrho[x,t] /. constJxrules;
     finalJ[x,t] = Expand[TrigExpand[finalJ[x,t]]];
     Print["The solution for J is: "];
(* WH 10/04/2003 *)
     Print[finalJ[x,t]]
   ];
Print[" "];
Print["Now finalizing the form of the solution."];
If[(finalrho[x,t]=!=0)&&(finalJ[x,t]=!=0),
   {finalrho[x,t],finalJ[x,t]} = replaceConstants[finalrho[x,t],finalJ[x,t]];
   {finalrho[x,t],finalJ[x,t]} = deFraction[finalrho[x,t],finalJ[x,t]]
  ];

rho[x,t]=TrigExpand[finalrho[x,t]];
J[x,t]=TrigExpand[finalJ[x,t]];
(* HERE HERE HERE *)
Print[" "];
Print["Final rho[x,t] = "];
(* WH 10/04/2003 *)
Print[rho[x,t]];
(* HERE HERE HERE *)
(* 10/05/2003 capturing the rho in list *)
  If[rho[x,t] =!= 0, listallrhos = Append[listallrhos,rho[x,t]] ];
  If[debugevaluate,
    Print["At Pt. EVAL1, listallrhos = "];
    Print[listallrhos]
    ];
Print["Final J[x,t] = "];
(* WH 10/04/2003 *)
Print[J[x,t]];
(* 10/05/2003 capturing the fluxes in list *)
  If[J[x,t] =!= 0, listallfluxes = Append[listallfluxes,J[x,t]]];
  If[debugevaluate,
    Print["At Pt. EVAL2, listallfluxes = "];
    Print[listallfluxes]
    ];
(* HERE HERE HERE *)
Print[" "];
Print["These can be accessed by typing: rho[x,t] and J[x,t]"];
(* WH 10/04/2003 *)
(* Print[" "]; *)
Print["Sorting rho"];
If[rho[x,t]=!=0, sortedrho = sortIt[rho[x,t]]];
(* WH 10/04/2003 *)
(* Print[" "]; *)
(* WH 10/05/2003 changed the name *)
Print["Sorting J"];
If[J[x,t]=!=0, sortedflux = sortIt[J[x,t]]];

Print[" "];
Print["Testing final solution."];
(* WH 10/05/2003 *)
(* done in one line now *)
testsuccess=TrigExpand[(Expand[D[rho[x,t],t]/.dTrules]+D[J[x,t],x])];
    (* Mike C.: Run TrigExpand on the expression, too. *)
    (* The Sine-Gordon System (33) wrongly fails without TrigExpand *)
    (* testsuccess=TrigExpand[testsuccess]; *)
Print["D_t(rho) + D_x(J): ", testsuccess];
If[TrueQ[testsuccess==0],Print["   D_t(rho) + D_x(J) = 0, Success!"],
                         Print["   D_t(rho) + D_x(J) =!= 0, ERROR!!!"]];

time1stop=SessionTime[];
transtime = time1stop-time1start;

(* Print[" "]; *)
Print[" "];
Print["*********************** SUMMARY ***********************"];
Print[" "];
Print["Total session time used in the current session is ",transtime,
      " seconds."];
Print[" "];
Print["To see all the densities type: listallrhos," ];
Print["or pdeform[listallrhos]."];
Print[" "];
Print["To see the last density type: rho[x,t], or use pdeform[rho[x,t]]."];
Print[" "];
Print["To see the last density sorted by coefficients"];
Print["type 'sortedrho' or `sortIt[rho[x,t]]'"];
Print[" "];
Print["To see all the fluxes type: listallfluxes,"];
Print["or pdeform[listallfluxes]."];
Print[" "];
Print["To see the last flux type: J[x,t], or use pdeform[J[x,t]]."];
Print[" "];
Print["To see the last flux sorted by coefficients"];
Print["type 'sortedflux' or `sortIt[J[x,t]]'"];
Print[" "];
Print["*************************** SUMMARY ***************************"];
Print[" "];

If[storeLog,CloseLog[]]
(* Because this function will be called from the condens main, and it 
   completes all necessary operations without further need to return 
   to the original condens (main) code, the operations are aborted here. *)
); (* end maintrans *)

(* End of the auxiliary functions and procedures *)

(* ************************************************************************ *)
(* ************************************************************************ *)
(* ************************************************************************ *)
(* ************************************************************************ *)

(* Start of the executable code lines *)

debugLevel=0;   (* set as a default *)
debug =  True;  (* edit this as necessary *)
debugT = True;  (* edit this as necessary, for the transcendental code *)

(* WH 10/04/2003 *)
(* Old loading homotopy functions (Ryan Sayers, May 29, 2003) *)

(* Change by Mike C., July 1, 2003 *)
(* <<loadHomotopyFunctions.m *)
(* The Homotopy Operator code will be in a different directory than the     *)
(* current directory.  It is up one level in its own directory.             *)

(* WH 10/04/2003 *)
(* Homotopy code is now entered in this code *)
(*
  $Path = Append[$Path, "../PDE1DFlx"];
  Get["loadHomotopyFunctions.m"];
*)

(* WH 10/04/2003 *)
(* ***************** START HOMOTOPY OPERATOR CODES ********************* *)

(* File PDEHOPER.M: all pieces for homotopy operator in one file! *)
(* WH 10/04/2003 *)
(* Based on code of 10/01/2003 *)

(* ************************* BEGIN of all ***************************** *)

(* ********************* BEGIN loadHomotopyFunctions.m ********** *)

(************************************************************************)
(* loadHomotopyFunctions                                                *)
(* Purpose: To load the common functions used for finding the conserved *)
(*          fluxes of systems of PDEs                                   *)
(* Authors: Ingo Kabirschke, Kara Namanny, Frances Martin               *)
(* Input: None                                                          *)
(* Output: None                                                         *)
(* Created: May 21, 2003                                                *)
(* Last Modified: May 21, 2003 @ 4:59 PM @ MLRC                         *)
(************************************************************************)

(*
<<eulerOperator.m
<<interiorProduct.m
<<orderOfExpression.m
<<lambdaReplace.m
<<homotopyOperator.m
*)

(* ********************* END loadHomotopyFunctions.m ********** *)

(* ********************* BEGIN eulerOperator.m ************************ *)

(* Time-stamp: <Fri May 30 11:50:43 Mountain Daylight Time 2003>        *)
(************************************************************************)
(* eulerOperator[expression_, orderOfOperator_, dependVar_,             *)
(*               independVar_]                                          *)
(* Purpose: To calculate a specified order Euler operator of a given    *)
(*          expression with respect to the dependent variable           *)
(* Authors: Ingo Kabirschke, Kara Namanny, Frances Martin, Adam Ringler *)
(* Input: An expression: expression,                                    *)
(*        the order of the desired operator: orderOfOperator,           *)
(*        a dependent variable: dependVar,                              *)
(*        an independent variable: independVar                          *)
(* Output: A new expression that is the result of applying the specified*)
(*         order Euler Operator to the original expression.             *)
(* Created: May 19, 2003 at CSM                                         *)
(* Last Modified: May 30, 2003 at 11:30 AM at CSM                       *)
(************************************************************************)

(* Debug Flags for the Euler Operator *)
debugEulerOperatorInput = False;
debugEulerTerms = False;
debugEulerResult = False;

eulerOperator[expression_, orderOfOperator_, dependVar_, independVar_] :=
  Module[{i, orderOfExp, eulerResult},

    (* Debugging input to Euler Operator *)
    If[debugEulerOperatorInput, 
      Print["Applying the ", orderOfOperator, " order Euler Operator to "]; 
      Print[expression];
    ];

    (* Finding the highest order derivative in the expression. This    *)
    (* will be used to limit the sum in the Euler operator.            *)
    orderOfExp = orderOfExpression[expression, dependVar, independVar];
                                    
    (* Calculating specified euler operator (variational derivative)   *)
    (* using \cal{L}_u ^{(i)} on page 16 of Hereman's notes            *)
    eulerResult = Expand[
                    Sum[((-1)^(i - orderOfOperator)) * 
                        Binomial[i, orderOfOperator] * 
                        D[ D[expression, 
                             {Derivative[i, 0][dependVar][independVar, t], 1}],
                          {independVar, (i - orderOfOperator)}], 
                       {i, orderOfOperator, orderOfExp}]
                  ];

    (* Debugging terms: Prints terms, produced by applying the Euler   *)
    (* operator, in tabular form.                                      *)
    If[debugEulerTerms,
      Print["The terms from applying the Euler operator are: ",
            Expand[
              Table[((-1)^(i - orderOfOperator)) * 
                      Binomial[i, orderOfOperator] * 
                      D[ D[expression, 
                           {Derivative[i, 0][dependVar][independVar, t], 1}],
                        {independVar, (i - orderOfOperator)}], 
                    {i, orderOfOperator, orderOfExp}]
            ]
      ]
    ];
    
    (* Debugging result of applying the Euler Operator *)
    If[debugEulerResult,
      Print["The result of applying the ", orderOfOperator, " order
            Euler Operator to "];
      Print[expression];
      Print[" is: "];
      Print[eulerResult];
    ];
 
    (* Returning Euler Operator of specified order *)
    Return[eulerResult];
  ];

(* Print["Euler Operator loaded successfully."]; *)                 

(* ********************* END eulerOperator.m ************************ *)

(* ********************* BEGIN interiorProduct.m ************************ *)

(* Time-stamp: <Tue Jun 10 11:57:36 Mountain Daylight Time 2003>        *)
(************************************************************************)
(* interiorProduct[expression_, dependVar_, independVar_]               *)
(* Purpose: To calculate the interior product of an expression          *)
(* Authors: Ingo Kabirschke, Kara Namanny, Adam Ringler                 *)
(* Input: An expression: expression,                                    *)
(*        a dependent variable: dependVar,                              *)
(*        an independent variable: independVar                          *)
(* Output: The interior product of the expression, using definitions    *)
(*         from Hereman's notes, page 17                                *)
(* Created: May 21, 2003 at CSM                                         *)
(* Last Modified: May 30, 2003 at 11:50 AM at CSM                       *)
(************************************************************************)

(* Debug Flags for the Interior Product *)
debugInteriorProductInput = False;
debugInteriorProductTerms = False;
debugInteriorProductResult = False;

interiorProduct[expression_, dependVar_, independVar_] := 
  Module[{i, interiorProductResult},

    (* Debugging input of interiorProduct *)
    If[debugInteriorProductInput, 
       Print["Interior Product acting on "];
       Print[expression];
       Print[" with dependent variable ", dependVar, 
             " with independent variable ", independVar];
    ];

    (* Calculates the interior product (little j) using the definition *)
    (* on page 17 of Hereman's notes                                   *)
    interiorProductResult = 
      Expand[
        Sum[D[dependVar[independVar, t] *
              eulerOperator[expression, i + 1, dependVar,
                            independVar], {independVar, i}],
          {i, 0, orderOfExpression[expression, dependVar, independVar] - 1}
        ]
      ];

    (* Debugging terms - prints terms of interior product in table form *)
    If[debugInteriorProductTerms, 
       Print["The terms of the interior product are ", 
         Expand[Table[D[dependVar[independVar, t] *
                        eulerOperator[expression, i + 1, dependVar,
                                      independVar], 
                        {independVar, i}],
                  {i, 0, orderOfExpression[expression, 
                                           dependVar, independVar] - 1}
                ] 
         ]
       ]
    ];

    (* Debugging result *)
    If[debugInteriorProductResult,
       Print["The Interior Product of "];
       Print[expression];
       Print[" with respect to ", dependVar, " is: "];
       Print[interiorProductResult];
    ];

    (* Returns interior product of given expression *)
    Return[interiorProductResult]
]; 

(* Print["Interior Product loaded successfully."]; *)

(* ********************* END interiorProduct.m ************************ *)

(* ********************* BEGIN orderOfExpression.m ******************** *)

(* Time-stamp: <Mon Sep 22 21:55:54 MDT 2003>  *)
(******************************************************************)
(* orderOfExpression[expression_, dependVar_, independVar_]       *)
(* Purpose: To determine the maximum derivative in expression of  *)
(*          dependVar with respect to independVar                 *)
(* Authors: Ryan Sayers, Frances Martin, Ingo Kabirschke,         *)
(*          Kara Namanny, Adam Ringler                            *)
(* Input: An expression: expression,                              *)
(*        a dependent variable: dependVar,                        *)
(*        and an independent variable: independVar                *)
(* Output: The maximum derivative in expression of dependVar      *)
(*         with respect to independVar                            *)
(* Created: May 15, 2003, 2:39 PM at CSM                          *)
(* Last Modified: Sep 22, 2003, 9:53 AM by Mike C.                *)
(******************************************************************)

(* Debug Flags for the Order of the Expression *)
debugOrderList = False;
debugOrderCasesList = False;
debugOrderInput = False;

orderOfExpression[expression_, dependVar_, independVar_] := 
  Module[{xDerivRule, orderCasesList, orderList, orderResult, order},

    (* Debugging input of orderOfExpression *)
    If[debugOrderInput, 
       Print["Input expression is "];
       Print[expression];
    ];

    (* Replaces derivatives of dependVar with their order         *)
    (* Replaces dependVar with 0, since it has 0 derivatives      *)
    (* Mike C.: *)
    (* Changed dummy variable "i" to "order."  "i" is too risky   *)
    xDerivRule = {Derivative[order_, 0][dependVar][independVar, t] -> order,
                  dependVar[independVar, t]-> 0};

    (* Cases will find every occurance of dependVar and its       *)
    (* derivatives in the expression and put them in a list       *)
    orderCasesList = Union[
                       Cases[expression, 
                             Derivative[order_, 0][dependVar][independVar, t], 
                         {0, Infinity}],
                       Cases[expression, 
                             dependVar[independVar, t], 
                         {0, Infinity}]
                     ];

    (* Debugging list of occurances of the dependent variable *)
    If[debugOrderCasesList, 
       Print["Cases are ", orderCasesList]
    ];

    (* Apply replacement rule *)
    orderList = orderCasesList /. xDerivRule;

    (* Debug list of orders *)
    If[debugOrderList, 
       Print["Order List is ", orderList]
    ];

    (* Find the maximum order in the list *)
    orderResult = Max[orderList];

    (* Return the highest order of the expression *)
    Return[orderResult];
  ];

(* Print["Order of Expression loaded successfully."]; *)

(* ********************* END orderOfExpression.m ******************** *)

(* ********************* BEGIN lambdaReplace.m ******************** *)

(* Time-stamp: <Fri May 30 11:23:55 Mountain Daylight Time 2003>        *)
(************************************************************************)
(* lambdaReplace[expression_, dependVarHead_, independVar_]             *)
(* Purpose: To replace the dependent variable with lambda * dependVar   *)
(* Authors: Kara Namanny and Adam Ringler                               *)
(* Input: An expression: expression,                                    *) 
(*        a dependent variable head: dependVarHead,                     *)
(*          (For example, for u[1], u[2], and u[3] the head would be u) *)
(*        and an independent variable: independVar                      *)
(* Output: The expression with the dependent variable replaced          *)
(*         by lambda * dependent variable                               *)
(* Created: May 21, 2003 @ 3:41 PM @ CSM                                *)
(* Last Modified: May 30, 2003 @ 11:19 AM @ MLRC                        *)
(************************************************************************)

(* Debug Flags *)
debugLambdaReplaceInput = False;
debugLambdaReplaceResult = False;

lambdaReplace[expression_, dependVarHead_, independVar_] :=
  Module[{i, j, lambdaReplaceRule, lambdaReplaceResult},

    (* Debugging Lambda Replacement input *)
    If[debugLambdaReplaceInput, 
      Print["Replacing the dependent variables, ", 
        dependVarHead, "[i], in the expression, "];
      Print[expression]; 
      Print[", with lambda * ", dependVarHead, "[i]"];
    ];

    (* Defining replacement rules: multiplies every occurance of the *)
    (* dependent variable and its derivatives by lambda              *)
    lambdaReplaceRule = {
      dependVarHead[i_][independVar, t] -> 
        lambda * dependVarHead[i][independVar, t], 
      Derivative[i_, j_][dependVarHead[k_]][independVar, t] -> 
        lambda * Derivative[i, j][dependVarHead[k]][independVar, t]};

    (* Apply replacement rule to expression *)
    lambdaReplaceResult = Expand[expression /. lambdaReplaceRule];

    (* Debugging result of Lambda Replacement *)
    If[debugLambdaReplaceResult, 
      Print["The result of replacement is: "];
      Print[lambdaReplaceResult];
    ];

    (* Return expression with lambda replacement *)
    Return[lambdaReplaceResult];
  ];

(* Print["Lambda Replace loaded successfully."]; *)

(* ********************* END lambdaReplace.m ******************** *)

(* ********************* BEGIN homotopyOperator.m ******************** *)

(* Time-stamp: <Tue Jul  1 03:34:48 MDT 2003>        *)
(************************************************************************)
(* homotopyOperator[expression_, dependVarHead_, numDependVar_ ,        *)
(*                  independVar_]                                       *)
(* Purpose: To calculate the Total Homotopoy Operator of an expression  *)
(* Authors: Ingo Kabirschke, Frances Martin                             *)
(* Input: An expression: expression,                                    *) 
(*        a dependent variable head: dependVarHead,                     *)
(*          (For example, for u[1], u[2], and u[3] the head would be u) *)
(*        the number of elements in the vector: numDependVar,           *) 
(*        an independent variable: independVar                          *)
(* Output: The Total Homotopy Operator of expression                    *)
(* Created: May 21, 2003 at CSM                                         *)
(* Last Modified: July 1, 2003, at 3:30 AM by Mike C.                   *)
(************************************************************************)

(* Debug Flags *)
debugHomotopyOpInput = False;
debugHomotopyOpResult = False;

(* WH 10/04/2003 *)
continuousHomotopyOperator[expression_, dependVarHead_, numDependVar_, 
  independVar_] := 
  Module[{i, fullIntProduct, lambdaExpression, homotopyOpResult, integrand},

    (* Debugging input *)
    If[debugHomotopyOpInput,
       Print["Applying the Total Homotopy Operator to "];
       Print[expression];
    ];

    (* Checking for an integrable expression (Zeroth Euler Operator = 0) *)
    For[i = 1, i <= numDependVar, i++,
      If[eulerOperator[expression, 0, dependVarHead[i], independVar] != 0,
         (* Then (if not integrable) *)
         Print["The expression input, "];
         Print[expression];
         Print[" is not integrable."];
         Print["Aborting calculations."];
         Return[False];
      ];
    ];

    (* Steps below taken from Hereman's Notes, pg 17 *)
    (*************************************************)

    (* Calculate the interior product (little j) of the given expression *)
    fullIntProduct = Sum[     
                       interiorProduct[expression, dependVarHead[i], 
                                       independVar],
                       {i, 1, numDependVar}
                     ];

    (* Replace dependent variable with lambda * dependent variable *)
    lambdaExpression = 
      lambdaReplace[fullIntProduct, dependVarHead, independVar];

    (* Divide by lambda *)
    integrand = Expand[lambdaExpression / lambda];

    (* Integrate with respect to lambda to find flux (big J) *)
    homotopyOpResult = Expand[Integrate[integrand, {lambda, 0, 1}]];

    (* 06/11/03 Mike C: *)
    homotopyOpResult = TrigExpand[homotopyOpResult];

    (* Debugging result *) 
    If[debugHomotopyOpResult,
       Print["The result of applying the Total Homotopy Operator is "];
       Print[homotopyOpResult];
    ];

    (* Return final result of applying the PDE Homotopy Operator *)
    Return[homotopyOpResult];
];

(* Print["Homotopy Operator loaded successfully."]; *)

(* ********************* END homotopyOperator.m ******************** *)

(* ********************* BEGIN pdeform.m ******************** *)

(*****************************************************************************)
(* pdeform[]: takes an expression and prints it with subscripted derivatives *)
(*            and removes the functional dependence (i.e. f[x,t] -> f)       *)
(*            Written by Tracy Otto & Tony Miller (CSM-1995).                *)
(*            Modified by U. Goktas.                                         *)
(*****************************************************************************)
pdeform[expres_] := expres /. {
        Derivative[n__][u[k_]][x__] :>
                SequenceForm[u, Subscript[k],Subscript[","],
                        Subscript[
                         SequenceForm @@ Flatten[Table[#[[1]], {#[[2]]}]& /@
                                        Transpose[{{x}, {n}}]]]],
        u[n_][x__] :> SequenceForm[u,Subscript[n]] }; (* end pdeform *)

(* ********************* END pdeform.m ******************** *)

(* ********************************** end of all ************************ *)

(* ***************** END HOMOTOPY OPERATOR CODES ********************* *)
(* WH 10/04/2003 *)

(* End of homotopyOperator integration in this function *)

(* ***************************************************************** *)

menu;

(* HERE INITIALLY *)
(* 10/05/2003 WH/HE setting up empty lists for listallrhos, listallfluxes *)
listallrhos = {};
listallfluxes = {};

If[storeLog, OpenLog[myfile]];
commentinter[]; 
If[formrho[x,t] === 0, formrho[x,t] = {}];
If[Not[ListQ[formrho[x,t]]], formrho[x,t] = {formrho[x,t]}];

Print[" "];
Print["Working with the data file for the ",name,"."]; 
(* WH 10/04/2003 *)
(* Print[" "]; *)
setup;  
For[i = 1,i <= noeqs,i++,
   eq[i][x,t] = Expand[eq[i][x,t]];
   Print[" "];
   Print["Equation ",i," of the system with ",noeqs," equation(s):"];
(* WH 10/05/2003 *)
(*   Print[" "]; *)
   Print["  ",pdeform[eq[i][x,t]]," = 0"];
   ];

eq[x,t] = Sum[eq[i][x,t],{i,1,noeqs}];
highestordereq = highestorder[eq[x,t],False,False,False,False,True,False];
Print[" "];
Print["Highest order of derivative in the system is ",highestordereq,"."];
Print[" "];

mainsolforlist = findMainsolforlist; 

If[debugLevel>=3, Print["   parameters: ",parameters]];
If[debugLevel>=3, Print["   weightpars: ",weightpars]];
If[debugLevel>=3, Print["   givenscalerules: ",givenscalerules]];

If[formrho[x,t] === {}, (* start point rhogiven *)
   eqlist = Flatten[Expand[Table[eq[i][x,t],{i,1,noeqs}]]];

scalerules = findWeightSystem[eqlist]; 

If[Length[varsweightzero]>0,
    Print["--> The following variables have weight of zero: ",varsweightzero];
 (* Only one variable of weight zero is allowed *)
    wtzvar = varsweightzero[[1]]
  ];

If[Length[varsweightzero]>1,
    Print["  Error.  Software not able to continue with more than one",
          " variable of weight zero.  Aborting computations."];
    Abort[]];

counternegweight = 0;
listfreeweights = {};
varscalelist = {};
allchoices = {};
Print[" "];
(* Here the weights are assigned to the variables weightu[i] *)
Print["Program determines the weights of the variables (and parameters)."];
For[i = 1,i <= noeqs,i++,
    weightu[i] = weightu[i] /. scalerules;
    If[i === 1, Print[" "]; Print["For the given system:"]; Print[" "]];
    Print["* weight of u[",i,"] is ",weightu[i],"."]; 
  (* Next If statement fixes the list used to construct the form of rho *)
    If[weightu[i]>0, 
       varscalelist = Union[varscalelist,{{u[i][x,t],weightu[i]}}],
      (* Next lines are for variables of weight zero *) 
       temp = Select[mainsolforlist, Not[FreeQ[#, u[i]]] &];
       If[(temp /. Derivative[a__, 1][u[b_]][x, t] -> a)[[1]] == 0,
          varscalelist = Union[varscalelist, {{D[u[i][x, t], x], 1}}],
          varscalelist = Union[varscalelist, temp /. 
             Derivative[a__, 1][u[b_]][x,t] -> 
                                 {Derivative[a, 0][u[b]][x, t], a}]
         ];
       Clear[temp]
      ];  
(* this last If statement takes care of weight zero u[#] components *)
    If[Not[NumberQ[weightu[i]]],
       If[Length[weightu[i]] === 1,
          listfreeweights = Union[listfreeweights,{weightu[i]}]],
       If[weightu[i] < 0,counternegweight++];
      ];
   ];  (* end For loop *)
lenweightpars = Length[weightpars];

If[debugLevel>=3, Print["Parameters with weight, weightpars: ",weightpars];
      Print["  and varscalelist = ",varscalelist]];
  For[i = 1,i <= lenweightpars,i++,
     weight[Part[weightpars,i]] = 
             weight[Part[weightpars,i]] /. scalerules; 
     Print["* weight of ",Part[weightpars,i]," is ",
           weight[Part[weightpars,i]],"."];
     varscalelist = Union[varscalelist,
                    {{Part[weightpars,i],weight[Part[weightpars,i]]}}];
     If[Not[NumberQ[weight[Part[weightpars,i]]]],
       If[Length[weight[Part[weightpars,i]]] === 1,
         listfreeweights = 
            Union[listfreeweights,{weight[Part[weightpars,i]]}]],
       If[weight[Part[weightpars,i]] < 0,counternegweight++];
       ];
     ]; 
(* Here the weight[d/dt] is assigned *)
  weight[d/dt] = weight[d/dt] /. scalerules;
  Print["* weight of d/dt is ",weight[d/dt],"."];

If[debugLevel>=3, Print["Variable scaling, varscalelist: ",varscalelist]]; 

  tvarscalelist = Union[varscalelist,{{"d/dt",weight[d/dt]}}];
  If[Not[NumberQ[weight[d/dt]]],
    If[Length[weight[d/dt]] === 1,
      listfreeweights = Union[listfreeweights,{weight[d/dt]}]]
    ];
  counterfreeweight = Length[listfreeweights];
  If[counternegweight =!= 0,        (* start of If 1 *)
    Print[" "];
    Print["One or more of the weights are negative."];
    Print["Negative weights are not allowed."];
    Print["Aborting the computations!"];
    CloseLog[]; Abort[], 
    If[counterfreeweight =!= 0,
      If[counterfreeweight > 1,
        Print[" "];
        Print["Two or more of weights have freedom."];
        Print["Enter your values for the free weights or type Abort[];"];
        Print["Your values for the free weights can be entered by typing"];
        Print["`weightu[label] = val' or `weight[variable] = val' and"];
        Print["putting `;' between your entries."];
        Input[": "], (* else of counterfreeweight > 1 *)
        Print[" "];
        Print["Program will try to determine CHOICES for ",
              listfreeweights[[1]],":"];
        Print[" "];
        tempvarscalelist = Sort[Table[Part[tvarscalelist,k][[2]], 
                                 {k,1,Length[tvarscalelist]}]];
        tempvarscalelist = 
               Complement[tempvarscalelist,Select[tempvarscalelist,NumberQ]];
        lentempvarscalelist = Length[tempvarscalelist];
        Do[ (* begin Do loop *)
          Print["* CHOICE ",k];
          Print[" "];
          Print["Solving the equation: ",Part[tempvarscalelist,k]," = 1."];
          Print[" "];
          attempt = Flatten[Solve[Part[tempvarscalelist,k] == 1]];
          If[attempt =!= {},
            solfreeweight = Part[listfreeweights,1] /. attempt;
            While[solfreeweight <= 0, 
                 solfreeweight = solfreeweight + 1;
                 Print["Since the weight was zero or negative,"];
                 Print["it was incremented with 1."];
                 Print[" "]
                 ];
            Print[Part[listfreeweights,1]," = ",solfreeweight];
            weightrule = Part[listfreeweights,1] -> solfreeweight;
            tempvarscalelistval = tempvarscalelist /. weightrule;
            If[MemberQ[NonNegative[tempvarscalelistval],False],
              Print["Choice is rejected!"],
              varscalelistval = varscalelist /. weightrule;
              tvarscalelistval = tvarscalelist /. weightrule;    
              Print[" "];
              Print["List of all the variables (and parameters) with their"]; 
              Print["weights:"];
              Print[" "];
              Print[tvarscalelistval];
weight[d/dt] = tvarscalelistval /. {___, {"d/dt", n_}, ___} -> n;
              Print[" "];
              testvarscalelist = Table[Part[varscalelistval,k][[2]],
                        {k,1,Length[varscalelist]}];
              If[Union[Positive[testvarscalelist]] === {False},
                Print["Since all the weights of u[i] are zero"];
                Print["this choice is rejected!"];
                Print[" "],
                allchoices = Union[allchoices,{solfreeweight}];
                ];
              ];
            ],{k,1,lentempvarscalelist} ];  (* end do *)
        Print[" "];
        Print["Simple POSITIVE choices are considered."];
        Print[" "];
        intchoices = Select[allchoices,IntegerQ];
        fracchoices = Complement[allchoices,intchoices];
        If[intchoices =!= {},
          (* Picking the minimum integer choice *)
          intchoices = Min[intchoices]; 
          choicerule = Part[listfreeweights,1] -> intchoices, (* else *)
          If[fracchoices =!= {},
           (* Picking the minimum fractional choice *)
           fracchoices = Min[fracchoices]; 
           choicerule = Part[listfreeweights,1] -> fracchoices, (* else *)
           Print["Enter your choice for the value of ",listfreeweights[[1]],
                 "by typing its value. NO semi-colon at the end!"];
           choicerule = Part[listfreeweights,1]-> Input[": "];
           ]; 
          ]; 
        varscalelistchoice = varscalelist /. choicerule;
        tvarscalelistchoice = tvarscalelist /. choicerule;
        If[Length[allchoices] > 1,
          Print["List of all positive choices considered: "];
          Print[" "];
          Print[Union[Table[Part[listfreeweights,1] -> Part[allchoices,k], 
                             {k,1,Length[allchoices]}]]];
          Print[" "];
          Print["Some of the weights of u[i], but not all, could be zero."];
          Print["In the data file you could enter your choices in the format"];
          Print["`weightu[label] = value;' or `weight[variable] = value;'."];
          Print[" "]; 
          Print["Program continues with the choice: ", choicerule];
          Print[" "];
          Print["corresponding to the weights:"];
          Print[" "]; 
          Print[tvarscalelistchoice]; 
          Print[" "]; 
          ];
        varscalelist = varscalelistchoice;
      ] ] ]; (* end if 1 *)

  Print[" "];
  Print["The rank of rho should be an integer multiple of the lowest weight"];
  Print["of the DEPENDENT variable(s). Fractional weights are allowed."];
  If[
    formrho[x,t] === {},
    If[Not[NumberQ[rhorank]], rhorank = Input["Enter the rank of rho: "]];
    Print[" "]
    ];
  If[formrho[x,t] === {},
    Print["Computation of the density (and flux) of RANK = ",rhorank] 
    ];  

  nodims = noeqs + Length[weightpars];
  varscalelist = Reverse[Sort[varscalelist,OrderedQ[{#1[[2]],#2[[2]]}]&]];
  formrho[x,t]=constructform[nodims,varscalelist,rhorank,True];
  If[Head[formrho[x,t]] === Times, 
    lenformrho = 1,lenformrho = Length[formrho[x,t]]
    ],                      (* else point for rhogiven *)
  formrho[x,t] = Part[formrho[x,t],1];
  formrho[x,t] = Expand[formrho[x,t]];
  If[Head[formrho[x,t]] === Times, 
    lenformrho = 1,lenformrho = Length[formrho[x,t]]
    ];
  Print[" "];
  Print["The program will only test the given form of the density rho."];
  Print["No determination of scaling properties."];
  Print["You have given this form for rho :"];
  Print[" "]; 
  Print[pdeform[formrho[x,t]]];
  Print[" "];
  ]; (* end point rhogiven *)

(* Solving for u[i]_t and calculation of the x-derivatives *)
If[debugLevel>=1,
  Print["*************************************************"];
  Print["Trying to solve the equation(s) for ",mainsolforlist,","];
  Print["respectively."]; 
  Print[" "];
  Print["*************************************************"] ];
utlist = {};

(* Need to move this higher (maybe)  TODO PJA*)
For[i = 1,i <= noeqs,i++,
   For[j=1, j<= noeqs, j++,
      ut[i][j] = Solve[eq[i][x,t] == 0,mainsolforlist[[j]]];
      utlist = Flatten[Append[utlist,ut[i][j]]];
      ]
   ];
utlist=ExpandAll[utlist];

If[Length[varsweightzero]>0,
   maintrans;
   Abort[]];  (* This will run the solver for systems with trans fns if there
                   are vars with weight of zero (i.e., trans functions are in
                   the system), else it will proceed as normal with the rest
                   of the computations (to handle the original cases         *)

highestorderrho=highestorder[formrho[x,t],False,False,False,False,True,False];
Print["The highest-order of the terms in rho is ",
highestorderrho,"."];
eqlist = Flatten[Table[eq[i][x,t],{i,1,noeqs}]];
unknownlist = Table[c[i],{i,1,lenformrho}];
parameters = Union[parameters,weightpars];

Clear[i,k,lenformrho,counternegweight,counterfreeweight,scalerules,
     lenweightpars,varscalelist,tempvarscalelist,tvarscalelist,
     attempt,listfreeweights,allchoices,varscalelistval,temparscalelistval,
     tvarscalelistval,intchoices,fracchoices,choicerule,weightrule,
     nodims,highestordereq,weightu,lentempvarscalelist,solfreeweight,
     varscalelistchoice,tvarscalelistchoice,testvarscalelist,name(*,weight*)];

Clear[ut,mainsolforlist,i];
If[debug,
   Print["utlist: ",utlist];
   Print["formrho[x,t]: ", formrho[x,t]];
   Print["debugLevel: ", debugLevel];
   Print["noeqs: ", noeqs];
   Print["weights: weight[u[1]]= ", weight[u[1]]];
   Print["weights: weight[d/dt]= ", weight[d/dt] ];
   Print["rhorank: ", rhorank];
   Print[" "]
  ];
main[utlist];

(* Print[" "]; *)
Print[" "];
Print["*********************** SUMMARY ***********************"];
Print[" "];
Print["Total session time used in the current session is ",transtime,
      " seconds."];
Print[" "];
Print["To see all the densities type: listallrhos," ];
Print["or pdeform[listallrhos]."];
Print[" "];
Print["To see the last density type: rho[x,t], or use pdeform[rho[x,t]]."];
Print[" "];
Print["To see the last density sorted by coefficients"];
Print["type 'sortedrho' or `sortIt[rho[x,t]]'"];
Print[" "];
Print["To see all the fluxes type: listallfluxes,"];
Print["or pdeform[listallfluxes]."];
Print[" "];
Print["To see the last flux type: J[x,t], or use pdeform[J[x,t]]."];
Print[" "];
Print["To see the last flux sorted by coefficients"];
Print["type 'sortedflux' or `sortIt[J[x,t]]'"];
Print[" "];
Print["*************************** SUMMARY ***************************"];
Print[" "];

If[storeLog,CloseLog[]];

(* ******************************* END ************************************* *)
