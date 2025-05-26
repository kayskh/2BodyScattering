(* ::Package:: *)

LinearImpulse1PM[2,0] = (m1*m2*(-1 + 2*\[Gamma]^2)*\[Kappa]^2*Ca[2]*
     (-4*(-1 + \[Gamma]^2)*dot[b, a1]^2*dot[lI[\[Rho]], b] + 
      ((-1 + \[Gamma]^2)*dot[a1, a1] + dot[a1, v2]^2)*dot[b, b]*
       dot[lI[\[Rho]], b] + 2*dot[b, a1]*dot[b, b]*
       ((-1 + \[Gamma]^2)*dot[lI[\[Rho]], a1] + dot[a1, v2]*
         (-(\[Gamma]*dot[lI[\[Rho]], v1]) + dot[lI[\[Rho]], v2]))))/
    (16*Pi*(-1 + \[Gamma]^2)^(3/2)*dot[b, b]^3)
