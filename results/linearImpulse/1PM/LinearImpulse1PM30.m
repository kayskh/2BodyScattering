(* ::Package:: *)

LinearImpulse1PM[3,0] = (m1*m2*\[Gamma]*\[Kappa]^2*Ca[3]*
     (-8*dot[b, a1]*dot[b, b]*((-1 + \[Gamma]^2)*dot[lI[\[Rho]], a1] + 
        dot[a1, v2]*(-(\[Gamma]*dot[lI[\[Rho]], v1]) + dot[lI[\[Rho]], v2]))*
       eps[b, a1, v1, v2] + 4*(-1 + \[Gamma]^2)*dot[b, a1]^2*
       (6*dot[lI[\[Rho]], b]*eps[b, a1, v1, v2] - 
        dot[b, b]*eps[lI[\[Rho]], a1, v1, v2]) + 
      ((-1 + \[Gamma]^2)*dot[a1, a1] + dot[a1, v2]^2)*dot[b, b]*
       (-4*dot[lI[\[Rho]], b]*eps[b, a1, v1, v2] + 
        dot[b, b]*eps[lI[\[Rho]], a1, v1, v2])))/
    (24*Pi*(-1 + \[Gamma]^2)^(3/2)*dot[b, b]^4)
