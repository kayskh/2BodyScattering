(* ::Package:: *)

LinearImpulse1PM[4,0] = (m1*m2*(-1 + 2*\[Gamma]^2)*\[Kappa]^2*Ca[4]*
     (16*(-1 + \[Gamma]^2)^2*dot[b, a1]^4*dot[lI[\[Rho]], b] - 
      12*(-1 + \[Gamma]^2)*((-1 + \[Gamma]^2)*dot[a1, a1] + dot[a1, v2]^2)*
       dot[b, a1]^2*dot[b, b]*dot[lI[\[Rho]], b] + 
      ((-1 + \[Gamma]^2)*dot[a1, a1] + dot[a1, v2]^2)^2*dot[b, b]^2*
       dot[lI[\[Rho]], b] - 8*(-1 + \[Gamma]^2)*dot[b, a1]^3*dot[b, b]*
       ((-1 + \[Gamma]^2)*dot[lI[\[Rho]], a1] + dot[a1, v2]*
         (-(\[Gamma]*dot[lI[\[Rho]], v1]) + dot[lI[\[Rho]], v2])) + 
      4*((-1 + \[Gamma]^2)*dot[a1, a1] + dot[a1, v2]^2)*dot[b, a1]*
       dot[b, b]^2*((-1 + \[Gamma]^2)*dot[lI[\[Rho]], a1] + 
        dot[a1, v2]*(-(\[Gamma]*dot[lI[\[Rho]], v1]) + dot[lI[\[Rho]], 
           v2]))))/(16*Pi*(-1 + \[Gamma]^2)^(5/2)*dot[b, b]^5)
