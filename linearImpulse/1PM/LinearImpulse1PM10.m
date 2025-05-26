(* ::Package:: *)

LinearImpulse1PM[1,0] = (m1*m2*\[Gamma]*\[Kappa]^2*
     (-2*dot[lI[\[Rho]], b]*eps[b, v2, v1, a1] + 
      dot[b, b]*eps[lI[\[Rho]], v2, v1, a1]))/(8*Pi*Sqrt[-1 + \[Gamma]^2]*
     dot[b, b]^2)
