pCornishFisher <-
function(q,n, skew, ekurt) {
zp = pnorm(q)
CDF = zp  +   1/sqrt(n) *(skew/6 * (1-q^2))*dnorm(q) + 
     1/n *( ekurt/24*(3*q-q^3)+ (skew/6)^2/72*(10*q^3 - 15*q -q^5))*dnorm(q)
return(CDF)
}

