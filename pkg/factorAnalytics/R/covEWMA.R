covEWMA <-
function(factors, lambda=0.96) {
## Inputs:
## factors    N x K numerical factors data.  data is class data.frame
##            N is the time length and K is the number of the factors.  
## lambda     scalar. exponetial decay factor between 0 and 1. 
## Output:  
## cov.f.ewma  array. dimension is N x K x K.
  

if (is.data.frame(factors)){
  factor.names  = colnames(factors)
  t.factor      = nrow(factors)
  k.factor      = ncol(factors)
  factors       = as.matrix(factors)
  t.names       = rownames(factors)
} else {
  stop("factor data should be saved in data.frame class.") 
}
if (lambda>=1 || lambda <= 0){
  stop("exponetial decay facotr lambda should be between 0 and 1.")
} else {
  cov.f.ewma = array(,c(t.factor,k.factor,k.factor))
  cov.f = var(factors)  # unconditional variance as EWMA at time = 0 
  FF = (factors[1,]- mean(factors)) %*% t(factors[1,]- mean(factors))
  cov.f.ewma[1,,] = (1-lambda)*FF  + lambda*cov.f
  for (i in 2:t.factor) {
    FF = (factors[i,]- mean(factors)) %*% t(factors[i,]- mean(factors))
    cov.f.ewma[i,,] = (1-lambda)*FF  + lambda*cov.f.ewma[(i-1),,]
  }
    
}
 
  return(cov.f.ewma)  

}

