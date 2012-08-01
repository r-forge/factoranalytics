# performance attribution
# Yi-An Chen
# July 30, 2012

factorModelPerformanceAttribution <- 
  function(fit,benchmark=NULL) {
   
    # input
    # fit  :   Class of MacroFactorModel
    # benchmark: benchmark returns, default is NULL. If benchmark is provided, active returns 
    #            is used.
    # output
    # class of "FMattribution" 
    #    
    # plot.FMattribution     
    # summary.FMattribution    
    # print.FMattribution    
    require(zoo)
    if (class(fit) !="MacroFactorModel")
    {
      stop("Class has to be MacroFactorModel.")
    }

# return attributed to factors
    cum.attr.ret <- fit$beta.mat
    cum.spec.ret <- fit$alpha.vec
    factorName = colnames(fit$beta.mat)
    fundName = rownames(fit$beta.mat)
    
    attr.list <- list()
    
    for (k in fundName) {
    fit.lm = fit$asset.fit[[k]]
   
    ## extract information from lm object
    date <- names(fitted(fit.lm))
   
    actual.z = zoo(fit.lm$model[1], as.Date(date))
 

# attributed returns
# active portfolio management p.512 17A.9 
    
  cum.ret <-   Return.cumulative(actual.z)
  # setup initial value
  attr.ret.z.all <- zoo(, as.Date(date))
  for ( i in factorName ) {
  
    if (fit$beta.mat[k,i]==0) {
    cum.attr.ret[k,i] <- 0
  attr.ret.z.all <- merge(attr.ret.z.all,zoo(rep(0,length(date)),as.Date(date)))  
  } else {
  attr.ret.z <- actual.z - zoo(as.matrix(fit.lm$model[i])%*%as.matrix(fit.lm$coef[i]),
                               as.Date(date))  
  cum.attr.ret[k,i] <- cum.ret - Return.cumulative(actual.z-attr.ret.z)  
  attr.ret.z.all <- merge(attr.ret.z.all,attr.ret.z)
  }
  
  }
    
  # specific returns    
    spec.ret.z <- actual.z - zoo(as.matrix(fit.lm$model[,-1])%*%as.matrix(fit.lm$coef[-1]),
                                 as.Date(date))
    cum.spec.ret[k] <- cum.ret - Return.cumulative(actual.z-spec.ret.z)
  attr.list[[k]] <- merge(attr.ret.z.all,spec.ret.z)
   colnames(attr.list[[k]]) <- c(factorName,"specific.returns")
    }

    
    
    
    
    ans = list(cum.ret.attr.f=cum.attr.ret,
               cum.spec.ret=cum.spec.ret,
               attr.list=attr.list)
class(ans) = "FM.attribution"      
return(ans)
    }