#
# fmfit
#
# function to compute classical and robust multiple factor models
#
# Current Developer: Guy Yollin
# Previous Developers: Christopher G. Green and Yindeng Jiang
# Original Developer: Eric Aldrich
#
#
# REQUIRED ARGUMENTS:
#
#   fmdata
#   fmds object.
#
# timedates
#   Date object determining the date range for the factor model.
#
# exposures
#   character vector comprised of the names of variables to be used in the factor model.
#
# assets
#   character vector comprised of the tickers of the assets to be used in the factor model.
#
# OPTIONAL ARGUMENTS:
#
#   wls
#   logical flag. If TRUE, the cross sectional regressions are computed in two steps: 
#   (1) Once via OLS; (2) Again via WLS using the residuals of the OLS regressions to 
#   determine weights.  If FALSE,  the cross sectional regressions are computed via
#   OLS only.
#
# reg
#   character string.  If "robust", then cross sectional regressions are fit with "lmRob".
#   If "classic", then cross sectional regressions are fit with "lm".
#
# cov
#   character string.  If "robust", the covariance matrix for the factor returns is computed 
#   with "covRob". Similarly, if "full.resid.cov" is TRUE, the covariance matrix of the residuals 
#   is computed with "covRob", and if "full.resid.cov" is FALSE, the diagonal elements of the 
#   (diagonal) covariance matrix of the residuals are computed with "scale.tau".  If "classic", 
#   these covariance matrices are computed with "cov" and "var" (in the case that "full.resid.cov" 
#   is FALSE).
#
# full.resid.cov
#   logical flag. If TRUE, the final covariance matrix of the assets is computed using
#   the full estimated covariance matrix of the residuals.  If FALSE, the final covariance
#   matrix of the assets is computed by assuming a diagonal covariance matrix for the
#   residuals.
#
# robust.scale
#   logical flag. If TRUE, the fundamental data supplied by the user (as the "data"
#   argument) is standardized using "location.m" and "scale.tau". If FALSE, the
#   standardization is done with "mean" and "stdev".
#
# VALUE:
# an list suitable for coercion to an object of class "fm" or "fmRob", comprised of the following elements:
#   1. Cov.returns. A "list" object whose essential element is the covariance matrix of the assets.
#   2. Cov.facrets. An object of class "cov" or "covRob" which contains the covariance matrix of the 
#      factor returns (including intercept).
#   3. Cov.resids. An object of class "cov" or "covRob" which contains the covariance matrix of the 
#      residuals, if "full.resid.cov" is TRUE.  NULL if "full.resid.cov" is FALSE.
#   4. resid.vars. A vector of variances estimated from the OLS residuals for each asset. If "wls" is 
#      TRUE, these are the weights used in the weighted least squares regressions.  If "cov = robust"
#      these values are computed with "scale.tau".  Otherwise they are computed with "var".
#   5. facrets. A "timeSeries" object containing the times series of estimated factor returns and 
#      intercepts.
#   6. resids. A "timeSeries" object containing the time series of residuals for each asset.
#   7. tstats. A "timeSeries" object containing the time series of t-statistics for each exposure.
#   8. returns.data. A "data.frame" object containing the returns data for the assets in the factor model.
#   9. exposure.data. A "data.frame" object containing the data for the variables in the factor model.
#   10. assets. A character vector of the ID's for the assets in the factor model.
#   11. tickers. A character vector of the labels for the assets in the factor model.
#   12. call. The function call.
#
fmfit <- function(fmdsobj, timedates, exposures, assets, wls=T, regression="robust", covariance="robust", 
  full.resid.cov=F, robust.scale=F, returnsvar="RETURN", verbose=F){

  # input checking
  if ( !is(fmdsobj, "fmds") )
    stop("First argument must come from a class derived from 'fmds'.")
    
  if ( !is(timedates, "Date") )
    stop("timedates argument must be of class Date.")
  if ( length(timedates) < 2 )
    stop("At least two time points, t and t-1, are needed for fitting the factor model.") 
    
  # cgg: 4/20/05 annoying bug in is.vector---named atomic vectors 
  # don't pass whatever is.vector uses for a test
  # use is(x,"vector") instead
  if ( !is(exposures,"vector") || !is.character(exposures) )
    stop("exposure argument invalid---must be character vector.")
  if ( !is(assets,"vector")    || !is.character(assets) )
    stop("assets argument invalid---must be character vector.")

  wls            <- as.logical(wls)
  full.resid.cov <- as.logical(full.resid.cov)
  robust.scale   <- as.logical(robust.scale)

  # 6/20/05 this is cheating, assumes an fmdsCube object
  # very ugly hack, will fix later
  datevar  <- fmdsobj@index$datevar
  assetvar <- fmdsobj@index$assetvar
  
  if ( !match(regression, c("robust","classic"),F) ) stop("regression must one of 'robust', 'classic'.")
  if ( !match(covariance, c("robust","classic"),F) ) stop("covariance must one of 'robust', 'classic'.")
  
  this.call <- match.call()
#  this.call <- "call"

    # only used for memory tuning
    if ( verbose ) {
        cat("Running fmfit on ",date(),"\n")
        cat("\ttime at start: ",round(proc.time(),3)," ")
        cat("mem usage(MB):",memsize(),"\n")
        on.exit(print(mem.tally.report("Max allocation upon exiting fmfit: ")))
        print(storageSummary())
        cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n\n")
  }

  # cgg 4/29/05 some exposures have mixed case names, so for now leave this out
  # users must now enter the exposures with the correct case
  # exposures <- casefold(exposures, upper=T)

  # we need returns for various calculations, but the user should not specify the return variable
  # as an exposure
  if ( match(returnsvar, exposures, F) ) stop(paste(returnsvar,"cannot be used as an exposure."))

  if ( verbose  )
      cat("\tBefore first subset...start time: ",round(proc.time(),3),
      " mem usage(MB):",memsize(),"\n")
 

  # subset down to desired assets, dates, and exposures
  fmdsobj <- subset(fmdsobj, assetsToRetrieve=assets, verbose=verbose,
    datesToRetrieve=list("start"=timedates[1], "end"=timedates[length(timedates)]),
    variablesToRetrieve=c(returnsvar,exposures))
    
  if ( verbose  )
      cat("\tAfter first subset...start time: ",round(proc.time(),3),
      " mem usage(MB):",memsize(),"\n")

  # adjust dates
  timedates <- as.Date(getAvailableTimes(fmdsobj))

  # drop any asset that has missing data for any time point
  # 5/4/05 cgg later we'll want to remove this restriction
  #fmdsobj <- na.screen(fmdsobj, c(returnsvar,exposures), verbose=verbose)

  if ( verbose  )
      cat("\tAfter na.screen...start time: ",round(proc.time(),3),
      " mem usage(MB):",memsize(),"\n")

  # update our list of asset labels
  assets <- getAssetIDs(fmdsobj)
  
  # 6/23/05 tickers
  tickers <-  unlist(getAssetLabels(fmdsobj))
  # getAssetLabels always has names
  tickers <- tickers[order(names(tickers))]
  
  # check these are in the same order
  if ( any(names(tickers) != assets) ) stop("assets and tickers don't match, debug")
  # named objects can be trouble
  names(tickers) <- NULL
  
  # separate out returns
  # cgg: 5/2/05 original code was wrong, regressed returns at time t on exposures at time t
  # introduce this offset here by subsetting one time unit ahead
  returnsobj    <- subset(fmdsobj, variablesToRetrieve=returnsvar, verbose=verbose,
    datesToRetrieve=list("start"=timedates[2],"end"=timedates[length(timedates)]))
    
  # need to adjust the dates in the returns object to match those on the exposure 
  # data so that the by code will work properly
  #
  # at time t, we regress returns from t-1 to t on exposures at t-1, so we label this 
  # point time t
  #
  # will take care of this below, after the flatten, so that the change doesn't depend
  # on the representation
  if ( verbose  )       
    cat("\tAfter subset to returns...start time: ",round(proc.time(),3),
        " mem usage(MB):",memsize(),"\n")   

  # yj: 5/2/2005 can combine these two steps
  # drop returns variable and scale numeric factors
  fmdsobj <- scaleNumericFactors(subset(fmdsobj, variablesToDrop=returnsvar, verbose=verbose, 
    datesToRetrieve=list("start"=timedates[1],"end"=timedates[length(timedates)-1])), 
    robust.location=robust.scale, robust.scale=robust.scale)

  if ( verbose  ) 
      cat("\tAfter drop returns and scale...start time: ",round(proc.time(),3),
        " mem usage(MB):",memsize(),"\n")
    

  numTimePoints <- length(getAvailableTimes(fmdsobj))
  numExposures  <- length(exposures)
  numAssets     <- length(assets)

  # flatten data sets to data frames, then rejoin returns data with factor data
  # slightly faster to put the calls to flatten inside the call to merge, but that
  # uses more memory
  # this provides a uniform way of treating different fmds objects for now
  # later, iterators
  flat.factors <- flatten(fmdsobj   ,along="date")
  flat.returns <- flatten(returnsobj,along="date")

  # fix up dates now
  flat.factors[[datevar]] <- as.character(timedates[match(as.character(flat.factors[[datevar]]),as.character(timedates),0)+1])

  # rejoin
  fulldata     <- merge(flat.returns,flat.factors, by=c(assetvar,datevar), all=T)

  # make assetvar an ordered factor
  #ordered(fulldata[[assetvar]]) <- sort(levels(fulldata[[assetvar]]))
  fulldata[[datevar]] <- as.Date(fulldata[[datevar]])

  # sort by date, then permno
  fulldata <- fulldata[order(fulldata[[datevar]],fulldata[[assetvar]]),]

  # adjust row names to correspond to permnos (easier to keep track on things later)
  # allow duplicated row names for faster computations
  #attr(fulldata, "dup.row.names") <- T
  #row.names(fulldata) <- as.character(fulldata[[assetvar]])

  # 6/20/05 if user only wants the cross sections stop here
  #if ( cross.sections.only ) {
  #  output <- fulldata
  #} else {
    # build a terms object to represent the model
    # will use with different data frames to build model matrices
  
    # cgg 4/11/05 need to separate numeric and factor variables
    which.numeric     <- sapply(fulldata[,exposures,drop=F],is.numeric)
    exposures.numeric <- exposures[ which.numeric]
    exposures.factor  <- exposures[!which.numeric]

    # cgg 4/12/05 how to handle multiple factor variables? identifiability problem...
    if ( length(exposures.factor) > 1 ) 
      stop("Only one nonnumeric variable can be used at this time.")
      
    regression.formula <- paste("~",paste(exposures,collapse="+"))
    # are there any nonnumeric exposures, if so we remove the intercept term
    if ( length(exposures.factor) ) {
      regression.formula <- paste(regression.formula,"- 1")
    
      # GDY test 2011-05-18
      fulldata[,exposures.factor] <- as.factor(fulldata[,exposures.factor])

      exposuresToRecode <- names(fulldata[,exposures,drop=F])[!which.numeric]
      contrasts.list <- lapply(seq(length(exposuresToRecode)), function(i) function(n,m) contr.treatment(n,contrasts=F) )
      names(contrasts.list) <- exposuresToRecode
    } else {
      contrasts.list <- NULL
    }
    

    if ( verbose  )
      cat("\tbefore x-sectional regressions...start time: ",round(proc.time(),3),
        " mem usage(MB):",memsize(),"\n")

    ######################################################################
    #
    # cross-sectional regression code
    #
    ######################################################################

   regression.formula <- eval(parse(text=paste(returnsvar,regression.formula))) 

    # perform a regression at each time index
    # the inner function stacks things in this order
    # first number is the number of coefficients
    # next block  are coefficients of the fit
    # next  block are the t-stats (same length as #coefficients)
    # rest are residuals of the fit

  ols.robust <- function(xdf, modelterms, conlist) {
    if( length(exposures.factor) ) {
    zz <- xdf[[exposures.factor]]
    xdf[[exposures.factor]] <- if ( is.ordered(zz) ) 
      ordered(zz,levels=sort(unique.default(zz))) else 
      factor(zz)
    }
    model <- lmRob(modelterms, data=xdf, contrasts=conlist,
      control=lmRob.control(mxr=200,mxf=200,mxs=200))
      
    sdest <- sqrt(diag(model$cov))            
    names(sdest) <- names(model$coef)
    coefnames <- names(model$coef)
    alphaord  <- order(coefnames)
    model$coef <- model$coef[alphaord]
    sdest      <- sdest[alphaord]
    c(length(model$coef), model$coef, model$coef/sdest, model$resid)
  }
            
  ols.classic <- function(xdf, modelterms, conlist) {
    model <- try(lm(formula=modelterms,data=xdf,contrasts=conlist,singular.ok=F))
    if ( is(model,"Error") ) {
      mess <- get.message(model,even.if.used=T) 
      nn   <- regexpr("computed fit is singular",mess)
      if ( nn > 0 ) {
        # strip off the "Problem in lm.fit.qr(..." stuff
        cat("At time:",substring(mess,nn),"\n")
        # refit with singular turned on
        model <- lm(formula=modelterms,data=xdf,contrasts=conlist,singular.ok=T)
      }
      else 
        stop(mess)
    }
    tstat <- rep(NA,length(model$coef))
    tstat[!is.na(model$coef)] <- summary(model,cor=F)$coef[,3] 
    alphaord <- order(names(model$coef))
    c(length(model$coef), model$coef[alphaord], tstat[alphaord], model$resid)
    }
    
  wls.robust <-function(xdf, modelterms, conlist, w) {
    # cgg 5/19/05 this gets around a bug in lmRob in S-PLUS 7
      assign("w",w,pos=1)
    
    if( length(exposures.factor) ) {
    zz <- xdf[[exposures.factor]]
    xdf[[exposures.factor]] <- if ( is.ordered(zz) ) 
      ordered(zz,levels=sort(unique.default(zz))) else 
      factor(zz)
    }

    model <- lmRob(modelterms, data=xdf, weights=w, contrasts=conlist,
      control=lmRob.control(mxr=200,mxf=200,mxs=200))  
    sdest <- sqrt(diag(model$cov))            
    names(sdest) <- names(model$coef)
    coefnames <- names(model$coef)
    alphaord  <- order(coefnames)
    model$coef <- model$coef[alphaord]
    sdest <- sdest[alphaord]
    c(length(model$coef), model$coef, model$coef/sdest, model$resid)
    }
            
  wls.classic <- function(xdf, modelterms, conlist, w) {
      assign("w",w,pos=1)
    model <- try(lm(formula=modelterms,data=xdf,contrasts=conlist,weights=w,singular.ok=F))
    if ( is(model,"Error") ) {
      mess <- get.message(model,even.if.used=T) 
      nn   <- regexpr("computed fit is singular",mess)
      if ( nn > 0 ) {
        # strip off the "Problem in lm.fit.qr(..." stuff
        cat("At time:",substring(mess,nn),"\n")
        # refit with singular turned on
        model <- lm(formula=modelterms,data=xdf,contrasts=conlist,weights=w)
      }
      else 
        stop(mess)
    }
    tstat <- rep(NA,length(model$coef))
    tstat[!is.na(model$coef)] <- summary(model,cor=F)$coef[,3] 
    alphaord <- order(names(model$coef))
    c(length(model$coef), model$coef[alphaord],tstat[alphaord], model$resid)
    }


    if (!wls) {
      # stand alone OLS cross-sectional regressions
      if (regression == "robust") {
        # robust
        FE.hat <- by(data=fulldata, INDICES=as.numeric(fulldata[[datevar]]),
          FUN=ols.robust, modelterms=regression.formula, conlist=contrasts.list)
      } else {
        # classic
        FE.hat <- by(data=fulldata, INDICES=as.numeric(fulldata[[datevar]]), 
          FUN=ols.classic, modelterms=regression.formula, conlist=contrasts.list)
      }
    } else {
      ## OLS and WLS cross-sectional regressions

      # first compute weights
      if (regression == "robust") {
        E.hat <- by(data=fulldata, INDICES=as.numeric(fulldata[[datevar]]),
            FUN=function(xdf, modelterms, conlist) {
              lmRob(modelterms, data=xdf, contrasts=conlist,
              control=lmRob.control(mxr=200,mxf=200,mxs=200))$resid
            }, 
          modelterms=regression.formula, conlist=contrasts.list)

        # convert to matrix with columns corresponding to time
        E.hat <- apply(E.hat,1,unlist)
        # weight estimates are the variances of the residuals (over time) for each asset
        weights <- if (covariance == "robust") apply(E.hat,1,scaleTau2)^2 else apply(E.hat,1,var)

        FE.hat <- by(data=fulldata, INDICES=as.numeric(fulldata[[datevar]]),
            FUN=wls.robust, modelterms=regression.formula, conlist=contrasts.list, w=weights)

      } else {
        E.hat <- by(data=fulldata, INDICES=as.numeric(fulldata[[datevar]]), 
            FUN=function(xdf, modelterms, conlist) {
              lm(formula=modelterms,data=xdf,contrasts=conlist,singular.ok=T)$resid
            },
            modelterms=regression.formula, conlist=contrasts.list)
            
        # convert to matrix with columns corresponding to time
        E.hat <- apply(E.hat,1,unlist)
        # weight estimates are the variances of the residuals (over time) for each asset
        weights <- if (covariance == "robust") apply(E.hat,1,scaleTau2)^2 else apply(E.hat,1,var)

        FE.hat <- by(data=fulldata, INDICES=as.numeric(fulldata[[datevar]]), 
            FUN=wls.classic, modelterms=regression.formula, conlist=contrasts.list, w=weights)
       }
  
    } # end of regression branches

    if ( verbose  )
      cat("\tafter x-sectional regressions...start time: ",round(proc.time(),3), 
        " mem usage(MB):",memsize(),"\n")

    # result of by is an array of mode list
    # cgg 5/4/05 may contain missing values if the fits resulted in missing values (usually 
    # happens when one level of a categorical variable does not appear in a time slice
    # with the fixes made above, we should not run into the problem of list elements with 
    # differing lengths

    # create FE.hat.mat matrix 
    if ( length(exposures.factor) ) {
      numCoefs <- length(exposures.numeric) + length(levels(fulldata[,exposures.factor]))
      ncols <- 1 + 2*numCoefs + numAssets
      fnames <- c(exposures.numeric,paste(exposures.factor,levels(fulldata[,exposures.factor]),sep=""))
      cnames <- c("numCoefs",fnames,paste("t",fnames,sep="."),assets)
    } else {
      numCoefs <- 1 + length(exposures.numeric)
      ncols <- 1 + 2*numCoefs + numAssets
      cnames <- c("numCoefs","(Intercept)",exposures.numeric,paste("t",c("(Intercept)",exposures.numeric),sep="."),assets)
    }
    FE.hat.mat <- matrix(NA,ncol=ncols,nrow=numTimePoints,dimnames=list(as.character(as.Date(as.numeric(names(FE.hat)))),cnames))
    for(i in 1:length(FE.hat))
    {
      names(FE.hat[[i]])[1] <- "numCoefs"
      nc <- FE.hat[[i]][1]
      names(FE.hat[[i]])[(2+nc):(1+2*nc)] <- paste("t",names(FE.hat[[i]])[2:(1+nc)],sep=".")
      if( length(FE.hat[[i]]) != (1+2*nc+numAssets) )
        stop(paste("bad count in row",i,"of FE.hat"))
      names(FE.hat[[i]])[(2+2*nc):(1+2*nc+numAssets)] <- assets
      idx <- match(names(FE.hat[[i]]),colnames(FE.hat.mat))
      FE.hat.mat[i,idx] <- FE.hat[[i]]
    }
    
    timedates <- as.Date(as.numeric(dimnames(FE.hat)[[1]]),origin="1970-01-01")
   
    # coefs
    coefs.names <- colnames(FE.hat.mat)[2:(1+numCoefs)]
    F.hat <- zoo(x=FE.hat.mat[,2:(1+numCoefs)],order.by=timedates)
    # outlier detection
    gomat <- apply(coredata(F.hat), 2, 
      function(x) abs(x - median(x,na.rm=T)) > 4*mad(x,na.rm=T))
  
    if ( any(gomat, na.rm=T) & verbose ) {
      #if ( interactive() ) {
      # guiCreate("Report",Name="Outliers from Factor Model Fit")       
      #}
      cat("\n\n*** Possible outliers found in the factor returns:\n\n")
      for ( i in which(apply(gomat,1,any,na.rm=T)) )
        print(F.hat[i,gomat[i,],drop=F])
    }
      
    # t-statistics
    tstats <- zoo(x=FE.hat.mat[,(2+nc):(1+2*nc)],order.by=timedates)

    # residuals 
    E.hat <- zoo(x=FE.hat.mat[,(2+2*numCoefs):(1+2*numCoefs+numAssets)],order.by=timedates)
    # 6/26/05 adjustment to use tickers for assets (so plots will be labeled by tickers)
    colnames(E.hat) <- tickers
        
    if (covariance == "robust") {
      # cgg: 5/04/05 will turn off distances in facrets calculation for now,
      # they don't work with missing data
      # cgg: 4/16/05 eval/substitute gets around a bug in cov, where the frame containing F.hat is
      # not passed along?
      # cgg: 6/23 there is a bug in covRob when the input matrix is very ill-conditioned 
      # and estim is pairwiseGK
      if ( kappa(na.exclude(coredata(F.hat))) < 1e10 ) {
        Cov.facrets <- eval(substitute(covRob(nnn, estim="pairwiseGK",distance=F, na.action=na.omit), list(nnn=coredata(F.hat))))
      } else {
        cat("Covariance matrix of factor returns is singular.\n")
        Cov.facrets <- eval(substitute(covRob(nnn, 
            distance=F, na.action=na.omit), list(nnn=coredata(F.hat))))
      }
  
      resid.vars <- apply(coredata(E.hat),2,scaleTau2,na.rm=T)^2

      D.hat <- if(full.resid.cov) covOGK(coredata(E.hat), sigmamu = scaleTau2, n.iter=1) else diag(resid.vars)

    } else {
      # cgg: 5/04/05 will turn off distances in facrets calculation for now,
      # they don't work with missing data
      # cgg: 4/16/05 this gets around a bug in cov, where the frame containing F.hat is
      # not passed along?
      Cov.facrets <- eval(substitute(ccov(nnn, distance=F, na.action=na.omit),list(nnn=coredata(F.hat))))
      resid.vars <- apply(coredata(E.hat),2,var,na.rm=T)
      D.hat <- if(full.resid.cov) ccov(coredata(E.hat), distance=F, na.action=na.omit) else diag(resid.vars)
    }
  
  
    
    ## covariance matrix of returns
    # get factor returns for last time period
    #print(numAssets)
    B.final <- matrix(0,nrow=numAssets,ncol=numCoefs)
    colnames <- coefs.names #dimnames(FE.hat)[[1]][1:numCoefs]
    # was there an intercept?
    B.final[,match("(Intercept)",colnames,0)] <- 1
    # numeric exposures
    numeric.columns <- match(exposures.numeric,colnames,0)
    #print(fulldata[[assetvar]][as.numeric(fulldata[[datevar]])==timedates[numTimePoints]])
    #print(numTimePoints)
    #print(timedates)
    #print(dim(B.final))
    #print(exposures.numeric)
    B.final[,numeric.columns] <-
      as.matrix(fulldata[as.numeric(fulldata[[datevar]])==timedates[numTimePoints], exposures.numeric])
    # factor exposures
    # for each factor exposure, find all related columns (regression forms names like this: variablelevel)
    # would have to alter this if we can have more than one nonnumeric exposures
    if ( length(exposures.factor) )
    B.final[,grep(exposures.factor,x=colnames)][cbind(seq(numAssets),
      as.numeric(fulldata[fulldata[[datevar]]==timedates[numTimePoints], exposures.factor]))] <- 1
  #print(dim(B.final))
  #print(dim(Cov.facrets$cov))
  #print(dim(D.hat$cov))
  # Sigma.R = Beta %*% Sigma.F %*% t(Beta) + Sigma.epsilon
    cov.returns <- B.final %*% Cov.facrets$cov %*% t(B.final) + if (full.resid.cov) D.hat$cov else D.hat
    dimnames(cov.returns) <- list(tickers, tickers)
  
   
    Cov.returns <- list(cov=cov.returns, 
      center=tapply(fulldata[[returnsvar]],fulldata[[assetvar]],mean), 
      evals=eigen(cov.returns, only.values=T, symmetric=T)$values,
      corr=F)

    ## full covariance matrix of residuals
    if(full.resid.cov) {
      Cov.resids <- D.hat
      dimnames(Cov.resids$cov) <- list(tickers, tickers)
    } else {
      Cov.resids <- NULL
    }
    
    ## output list suitable for coercion to class fm or fmRob 
    output <- list("Cov.returns" = Cov.returns, "Cov.facrets" = Cov.facrets,
         "Cov.resids" = Cov.resids, "resid.vars" = diag(D.hat),
         "facrets" = F.hat, "resids" = E.hat, "tstats" = tstats,
         "returns.data" = fulldata[,c(datevar,assetvar,returnsvar)],
         "exposure.data" = fulldata[,c(datevar,assetvar,exposures)],
         "assets" = assets, "tickers" = tickers, "call" = this.call)

  #} # if cross.sections.only is FALSE
}
