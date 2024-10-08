# main function to simulate a whole vector of true models
flexFitCustom <- function(data, nfactorsTrue, nfactorsTest, iterations, fitMes = c("srmr", "rmsea", "cfi", "tli"), Nmanual = NULL){
  
  nModels <- length(nfactorsTrue)
  
  if(nModels == 1){
    
    Out <- flexFitCore(data=data, nfactorsTrue=nfactorsTrue, nfactorsTest=nfactorsTest, iterations=iterations, fitMes=fitMes)
    
  }else{
    
    Out <- vector(mode = "list", length = nModels)
    
    for(i in 1:nModels){
      
      Out[[i]] <- flexFitCore(data=data, nfactorsTrue=nfactorsTrue[i], nfactorsTest=nfactorsTest, iterations=iterations, fitMes=fitMes, Nmanual = Nmanual)
      names(Out)[i] <- paste0(nfactorsTrue[i], "factorsTrueModel")
    }
  }
  return(Out)
}


# core function which fits only one true model
flexFitCore <- function(data, nfactorsTrue, nfactorsTest, iterations, fitMes = c("srmr", "rmsea", "cfi", "tli"), Nmanual = NULL){
  
  model <- fitESEMpromax(data, nfactors = nfactorsTrue)
  
  # sample size
  if(is.numeric(Nmanual)){
    N = Nmanual
  }else{
    N = lavInspect(model, what = "nobs")
  }
  
  # parameters of target model
  parTargetModel <- partable(model)
  
  dfout <- as.data.frame(
    matrix(NA, nrow = iterations, ncol = length(fitMes),
                  dimnames = list(NULL, fitMes))
  )
  
  for(i in 1:iterations){
    
    dat <- simulateData(parTargetModel, sample.nobs = N)
    dfout[i,] <- fitMeasures(fitESEMpromax(dat, nfactors = nfactorsTest), fitMes)
    tryCatch({

      # simulate data
      dat <- simulateData(parTargetModel, sample.nobs = N)
      #dat <- mvrnorm(n = N, mu = rep(0, nrow(lavInspect(model, what = "cor.ov"))), Sigma = as.matrix(lavInspect(model, what = "cor.ov")))
      dfout[i,] <- fitMeasures(fitESEMpromax(dat, nfactors = nfactorsTest), fitMes)

      # make NA if warning or error and continue
      }, warning = function(w){
      dfout[i,] <- NA
    }, error = function(e){
      dfout[i,] <- NA
      next
    })
    
  }
  
  return(dfout)
}


# helper function to conduct ESEM
fitESEMpromax <- function(data, nfactors){
  
  esem_efaPromax <- psych::fa(data, nfactors=nfactors, fm="pa", rotate="promax", residuals = TRUE)
  esem_efaPromax <- esem_syntax(esem_efaPromax)
  esem_efaPromax <- cfa(model=esem_efaPromax, data=dfSDERS, std.lv=TRUE, estimator = "ML")
  return(esem_efaPromax)
  
}


# helpful function to find a good tradeoff in sensitivity and specificty when strict errors (e.g. 5%) do not distinguish distributions
findThresh <- function(start, stop, by, FitNull, FitAlt, fitMeasure, fitType){
  
  thresholds <- seq(start, stop, by)
  
  FitNull <- FitNull[,fitMeasure]
  FitAlt <- FitAlt[,fitMeasure]
  
  outdf <- as.data.frame(
    matrix(nrow=length(thresholds), ncol=3,
           dimnames = list(NULL, c("thresholds", "FitNull", "FitAlt")))
  )
  outdf[,1] <- thresholds
  
  if(fitType == "BoF"){
    for(i in 1:length(thresholds)){ outdf[i, c(2:3)] <- c(1-percRank(FitNull, thresholds[i]), percRank(FitAlt, thresholds[i]))}
  }else if(fitType == "GoF"){
    for(i in 1:length(thresholds)){ outdf[i, c(2:3)] <- c(percRank(FitNull, thresholds[i]), 1-percRank(FitAlt, thresholds[i]))}
  }else{
    warning("Invalid argument for 'fittype'. Musst be 'GoF' [goodness of fit] or 'BoF' [fadness of fit]")
  }
  
  return(outdf)
  
}


# helper function for percentile rank
percRank <- function(x, value){
  x_new <- x[!is.na(x)]
  sum(x_new <= value, na.rm=TRUE)/length(x_new)
  
}









