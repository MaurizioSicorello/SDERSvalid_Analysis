# source for formula: https://personality-project.org/r/psych/HowTo/factor.pdf

reliabilityEFACustom <- function(x, forItems = NULL, empCor = FALSE){
  
  # names of scale items (total score if NULL)
  if(is.null(forItems)){
    itemNames <- row.names(x$loadings)
  }else{
    itemNames <- forItems
  }
  
  # save loadings and uniqueness
  EFAloadings <- unclass(x$loadings)[itemNames, ]
  UniqueVec <- x$uniquenesses[itemNames]
  
  # calculate total variance in composite
  if(empCor == TRUE){
    varTot <- sum(x$r[itemNames,itemNames])
  }else{
    varTot <- sum((EFAloadings %*% x$Phi %*% t(EFAloadings)) + diag(UniqueVec))
  }
  
  # return reliability
  rel <- 1 - sum(UniqueVec)/varTot
  return(round(rel, 2))
}





