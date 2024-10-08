# for future tasks: automatically sorting the matrices within FactorValidity. Also: make it useful for 
# psych::pa objects

FactorValidityCustom <- function(model, item_factor_assignment, cluster = "PARTICIPANT_ID", correlation = FALSE){
  
  withinCluster <- FactorValidity(loadings = inspect(model, what="std")$within$lambda,
                                  factorCorrelations = inspect(model, what="std")$within$psi,
                                  item_correlation_matrix = inspect(model, what="cor.ov")$within,
                                  item_factor_assignment = item_factor_assignment,
                                  correlation=correlation)
  
  betweenCluster <- FactorValidity(loadings = inspect(model, what="std")[[cluster]]$lambda,
                                  factorCorrelations = inspect(model, what="std")[[cluster]]$psi,
                                  item_correlation_matrix = inspect(model, what="cor.ov")[[cluster]],
                                  item_factor_assignment = item_factor_assignment,
                                  correlation=correlation)
  
  return(list("within"=withinCluster, "between"=betweenCluster))
  
  
}



FactorValidity <- function(loadings, factorCorrelations, item_correlation_matrix, item_factor_assignment, correlation = FALSE){
  
  #some warning messages
  if(ncol(loadings)*ncol(factorCorrelations)*length(item_factor_assignment) != length(item_factor_assignment)^3){
    warning("Number of factors do not match between loadings, correlations, and assignment")
  }
  if(nrow(loadings) != nrow(item_correlation_matrix)){
    warning("number of items does not match between loadings and item correlation matrix")
  }
  if(any(colnames(loadings) != names(item_factor_assignment))){
    warning("Factors of loading matrix and item-factor-assignment do not match. Check names (including order!)")
  }
  if(any(colnames(loadings) != colnames(factorCorrelations))){
    warning("Factors of loading matrix and correlation matrix do not match. Check names (including order!)")
  }
  if(any(colnames(factorCorrelations) != names(item_factor_assignment))){
    warning("Factors of loading matrix and item-factor-assignment do not match. Check names (including order!)")
  }
  
  # create matrix of loading sums
  numFactors <- length(item_factor_assignment)
  matLoadingSums <- matrix(nrow=numFactors, ncol=numFactors)
  compositeSDs <- numeric(4)
  for(i in 1:numFactors){
    compositeSDs[i] <- sqrt(sum(item_correlation_matrix[item_factor_assignment[[i]], item_factor_assignment[[i]]]))
    loadingsTemp <- loadings[item_factor_assignment[[i]], ]
    if(is.null(nrow(loadingsTemp))){
      matLoadingSums[i, ] <- loadingsTemp
    }else{
      matLoadingSums[i, ] <- colSums(loadingsTemp)  
    }
    
  }
  
  # create output matrix
  matOut <- matLoadingSums %*% factorCorrelations
  matOut <- diag(1/compositeSDs) %*% matOut
  dimnames(matOut) <- list(paste0("Composite: ", names(item_factor_assignment)), 
                           paste0("Factor: ", names(item_factor_assignment)))
  
  if(correlation==FALSE){matOut <- matOut^2}
  
  return(round(matOut, 2))
  
}
