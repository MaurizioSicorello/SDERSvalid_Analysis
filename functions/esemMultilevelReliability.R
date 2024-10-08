
# Helper Function: Extracts the relevant matrices from lavaan model for specified items
# Careful: only works when cluster variable is named "PARTICIPANT_ID"
extractRelComps <- function(model, items){
  
  modelImpl_within = sum(inspect(model, what="cov.ov")$within[c(items), c(items)])
  error_within = sum(inspect(model, what="est")$within$theta[c(items), c(items)])
  predicted_within = sum(modelImpl_within-error_within)
  
  modelImpl_between = sum(inspect(model, what="cov.ov")$PARTICIPANT_ID[c(items), c(items)])
  error_between = sum(inspect(model, what="est")$PARTICIPANT_ID$theta[c(items), c(items)])
  predicted_between = sum(modelImpl_between-error_between)
  
  modelImpl_total = sum(modelImpl_within+modelImpl_between)
  
  clusterSize = inspect(model, what="average.cluster.size")
  
  outVec <- setNames(
    c(modelImpl_within, error_within, predicted_within, 
      modelImpl_between, error_between, predicted_between,
      modelImpl_total, clusterSize), 
    c("modelImpl_within", "error_within", "predicted_within", 
      "modelImpl_between", "error_between", "predicted_between",
      "modelImpl_total", "clusterSize"))
  
  return(outVec)
  
}


# Calculates latent-state-trait components
LSTcustom_core <- function(model, items){
  
  RelComps <- extractRelComps(model, items)
  
  commonTrait = RelComps["predicted_between"]/RelComps["modelImpl_total"]
  uniqueTrait = RelComps["error_between"]/RelComps["modelImpl_total"]
  commonState = RelComps["predicted_within"]/RelComps["modelImpl_total"]
  uniqueState = RelComps["error_within"]/RelComps["modelImpl_total"]
  
  return(round(
    data.frame(commonTrait = commonTrait, 
                    uniqueTrait = uniqueTrait,
                    commonState = commonState,
                    uniqueState = uniqueState,
                    row.names = NULL), 
    2)
  )
  
}


# wrapper that calculates LST decomposition for all composites, including total score
LSTcustom <- function(model, item_factor_assignment){
  
  nFac <- length(item_factor_assignment)
  matOut <- as.data.frame(
    matrix(
      nrow=4,
      ncol=(nFac+1),
      dimnames = list(c("commonTrait", "uniqueTrait", "commonState", "uniqueState"),
                      c(names(item_factor_assignment), "Total")
      )
    )
  )
  
  for(i in 1:nFac){
    matOut[,i] <- t(
      as.matrix(
        LSTcustom_core(model=model, items=item_factor_assignment[[i]])
      )
    )
  }
  
  matOut[,i+1] <- t(
    as.matrix(
      LSTcustom_core(model=model, items=unlist(item_factor_assignment))
    )
  )
  
  return(matOut)
  
}





# Calculates reliability following Lai (2020)
ESEMlaiRelcustom_core <- function(model, items, clusterSize = NULL){
  
  RelComps <- extractRelComps(model, items)
  
  if(!is.null(clusterSize)){RelComps["clusterSize"] <- clusterSize}
  
  omega2L <- (RelComps["predicted_within"] + RelComps["predicted_between"])/RelComps["modelImpl_total"]
  omegaWithin <- RelComps["predicted_within"]/RelComps["modelImpl_within"]
  omegaBetween <- RelComps["predicted_between"]/(RelComps["modelImpl_between"]+RelComps["modelImpl_within"]/RelComps["clusterSize"])
  
  return(round(
    data.frame(omega2L = omega2L, 
               omegaWithin = omegaWithin, 
               omegaBetween = omegaBetween, 
               row.names = NULL),
    2)
  )
} 

# wrapper to calculate reliability for all composites and the total score
ESEMlaiRelcustom <- function(model, item_factor_assignment, clusterSize = NULL){
  
  nFac <- length(item_factor_assignment)
  matOut <- as.data.frame(
    matrix(
      nrow=3,
      ncol=(nFac+1),
      dimnames = list(c("omega2L", "omegaWithin", "omegaBetween"),
                        c(names(item_factor_assignment), "Total")
      )
    )
  )
  
  for(i in 1:nFac){
    matOut[,i] <- t(
      as.matrix(
        ESEMlaiRelcustom_core(model=model, items=item_factor_assignment[[i]], clusterSize=clusterSize)
        )
    )
    }
  
  matOut[,i+1] <- t(
    as.matrix(
      ESEMlaiRelcustom_core(model=model, items=unlist(item_factor_assignment), clusterSize=clusterSize)
    )
  )
  
  return(matOut)
  
}


