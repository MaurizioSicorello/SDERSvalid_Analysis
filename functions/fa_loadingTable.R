

fa_loadingTable <- function(x, cut, structure = TRUE){
  
  #get sorted loadings
  modelLoad <- round(fa.sort(x)$loadings, 2)
  #supress loadings
  cutInd <- modelLoad < cut
  modelLoad[cutInd] <- ""
  modelLoad <- as.data.frame(unclass(modelLoad))
  #add structure coefficients
  modelStruct <- round(fa.sort(x)$Structure, 2)
  modelStruct[cutInd] <- ""
  modelStruct <- as.data.frame(unclass(modelStruct))
  # combine loadings and structure coefficients, if requested
  if(structure == TRUE){
    for (i in 1:nrow(modelLoad)) {
      for (j in 1:ncol(modelLoad)) {
        modelLoad[i, j] <- paste0(modelLoad[i, j], " (", modelStruct[i, j], ")")
      }
    }
  }
  
  addInfo <- round(data.frame("h2" = fa.sort(x)$communality, "complexity" = fa.sort(x)$complexity), 2)
  
  
  modelLoad <- merge(modelLoad, addInfo, by = "row.names", sort = FALSE)
  return(modelLoad)
}
