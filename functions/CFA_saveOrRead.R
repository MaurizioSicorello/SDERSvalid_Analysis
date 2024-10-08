# wrapper of lavaan.cfa which only performs CFA if it is not found in a given folder. 
CFA_saveOrRead <- function(model, data, cluster, std.lv = FALSE, estimator, rotation = NULL, target=NULL, filename=NULL, fullPath=NULL){
  
  # file path
  if(!is.null(fullPath)){
    filePath <- fullPath
  }else{
    filenameExt <- paste0(filename, ".rds")
    filePath <- here::here("manuscripts", "SDERSvalid_dailyLife", "models", filenameExt)
  }
  
  
  if(!file.exists(filePath)){
    cat("Model not in folder. Calculating now...\n")
    
    if(is.null(rotation)){
      cfaFitTemp <- cfa(model=model, data=data, cluster=cluster, std.lv=std.lv, estimator=estimator)
    }else{
      if(is.null(target)){
        cfaFitTemp <- cfa(model=model, data=data, cluster=cluster, std.lv=std.lv, estimator=estimator, rotation=rotation)
      }else{
        cfaFitTemp <- cfa(model=model, data=data, cluster=cluster, std.lv=std.lv, estimator=estimator, rotation=rotation, rotation.args = list(target))
      }
    }
    
    saveRDS(cfaFitTemp, file = filePath)
    
  }else{
    cat("Loading model from folder \n")
    cfaFitTemp <- readRDS(file = filePath)
  }
  
  return(cfaFitTemp)
  
}
