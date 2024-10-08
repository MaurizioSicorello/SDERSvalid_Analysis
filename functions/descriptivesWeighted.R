# helper function for weighted stats from multilevel model
descriptivesWeighted <- function(data = dfSDERS_ID, x){
  
  f <- as.formula(paste(x, "~1"))
  nullTwoLevel <- lme(f,
                      random = list(PARTICIPANT_ID =~ 1), data = data,
                      method = "REML")
  
  meanOut <- round(mean(data[,x]),2)
  sdBetween <- round(as.numeric(nlme::VarCorr(nullTwoLevel)[1,2]), 2)
  sdWithin <- round(as.numeric(nlme::VarCorr(nullTwoLevel)[2,2]), 2)
  rangeOUt <- paste0(min(data[,x]), "-",max(data[,x]))
  
  dfOut <- c(meanOut, sdBetween, sdWithin, rangeOUt)
  return(dfOut)
  
}
