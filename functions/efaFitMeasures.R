EFAfitMeasures <- function(x){
  
  getFit <- c(x$rms, 
              x$RMSEA["RMSEA"], 
              ((x$null.chisq-x$null.dof)-(x$STATISTIC-x$dof))/(x$null.chisq-x$null.dof), # CFI, Scource: https://gist.github.com/tonosan/cb7581f3459ae7c4217a
              x$TLI,
              x$STATISTIC, 
              x$dof, 
              x$PVAL)
  
  getFit[1:4] <- round(getFit[1:4], 2)
  getFit[5] <- round(getFit[5], 1)
  getFit[7] <- round(getFit[7], 3)
  names(getFit) <- c("srmr", "rmsea", "cfi", "tli", "chi_squared", "df", "p-value")
  return(getFit)
  
}
