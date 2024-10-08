# the reliability was calculated rom the lavaan model. Previous functions to estimate omega have issues
# with ESEM models, when they are not defined as hierarchical or bifactor models. For example,
# the function compRelSEM of the semTools-Package works well for our CFA model, because it knows which
# Factor belongs to which item, as there are not sideloadings. For the ESEM with sideloadings, the
# function estimates how much variance in the sum score of *all* items is uniquely due to a specific factor.
# In turn, the omega functionality of the psych package assumes a bifactor model, which is not the appropriate
# CFA model in our application. Therefore, a custom function was build that calculates for a specified set of items: 
# (a) the variance of the sum score attributable to a specific factor ("Reliability_Specific") using the formula of the compRelSEM-Function
# (b) the variance of the sum score attributable to all factors and their intercorrelation as "1-(sum(error_variances)/total_variance)"
# This function yields identical results for the CFA model to compRelSEM and more valid results for the ESEM model

# The function takes as arguments: 
# 1) A lavaan model
# 2) A named list with unique assignments of items to subscales. Example:

# main_loadings_list <-   list(
#   NonAccept  = c("S.DERS8_BL", "S.DERS4_BL", "S.DERS1_BL", "S.DERS5_BL", "S.DERS12_BL", "S.DERS20_BL", "S.DERS18_BL"),
#   Modulate = c("S.DERS13_BL", "S.DERS17_BL", "S.DERS10_BL", "S.DERS3_BL", "S.DERS15_BL", "S.DERS21_BL", "S.DERS9_BL"),
#   Awareness = c("S.DERS6_BL.r", "S.DERS11_BL.r", "S.DERS2_BL.r", "S.DERS19_BL.r", "S.DERS16_BL.r"),
#   Clarity = c("S.DERS14_BL", "S.DERS7_BL")
# )

# WARNING: When addapting this function to new datasets, it's important to check that the column order of 
# "inspect(model, what="std")$lambda" matches the order of composites in "main_loadings_list"



# Problem: the error variance and "sum(colSums(loadingsMat)^2)" only sum up to the variance of the composite
# expected based on the implied covariance matrix, not the empirical covariance matrix. This is less of 
# a problem for a better fitting model.


########### MAIN FUNCTION ########### 
omegaCustom <- function(model, main_loadings_list){
  
  nFactors <- length(main_loadings_list)
  
  # Total Scale Reliability
  loadingsMat <- inspect(model, what="std")$lambda
  varTotal <- sum(lavInspect(model, what = "cor.ov"))
  errTotal <- sum(diag(lavInspect(model, "std")$theta))
  ReliabilityTotal <- round(1-(errTotal/varTotal), 2)
  
  # Unique factor contributions to Total Scale Reliability
  FactorContr <- colSums(loadingsMat)^2/varTotal
  FactorContr <- round(rbind(FactorContr, FactorContr/sum(FactorContr)),2)
  dimnames(FactorContr) <- list(c("Percent Unique Variance in Total Score: ", "Relative between Factors: "),
                             names(main_loadings_list))   
  
  # Subscale Reliabilities
  outdf <- matrix(nrow=3, ncol=nFactors,
                  dimnames = list(c("Specific", "Total", "Ratio"), paste("Factor", c(1:nFactors)))
                  )
  for(i in 1:nFactors){
    outdf[,i] <- omegaCustomHelp(model = model, ItemNames = main_loadings_list[[i]], FactorNum = i)
    colnames(outdf)[i] <- names(main_loadings_list)[i]
  }
 
  return(list("Reliability Total Score" = ReliabilityTotal,
              "Scale contributions to Total Score" = FactorContr,
              "Subscale Reliabilities" = outdf))
  
}


########### HELPER FUNCTION ###########
omegaCustomHelp <- function(model, ItemNames, FactorNum){
  
  # Variance of Composite (using model implied correlation matrix)
  Denominator <- sum(lavInspect(model, what = "cor.ov")[ItemNames, ItemNames])
  
  # sum loadings and square them
  loadingsMat <- inspect(model, what="std")$lambda[ItemNames, FactorNum]
  Numerator_S <- sum(loadingsMat)^2
  
  # error variance in composite
  compError <- sum(diag(lavInspect(model, "std")$theta)[ItemNames])
  
  # calculate indices
  Reliability_Specific <- round(Numerator_S/Denominator, 2)
  Reliability_Total <- round(1-(compError/Denominator), 2)
  Reliability_Ratio <- round(Reliability_Specific/Reliability_Total, 2)
  
  return(c(Reliability_Specific, Reliability_Total,Reliability_Ratio))
}





################################DEPRECATED###################################

# DEPRECATED
# omegaCustom <- function(model, ItemNames, FactorNum){
#   
#   # Variance of Composite
#   Denominator <- sum(lavCor(model)[ItemNames, ItemNames])
#   
#   # sums loadings and squares them
#   loadingsMat <- inspect(model, what="std")$lambda[ItemNames, FactorNum]
#   if(length(FactorNum) > 1){
#     Numerator_S <- sum(colSums(loadingsMat)^2)
#   }else if(length(FactorNum) == 1){
#     Numerator_S <- sum(loadingsMat)^2
#   }else{
#     warning("Invalid Argument for FactorNum")
#   }
#   
#   # error variance in composite
#   compError <- sum(diag(lavInspect(model, "std")$theta)[ItemNames])
#   
#   # calculate indices
#   Reliability_Specific <- round(Numerator_S/Denominator, 2)
#   Reliability_Total <- round(1-(compError/Denominator), 2)
#   Reliability_Ratio <- round(Reliability_Specific/Reliability_Total, 2)
#   
#   return(
#     list("Reliability_Specific" = Reliability_Specific, 
#          "Reliability_Total" = Reliability_Total,
#          "Reliability_Ratio" = Reliability_Ratio)
#   )
# }
