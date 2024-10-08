
# syntax for saturared model from variable names
generate_saturated_syntax <- function(variables) {
  lavaan_code <- ""
  for (i in seq_along(variables)) {
    lavaan_code <- paste(lavaan_code, paste(variables[i], "~~", paste(variables[i:length(variables)], collapse = " + ")))
    if (i < length(variables)) {
      lavaan_code <- paste(lavaan_code, "\n")
    }
  }
  return(lavaan_code)
}


# combines two syntaxes into multilevel syntax
combineTwoLevelSyntax <- function(level1, level2){
  
  twoLevelSyntax <- paste0('level: 1 \n\n',
                           level1,
                           '\n\n\n level: 2 \n\n',
                           level2)
  
  cat(rep("-", 20), " \n SHOW MODEL SYNTAX\n", rep("-", 20), "\n", twoLevelSyntax)
  return(twoLevelSyntax)
}


# ESEM Syntax across vector of factor names
make_ESEM_Syntax <- function(model, factors, referents, paramLetters, between = FALSE, std = TRUE, covMat = NULL, oneZeroAnchors = FALSE){
  
  SyntaxOut <- ""
  
  for(i in 1:length(factors)){
    
    SyntaxOut <- paste0(SyntaxOut,
                        make_ESEM_Syntax_single(model=model, 
                                                factor=factors[i], 
                                                referents=referents, 
                                                paramLetter=paramLetters[i], 
                                                between = between, 
                                                std = std, 
                                                covMat = covMat, 
                                                oneZeroAnchors = oneZeroAnchors),
                        "\n\n"
    )
    
  }
  
  return(SyntaxOut)
  
}

# ESEM Syntax core function for "make_ESEM_syntax" for one factor only
make_ESEM_Syntax_single <- function(model, factor, referents, paramLetter, between = FALSE, std = TRUE, covMat = NULL, oneZeroAnchors = FALSE){
  
  if(std == FALSE){
    if(is.null(covMat)){warning("Need to provide covariance matrix to obtain unstandardized weights")}
    sdDiag <- sqrt(diag(covMat))
    sdMat <- matrix(diag(sdDiag), ncol=20)
    row.names(sdMat) <- row.names(model$loadings)
    model$loadings <- sdMat %*% model$loadings
    
  }
  
  # referent loadings (option to use 1 for main and 0 for sideloadings)
  referentInd <- row.names(model$loadings) %in% unlist(referents)
  referentLoadings <- round(model$loadings[referentInd, factor],3)
  if(oneZeroAnchors == TRUE){
    referentLoadings <- ifelse(referentLoadings == max(abs(referentLoadings)), 1, 0)
    if(sum(referentLoadings) != 1){warning(paste0("Binary referent loadings for factor '", factor, "' do not sum up to 1!"))}
  }
  referentPaste <- paste0(referentLoadings, "*", names(referentLoadings), collapse = " + ")
  
  # starting values
  startValues <- round(model$loadings[!referentInd, factor],3)
  startValuesPaste <- paste0("start(", startValues, ")*", names(startValues), collapse = " + ")
  
  #parameters
  paramsPaste <- paste0(paramLetter, 1:length(startValues), "*", names(startValues), collapse = " + ")
  
  #if between factor
  if(between==TRUE){factor <- paste0(factor, "b")}
  
  # combine
  if(oneZeroAnchors==TRUE){
    outPaste <- paste0(factor, " =~ ", referentPaste, " + ", paramsPaste)
  }else{
    outPaste <- paste0(factor, " =~ ", referentPaste," + ",startValuesPaste," + ", paramsPaste)
  }
  
  return(outPaste)
}


EFAmodelSyntax <- function(nfactors, withinBetween=c("within","between"), varNames){
  
  withinBetweenInd <- match.arg(withinBetween, c("within","between"))
  facSyntax <- paste0(rep(paste0("efa('", withinBetweenInd, "')"), 4), "*F", withinBetweenInd, c(1:nfactors), collapse = " + ")
  varSyntax <- paste0(varNames, collapse = " + ")
  syntaxOut <- paste0(facSyntax, " =~ ", varSyntax)  
  return(syntaxOut)
}


