#' Muthen Decomp
#'
#' Takes a DF and clustering variables and returns a list including decomposed covariance matrices
#'
#' Details to be written
#'
#' @param l1Var
#' @param l2Var
#' @param dat dataframe
#'
#' @export
# -----------------------------------------------------------------------------

# credit script to Francis L. Huang
# Edited for my purposes
# working paper on mcfa in R with Lavaan
mcfa.input<-function(l1Var, l2Var, dat){
  dat1    <- dat[complete.cases(dat),]
  g       <- dat1[,l2Var]            #grouping
  freq    <- data.frame(table(g))
  dat2    <- dat1[,!names(dat1) %in% l2Var] #select all but l2 vars
  G       <- length(table(g))
  n       <- nrow(dat2)
  k       <- ncol(dat2)
  scaling <- (n^2-sum(freq$Freq^2)) / (n*(G-1))
  varn    <- names(dat2)
  ms      <- matrix(0,n,k)
  # it appears as if the 'tibble' format might be causing issues
  dat2 <- as.data.frame(dat2)
  for (i in 1:k){
    ms[,i]<-ave(dat2[,i],g)
  }
  cs           <- dat2-ms #deviation matrix, centered scores
  colnames(ms) <- colnames(cs)<-varn
  b.cov        <- (cov(ms) * (n - 1))/(G-1) #group level cov matrix
  w.cov        <- (cov(cs) * (n - 1))/(n-G) #individual level cov matrix
  pb.cov       <- (b.cov-w.cov)/scaling #estimate of pure/adjusted between cov matrix
  w.cor        <- cov2cor(w.cov) #individual level cor matrix
  b.cor        <- cov2cor(b.cov) #group level cor matrix
  pb.cor       <- cov2cor(pb.cov) #estimate of pure between cor matrix
  icc          <- round(diag(pb.cov)/(diag(w.cov)+diag(pb.cov)),3) #iccs
  return(list(b.cov=b.cov,pw.cov=w.cov,ab.cov=pb.cov,pw.cor=w.cor,
              b.cor=b.cor,ab.cor=pb.cor,
              n=n,G=G,c.=scaling,sqc=sqrt(scaling),
              icc=icc,dfw=n-G,dfb=G) )
}