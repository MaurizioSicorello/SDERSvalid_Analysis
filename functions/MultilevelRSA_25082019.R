
## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
##   Main functions for average surface
## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

MLRSA_AverageSurface_FixedEffects <- function(bs,vs) {

   # Computes the RSA parameters 

   a1   <- (bs[1]+bs[2])
   v.a1 <- (vs[1,1]+vs[2,2]+2*vs[1,2])
   z1   <- a1/sqrt(v.a1)
   p1   <- 2*pnorm(-abs(z1))

   a2   <- (bs[3]+bs[4]+bs[5])
   v.a2 <- (vs[3,3]+vs[4,4]+vs[5,5]+2*(vs[3,4]+vs[3,5]+vs[4,5]))
   z2   <- a2/sqrt(v.a2)
   p2   <- 2*pnorm(-abs(z2))
   
   a3   <- (bs[1]-bs[2])
   v.a3 <- (vs[1,1]+vs[2,2]-2*vs[1,2])
   z3   <- a3/sqrt(v.a3)
   p3   <- 2*pnorm(-abs(z3))
   
   a4   <- (bs[3]-bs[4]+bs[5])
   v.a4 <- (vs[3,3]+vs[4,4]+vs[5,5]-2*(vs[3,4]-vs[3,5]+vs[4,5]))
   z4   <- a4/sqrt(v.a4)
   p4   <- 2*pnorm(-abs(z4))
   
   a5   <- (bs[3]-bs[5])
   v.a5 <- (vs[3,3]+vs[5,5]-2*vs[3,5])
   z5   <- a5/sqrt(v.a5)
   p5   <- 2*pnorm(-abs(z5))
     
   out <- data.frame(Parameter=c("a1","a2","a3","a4","a5"),
                     Value=c(a1,a2,a3,a4,a5),
                     SEs=c(sqrt(v.a1),sqrt(v.a2),sqrt(v.a3),sqrt(v.a4),sqrt(v.a5)),
                     z=round(c(z1,z2,z3,z4,z5),3),
                     p=round(c(p1,p2,p3,p4,p5),3))
   out

}

MLRSA_AverageSurface_RandomVariance <- function(model,name_vars,random_vars) {

   # Computes the variance of the RSA parameters
   # Get the random-effects covariance matrix
   vs = MLRSA_Utils_RandomEffectMatrix(model,name_vars,random_vars)

   # Compute the Level 2 random variance parameters
   v.a1 <- (vs[1,1]+vs[2,2]+2*vs[1,2])
   v.a2 <- (vs[3,3]+vs[4,4]+vs[5,5]+2*(vs[3,4]+vs[3,5]+vs[4,5]))
   v.a3 <- (vs[1,1]+vs[2,2]-2*vs[1,2])
   v.a4 <- (vs[3,3]+vs[4,4]+vs[5,5]-2*(vs[3,4]-vs[3,5]+vs[4,5]))
   v.a5 <- (vs[3,3]+vs[5,5]-2*vs[3,5])
   tmp.v <- round(c(v.a1,v.a2,v.a3,v.a4,v.a5),3)
   
   tmp.na <- which(is.na(random_vars))   
   tmp.v[tmp.na] <- NA
   return(tmp.v)

}

MLRSA_AverageSurface <- function(model,name_vars=NULL,random_vars=NULL) {

      ## model = a lme4 object that was fitted to the data
      ## name_vars = name of the variable in the following order: 
      ##             1. the first predictor variable
      ##             2. the second predictor variable
      ##             3. square term of the first predictor
      ##             4. interaction
      ##             5. square term of the second predictor
      
      # check for empty fixed-effects covariance matrix
      if(length(vcov(model)) == 0) {
        stop("ERROR: No Fixed-effects covariance matrix.")
      }
      
      # check for empty fixed-effects vector
      if(length(fixef(model)) == 0) {
        stop("ERROR: No Fixed-effects.")
      }
      
      # Step 1: we extract the correct rows and columns of the
      #         fixed-effects covariance matrix and the fixed-effects vector 
      rowsandcols = which(vcov(model)@Dimnames[[1]] %in% name_vars)
      positions_b = which(attributes(fixef(model))$names %in% name_vars)
      v <- vcov(model)[rowsandcols,rowsandcols]
      b <- fixef(model)[positions_b]
      
      # Step 2: now we compute the Basis RSA parameters
      res <- MLRSA_AverageSurface_FixedEffects(b,v)
      
      # Step 3: compute variability of the RSA parameters (if possible)
      if ( sum(is.na(random_vars)) != 5L ) {
        res$L2_Variance = MLRSA_AverageSurface_RandomVariance(model,name_vars,random_vars)
      } else { res$L2_Variance <- NA }
      
      return(res)

}

## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
##     Main functions for pick-a-point
## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

MLRSA_Utils_RandomEffectMatrix <- function(model,name_vars,random_vars) {

   # Get the random-effects covariance matrix
   # nC = length(random_vars)-sum(is.na(random_vars))+1
   # v = VarCorr(model)[[1]][1:nC,1:nC]
   # rowsandcols = which(attributes(v)$dimnames[[1]] %in% random_vars)
   # v = v[rowsandcols,rowsandcols] 
   tmp.idx <- which(!is.na(random_vars))
   v <- as.matrix( VarCorr(model)[[1]][ tmp.idx + 1, tmp.idx + 1 ] )
  
   #tmp.idx = which(random_vars %in% name_vars)
   vs <- matrix(0,5,5)
   for (i in 1:length(tmp.idx)) {
      for (j in 1:length(tmp.idx)) {
       vs[tmp.idx[i],tmp.idx[j]] <- v[i,j]
      }
   }
   
   return(vs)
   
}

MLRSA_Compute_PickaPoint <- function(bs,vs,g) {

   # Computes the RSA parameters for specific values g
   
   a1   <- (bs[1]+bs[2])+(bs[6]+bs[7])*g
   v.a1 <- g^2*(vs[6,6]+vs[7,7]+2*vs[6,7])+2*g*(vs[1,6]+vs[1,7]+vs[2,6]+vs[2,7])+(vs[1,1]+vs[2,2]+2*vs[1,2])
   z1   <- a1/sqrt(v.a1)
   p1   <- 2*pnorm(-abs(z1))

   a2   <- (bs[3]+bs[4]+bs[5])+(bs[8]+bs[9]+bs[10])*g
   v.a2 <- g^2*(vs[8,8]+vs[9,9]+vs[10,10]+2*(vs[8,9]+vs[8,10]+vs[9,10]))+
           2*g*(vs[3,8]+vs[3,9]+vs[3,10]+vs[4,8]+vs[4,9]+vs[4,10]+vs[5,8]+vs[5,9]+vs[5,10])+
           (vs[3,3]+vs[4,4]+vs[5,5]+2*(vs[3,4]+vs[3,5]+vs[4,5]))
   z2   <- a2/sqrt(v.a2)
   p2   <- 2*pnorm(-abs(z2))
   
   a3   <- (bs[1]-bs[2])+(bs[6]-bs[7])*g
   v.a3 <- g^2*(vs[6,6]+vs[7,7]-2*vs[6,7])+2*g*(vs[1,6]-vs[1,7]-vs[2,6]+vs[2,7])+(vs[1,1]+vs[2,2]-2*vs[1,2])
   z3   <- a3/sqrt(v.a3)
   p3   <- 2*pnorm(-abs(z3))
   
   a4   <- (bs[3]-bs[4]+bs[5])+(bs[8]-bs[9]+bs[10])*g
   v.a4 <- g^2*(vs[8,8]+vs[9,9]+vs[10,10]-2*(vs[8,9]-vs[8,10]+vs[9,10]))+
           2*g*(vs[3,8]-vs[3,9]+vs[3,10]-vs[4,8]+vs[4,9]-vs[4,10]+vs[5,8]-vs[5,9]+vs[5,10])+
           (vs[3,3]+vs[4,4]+vs[5,5]-2*(vs[3,4]-vs[3,5]+vs[4,5]))
   z4   <- a4/sqrt(v.a4)
   p4   <- 2*pnorm(-abs(z4))
   
   a5   <- (bs[3]-bs[5])+(bs[8]-bs[10])*g
   v.a5 <- g^2*(vs[8,8]+vs[10,10]-2*vs[8,10])+
           2*g*(vs[3,8]-vs[3,10]-vs[5,8]+vs[5,10])+
           (vs[3,3]+vs[5,5]-2*vs[3,5])
   z5   <- a5/sqrt(v.a5)
   p5   <- 2*pnorm(-abs(z5))
     
   out <- data.frame(Parameter=c("a1","a2","a3","a4","a5"),
                     Coefs=c(a1,a2,a3,a4,a5),
                     SEs=c(sqrt(v.a1),sqrt(v.a2),sqrt(v.a3),sqrt(v.a4),sqrt(v.a5)),
                     z=c(z1,z2,z3,z4,z5),
                     p=round(c(p1,p2,p3,p4,p5),3))
   return(out)

}

## Some useful functions:

MLRSA_PickaPoint <- function(model,name_vars=NULL,name_cov=NULL,value_cov=0) {

   ## Computes the RSA parameters for specific of the covariate
   ## name_vars = name of the variable in the following order: 
   ##             1. the first predictor variable
   ##             2. the second predictor variable
   ##             3. square term of the first predictor
   ##             4. interaction
   ##             5. square term of the second predictor
   ## name_cov = name of covariate
   
   ## check for empty fixed-effects covariance matrix
   if(length(vcov(model)) == 0) {
      stop("ERROR: No Fixed-effects covariance matrix.")
   }
      
   # check for empty fixed-effects vector
   if(length(fixef(model)) == 0) {
      stop("ERROR: No Fixed-effects.")
   }
   
   # check for number of covariates
   if(length(name_cov)!=1) {
      stop("ERROR: Too many or too number of covariates.")
   }
   
   ## Step 1: we extract the correct rows and columns of the
   ##         fixed-effects covariance matrix and the fixed-effects vector 
   name_vars = c(name_vars,paste(name_cov,":",name_vars,sep=""))
   rowsandcols = which(vcov(model)@Dimnames[[1]] %in% name_vars)
   positions_b = which(attributes(fixef(model))$names %in% name_vars)
   v <- vcov(model)[rowsandcols,rowsandcols]
   b <- fixef(model)[positions_b]
   
   # Step 2: now we compute the Basis RSA parameters
   res <- MLRSA_Compute_PickaPoint(b,v,value_cov)
   return(res)
   
} 

## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
##      Main functions region of congruence
## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

MLRSA_Utils_Roots_QuadFunction <- function(a,b,c) {

    # Computes the roots of quadratic functions
    sqrt_term = b^2 - 4*a*c
    if (sqrt_term >= 0) {
       pos_root <- ((-b) + sqrt(sqrt_term)) / (2*a)
       neg_root <- ((-b) - sqrt(sqrt_term)) / (2*a)
    } else {
       pos_root = neg_root = NA
    } 
    out <- sort(c(pos_root,neg_root))
    return(out)
    
}

MLRSA_Compute_Region <- function(bs,vs,z) {

    ## Compute the g values for significance of the 
    ## conditional RSA parameters
    
    z2 <- z^2

    ## Compute roots a1
    cq <- z2*(vs[1,1]+vs[2,2]+2*vs[1,2])-(bs[1]+bs[2])^2
    bsq <- 2*z2*(vs[1,6]+vs[1,7]+vs[2,6]+vs[2,7])-2*(bs[1]+bs[2])*(bs[6]+bs[7])
    aq <- z2*(vs[6,6]+vs[7,7]+2*vs[6,7])-(bs[6]+bs[7])^2
    r.a1 = MLRSA_Utils_Roots_QuadFunction(aq,bsq,cq)

    ## Compute roots a2
    cq <- (bs[3]+bs[4]+bs[5])^2 - z2*(vs[3,3]+vs[4,4]+vs[5,5]+2*(vs[3,4]+vs[3,5]+vs[4,5]))
    bsq <- 2*(bs[3]+bs[4]+bs[5])*(bs[8]+bs[9]+bs[10])-2*z2*(vs[3,8]+vs[3,9]+vs[3,10]+vs[4,8]+vs[4,9]+vs[4,10]+vs[5,8]+vs[5,9]+vs[5,10])
    aq <- (bs[8]+bs[9]+bs[10])^2 - z2*(vs[8,8]+vs[9,9]+vs[10,10]+2*(vs[8,9]+vs[8,10]+vs[9,10]))
    r.a2 = MLRSA_Utils_Roots_QuadFunction(aq,bsq,cq)

    ## Compute roots a3
    cq <- (bs[1]-bs[2])^2 - z2*(vs[1,1]+vs[2,2]-2*vs[1,2])
    bsq <- 2*(bs[1]-bs[2])*(bs[6]-bs[7])-2*z2*(vs[1,6]-vs[1,7]-vs[2,6]+vs[2,7])
    aq <- (bs[6]-bs[7])^2 - z2*(vs[6,6]+vs[7,7]-2*vs[6,7])
    r.a3 = MLRSA_Utils_Roots_QuadFunction(aq,bsq,cq)

    ## Compute roots a4
    cq <- (bs[3]-bs[4]+bs[5])^2 - z2*(vs[3,3]+vs[4,4]+vs[5,5]-2*(vs[3,4]-vs[3,5]+vs[4,5]))
    bsq <- 2*(bs[3]-bs[4]+bs[5])*(bs[8]-bs[9]+bs[10])-2*z2*(vs[3,8]-vs[3,9]+vs[3,10]-vs[4,8]+vs[4,9]-vs[4,10]+vs[5,8]-vs[5,9]+vs[5,10])
    aq <- (bs[8]-bs[9]+bs[10])^2 - z2*(vs[8,8]+vs[9,9]+vs[10,10]-2*(vs[8,9]-vs[8,10]+vs[9,10]))
    r.a4 = MLRSA_Utils_Roots_QuadFunction(aq,bsq,cq)

    ## Compute roots a5
    cq <- (bs[3]-bs[5])^2-z2*(vs[3,3]+vs[5,5]-2*vs[3,5])
    bsq <- 2*(bs[3]-bs[5])*(bs[8]-bs[10])-2*z2*(vs[3,8]-vs[3,10]-vs[5,8]+vs[5,10])
    aq <- (bs[8]-bs[10])^2-z2*(vs[8,8]+vs[10,10]-2*vs[8,10])
    r.a5 = MLRSA_Utils_Roots_QuadFunction(aq,bsq,cq)

    out <- data.frame(Parameter=c("a1","a2","a3","a4","a5"),
                      LowValue=c(r.a1[1],r.a2[1],r.a3[1],r.a4[1],r.a5[1]),
                      HighValue=c(r.a1[2],r.a2[2],r.a3[2],r.a4[2],r.a5[2]))
    return(out)

}

MLRSA_Compute_RegionIntervalsParameter <- function(parm,lroot,hroot,
                                                   model,
                                                   name_vars,
                                                   name_cov,
                                                   z) {

    ## Compute the intervals of negative significance, nonsignificance and 
    ## positive significance for a specific RSA parameter 
    
    negsig = nonsig = possig = NA
    
    ## there are two basic cases: if lroot = hroot = NA, than the RSA parameter 
    ## is negative significant, nonsignificant or positive significant for all
    ## values of g:
    if (sum(is.na(c(lroot,hroot))) == 2) {
       tmp.res = MLRSA_PickaPoint(model,name_vars=name_vars,name_cov=name_cov,value_cov=0)
       tmp.dec <- tmp.res[tmp.res$Parameter==parm,]$z[1]
       if (round(tmp.dec,2) <= -z) { negsig <- "]-inf,inf[" 
       } else if (round(tmp.dec,2) >= z) { possig <- "]-inf,inf[" 
       } else { nonsig <- "]-inf,inf[" }   
    } 
    
    if (sum(is.na(c(lroot,hroot))) == 0) {
       # Step 1: Determine the root value for the computation
       tmp.root <- (hroot+lroot)/2
       # Step 2: Use MLRSA_PickaPoint to determine significance of tmp.root
       tmp.res <- MLRSA_PickaPoint(model,name_vars=name_vars,name_cov=name_cov,value_cov=tmp.root) 
       # Step 3: Is the parameter significant?
       tmp.dec <- tmp.res[tmp.res$Parameter==parm,]$z[1]
       # Step 4: Now, we compute the intervals
       lroot <- round(lroot,4)
       hroot <- round(hroot,4)
       if (round(tmp.dec,2) > -z & round(tmp.dec,2) < z ) {
          negsig = paste("]-inf,",lroot,"[",sep="")
          nonsig = paste("[",lroot,",",hroot,"]",sep="")
          possig = paste("]",hroot,",inf[",sep="")    
       } else if (round(tmp.dec,2) <= -z) {
          negsig = paste("[",lroot,",",hroot,"]",sep="")
          nonsig = paste("]-inf,",lroot,"[ and ]",hroot,",inf[",sep="")
       } else if (round(tmp.dec,2) >= z) {
          nonsig = paste("]-inf,",lroot,"[ and ]",hroot,",inf[",sep="")
          possig = paste("[",lroot,",",hroot,"]",sep="")
       }
    }
    
    out <- c(negsig,nonsig,possig)
    return(out)

}

MLRSA_Region <- function(model,name_vars=NULL,name_cov=NULL,z=1.94) {

   ## Computes the RSA parameters for specific of the covariate
   ## name_vars = name of the variable in the following order: 
   ##             1. the first predictor variable
   ##             2. the second predictor variable
   ##             3. square term of the first predictor
   ##             4. interaction
   ##             5. square term of the second predictor
   ## name_cov = name of covariate
   
   ## check for empty fixed-effects covariance matrix
   if(length(vcov(model)) == 0) {
      stop("ERROR: No Fixed-effects covariance matrix.")
   }
      
   # check for empty fixed-effects vector
   if(length(fixef(model)) == 0) {
      stop("ERROR: No Fixed-effects.")
   }

   ## Step 1: we extract the correct rows and columns of the
   ##         fixed-effects covariance matrix and the fixed-effects vector 
   name_vars = c(name_vars,paste(name_cov,":",name_vars,sep=""))
   rowsandcols = which(vcov(model)@Dimnames[[1]] %in% name_vars)
   positions_b = which(attributes(fixef(model))$names %in% name_vars)
   v <- vcov(model)[rowsandcols,rowsandcols]
   b <- fixef(model)[positions_b] 

   ## Step 2: lower and higher values of the region
   res <- MLRSA_Compute_Region(b,v,z)
   ## Step 3: compute the respective intervals
   res$NegSignificance <- "NA"
   res$NonSignificance <- "NA"
   res$PosSignificance <- "NA"
   for (i in 1:nrow(res)) {
       tmp <- MLRSA_Compute_RegionIntervalsParameter(res$Parameter[i],
              res$LowValue[i],res$HighValue[i],                                         
              model=model,name_vars=name_vars,name_cov=name_cov,z=z)
       res$NegSignificance[i] <- tmp[1]
       res$NonSignificance[i] <- tmp[2]
       res$PosSignificance[i] <- tmp[3]
   }
   return(res)

}

## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
##    Helper functions for Plot
## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

MLRSA_AverageSurfacePlot <- function(model,
                                     name_vars=NULL,
                                     outcome=NULL,
                                     data,
                                     xlab="x", ylab="y", zlab="z",
                                     param=TRUE,
                                     main="") {

     ## Plots the average surface in ML-RSA
     ## Step 0: Some checks
     if(length(fixef(model)) == 0) {
        stop("ERROR: No Fixed-effects.")
     }
     
     if(length(outcome) != 1) {
        stop("ERROR: Too many or too few outcome variables")
     }
     
     if(nrow(data) == 0) {
        stop("ERROR: No data?")
     }
     
     ## Step 1: Get the fixed effects coefficients
     b0 <- fixef(model)["(Intercept)"]
     b1 <- fixef(model)[name_vars[1]]
     b2 <- fixef(model)[name_vars[2]]
     b3 <- fixef(model)[name_vars[3]]
     b4 <- fixef(model)[name_vars[4]]
     b5 <- fixef(model)[name_vars[5]]
  
     ## Step 2: find reasonable axis range for the predictors and the outcome
     x <- data[,c(name_vars[1])]
     y <- data[,c(name_vars[2])]
     z <- data[,c(outcome)]
     x_range <- max(c(abs(min(x, na.rm=T)), abs(max(x, na.rm=T))))
     y_range <- max(c(abs(min(y, na.rm=T)), abs(max(y, na.rm=T))))
     common_range <- max(c(x_range, y_range))
     z_range <- max(c(abs(min(z, na.rm=T)), abs(max(z, na.rm=T))))
     
     xlim = c(-common_range, common_range)
     ylim = c(-common_range, common_range)
     zlim = c(-z_range,z_range) # sinnvoll festlegen
  
     # Step 3: build plot of specified model
     plot <- plotRSA(b0=b0, x=b1, y=b2, x2=b3, xy=b4, y2=b5,
                     type="3d",
                     main=main, 
                     xlim=xlim, ylim=ylim, zlim=zlim, 
                     xlab=xlab, ylab=ylab, zlab=zlab,
                     cex.axesLabel=1.3, 
                     axes=c("LOC","LOIC","PA1"), 
                     project=c("LOC","LOIC","PA1","contour"), 
                     param=param,
                     legend=FALSE,
                     points=list(data=data[,c(name_vars[1],name_vars[2],outcome)], 
                     color=rgb(1, 1, 0, 0)),
                     hull=TRUE)
      # show plot
      print(plot)
     
}

MLRSA_PickaPointPlot <- function(model,
                                 name_vars=NULL,
                                 outcome=NULL,
                                 name_cov="g.s",value_cov=1,
                                 data,
                                 xlab="x", ylab="y", zlab="z",
                                 param=TRUE,
                                 main="") {

     ## Plots the average surface in ML-RSA
     ## Step 0: Some checks
     if(length(fixef(model)) == 0) {
        stop("ERROR: No Fixed-effects.")
     }
     
     if(length(outcome) != 1) {
        stop("ERROR: Too many or too few outcome variables")
     }
     
     if(length(name_cov) != 1) {
        stop("ERROR: Too many or too few Level 2 predictors")
     }
     
     if(nrow(data) == 0) {
        stop("ERROR: No data?")
     }
     
     ## Step 1: Get the fixed effects coefficients
     name_vars = c(name_vars,paste(name_cov,":",name_vars,sep=""))
     b0 <- fixef(model)["(Intercept)"]
     b1 <- fixef(model)[name_vars[1]]+fixef(model)[name_vars[6]]*value_cov
     b2 <- fixef(model)[name_vars[2]]+fixef(model)[name_vars[7]]*value_cov
     b3 <- fixef(model)[name_vars[3]]+fixef(model)[name_vars[8]]*value_cov
     b4 <- fixef(model)[name_vars[4]]+fixef(model)[name_vars[9]]*value_cov
     b5 <- fixef(model)[name_vars[5]]+fixef(model)[name_vars[10]]*value_cov
  
     ## Step 2: find reasonable axis range for the predictors and the outcome
     x <- data[,c(name_vars[1])]
     y <- data[,c(name_vars[2])]
     z <- data[,c(outcome)]
     x_range <- max(c(abs(min(x, na.rm=T)), abs(max(x, na.rm=T))))
     y_range <- max(c(abs(min(y, na.rm=T)), abs(max(y, na.rm=T))))
     common_range <- max(c(x_range, y_range))
     z_range <- max(c(abs(min(z, na.rm=T)), abs(max(z, na.rm=T))))
     
     xlim = c(-common_range, common_range)
     ylim = c(-common_range, common_range)
     zlim = c(-z_range,z_range) # sinnvoll festlegen
  
     # Step 3: build plot of specified model    
     plot <- plotRSA(b0=b0, x=b1, y=b2, x2=b3, xy=b4, y2=b5,
                     type="3d",
                     main=main,
                     xlim=xlim, ylim=ylim, zlim=zlim, 
                     xlab=xlab, ylab=ylab, zlab=zlab,
                     cex.axesLabel=1.3, 
                     axes=c("LOC","LOIC","PA1"), 
                     project=c("LOC","LOIC","PA1","contour"), 
                     param=param,
                     legend=FALSE,
                     points=list(data=data[,c(name_vars[1],name_vars[2],outcome)], 
                     color=rgb(1, 1, 0, 0)),
                     hull=TRUE)
      # show plot
      print(plot)
     
}