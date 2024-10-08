#############################################################################################
# Settings

# define which sample should be used for factor analyses
# one of: c("full", "pt1", "pt2")
sample <- "pt2"

#############################################################################################

library("here")
library("nlme")
library("psych")
library("plyr")
library("stringr")
library("flextable")
library("corrplot")
library("lavaan")
library("esem")
library("esemComp")
library("misty")
library("semTools")
library("nonnest2")
library("reshape2")
library("ggplot2")
library("caret")

df <- read.csv(here::here("data", "SDERSvalid_DailyLife_data_preprocessed.csv"))

source(here::here("functions", "descriptivesWeighted.r"))
source(here::here("functions", "muthenDecomposition.r"))
source(here::here("functions", "multilevelSyntaxHelper.r"))
source(here::here("functions", "efaFitMeasures.r"))
source(here::here("functions", "CFA_saveOrRead.r"))
source(here::here("functions", "esemMultilevelReliability.r"))
source(here::here("functions", "factorValidity.r"))



######################################################################
# full data or split-half


if(sample != "full"){
  
  dfsummarized <- ddply(df, .(PARTICIPANT_ID), summarize, SDERS_pm = mean(S.DERS_Total_EMAmean))
  set.seed(3000)
  holdOutIndex <- createDataPartition(dfsummarized$SDERS_pm, p = .5, list = FALSE, times = 1)
  dfholdOut_IDs <- dfsummarized[holdOutIndex, 1]
  
  if(sample == "pt1"){
    df <- df[!(df$PARTICIPANT_ID %in% dfholdOut_IDs), ]
  }else if(sample == "pt2"){
    df <- df[df$PARTICIPANT_ID %in% dfholdOut_IDs, ]
  }else{
    error("Invalid argument for which sample to use")
  }
  
}



######################################################################
# S-DERS variables and scale assignment

# save item names
SDERSnames <- c('S.DERS1_ESM','S.DERS2_ESM.r','S.DERS3_ESM','S.DERS4_ESM','S.DERS5_ESM','S.DERS6_ESM.r','S.DERS7_ESM',
                'S.DERS8_ESM','S.DERS9_ESM','S.DERS10_ESM','S.DERS11_ESM.r','S.DERS12_ESM','S.DERS13_ESM','S.DERS14_ESM',
                'S.DERS15_ESM','S.DERS16_ESM.r','S.DERS17_ESM','S.DERS18_ESM','S.DERS19_ESM.r','S.DERS20_ESM','S.DERS21_ESM')
dfSDERS <- df[,SDERSnames]
dfSDERS_ID <- df[,c("PARTICIPANT_ID", SDERSnames)]

itemOrder <- c("S.DERS8_ESM", "S.DERS4_ESM", "S.DERS1_ESM", "S.DERS5_ESM", "S.DERS12_ESM", "S.DERS20_ESM", "S.DERS18_ESM", 
               "S.DERS13_ESM", "S.DERS17_ESM", "S.DERS10_ESM", "S.DERS3_ESM", "S.DERS15_ESM", "S.DERS21_ESM", "S.DERS9_ESM",
               "S.DERS6_ESM.r", "S.DERS11_ESM.r", "S.DERS2_ESM.r", "S.DERS19_ESM.r", "S.DERS16_ESM.r",
               "S.DERS14_ESM", "S.DERS7_ESM")

SDERSsumScaleNames <- c("S.DERS_NonAccept_EMAmean", "S.DERS_Modulate_EMAmean", "S.DERS_Awareness_EMAmean", "S.DERS_Clarity_EMAmean", "S.DERS_Total_EMAmean")



######################################################################
# Hypothesis 1: Non-negligible within-person variation

scaleICCs <- statsBy(df[, c("PARTICIPANT_ID", SDERSsumScaleNames)], group = "PARTICIPANT_ID")
scaleICCs <- cbind(scaleICCs$ICC1[-1], scaleICCs$ci1[-1,])
round(scaleICCs, 2)



######################################################################
# Hypothesis 2: Factor Structure (without Item 10)

SDERSnames_no10 <- SDERSnames[-which(SDERSnames == "S.DERS10_ESM")]
dfSDERS_no10 <- df[,SDERSnames_no10]
dfSDERS_ID_no10 <- df[,c("PARTICIPANT_ID", SDERSnames_no10)]

itemOrder_no10 <- c("S.DERS8_ESM", "S.DERS4_ESM", "S.DERS1_ESM", "S.DERS5_ESM", "S.DERS12_ESM", "S.DERS20_ESM", "S.DERS18_ESM", 
               "S.DERS13_ESM", "S.DERS17_ESM", "S.DERS3_ESM", "S.DERS15_ESM", "S.DERS21_ESM", "S.DERS9_ESM",
               "S.DERS6_ESM.r", "S.DERS11_ESM.r", "S.DERS2_ESM.r", "S.DERS19_ESM.r", "S.DERS16_ESM.r",
               "S.DERS14_ESM", "S.DERS7_ESM")

mcfaInput_no10 <- mcfa.input(l2Var = "PARTICIPANT_ID", dat = dfSDERS_ID_no10) 
combined.cov_no10 <- list(within = mcfaInput_no10$pw.cov, between = mcfaInput_no10$b.cov)
combined.n_no10 <- list(within = mcfaInput_no10$n - mcfaInput_no10$G, between = mcfaInput_no10$G)

main_loadings_lavender_no10 <-   list(
  NonAccept  = c("S.DERS8_ESM", "S.DERS4_ESM", "S.DERS1_ESM", "S.DERS5_ESM", "S.DERS12_ESM", "S.DERS20_ESM", "S.DERS18_ESM"),
  Modulate = c("S.DERS13_ESM", "S.DERS17_ESM", "S.DERS3_ESM", "S.DERS15_ESM", "S.DERS21_ESM", "S.DERS9_ESM"),
  Awareness = c("S.DERS6_ESM.r", "S.DERS11_ESM.r", "S.DERS2_ESM.r", "S.DERS19_ESM.r", "S.DERS16_ESM.r"),
  Clarity = c("S.DERS14_ESM", "S.DERS7_ESM")
)

main_loadings_constructionSample <-   list(
  NonAccept  = c("S.DERS8_ESM", "S.DERS4_ESM", "S.DERS1_ESM", "S.DERS5_ESM", "S.DERS12_ESM", "S.DERS20_ESM", "S.DERS18_ESM", "S.DERS15_ESM"),
  Modulate = c("S.DERS13_ESM", "S.DERS17_ESM", "S.DERS3_ESM","S.DERS21_ESM"),
  Awareness = c("S.DERS6_ESM.r", "S.DERS11_ESM.r", "S.DERS2_ESM.r", "S.DERS19_ESM.r", "S.DERS16_ESM.r"),
  Clarity = c("S.DERS14_ESM", "S.DERS7_ESM")
)

main_loadings_8item <-   list(
  NonAccept  = c("S.DERS12_ESM", "S.DERS18_ESM"),
  Modulate = c("S.DERS17_ESM", "S.DERS3_ESM"),
  Awareness = c("S.DERS6_ESM.r", "S.DERS2_ESM.r"),
  Clarity = c("S.DERS14_ESM", "S.DERS7_ESM")
)

main_loadings_4item <-   list(
  NonAccept  = c("S.DERS18_ESM"),
  Modulate = c("S.DERS17_ESM"),
  Awareness = c("S.DERS2_ESM.r"),
  Clarity = c("S.DERS7_ESM")
)



#####################################
# Model 1: 4 Factors simple structure with equal loadings

cfa4fixed <-  '
level: 1
  
  NonAccept =~ a*S.DERS8_ESM + b*S.DERS4_ESM + c*S.DERS1_ESM + d*S.DERS5_ESM + e*S.DERS12_ESM + f*S.DERS20_ESM + g*S.DERS18_ESM 
  Modulate =~ h*S.DERS13_ESM + i*S.DERS17_ESM + k*S.DERS3_ESM + l*S.DERS15_ESM + m*S.DERS21_ESM + n*S.DERS9_ESM 
  Awareness =~ o*S.DERS6_ESM.r + p*S.DERS11_ESM.r + q*S.DERS2_ESM.r + r*S.DERS19_ESM.r + s*S.DERS16_ESM.r 
  Clarity =~ t*S.DERS14_ESM + u*S.DERS7_ESM
  
  NonAccept ~~ NA*NonAccept
  Modulate ~~ NA*Modulate
  Awareness ~~ NA*Awareness
  Clarity ~~ NA*Clarity

level: 2

  NonAccept  =~ a*S.DERS8_ESM + b*S.DERS4_ESM + c*S.DERS1_ESM + d*S.DERS5_ESM + e*S.DERS12_ESM + f*S.DERS20_ESM + g*S.DERS18_ESM
  Modulate =~ h*S.DERS13_ESM + i*S.DERS17_ESM + k*S.DERS3_ESM + l*S.DERS15_ESM + m*S.DERS21_ESM + n*S.DERS9_ESM
  Awareness =~ o*S.DERS6_ESM.r + p*S.DERS11_ESM.r + q*S.DERS2_ESM.r + r*S.DERS19_ESM.r + s*S.DERS16_ESM.r
  Clarity =~ t*S.DERS14_ESM + u*S.DERS7_ESM
  
  
'

cfa4fixed_fit <- CFA_saveOrRead(cfa4fixed, data = df, cluster = 'PARTICIPANT_ID', std.lv = TRUE, estimator="MLR", filename = paste0("cfa4fixed_fit_",sample))
summary(cfa4fixed_fit, fit.measures = TRUE, standardize = TRUE)
fitMeasures_cfa4fixed <- fitMeasures(cfa4fixed_fit, c("srmr_within", "srmr_between", "rmsea.robust", "cfi.robust", "tli.robust", "AIC", "BIC", "chisq.Scaled", "df.Scaled", "pvalue.Scaled"), output = "matrix")




# "a model whose null model RMSEA is less than 0.158 and whose RMSEA is 0.05 must have a TLI of less than .90": https://davidakenny.net/cm/fit.htm
# Hox: Factor loadings is often stronger on aggregate level, because error accumulates on lower levels (p. 278)



#####################################
# Model 2: 4 Factors simple structure with unequal loadings

cfa4free <-  '
level: 1
  
  NonAccept =~ S.DERS8_ESM + S.DERS4_ESM + S.DERS1_ESM + S.DERS5_ESM + S.DERS12_ESM + S.DERS20_ESM + S.DERS18_ESM 
  Modulate =~ S.DERS13_ESM + S.DERS17_ESM + S.DERS3_ESM + S.DERS15_ESM + S.DERS21_ESM + S.DERS9_ESM 
  Awareness =~ S.DERS6_ESM.r + S.DERS11_ESM.r + S.DERS2_ESM.r + S.DERS19_ESM.r + S.DERS16_ESM.r 
  Clarity =~ S.DERS14_ESM + S.DERS7_ESM

level: 2

  NonAccept  =~ S.DERS8_ESM + S.DERS4_ESM + S.DERS1_ESM + S.DERS5_ESM + S.DERS12_ESM + S.DERS20_ESM + S.DERS18_ESM
  Modulate =~ S.DERS13_ESM + S.DERS17_ESM + S.DERS3_ESM + S.DERS15_ESM + S.DERS21_ESM + S.DERS9_ESM
  Awareness =~ S.DERS6_ESM.r + S.DERS11_ESM.r + S.DERS2_ESM.r + S.DERS19_ESM.r + S.DERS16_ESM.r
  Clarity =~ S.DERS14_ESM + S.DERS7_ESM
  
'

cfa4free_fit <- CFA_saveOrRead(cfa4free, data = df, cluster = 'PARTICIPANT_ID', std.lv = FALSE, estimator="MLR", filename = paste0("cfa4free_fit",sample))
summary(cfa4free_fit, fit.measures = TRUE, standardize = TRUE)
fitMeasures_cfa4free <- fitMeasures(cfa4free_fit, c("srmr_within", "srmr_between", "rmsea.robust", "cfi.robust", "tli.robust", "AIC", "BIC", "chisq.Scaled", "df.Scaled", "pvalue.Scaled"), output = "matrix")


#rmsea of the null model (.141)
if(!file.exists(here::here("manuscripts", "SDERSvalid_dailyLife", "models", "cfa4free_rmseaNullModel.rds")) & sample=="pt1"){
  cfa4free_fit4null <- cfa(cfa4free, data = df, cluster = 'PARTICIPANT_ID', std.lv = TRUE, estimator="MLR")
  cfa4free_rmseaNullModel <- nullRMSEA(cfa4free_fit4null, scaled = TRUE, silent = FALSE)
  saveRDS(cfa4free_rmseaNullModel, file = here::here("manuscripts", "SDERSvalid_dailyLife", "models", "cfa4free_rmseaNullModel.rds"))
}else{
  cfa4fixed_rmseaNullModel <- readRDS(file = here::here("manuscripts", "SDERSvalid_dailyLife", "models", "cfa4free_rmseaNullModel.rds"))
}
cfa4fixed_rmseaNullModel




#####################################
# Model 3: Cross-loadings with equality constraints across levels

# perform EFA on full dataset and determine referent vars
efaModel4between <- psych::fa(mcfaInput_no10$ab.cov, n.obs=combined.n_no10$between, nfactors=4, rotate="promax", fm="pa")
print(efaModel4between$loadings, cutoff=0.3, sort = TRUE)
if(sample == "full"){
  facNames <- c("NonAccept","Awareness","Modulate","Clarity")  
}else{
  facNames <- c("NonAccept","Modulate","Awareness","Clarity")
}

colnames(efaModel4between$loadings) <- facNames
referentsTotal <- find_referents(efaModel4between,factor_names = facNames)

ESEM4fixed <- combineTwoLevelSyntax(make_ESEM_Syntax(efaModel4between, facNames, referentsTotal, c("a", "b", "c", "d"), std = FALSE, covMat = cov(dfSDERS_no10)),
                                    make_ESEM_Syntax(efaModel4between, facNames, referentsTotal, c("a", "b", "c", "d"), std = FALSE, covMat = cov(dfSDERS_no10)))

ESEM4fixed_fit <- CFA_saveOrRead(ESEM4fixed, data = df, cluster = 'PARTICIPANT_ID', estimator = "MLR", filename = paste0("ESEM4fixed_fit_", sample))

summary(ESEM4fixed_fit, fit.measures = TRUE, standardize = TRUE)
fitMeasures_ESEM4fixed <- fitMeasures(ESEM4fixed_fit, c("srmr_within", "srmr_between", "rmsea.robust", "cfi.robust", "tli.robust", "AIC", "BIC", "chisq.Scaled", "df.Scaled", "pvalue.Scaled"), output = "matrix")
fitMeasures_ESEM4fixed


#loadings
cut <- 0.3
#inspect(ESEM4fixed_fit, what = "std")$within$lambda
loadingswithin = inspect(ESEM4fixed_fit, what = "std")$within$lambda
loadingsBetween = inspect(ESEM4fixed_fit, what = "std")$PARTICIPANT_ID$lambda

ifelse(abs(loadingswithin) > cut, loadingswithin, "")
#inspect(ESEM4fixed_fit, what = "std")$PARTICIPANT_ID$lambda
ifelse(abs(loadingsBetween) > cut, loadingsBetween, "")


dfplotLoadings <- cbind(melt(loadingswithin), melt(loadingsBetween)$value)
names(dfplotLoadings) <- c("item", "factor", "withinLoading", "betweenLoading")

dfplotLoadings$items_num <- as.numeric(gsub("[^0-9]", "", dfplotLoadings$item))

ggplot(data = dfplotLoadings, aes(x=betweenLoading, y=withinLoading, label = items_num, colour=factor)) +
  geom_text(show.legend = FALSE) +
  geom_point(alpha=0) +
  theme_classic() +
  theme(legend.position = c(0.8, 0.2)) + 
  
  guides(colour = guide_legend(title="Factor", override.aes=list(alpha=1, shape=15, size=5))) +
  
  scale_x_continuous(breaks = seq(-0.2, 1.2, by = 0.1)) + 
  scale_y_continuous(breaks = seq(-0.2, 1.2, by = 0.1)) + 
  
  geom_hline(yintercept=0.3, linetype="dashed") +
  geom_vline(xintercept=0.3, linetype="dashed") 

ggsave(here::here("manuscripts", "SDERSvalid_dailyLife","figures", paste("loadingsCorrespondance_", sample, ".svg")), device="svg")
#OPTION FÜR UNLESERLICHE DREI LOADINGS. HÄNDISCH JITTERN! EINFACH WERTE IN TABELLE ERSETZEN


#####################################
# Model 4: Cross-loadings without equality constraints

# correct correlation table estimates with: https://francish.netlify.app/docs/MCFAinRHUANG.pdf

efaModel4within <- psych::fa(combined.cov_no10$within, n.obs=combined.n_no10$within, nfactors=4, rotate="promax", fm="pa")
colnames(efaModel4within$loadings) <- facNames_within <- c("NonAccept", "Modulate", "Awareness", "Clarity")
find_referents(efaModel4within,factor_names = facNames_within)
esem_Model4within <- esem_syntax(efaModel4within)
efaModel4between <- psych::fa(mcfaInput_no10$ab.cov, n.obs=combined.n_no10$between, nfactors=4, rotate="promax", fm="pa")
colnames(efaModel4between$loadings) <- facNames_between <- c("NonAccept", "Awareness", "Modulate", "Clarity")
find_referents(efaModel4between,factor_names = facNames_between)

#colnames(efaModel4between$loadings) <- c("PA1b", "PA2b", "PA3b", "PA4b")
# esem_Model4between <- esem_syntax(efaModel4between)
# ESEM4free <- combineTwoLevelSyntax(esem_Model4within, esem_Model4between)
# ESEM4free_fit <- CFA_saveOrRead(ESEM4free, data = df, cluster = 'PARTICIPANT_ID', std.lv=TRUE, estimator = "MLR", filename = "ESEM4free_fit")

ESEM4free <- combineTwoLevelSyntax(make_ESEM_Syntax(efaModel4within, facNames_within, find_referents(efaModel4within,factor_names = facNames_within), c("a", "b", "c", "d"), std = FALSE, covMat = mcfaInput_no10$pw.cov),
                                    make_ESEM_Syntax(efaModel4between, facNames_between, between=TRUE,find_referents(efaModel4between,factor_names = facNames_between), c("e", "f", "g", "h"), std = FALSE, covMat = mcfaInput_no10$ab.cov))

ESEM4free_fit <- CFA_saveOrRead(ESEM4free, data = df, cluster = 'PARTICIPANT_ID', std.lv=TRUE, estimator = "MLR", filename = paste0("ESEM4free_fit_", sample))
summary(ESEM4free_fit, fit.measures = TRUE, standardize = TRUE)
fitMeasures_ESEM4free <- fitMeasures(ESEM4free_fit, c("srmr_within", "srmr_between", "rmsea.robust", "cfi.robust", "tli.robust", "AIC", "BIC", "chisq.Scaled", "df.Scaled", "pvalue.Scaled"), output = "matrix")
fitMeasures_ESEM4free



# ALTERNATIVE USING BUILT IN MULTILEVEL FACTOR ROTATION. Has orthogonal between factors on between level, which is implausible.
# ESEM4free_EFA <- combineTwoLevelSyntax(EFAmodelSyntax(nfactors=4, "within", SDERSnames_no10), EFAmodelSyntax(nfactors=4, "between", SDERSnames_no10))
# ESEM4free_fit_EFA <- CFA_saveOrRead(ESEM4free, data = df, cluster = 'PARTICIPANT_ID', estimator = "MLR", filename = "ESEM4free_fit_EFA", rotation="promax")
# summary(ESEM4free_fit_EFA)
# fitMeasures(ESEM4free_fit_EFA, c("srmr_within", "srmr_between", "rmsea.robust", "cfi.robust", "tli.robust", "AIC", "BIC", "chisq.Scaled", "df.Scaled", "pvalue.Scaled"), output = "matrix")

# level-specific fit for Model 2 (no-crossloadings, non-invariance) (53min)
# if(!file.exists(here::here("manuscripts", "SDERSvalid_dailyLife", "models", "ESEM4free_levelSpecFit.rds"))){
#   ESEM4free_levelSpecFit <- multilevel.fit(ESEM4free_fit)
#   saveRDS(ESEM4free_levelSpecFit, file = here::here("manuscripts", "SDERSvalid_dailyLife", "models", "ESEM4free_levelSpecFit.rds"))
# }else{
#   ESEM4free_levelSpecFit <- readRDS(file = here::here("manuscripts", "SDERSvalid_dailyLife", "models", "ESEM4free_levelSpecFit.rds"))
# }
# ESEM4free_levelSpecFit




#####################################
# Model comparison of preregistered models

FitCombined <- as.data.frame(
  round(
    rbind(t(fitMeasures_cfa4fixed), t(fitMeasures_cfa4free), t(fitMeasures_ESEM4fixed), t(fitMeasures_ESEM4free)), 
    3), 
  row.names = c("Simple Structure; fixed", "Simple Structure; free", "Cross-loadings; fixed", "Cross-loadings; free"))
FitCombined$chisq.scaled <- round(FitCombined$chisq, 1)
FitCombined

FitCombined_noIC <- FitCombined[, -which(names(FitCombined) %in% c("aic", "bic"))]
FitCombined_noIC <- cbind(rownames(FitCombined_noIC), FitCombined_noIC)
names(FitCombined_noIC)[1] <- "model"
save_as_docx(flextable(FitCombined_noIC),path = here::here("manuscripts", "SDERSvalid_dailyLife", "tables", paste0("fitMeasuresPreReg_", sample, ".docx")))



#####################################
# reliability


main_loadings_constructionSample <-   list(
  NonAccept  = c("S.DERS8_ESM", "S.DERS4_ESM", "S.DERS1_ESM", "S.DERS5_ESM", "S.DERS12_ESM", "S.DERS20_ESM", "S.DERS18_ESM", "S.DERS15_ESM"),
  Modulate = c("S.DERS13_ESM", "S.DERS17_ESM", "S.DERS3_ESM","S.DERS21_ESM"),
  Awareness = c("S.DERS6_ESM.r", "S.DERS11_ESM.r", "S.DERS2_ESM.r", "S.DERS19_ESM.r", "S.DERS16_ESM.r"),
  Clarity = c("S.DERS14_ESM", "S.DERS7_ESM")
)

main_loadings_8item <-   list(
  NonAccept  = c("S.DERS12_ESM", "S.DERS18_ESM"),
  Modulate = c("S.DERS17_ESM", "S.DERS3_ESM"),
  Awareness = c("S.DERS6_ESM.r", "S.DERS2_ESM.r"),
  Clarity = c("S.DERS14_ESM", "S.DERS7_ESM")
)

main_loadings_4item <-   list(
  NonAccept  = c("S.DERS18_ESM"),
  Modulate = c("S.DERS17_ESM"),
  Awareness = c("S.DERS2_ESM.r"),
  Clarity = c("S.DERS7_ESM")
)


reliabilityModel <-  CFA_saveOrRead(ESEM4fixed, data = df, cluster = 'PARTICIPANT_ID', estimator = "MLR", filename = "ESEM4fixed_fit_pt2", "pt2")

# reliability within
withinReliability <- rbind(ESEMlaiRelcustom(reliabilityModel, main_loadings_lavender_no10),
                           ESEMlaiRelcustom(reliabilityModel, main_loadings_constructionSample),
                           ESEMlaiRelcustom(reliabilityModel, main_loadings_8item),
                           ESEMlaiRelcustom(reliabilityModel, main_loadings_4item))
withinReliability <- withinReliability[str_detect(row.names(withinReliability), "omegaWithin"), ]
withinReliability <- data.frame(Version = c("Lavender et al.", "Sicorello et al.", "8-Item", "4-Item"), withinReliability, row.names = NULL)
withinReliability

save_as_docx(flextable(withinReliability),path = here::here("manuscripts", "SDERSvalid_dailyLife", "tables", "withinReliabilities.docx"))


# reliability between

numN <- 50
versions <- list("19item"=main_loadings_constructionSample, 
                 "8item"=main_loadings_8item, 
                 "4item"=main_loadings_4item)
betweenReliability <- as.data.frame(
  matrix(nrow=numN*3,
         ncol=5,
         dimnames=list(
           NULL,
           names(withinReliability)[-1]
           )
         )
)

for(i in 1:3){
  
  for(j in 1:numN){
    betweenReliability[j+numN*(i-1), ] <- ESEMlaiRelcustom(reliabilityModel, versions[[i]], clusterSize=j)[3,]
    }
}

betweenReliability <- data.frame(Version=factor(rep(names(versions), each=numN), levels=names(versions)), 
                                 ClusterSize=rep(c(1:numN),3), 
                                 betweenReliability)
betweenReliability <- melt(betweenReliability, id.vars=c("Version","ClusterSize"))

ggplot(betweenReliability, aes(x=ClusterSize, y=value, colour=variable)) + 
  geom_line() + 
  
  facet_grid(~ Version)
ggsave(here::here("manuscripts", "SDERSvalid_dailyLife","figures", "betweenReliabilities.svg"), device="svg")



LSTcustom(reliabilityModel, main_loadings_lavender_no10)



FactorValidityCustom(ESEM4fixed_fit, main_loadings_lavender_no10[c("NonAccept", "Awareness", "Modulate", "Clarity")])

FactorValidityCustom(ESEM4fixed_fit, main_loadings_constructionSample[c("NonAccept", "Awareness", "Modulate", "Clarity")])

















#####################################################################################################################
# DEPRECATED CODE
# 
# 
# 
# cfa4free <-  '
#   NonAccept =~ S.DERS8_ESM + S.DERS4_ESM + S.DERS1_ESM + S.DERS5_ESM + S.DERS12_ESM + S.DERS20_ESM + S.DERS18_ESM 
#   Modulate =~ S.DERS13_ESM + S.DERS17_ESM + S.DERS3_ESM + S.DERS15_ESM + S.DERS21_ESM + S.DERS9_ESM 
#   Awareness =~ S.DERS6_ESM.r + S.DERS11_ESM.r + S.DERS2_ESM.r + S.DERS19_ESM.r + S.DERS16_ESM.r 
#   Clarity =~ S.DERS14_ESM + S.DERS7_ESM
# '
# 
# # independence syntax does not work yet, because covariances are assumed
# generate_independence_syntax <- function(variables){
#   lavaan_code <- ""
#   for(i in 1:length(variables)){
#     lavaan_code <- paste0(lavaan_code, variables[i], " ~~ ", variables[i], "\n")
#   }
#   return(lavaan_code)
# }
# 
# cat(generate_independence_syntax(SDERSnames_no10))
# 
# 
# #######################################
# # Muthen steps
# 
# 
# #####################
# # Step 1: Combined CFA
# 
# cfa4freeSingle <-  '
#   NonAccept =~ S.DERS8_ESM + S.DERS4_ESM + S.DERS1_ESM + S.DERS5_ESM + S.DERS12_ESM + S.DERS20_ESM + S.DERS18_ESM 
#   Modulate =~ S.DERS13_ESM + S.DERS17_ESM + S.DERS3_ESM + S.DERS15_ESM + S.DERS21_ESM + S.DERS9_ESM 
#   Awareness =~ S.DERS6_ESM.r + S.DERS11_ESM.r + S.DERS2_ESM.r + S.DERS19_ESM.r + S.DERS16_ESM.r 
#   Clarity =~ S.DERS14_ESM + S.DERS7_ESM
# '
# 
# CFA4total_fit <- cfa(cfa4freeSingle, data = df, std.lv = TRUE, estimator="MLR")
# summary(CFA4total_fit, fit.measures = TRUE, standardize = TRUE)
# fitMeasures_CFA4total<- fitMeasures(cfa4free_fit, c("srmr", "rmsea.robust", "cfi.robust", "tli.robust", "AIC", "BIC", "chisq.Scaled", "df.Scaled", "pvalue.Scaled"), output = "matrix")
# rmseaNullModelTotal <- nullRMSEA(CFA4total_fit, scaled = TRUE, silent = FALSE) 
# 
# 
# 
# 
# #####################
# # Step 2: within fit
# CFA4within_fit_ML <- cfa(cfa4freeSingle, sample.cov = mcfaInput_no10$pw.cov, sample.nobs = mcfaInput_no10$n, std.lv = TRUE, estimator="ML")
# summary(CFA4within_fit_ML, fit.measures = TRUE, standardize = TRUE)
# fitMeasures(CFA4within_fit_ML, c("srmr", "rmsea", "cfi", "tli", "AIC", "BIC", "chisq", "df", "pvalue"), output = "matrix")
# inspect(CFA4within_fit_ML, what="std")$lambda
# rmseaNullModelWithin <- nullRMSEA(CFA4within_fit_ML, scaled = TRUE, silent = FALSE) 
# 
# EFA4within <- psych::fa(mcfaInput_no10$pw.cov, n.obs=mcfaInput_no10$n, nfactors=4, rotate="promax", fm="pa")
# EFAfitMeasures(EFA4within)
# print(EFA4within$loadings, cutoff = 0.25, sort = TRUE)
# 
# 
# #####################
# # Step 3: between fit
# CFA4between_fit_ML <- cfa(cfa4freeSingle, sample.cov = mcfaInput_no10$ab.cov, sample.nobs = mcfaInput_no10$G, std.lv = TRUE, estimator="ML")
# summary(CFA4between_fit_ML, fit.measures = TRUE, standardize = TRUE)
# modIndicesBetween <- modindices(CFA4between_fit_ML, sort = TRUE)
# modIndicesBetween[modIndicesBetween[,2] == "=~", ]
# 
# 
# #####################
# # Step 3b: EFA between
# 
# # parallel analysis
# fa.parallel(mcfaInput_no10$ab.cor, n.obs=mcfaInput_no10$G, fm = "pa", fa = "fa", quant = .99)
# 
# # 4 factor model
# efaModel4Between <- psych::fa(mcfaInput_no10$ab.cor, n.obs=mcfaInput_no10$G, nfactors=4, rotate="promax", fm="pa")
# EFAfitMeasures(efaModel4Between)
# 
# 
# 
# #####################
# # Follow-up analyses to determine why only SRMR is good
# 
# ###########
# #EFA for 1-10 factors
# 
# maxFactors <- 10
# 
# EFAmodelFitsOutBetween <- data.frame(
#   matrix(nrow=maxFactors, ncol=7, 
#          dimnames=list(
#            paste0('#Factors = ', c(1:maxFactors)), 
#            c("srmr", "rmsea", "cfi", "tli", "chi_squared", "df", "p-value"))
#   )
# )
# 
# for(i in 1:maxFactors){
#   efaTemp <- psych::fa(mcfaInput_no10$ab.cov, n.obs=mcfaInput_no10$G, nfactors=i, rotate="promax", fm="pa")
#   EFAmodelFitsOutBetween[i,] <- EFAfitMeasures(efaTemp)
# }
# EFAmodelFitsOutBetween
# 
# 
# # plot cross-loadings
# print(efaModel4Between$loadings, cutoff = 0.4)
# hist(efaModel4Between$loadings)
# 
# 
# ###########
# # stepwise removal of lowest cross-loadings an refitting in CFA
# 
# # fit ESEM model with all cross-loadings
# efaModel4between <- psych::fa(mcfaInput_no10$ab.cov, n.obs=combined.n_no10$between, nfactors=4, rotate="promax", fm="pa")
# find_referents(efaModel4between,factor_names = facNames)
# esem_Model4between <- esem_syntax(efaModel4between)
# ESEMfitBetween <- cfa(esem_Model4between, sample.cov = mcfaInput_no10$ab.cov, sample.nobs=mcfaInput_no10$G, estimator="ML")
# # show summary
# summary(ESEMfitBetween)
# fitMeasures(ESEMfitBetween, c("srmr", "rmsea", "cfi", "tli", "chisq.Scaled", "df.Scaled", "pvalue.Scaled"), output = "matrix")
# 
# # Loop that gradually constrains cross-loadings to zero, from lowest to highest
# iterations = 40 
# fitESEMOut = as.data.frame(
#   matrix(nrow=iterations, ncol=4, dimnames=list(paste0("#loadings removed=",c(1:iterations)), c("srmr", "rmsea", "cfi", "tli")))
# )
# 
# parTableFit <- parTable(ESEMfitBetween)
# parTableFitUser <- parTableFit[parTableFit$user == 1, ]
# sortedID <- parTableFitUser[order(abs(parTableFitUser$est)), ]$id
# 
# PAnamesVec <- c("PA1", "PA2", "PA3", "PA4")
# 
# for(i in 1:iterations){
#   
#   parTableTemp <- parTableFit
#   parTableTemp <- parTableTemp[parTableTemp$id %in% sortedID[1:i] == FALSE, ]
#   
#   FacSyntax <- ""
#   
#   for(j in 1:length(PAnamesVec)){
#     
#     FacSyntax <- paste0(FacSyntax, 
#                         paste0("efa(block)*", PAnamesVec[j], " =~ ", 
#                                paste0(parTableTemp[parTableTemp$lhs == PAnamesVec[j] & parTableTemp$user == 1, "rhs"], collapse = " + ")),
#                         "\n\n")
#   }
#   
#   tryCatch({
#     TempFit <- cfa(FacSyntax, sample.cov = mcfaInput_no10$ab.cov, sample.nobs=mcfaInput_no10$G, estimator="ML", rotation="promax", std.lv = TRUE)
#     fitESEMOut[i,] <- round(fitMeasures(TempFit, c("srmr", "rmsea", "cfi", "tli"), output = "matrix"),2)
#   }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
# 
#   
# }
# 
# 
# 
# 
# 
# 
# summary(TempFit, standardized=TRUE)
# 
# cat(FacSyntax)
# 
# summary(test)
# 
# 
# for(i in 1:iterations){
#   
#   parTableTemp <- parTableFit
#   parTableTemp[parTableTemp$id %in% sortedID[1:i], "est"] <- 0
#   parTableTemp[parTableTemp$id %in% sortedID[1:i], "free"] <- 0
#   
#   ESEMfitBetween_refit <- cfa(parTableTemp, sample.cov = mcfaInput_no10$ab.cov, sample.nobs=mcfaInput_no10$G, estimator="ML")
#   fitESEMOut[i,] <- fitMeasures(ESEMfitBetween_refit, c("srmr", "rmsea", "cfi", "tli"), output = "matrix")
# }
# 
# 
# 
# EFAmodelSyntax(nfactors=4, "within", SDERSnames_no10)
# 
# 
# 
# 
# 
# 
