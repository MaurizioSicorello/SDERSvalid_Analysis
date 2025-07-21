########################################################################    
# load packages, custom functions and data

# packages
library("renv")
library("here")
library("apaTables")
library("stringr")
library("psych")
library("ggplot2")
library("svglite")
library("GPArotation")
library("lavaan")
library("FCO")
library("dynamic")
library("semPlot")
library("reshape2")
library("esem")
library("semTools")
library("nonnest2")
library("stats")
library("corrplot")
library("xlsx")
library("flextable")
library("BayesFactor")
library("ppcor")
library("bootnet")
library("qgraph")
#install.packages("psychonetrics")

# custom functions
source(here("functions", "reliabilityEFA.R"))
source(here("functions", "reliabilityESEM.R"))
source(here("functions", "fa_loadingTable.R"))

# data
df <- read.csv(here("data", "SDERSvalid_crossSec_data_preprocessed.csv"))


#####################################################################################
############################# PRELIMINARY ANALYSES ##################################
#####################################################################################


########################################################################  
# Sample Descriptives

# demographics
round(table(df$SEX)/length(df$SEX),2)
df$HOCHSCHULE <- ifelse(str_detect(df$HOCHSCHULE, regex('heidelberg', ignore_case = T)), "uni heidelberg", 
                        ifelse(str_detect(df$HOCHSCHULE, regex('regensburg', ignore_case = T)), "uni regensburg",
                               df$HOCHSCHULE))
table(df$HOCHSCHULE)
round(table(df$OCCUPATION)/length(df$OCCUPATION),2) # 2 = vocational school, 3 = University, 4 = employed, 6 = other
round(table(df$VPN_STUNDEN)/length(df$VPN_STUNDEN),2)
describe(df$AGE)
table(df$ACAD.DEGREE) # 3 = Realschule, 4 = Abitur, 5 = Hochschulabschluss, 6 = Promotion
table(df$FAM.STATUS) # 1 = ledig, 2 = partnerschaft, 3 = verheiratet


# mental health
table(df$MENT.HEALTH)
df$MENT.AGE_num <- as.numeric(ifelse(str_detect(df$MENT.AGE, "^[:digit:]+$"), df$MENT.AGE, NA))
hist(df$MENT.AGE_num)
mean(df$MENT.AGE_num, na.rm = TRUE)
sd(df$MENT.AGE_num, na.rm = TRUE)
table(df$MENT.MEDICINE)
table(df$MENT.TREATMENT)


describe(df$questDuration)


########################################################################
# Psychological Measures

apa.cor.table(df[, c("S.DERS_Total_sum", "DERS_Total_sum", "CTQ_Total_sum", "NEO_N_sum", "IERQ_Total_sum", "BIS_Total_sum", "FAH_Total_sum", "FFAF_Describe_sum", "FFAF_Awareness_sum")],
              filename = here("manuscripts", "SDERSvalid_crossSec", "tables", "SDERSbaselineMeasuresDescriptives.doc"))

apa.cor.table(df[, c("S.DERS_Total_sum", "S.DERS_NonAccept_sum", "S.DERS_Modulate_sum" , "S.DERS_Awareness_sum", "S.DERS_Clarity_sum")], 
              filename = here("manuscripts", "SDERSvalid_crossSec", "tables", "SDERSintercorr_Baseline.doc"))



########################################################################
# Manipulation check

df$reactivityValence <- df$AFFECT_PRE_X - df$AFFECT_POST_X # higher values mean larger increase of negative affect
df$reactivityArousal <- df$AFFECT_PRE_Y - df$AFFECT_POST_Y

cor(df$AFFECT_PRE_X, df$AFFECT_POST_X )

describe(df[, c("AFFECT_PRE_X", "AFFECT_POST_X", "AFFECT_PRE_Y", "AFFECT_POST_Y")])

# valence
t.test(df$AFFECT_PRE_X, df$AFFECT_POST_X, paired = TRUE) 
mean(df$AFFECT_POST_X - df$AFFECT_PRE_X)/sd(df$AFFECT_POST_X - df$AFFECT_PRE_X)
mean(df$AFFECT_PRE_X)
sd(df$AFFECT_PRE_X)
mean(df$AFFECT_POST_X)
sd(df$AFFECT_POST_X)

# arousal
t.test(df$AFFECT_PRE_Y, df$AFFECT_POST_Y, paired = TRUE)
mean(df$AFFECT_POST_Y - df$AFFECT_PRE_Y)/sd(df$AFFECT_POST_Y - df$AFFECT_PRE_Y)
mean(df$AFFECT_PRE_Y)
sd(df$AFFECT_PRE_Y)
mean(df$AFFECT_POST_Y)
sd(df$AFFECT_POST_Y)


cor(df$AFFECT_POST_X, df$AFFECT_PRE_X)


dfInduct <- rbind(setNames(df[, c("AFFECT_PRE_X", "AFFECT_PRE_Y")], c("Valence", "Arousal")), 
                  setNames(df[, c("AFFECT_POST_X", "AFFECT_POST_Y")], c("Valence", "Arousal")))
dfInduct$prePost <- rep(c("pre", "post"), each = (nrow(dfInduct)/2))

dfInductMean <- data.frame(Valence = c(mean(df$AFFECT_PRE_X), mean(df$AFFECT_POST_X)),
                           Arousal = c(mean(df$AFFECT_PRE_Y), mean(df$AFFECT_POST_Y)),
                           prePost = c("pre", "post"))

ggplot(dfInduct, aes(x=Valence, y=Arousal, group=prePost, colour=prePost)) +
  geom_jitter() +
  geom_point(data=dfInductMean,  mapping=aes(x = Valence, y = Arousal, shape = prePost), colour = "black", size = 3)
ggsave(here("manuscripts", "SDERSvalid_crossSec", "figures", "MoodInduction.svg"), device="svg")

cor.test(df$AFFECT_POST_X, df$S.DERS_Total_sum)
cor.test(df$AFFECT_PRE_X, df$S.DERS_Total_sum)
cor.test(df$reactivityValence, df$S.DERS_Total_sum)

cor.test(df$AFFECT_POST_X, df$NEO_N_mean)
cor.test(df$AFFECT_PRE_X, df$NEO_N_mean)
cor.test(df$reactivityValence, df$NEO_N_mean)

hist(df$AFFECT_POST_X - df$AFFECT_PRE_X)
table(df$AFFECT_POST_X - df$AFFECT_PRE_X)
sum((df$AFFECT_POST_X - df$AFFECT_PRE_X) < 0)

hist(df$AFFECT_POST_Y - df$AFFECT_PRE_Y)
sum((df$AFFECT_POST_Y - df$AFFECT_PRE_Y) < 0)

hist(df$MANIPULATION_CHECK)
mean(df$MANIPULATION_CHECK)
sd(df$MANIPULATION_CHECK)

sum(df$MANIPULATION_CHECK == 1)
sum(df$MANIPULATION_CHECK <= 2)





#####################################################################################
########################## S-DERS VALIDATION ANALYSES ###############################
#####################################################################################


# save item names
SDERSnames <- c('S.DERS1_BL','S.DERS2_BL.r','S.DERS3_BL','S.DERS4_BL','S.DERS5_BL','S.DERS6_BL.r','S.DERS7_BL',
                'S.DERS8_BL','S.DERS9_BL','S.DERS10_BL','S.DERS11_BL.r','S.DERS12_BL','S.DERS13_BL','S.DERS14_BL',
                'S.DERS15_BL','S.DERS16_BL.r','S.DERS17_BL','S.DERS18_BL','S.DERS19_BL.r','S.DERS20_BL','S.DERS21_BL')

# assignment of Items to Subscales
main_loadings_list <-   list(
  NonAccept  = c("S.DERS8_BL", "S.DERS4_BL", "S.DERS1_BL", "S.DERS5_BL", "S.DERS12_BL", "S.DERS20_BL", "S.DERS18_BL"),
  Modulate = c("S.DERS13_BL", "S.DERS17_BL", "S.DERS10_BL", "S.DERS3_BL", "S.DERS15_BL", "S.DERS21_BL", "S.DERS9_BL"),
  Awareness = c("S.DERS6_BL.r", "S.DERS11_BL.r", "S.DERS2_BL.r", "S.DERS19_BL.r", "S.DERS16_BL.r"),
  Clarity = c("S.DERS14_BL", "S.DERS7_BL")
)

sumScaleNames <- c("S.DERS_NonAccept_sum", "S.DERS_Modulate_sum", "S.DERS_Awareness_sum", "S.DERS_Clarity_sum", "S.DERS_Total_sum")

dfSDERS <- as.data.frame(scale(df[, SDERSnames]))
describe(dfSDERS)



############################################################################
# Check distribution

describe(dfSDERS)
par(mfrow = c(3, 3))
for(i in 1:7){hist(dfSDERS[,i])}
par(mfrow = c(3, 3))
for(i in 8:14){hist(dfSDERS[,i])}
par(mfrow = c(3, 3))
for(i in 15:21){hist(dfSDERS[,i])}
par(mfrow = c(1,1))
describe(df[, c("S.DERS_Total_mean" , "S.DERS_NonAccept_mean", "S.DERS_Modulate_mean", "S.DERS_Awareness_mean", "S.DERS_Clarity_mean")])



############################################################################
# Structural Validity

# plot correlations
svg(file=here("manuscripts", "SDERSvalid_crossSec","figures", "corrPlot.svg"))
corrM <- cor(dfSDERS)
rownames(corrM) <- as.numeric(unlist(str_extract_all(names(dfSDERS), "\\d+")))
colnames(corrM) <- as.numeric(unlist(str_extract_all(names(dfSDERS), "\\d+")))
corrplot(corrM, method = "square", order = 'hclust', addrect = 4, tl.col="black", tl.srt = 0)
dev.off()



####################################
# CFA

CFAmodel_simpel <-   'NonAccept  =~ S.DERS8_BL + S.DERS4_BL + S.DERS1_BL + S.DERS5_BL + S.DERS12_BL + S.DERS20_BL + S.DERS18_BL
                      Modulate =~ S.DERS13_BL + S.DERS17_BL + S.DERS10_BL + S.DERS3_BL + S.DERS15_BL + S.DERS21_BL + S.DERS9_BL
                      Awareness =~ S.DERS6_BL.r + S.DERS11_BL.r + S.DERS2_BL.r + S.DERS19_BL.r + S.DERS16_BL.r
                      Clarity =~ S.DERS14_BL + S.DERS7_BL'

### ML estimator
CFAmodel_simpel_fit <- cfa(CFAmodel_simpel, data = dfSDERS, std.lv = TRUE, estimator = "ML") 
summary(CFAmodel_simpel_fit, fit.measures = TRUE, standardize = TRUE)
fitMeasures_CFAmodel_simpel <- fitMeasures(CFAmodel_simpel_fit, c("srmr", "rmsea", "cfi", "tli", "AIC", "BIC", "chisq", "df", "pvalue"), output = "matrix")
fitMeasures_CFAmodel_simpel

### MLM estimator, robust for nonnormality
CFAmodel_simpel_fit_robustNonnormal <- cfa(CFAmodel_simpel, data = dfSDERS, std.lv = TRUE, estimator = "MLM") 
summary(CFAmodel_simpel_fit_robustNonnormal, fit.measures = TRUE, standardize = TRUE)

# # dynamic cutoffs
# fits.single <- gen_fit(mod1 = CFAmodel_simpel, assume.mvn = FALSE, x = dfSDERS, rep = 500)
# flex_co(fits = fits.single, index = c("CFI", "SRMR", "RMSEA", "TLI")) # normal: 0.93555044 0.05786768 0.03762022 0.92604149, nonnormal: 0.54147437  0.06175211  0.02060497 -5.61844858 
# recommend(fits.single)
# # Alternative to this procedure
# dynamic::cfaHB(CFAmodel_simpel_fit, plot = TRUE)

# simulate performance of Hu & Bentler cutoffs for CFA model
# iterations <- 1000
# CFAsimOUt <- as.data.frame(matrix(nrow=iterations, ncol=4, dimnames=list(NULL, c("srmr", "rmsea", "cfi", "tli"))))
# CFAimpliedCorMat <- inspect(CFAmodel_simpel_fit, what="cor.ov")
# N = nrow(df)
# set.seed(1000)
# for(i in 1:iterations){
#   
#   CFAsimDat <- MASS::mvrnorm(n = N, mu = rep(0, nrow(CFAimpliedCorMat)), CFAimpliedCorMat)
#   CFAsimFit <- cfa(CFAmodel_simpel, data = CFAsimDat, std.lv = TRUE, estimator = "ML")
#   CFAsimOUt[i, ] <- fitMeasures(CFAsimFit, c("srmr", "rmsea", "cfi", "tli"))
#   
# }
# hist(CFAsimOUt[, "srmr"])
# hist(CFAsimOUt[, "rmsea"])
# hist(CFAsimOUt[, "cfi"])
# hist(CFAsimOUt[, "tli"])
# 
# nrow(CFAsimOUt[CFAsimOUt$srmr < .08 & CFAsimOUt$rmsea < .06 & CFAsimOUt$cfi > .95 & CFAsimOUt$tli > .95, ])






####################################
# EFA Lavender
# Note: Principal Axis Factoring is not available in Lavaan and, conversely, not all fit measures are available in other efa applications in R
# Therefore, we first fit the efa model with the exact specification of Lavender et al. in Psych:fa. Then, we use these as starting values
# for a Lavaan efa to get fit measures.

fa.parallel(dfSDERS, fm = "pa", fa = "fa", quant = .99)


# 4 factor model
EFAmodel4 <- fa(dfSDERS, nfactor = 4, rotate = "Promax", fm = "pa")
colnames(EFAmodel4$loadings) <- c("Nonacceptance", "Modulate", "Awareness", "Clarity")
print(EFAmodel4)
print(EFAmodel4$loadings,cutoff = 0.4, sort = TRUE)
print(EFAmodel4$loadings,cutoff = 0.3, sort = TRUE)
plot(EFAmodel4)
diagram(EFAmodel4)

# save table of loadings and structure coefficients created with custom function
save_as_docx(
  flextable(
    fa_loadingTable(EFAmodel4, cut = -1, structure = TRUE)
    ),
  path = here("manuscripts", "SDERSvalid_crossSec", "tables", "loadings.docx")
)

# BETTER function to make apa_table for factor loadings
# datscience::apa_factorLoadings(psych::fa(EFAmodel4, nfactors = 4))


fitMeasures_EFAmodel4 <- c(EFAmodel4$rms, 
                           EFAmodel4$RMSEA["RMSEA"], 
                           ((EFAmodel4$null.chisq-EFAmodel4$null.dof)-(EFAmodel4$STATISTIC-EFAmodel4$dof))/(EFAmodel4$null.chisq-EFAmodel4$null.dof), # CFI, Scource: https://gist.github.com/tonosan/cb7581f3459ae7c4217a
                           EFAmodel4$TLI,
                           EFAmodel4$STATISTIC, 
                           EFAmodel4$dof, 
                           EFAmodel4$PVAL)




### rotation of Lavender (promax plus pa)
esem_efaPromax <- esem_efa(data=dfSDERS, 
                           nfactors =4,
                           fm = 'pa',
                           rotate="promax",
                           residuals = TRUE)

esem_modelPromax <- esem_syntax(esem_efaPromax)
writeLines(esem_modelPromax)
esem_fitPromax <- cfa(model=esem_modelPromax, data=dfSDERS, std.lv=TRUE, estimator = "ML")

# inspect model (careful: P1 = Nonaccept, P2 = Awareness, P3 = Modulate, P4 = Clarity)
summary(esem_fitPromax, fit.measures = TRUE, standardized = TRUE)

# inspect fit measures
fitMeasures_ESEMmodel_promax <- fitMeasures(esem_fitPromax, c("srmr", "rmsea", "cfi", "tli", "AIC", "BIC", "chisq", "df", "pvalue"), output = "matrix")
fitMeasures_ESEMmodel_promax

semPaths(esem_fitPromax)

# fits.single <- FCO::gen_fit(mod1 = CFAmodel_simpel, mod2 = esem_modelPromax, x = dfSDERS, rep = 500) # commented out, because takes a while to run
# flex_co(fits = fits.single, index = c("CFI", "SRMR", "RMSEA", "TLI")) # 0.9576457 0.05515293 0.03877745 0.9346000


#inspect residuals
resid(esem_fitPromax, type = "standardized")
stanRes <- resid(esem_fitPromax, type="standardized")$cov
stanRes[upper.tri(stanRes)] <- NA 
stanRes <- melt(stanRes)
stanRes <- stanRes[!is.na(stanRes$value), ]
stanRes$p <- pnorm(abs(stanRes$value), lower.tail = FALSE)
stanRes$pHolm <- p.adjust(stanRes$p, method = "holm")
stanRes$pFDR <- p.adjust(stanRes$p, method = "fdr")
stanRes <- stanRes[order(stanRes$p), ]
stanRes[stanRes$pHolm <= .05, ]
stanRes[stanRes$pFDR <= .05, ]
stanRes[stanRes$p <= .01, ] # we see a residual structure that reflect the blend of "strategy" and "impulse" within modulate", but not with accurately controlled p-values



# Check latent correlations with DASS, Neuroticism and mental disorders
SDERSpsypathNames <- c(SDERSnames, "DASS_Total_mean", "NEO_N_mean", "MENT.HEALTH")
dfSDERS_psypath <- as.data.frame(scale(df[, SDERSpsypathNames]))

esem_modelPromax_psypath <- paste(esem_modelPromax, 
"PA1 ~~ DASS_Total_mean + NEO_N_mean + MENT.HEALTH
PA2 ~~ DASS_Total_mean + NEO_N_mean + MENT.HEALTH
PA3 ~~ DASS_Total_mean + NEO_N_mean + MENT.HEALTH
PA4 ~~ DASS_Total_mean + NEO_N_mean + MENT.HEALTH")

esem_fitPromax_psypath <- cfa(model=esem_modelPromax_psypath, data=dfSDERS_psypath, std.lv=TRUE, estimator = "ML")

# inspect model (careful: P1 = Nonaccept, P2 = Awareness, P3 = Modulate, P4 = Clarity)
summary(esem_fitPromax_psypath, fit.measures = TRUE, standardized = TRUE)

psypath_estimates <- as.data.frame(lavInspect(esem_fitPromax_psypath, "est")$psi[5:7, 1:4])
names(psypath_estimates) <- c("Non-Acceptance", "Awareness", "Modulate", "Clarity")
#psypath_estimates <- abs(psypath_estimates)

psypath_CIupper <- psypath_estimates + 1.96*lavInspect(esem_fitPromax_psypath, "se")$psi[5:7, 1:4]
psypath_CIlower <- psypath_estimates - 1.96*lavInspect(esem_fitPromax_psypath, "se")$psi[5:7, 1:4]

plotPsypath <- cbind(melt(psypath_estimates), melt(psypath_CIlower)[,2], melt(psypath_CIupper)[,2], rep(rownames(psypath_estimates), 4))
names(plotPsypath) <- c("Subscale", "Correlation", "CIlower", "CIupper", "Outcome")

ggplot(data=plotPsypath, aes(x=Subscale, y=Correlation, fill=Outcome)) +
  geom_bar(stat="identity") +
  theme_classic() +
  xlab(NULL) +
  scale_fill_discrete(name = NULL, labels = c("DASS", "Mental Disorder", "Neuroticism")) 

ggsave(here("manuscripts", "SDERSvalid_crossSec", "figures", "psypath_latentCorrs.svg"), device="svg", height=3.25, width=6)


# # simulate performance of Hu & Bentler cutoffs for EFA model
# iterations <- 1000
# EFAsimOUt <- as.data.frame(matrix(nrow=iterations, ncol=4, dimnames=list(NULL, c("srmr", "rmsea", "cfi", "tli"))))
# EFAimpliedCorMat <- EFAmodel4$loadings %*% EFAmodel4$Phi %*% t(EFAmodel4$loadings) + diag(EFAmodel4$uniquenesses)
# N = nrow(df)
# set.seed(1000)
# for(i in 1:iterations){
# 
#   EFAsimDat <- as.data.frame(MASS::mvrnorm(n = N, mu = rep(0, nrow(CFAimpliedCorMat)), EFAimpliedCorMat))
#   EFAsimFit <- psych::fa(EFAsimDat, nfactors = 4, rotate = "promax", fm = "pa")
#   EFAsimOUt[i, ] <- c(EFAsimFit$rms, EFAsimFit$RMSEA["RMSEA"], ((EFAsimFit$null.chisq-EFAsimFit$null.dof)-(EFAsimFit$STATISTIC-EFAsimFit$dof))/(EFAsimFit$null.chisq-EFAsimFit$null.dof), EFAsimFit$TLI)
# 
# 
# }
# hist(EFAsimOUt[, "srmr"])
# hist(EFAsimOUt[, "rmsea"])
# hist(EFAsimOUt[, "cfi"])
# hist(EFAsimOUt[, "tli"])
# 
# nrow(EFAsimOUt[EFAsimOUt$srmr < .08 & EFAsimOUt$rmsea < .06 & EFAsimOUt$cfi > .95 & EFAsimOUt$tli > .95, ])



####################################
### Hierarchical

esem_modelPromax_H <- paste(esem_modelPromax, "G =~ PA1 + PA2 + PA3 + PA4")
writeLines(esem_modelPromax_H)
esem_fitPromax_H <- cfa(model=esem_modelPromax_H, data=dfSDERS, std.lv=TRUE, estimator = "ML")
summary(esem_fitPromax_H, fit.measures = TRUE, standardized = TRUE)
semPaths(esem_fitPromax_H, what = "std")
fitMeasures_hierarchical <- fitMeasures(esem_fitPromax_H, c("srmr", "rmsea", "cfi", "tli", "AIC", "BIC", "chisq", "df", "pvalue"), output = "matrix")
fitMeasures_hierarchical



####################################
### Bifactor

CFAmodel_bifactor <- paste(CFAmodel_simpel, "\n", 
                            "G =~ ", paste(SDERSnames, collapse = " + "))
CFAmodel_bifactor <- gsub(x = CFAmodel_bifactor, pattern = "Clarity =~ S.DERS14_BL \\+ S.DERS7_BL", replacement =  "Clarity =~ a*S.DERS14_BL + b*S.DERS7_BL")
CFAmodel_bifactor <- paste(CFAmodel_bifactor, "\n a==b")

CFAmodel_bifactor_fit <- cfa(CFAmodel_bifactor, data = dfSDERS, orthogonal = TRUE, std.lv = TRUE, estimator = "ML") 
summary(CFAmodel_bifactor_fit, fit.measures = TRUE, standardize = TRUE)
semPaths(CFAmodel_bifactor_fit, what = "std")
fitMeasures_bifactor <- fitMeasures(CFAmodel_bifactor_fit, c("srmr", "rmsea", "cfi", "tli", "AIC", "BIC", "chisq", "df", "pvalue"), output = "matrix")
fitMeasures_bifactor

inspect(CFAmodel_bifactor_fit, what = "dx.all")


# vuong test does not work with the equality constraint for the clarity factor, which, in turn, is needed for identification
# fixing the loadings to the estimates from the model with the equality constraint fixes this issue (for the price of one df)
CFAmodel_bifactorVuong <- paste(CFAmodel_simpel, "\n", 
                           "G =~ ", paste(SDERSnames, collapse = " + "))
CFAmodel_bifactorVuong <- gsub(x = CFAmodel_bifactorVuong, pattern = "Clarity =~ S.DERS14_BL \\+ S.DERS7_BL", replacement =  "Clarity =~ 0.573*S.DERS14_BL + 0.573*S.DERS7_BL")
CFAmodel_bifactorVuong_fit <- cfa(CFAmodel_bifactorVuong, data = dfSDERS, orthogonal = TRUE, std.lv = TRUE, estimator = "ML") 


####################################
# model comparison

svg(file=here("manuscripts", "SDERSvalid_crossSec","figures", "CFA models.svg"))
par(mfrow = c(2, 2))
semPaths(CFAmodel_simpel_fit, nodeLabels = c(rep(NA, 21), "N", "M", "A", "C"))
mtext("a", side = 3, line = 1, adj = 0.1, font = 2, cex = 1.2)
semPaths(esem_fitPromax, nodeLabels = c(rep(NA, 21), "N", "M", "A", "C"))
mtext("b", side = 3, line = 1, adj = 0.1, font = 2, cex = 1.2)
semPaths(esem_fitPromax_H, nodeLabels = c(rep(NA, 21), "N", "M", "A", "C", "ED"))
mtext("c", side = 3, line = 1, adj = 0.1, font = 2, cex = 1.2)
semPaths(CFAmodel_bifactor_fit, nodeLabels = c(rep(NA, 21), "N", "M", "A", "C", "ED"))
mtext("d", side = 3, line = 1, adj = 0.1, font = 2, cex = 1.2)
dev.off()

net(esem_fitPromax, CFAmodel_simpel_fit)
vuong_CFAsimpVsEFA <- nonnest2::vuongtest(CFAmodel_simpel_fit, esem_fitPromax, nested=FALSE)
anova(esem_fitPromax, CFAmodel_simpel_fit)

net(esem_fitPromax, esem_fitPromax_H)
vuong_EFAvsEFAh <- nonnest2::vuongtest(esem_fitPromax, esem_fitPromax_H, nested=FALSE)
anova(esem_fitPromax, esem_fitPromax_H)

net(esem_fitPromax, CFAmodel_bifactor_fit)
vuong_EFAvsBifac <- nonnest2::vuongtest(esem_fitPromax, CFAmodel_bifactorVuong_fit, nested=FALSE)
anova(esem_fitPromax, CFAmodel_bifactor_fit)


FitCombined <- as.data.frame(
  round(
    rbind(t(fitMeasures_CFAmodel_simpel), t(fitMeasures_ESEMmodel_promax), t(fitMeasures_hierarchical), t(fitMeasures_bifactor)), 
    3), 
  row.names = c("CFA Simple Structure", "ESEM Cross-Loadings", "ESEM Higher-order", "Bifactor"))
FitCombined$chisq <- round(FitCombined$chisq, 1)
#http://127.0.0.1:24059/graphics/plot_zoom_png?width=1140&height=900
FitCombined$'Model Comparison' <- c("", "Model 2 > Model 1", "Model 2 > Model 3", "Model 2 > Model 4")
FitCombined$'P-Value' <- c("", 
                           round(vuong_CFAsimpVsEFA$p_LRT$B, 3),
                           round(vuong_EFAvsEFAh$p_LRT$A, 3),
                           round(vuong_EFAvsBifac$p_LRT$A, 3))
flextable(cbind(row.names(FitCombined), FitCombined))

save_as_docx(
    flextable(cbind(row.names(FitCombined), FitCombined)),
    path = here("manuscripts", "SDERSvalid_crossSec", "tables", "fitMeasures.docx")
)


####################################
# Network model

SDERSnetwork <- estimateNetwork(dfSDERS, default="EBICglasso", labels=c(1:21))
print(SDERSnetwork)
plot(SDERSnetwork, groups = as.factor(group_assignment))


# Define colors for the four groups
colors <- c("red", "blue", "green", "purple")

# Create a named vector for group assignments
group_assignment <- rep(NA, length(colnames(dfSDERS)))  # Initialize vector with NA
names(group_assignment) <- colnames(dfSDERS)  # Set the names to match your variable names

# Assign each variable to a group
group_assignment[main_loadings_list$NonAccept] <- "Non-Acceptance"
group_assignment[main_loadings_list$Modulate] <- "Modulate"
group_assignment[main_loadings_list$Awareness] <- "Awareness"
group_assignment[main_loadings_list$Clarity] <- "Clarity"

# Create a color vector based on group assignment
nodeColors <- colors[group_assignment]

# Plot the network with custom node colors
svg(file=here("manuscripts", "SDERSvalid_crossSec","figures", "network.svg"))
qgraph(SDERSnetwork$graph, layout = "spring", labels = c(1:21), groups = as.factor(group_assignment), palette="pastel")
dev.off


centralityPlot()
centralityPlot(SDERSnetwork$graph, scale = "raw0", include = c("Strength",
                                              "Closeness", "Betweenness", "ExpectedInfluence"))

############################################################################
# reliability

reliabilityEFACustom(EFAmodel4)
reliabilityEFACustom(EFAmodel4, forItems = main_loadings_list$NonAccept)
reliabilityEFACustom(EFAmodel4, forItems = main_loadings_list$Modulate)
reliabilityEFACustom(EFAmodel4, forItems = main_loadings_list$Awareness)
reliabilityEFACustom(EFAmodel4, forItems = main_loadings_list$Clarity)

cor.test(dfSDERS$S.DERS7_BL, dfSDERS$S.DERS14_BL)

############################################################################
# Correlations with other constructs

constructNames <- c("DERS_Nonacceptance_sum", "DERS_Awareness_sum", "DERS_Clarity_sum", "DERS_Strategies_sum", "DERS_Impulse_sum", "DERS_Goals_sum", "DERS_Total_sum",
                    "DASS_Total_sum", "NEO_N_sum", "reactivityValence", "AFFECT_PRE_X", "AFFECT_POST_X", "reactivityArousal", "AFFECT_PRE_Y", "AFFECT_POST_Y", "FAH_Total_sum", "BIS_Total_sum", "FFAF_Describe_sum", "FFAF_Awareness_sum")

corrOut <- as.data.frame(
  matrix(nrow=length(constructNames), ncol=length(sumScaleNames),
         dimnames = list(constructNames, sumScaleNames))
)

BayesOut <- as.data.frame(
  matrix(nrow=length(constructNames), ncol=length(sumScaleNames),
         dimnames = list(constructNames, sumScaleNames))
)

for(i in 1:length(constructNames)){
  
  if(constructNames[i] == "FFAF_Describe_sum" | constructNames[i] == "FFAF_Awareness_sum"){
    direction <- 2
  }else{
    direction <- 1
  }
  
  
  for(j in 1:length(sumScaleNames)){
    
    varConstruct <- df[, constructNames[i]]
    varSDERS <- df[, sumScaleNames[j]]
    
    # correlation
    corrOut[i,j] <- round(cor(varSDERS, varConstruct), 2)
    
    # Bayes factor
    BFsingle <- correlationBF(varSDERS, varConstruct, nullInterval = c(0, 1))
    BFsingle <- round(extractBF(BFsingle)[direction, "bf"], 1)
    BayesOut[i,j] <- ifelse(BFsingle > 100, ">100", BFsingle)
    
  }
}


critical.r.onesided <- function(n, alpha = .05) {
  df <- n - 2
  critical.t <- qt(alpha, df, lower.tail = F)
  critical.r <- sqrt( (critical.t^2) / ( (critical.t^2) + df ) )
  return(round(critical.r, 2))
}
# Example usage: Critical correlation coefficient at sample size of n = 100
critical.r.onesided(nrow(df), alpha = .05)
critical.r.onesided(nrow(df), alpha = .01)
critical.r.onesided(nrow(df), alpha = .001)

corrTab <- flextable(cbind(row.names(corrOut), corrOut))
corrTab <- add_footer_lines(corrTab, paste("r_crit for p < .05 (one-sided):", critical.r.onesided(nrow(df), alpha = .05), "\n",
                                           "r_crit for p < .01 (one-sided):", critical.r.onesided(nrow(df), alpha = .01), "\n",
                                           "r_crit for p < .001 (one-sided):", critical.r.onesided(nrow(df), alpha = .001), "\n"))

save_as_docx(
  corrTab,
  path = here("manuscripts", "SDERSvalid_crossSec", "tables", "constructValidity.docx")
)
corrTab



hist(df$BIS_Total_sum)
svg(file=here("manuscripts", "SDERSvalid_crossSec","figures", "BIScorrels.svg"))
corrplot(cor(df[, c(sumScaleNames, "BIS_NonPlanning_sum", "BIS_Motor_sum", "BIS_Attention_sum", "BIS_Total_sum")]))
dev.off()

cor(df[, c(sumScaleNames, "BIS_Attention_sum")])
cor.mtest(df[, c(sumScaleNames, "BIS_Attention_sum")])


corrplot(cor(df[, c(sumScaleNames, "reactivityValence")]))



#####################################
# convergent and discriminant validity of subscales

semTools::discriminantValidity(esem_fitPromax)



############################################################################
# Exploratory analysis: Weighted vs unweighted S-DERS

df$S.DERS_Total_mean_unweighted <- rowSums(df[, c("S.DERS_NonAccept_mean", "S.DERS_Modulate_mean", "DERS_Awareness_mean", "S.DERS_Clarity_mean")])

t.test(data=df, S.DERS_Total_mean_unweighted~MENT.HEALTH)
t.test(data=df, S.DERS_Total_mean~MENT.HEALTH)
cohen.d(data=df, S.DERS_Total_mean_unweighted~MENT.HEALTH)
cohen.d(data=df, S.DERS_Total_mean~MENT.HEALTH)

cor.test(df$S.DERS_Total_mean_unweighted, df$DASS_Total_mean)
cor.test(df$S.DERS_Total_mean, df$DASS_Total_mean)

cor.test(df$S.DERS_Total_mean_unweighted, df$NEO_N_mean)
cor.test(df$S.DERS_Total_mean, df$NEO_N_mean)


#####################################
# simulate data from lavender loading matrix

# load and prep data
LavLoad <- xlsx::read.xlsx(here("data", "LavenderEFAloadings.xlsx"), sheetIndex=1)
LavLoad <- LavLoad[1:21, 1:5]
for(i in 2:5){LavLoad[,i] <- as.numeric(LavLoad[,i])}
revInd <- LavLoad$Item %in% c(2,6,11,16,19)
LavLoad$Item <- paste0("S.DERS", LavLoad$Item, "_BL")
LavLoad$Item <- ifelse(revInd == TRUE, paste0(LavLoad$Item, ".r"), LavLoad$Item)
LavFactCorr <- matrix(c(1, .70, .09, .48, 
                        .70, 1, .04, .56, 
                        .09, .04, 1, -.18,
                        .48, .56, -.18, 1), ncol=4, nrow=4)


# prep cor matrix to sample from
LavMat <- as.matrix(LavLoad[,2:5]) %*% LavFactCorr %*% t(as.matrix(LavLoad[,2:5]))
LavMat <- LavMat + diag(1-diag(LavMat))
dimnames(LavMat) <- list(LavLoad$Item, LavLoad$Item)


# SIM 1: Lavender is true model
iterations <- 1000
simResults <- numeric(iterations)
for(i in 1:iterations){
  
  dfsim <- MASS::mvrnorm(n = nrow(df), mu = rep(0, nrow(LavMat)), Sigma = LavMat)
  paSimFit <- psych::fa(dfsim, nfactors=4, rotate = "promax", fm = "pa")
  simResults[i] <- sort(paSimFit$loadings["S.DERS17_BL", ], decreasing = TRUE)[2]
}

sum(simResults >= EFAmodel4$loadings["S.DERS17_BL",4])/length(simResults)
sum(simResults >= 0.3)/length(simResults)


# SIM 2: Lavender is estimated model
iterations <- 1000
simResults <- numeric(iterations)
for(i in 1:iterations){
  
  # estimte EFA for Lavender data
  dfsimLav <- MASS::mvrnorm(n = 484, mu = rep(0, nrow(LavMat)), Sigma = LavMat)
  paSimLav <- psych::fa(dfsimLav, nfactors=4, rotate = "promax", fm = "pa")
  paSimImpliedCov <- paSimLav$loadings %*% paSimLav$Phi %*% t(paSimLav$loadings) + diag(paSimLav$uniquenesses)
  
  # estimate EFA for our data
  dfsimUs <- MASS::mvrnorm(n = nrow(df), mu = rep(0, nrow(paSimImpliedCov)), Sigma = paSimImpliedCov)
  paSimUs <- psych::fa(dfsimUs, nfactors=4, rotate = "promax", fm = "pa")
  simResults[i] <- sort(paSimUs$loadings["S.DERS7_BL", ], decreasing = TRUE)[2]
}

hist(simResults)
mean(simResults)
sum(simResults >= EFAmodel4$loadings["S.DERS7_BL",3])/length(simResults)



# SIM 3: SAmple size simulations

# ESEM model
# does not work, because it changes between simulation runs which factor is
# PA1, PA2 and so on. E.g., Non-Acceptance can be either factor 1 or 2.
# There would be a routine necessary, which determines which factor is which
# and match this with the population values.


assign_factor_labels <- function(param_table){
  
  recode_table = param_table
  
  param_8 <- param_table[param_table$rhs == "S.DERS8_BL" & param_table$op == "=~", ]
  name8 <- param_8$lhs[which.max(abs(param_8$est))]
  recode_table$lhs <- ifelse(recode_table$lhs == name8, "Non-Acceptance", recode_table$lhs)
  recode_table$rhs <- ifelse(recode_table$rhs == name8, "Non-Acceptance", recode_table$rhs)
  
  param_6 <- param_table[param_table$rhs == "S.DERS6_BL.r" & param_table$op == "=~", ]
  name6 <- param_6$lhs[which.max(abs(param_6$est))]
  recode_table$lhs <- ifelse(recode_table$lhs == name6, "Awareness", recode_table$lhs)
  recode_table$rhs <- ifelse(recode_table$rhs == name6, "Awareness", recode_table$rhs)
  
  param_13 <- param_table[param_table$rhs == "S.DERS13_BL" & param_table$op == "=~", ]
  name13 <- param_13$lhs[which.max(abs(param_13$est))]
  recode_table$lhs <- ifelse(recode_table$lhs == name13, "Modulate", recode_table$lhs)
  recode_table$rhs <- ifelse(recode_table$rhs == name13, "Modulate", recode_table$rhs)
  
  param_14 <- param_table[param_table$rhs == "S.DERS14_BL" & param_table$op == "=~", ]
  name14 <- param_14$lhs[which.max(abs(param_14$est))]
  recode_table$lhs <- ifelse(recode_table$lhs == name14, "Clarity", recode_table$lhs)
  recode_table$rhs <- ifelse(recode_table$rhs == name14, "Clarity", recode_table$rhs)
  
  reorder_table <- recode_table[order(recode_table$lhs, recode_table$op, recode_table$rhs), ]
  
  return(reorder_table)
}



# build data generating model
esem_efaPromax_Lav <- esem_efa(data=LavMat, 
                           nfactors =4,
                           fm = 'pa',
                           rotate="promax",
                           residuals = TRUE)
print(esem_efaPromax_Lav, cutoff=0.3)


esem_modelPromax_Lav <- esem_syntax(esem_efaPromax_Lav)
writeLines(esem_modelPromax_Lav)
esem_fitPromax_Lav <- cfa(model=esem_modelPromax_Lav, sample.cov=LavMat, sample.nobs=484, std.lv=TRUE, estimator = "ML")

summary(esem_fitPromax_Lav)

population_parameters <- assign_factor_labels(parameterEstimates(esem_fitPromax_Lav))
simulation_parameters <- parTable(esem_fitPromax_Lav)

# initialize output dfs

iterations = 20
numPars = nrow(population_parameters)

sim_est <- matrix(nrow=numPars, ncol=iterations)
sim_se <- matrix(nrow=numPars, ncol=iterations)
sim_ci <- matrix(nrow=numPars, ncol=iterations)

t1 <- Sys.time()

# for(i in 1:iterations){
#   
#   sim_data <- simulateData(simulation_parameters, sample.nobs = 214)
#   
#   sim_efa <- esem_efa(data=sim_data, 
#                                  nfactors =4,
#                                  fm = 'pa',
#                                  rotate="promax",
#                                  residuals = TRUE)
#   
#   sim_esem_syntax <- esem_syntax(sim_efa)
#   
#   sim_esem <- cfa(model=sim_esem_syntax, data=sim_data, std.lv=TRUE, estimator = "ML")
#   
#   sim_parEst <- assign_factor_labels(parameterEstimates(sim_esem))
#   
#   sim_est[,i] <- sim_parEst$est
#   sim_se[,i] <- sim_parEst$se
#   
#   sim_ci[,i] <- as.numeric(population_parameters$est >= sim_parEst$ci.lower & population_parameters$est <= sim_parEst$ci.upper)
#   sim_ci[,i] <- ifelse(sim_parEst$ci.lower == sim_parEst$ci.upper, NA, sim_ci[,i])
#   
# }

iterations <- 10000
numPars <- nrow(population_parameters)

sim_est <- matrix(nrow = numPars, ncol = iterations)
sim_se  <- matrix(nrow = numPars, ncol = iterations)
sim_ci  <- matrix(nrow = numPars, ncol = iterations)

problem_flags <- rep(FALSE, iterations)

t1 <- Sys.time()

for(i in 1:iterations){
  
  sim_data <- simulateData(simulation_parameters, sample.nobs = 214)
  
  sim_efa <- esem_efa(data = sim_data, 
                      nfactors = 4,
                      fm = 'pa',
                      rotate = "promax",
                      residuals = TRUE)
  
  sim_esem_syntax <- esem_syntax(sim_efa)
  
  sim_esem <- tryCatch({
    cfa(model = sim_esem_syntax, data = sim_data, std.lv = TRUE, estimator = "ML")
  }, error = function(e) NULL)
  
  if (is.null(sim_esem) || 
      !lavInspect(sim_esem, "converged") || 
      lavInspect(sim_esem, "post.check") == FALSE) {
    
    problem_flags[i] <- TRUE
    next
  }
  
  sim_parEst <- tryCatch({
    assign_factor_labels(parameterEstimates(sim_esem))
  }, error = function(e) NULL)
  
  if (is.null(sim_parEst) || 
      any(is.na(sim_parEst$est)) || 
      any(abs(sim_parEst$est) > 10)) {
    
    problem_flags[i] <- TRUE
    next
  }
  
  sim_est[, i] <- sim_parEst$est
  sim_se[, i]  <- sim_parEst$se
  
  sim_ci[, i] <- as.numeric(population_parameters$est >= sim_parEst$ci.lower &
                              population_parameters$est <= sim_parEst$ci.upper)
  
  sim_ci[, i] <- ifelse(sim_parEst$ci.lower == sim_parEst$ci.upper, NA, sim_ci[, i])
}

t2 <- Sys.time()


sim_est_mean <- rowMeans(sim_est[, !problem_flags], na.rm=TRUE)
sim_se_mean <- rowMeans(sim_se[, !problem_flags], na.rm=TRUE)
sim_ci_prop <- rowSums(sim_ci[, !problem_flags], na.rm=TRUE)/rowSums(!is.na(sim_ci[, !problem_flags]))

all_params <- cbind(population_parameters, sim_est_mean, sim_se_mean, sim_ci_prop)
loading_params <- all_params[all_params$op == "=~", ]
#loading_params <- all_params

loading_params$loading_abs_bias <- round(loading_params$est - loading_params$sim_est_mean, 4)
loading_params$loading_rel_bias <- round(loading_params$loading_abs_bias/loading_params$est, 4)

loading_params$se_abs_bias <- round(loading_params$se - loading_params$sim_se_mean, 4)
loading_params$se_rel_bias <- round(loading_params$se_abs_bias/loading_params$se, 4)


bias_ind <- which(abs(loading_params$loading_abs_bias) > 0.05)

titles <- paste0(loading_params$lhs[bias_ind], loading_params$op[bias_ind], loading_params$rhs[bias_ind])  # or your own custom vector

par(mfrow = c(4, 3))
for (i in 1:10) {hist(sim_est[bias_ind[i], ], main = titles[i])}


sum(loading_params$se_rel_bias > 0.05, na.rm=TRUE)



mean(abs(loading_params$loading_abs_bias))

hist(sim_est[which.max(abs(loading_params$loading_abs_bias)), ])

which(sim_est == max(sim_est[which(abs(loading_params$loading_abs_bias) > 0.05), ]), arr.ind = TRUE)

sum(((population_parameters$est - sim_est_mean) / population_parameters$est > 0.05))
sum(((population_parameters$se - sim_se_mean) / population_parameters$se > 0.05), na.rm=TRUE)
sum(sim_ci_prop < .9, na.rm=TRUE)

cbind(population_parameters, sim_ci_prop)[sim_ci_prop < .9, ]



plot((population_parameters$est - sim_est_mean)/population_parameters$est, population_parameters$est)
hist((population_parameters$est - sim_est_mean)/population_parameters$est)


testbias <- cbind(population_parameters, sim_est_mean)
testbias$relBias <- round((testbias$est - testbias$sim_est_mean)/testbias$est, 3)

(-0.000227386 - 0.066226270)/-0.000227386 * 100

cbind(population_parameters, sim_est_mean)[which.max(((population_parameters$est - sim_est_mean) / population_parameters$est)), ]

custom_prob <- function(z_crit, p) {
  1 - pnorm(z_crit - qnorm(1 - p))
}

custom_prob(1.96, 0.001)


# simulate CFA model

CFAmodel_simpel <-   'NonAccept  =~ S.DERS8_BL + S.DERS4_BL + S.DERS1_BL + S.DERS5_BL + S.DERS12_BL + S.DERS20_BL + S.DERS18_BL
                      Modulate =~ S.DERS13_BL + S.DERS17_BL + S.DERS10_BL + S.DERS3_BL + S.DERS15_BL + S.DERS21_BL + S.DERS9_BL
                      Awareness =~ S.DERS6_BL.r + S.DERS11_BL.r + S.DERS2_BL.r + S.DERS19_BL.r + S.DERS16_BL.r
                      Clarity =~ S.DERS14_BL + S.DERS7_BL'

# Fit CFA model to real data
CFA_fit <- cfa(model = CFAmodel_simpel, sample.cov=LavMat, sample.nobs=484, std.lv = TRUE, estimator = "ML")

# Extract parameters for simulation
population_parameters <- parameterEstimates(CFA_fit)
simulation_parameters <- parTable(CFA_fit)

# Initialize output matrices
iterations <- 1000
numPars <- nrow(population_parameters)

sim_est <- matrix(nrow=numPars, ncol=iterations)
sim_se <- matrix(nrow=numPars, ncol=iterations)
sim_ci <- matrix(nrow=numPars, ncol=iterations)

t1 <- Sys.time()

# Simulation loop
for(i in 1:iterations){
  
  # Simulate data based on CFA model parameters
  sim_data <- simulateData(simulation_parameters, sample.nobs = 214)
  
  # Fit CFA model to simulated data
  sim_CFA <- cfa(model = CFAmodel_simpel, data = sim_data, std.lv = TRUE, estimator = "ML")
  
  # Extract parameter estimates
  sim_parEst <- parameterEstimates(sim_CFA)
  
  sim_est[,i] <- sim_parEst$est
  sim_se[,i] <- sim_parEst$se
  
  sim_ci[,i] <- as.numeric(population_parameters$est >= sim_parEst$ci.lower & population_parameters$est <= sim_parEst$ci.upper)
}

t2 <- Sys.time()

# Compute summary statistics
sim_est_mean <- rowMeans(sim_est)
sim_se_mean <- rowMeans(sim_se)
sim_ci_prop <- rowSums(sim_ci, na.rm=TRUE) / rowSums(!is.na(sim_ci))

# Compare simulated estimates to population parameters
sum(((population_parameters$est - sim_est_mean) / population_parameters$est > 0.05))
sum(((population_parameters$se - sim_se_mean) / population_parameters$se > 0.05), na.rm=TRUE)
sum(sim_ci_prop < .9, na.rm=TRUE)



###########################
# bi-factor model


bifactor_model <- '
  # General factor (weak for Awareness items)
  G =~ 0.52*S.DERS8_BL + 0.60*S.DERS4_BL + 0.41*S.DERS1_BL + 0.58*S.DERS5_BL + 0.47*S.DERS12_BL + 0.50*S.DERS20_BL + 0.40*S.DERS18_BL +
       0.44*S.DERS13_BL + 0.53*S.DERS17_BL + 0.59*S.DERS10_BL + 0.49*S.DERS3_BL + 0.57*S.DERS15_BL + 0.48*S.DERS21_BL + 0.42*S.DERS9_BL +
       0.11*S.DERS6_BL.r + 0.08*S.DERS11_BL.r + 0.12*S.DERS2_BL.r + 0.09*S.DERS19_BL.r + 0.07*S.DERS16_BL.r +
       0.50*S.DERS14_BL + 0.54*S.DERS7_BL

  # Specific factors (all strong, including Awareness)
  NonAccept =~ 0.45*S.DERS8_BL + 0.57*S.DERS4_BL + 0.51*S.DERS1_BL + 0.60*S.DERS5_BL + 0.42*S.DERS12_BL + 0.49*S.DERS20_BL + 0.56*S.DERS18_BL
  Modulate   =~ 0.58*S.DERS13_BL + 0.40*S.DERS17_BL + 0.55*S.DERS10_BL + 0.48*S.DERS3_BL + 0.60*S.DERS15_BL + 0.43*S.DERS21_BL + 0.52*S.DERS9_BL
  Awareness  =~ 0.53*S.DERS6_BL.r + 0.44*S.DERS11_BL.r + 0.60*S.DERS2_BL.r + 0.50*S.DERS19_BL.r + 0.47*S.DERS16_BL.r
  Clarity    =~ 0.45*S.DERS14_BL + 0.59*S.DERS7_BL

  # Uncorrelated factors
  G ~~ 0*NonAccept
  G ~~ 0*Modulate
  G ~~ 0*Awareness
  G ~~ 0*Clarity
  NonAccept ~~ 0*Modulate
  NonAccept ~~ 0*Awareness
  NonAccept ~~ 0*Clarity
  Modulate ~~ 0*Awareness
  Modulate ~~ 0*Clarity
  Awareness ~~ 0*Clarity
  
'



# Extract population parameters
population_parameters <- parameterEstimates(cfa(bifactor_model, data=simulateData(bifactor_model, sample.nobs=10000), std.lv = TRUE, estimator = "ML"))

# Initialize output
iterations <- 10
numPars <- nrow(population_parameters)

sim_est <- matrix(nrow = numPars, ncol = iterations)
sim_se  <- matrix(nrow = numPars, ncol = iterations)
sim_ci  <- matrix(nrow = numPars, ncol = iterations)
problem_flags <- rep(FALSE, iterations)

t1 <- Sys.time()

for (i in 1:iterations) {
  
  # Simulate data
  sim_data <- simulateData(bifactor_model, sample.nobs = 214)
  
  # Fit bifactor model to simulated data, with error handling
  sim_CFA <- tryCatch({
    cfa(model = CFAmodel_bifactor, data = sim_data, std.lv = TRUE, estimator = "ML")
  }, error = function(e) NULL)
  
  if (is.null(sim_CFA) || 
      !lavInspect(sim_CFA, "converged") || 
      lavInspect(sim_CFA, "post.check") == FALSE) {
    problem_flags[i] <- TRUE
    next
  }
  
  sim_parEst <- tryCatch({
    parameterEstimates(sim_CFA)
  }, error = function(e) NULL)
  
  if (is.null(sim_parEst) || 
      any(is.na(sim_parEst$est)) || 
      any(abs(sim_parEst$est) > 10)) {
    problem_flags[i] <- TRUE
    next
  }
  
  # Store results
  sim_est[, i] <- sim_parEst$est
  sim_se[, i]  <- sim_parEst$se
  
  sim_ci[, i] <- as.numeric(population_parameters$est >= sim_parEst$ci.lower & 
                              population_parameters$est <= sim_parEst$ci.upper)
  
  sim_ci[, i] <- ifelse(sim_parEst$ci.lower == sim_parEst$ci.upper, NA, sim_ci[, i])
}

t2 <- Sys.time()


sim_est_mean <- rowMeans(sim_est[, !problem_flags], na.rm=TRUE)
sim_se_mean <- rowMeans(sim_se[, !problem_flags], na.rm=TRUE)
sim_ci_prop <- rowSums(sim_ci[, !problem_flags], na.rm=TRUE)/rowSums(!is.na(sim_ci[, !problem_flags]))

all_params <- cbind(population_parameters, sim_est_mean, sim_se_mean, sim_ci_prop)
#loading_params <- all_params[all_params$op == "=~", ]
loading_params <- all_params

loading_params$loading_abs_bias <- round(loading_params$est - loading_params$sim_est_mean, 4)
loading_params$loading_rel_bias <- round(loading_params$loading_abs_bias/loading_params$est, 4)

loading_params$se_abs_bias <- round(loading_params$se - loading_params$sim_se_mean, 4)
loading_params$se_rel_bias <- round(loading_params$se_abs_bias/loading_params$se, 4)


bias_ind <- which(sim_ci_prop < 0.90)

titles <- paste0(loading_params$lhs[bias_ind], loading_params$op[bias_ind], loading_params$rhs[bias_ind])  # or your own custom vector

par(mfrow = c(4, 3))
for (i in 1:10) {hist(sim_est[bias_ind[i], ], main = titles[i])}



###########
# simsem

library(simsem)

CFAmodel_bifactor <- '
  # Specific factors
  NonAccept  =~ NA*S.DERS8_BL + S.DERS4_BL + S.DERS1_BL + S.DERS5_BL + S.DERS12_BL + S.DERS20_BL + S.DERS18_BL
  Modulate   =~ NA*S.DERS13_BL + S.DERS17_BL + S.DERS10_BL + S.DERS3_BL + S.DERS15_BL + S.DERS21_BL + S.DERS9_BL
  Awareness  =~ NA*S.DERS6_BL.r + S.DERS11_BL.r + S.DERS2_BL.r + S.DERS19_BL.r + S.DERS16_BL.r
  Clarity    =~ 0.5*S.DERS14_BL + S.DERS7_BL

  # General factor
  G =~ NA*S.DERS8_BL + S.DERS4_BL + S.DERS1_BL + S.DERS5_BL + S.DERS12_BL + S.DERS20_BL + S.DERS18_BL +
       S.DERS13_BL + S.DERS17_BL + S.DERS10_BL + S.DERS3_BL + S.DERS15_BL + S.DERS21_BL + S.DERS9_BL +
       S.DERS6_BL.r + S.DERS11_BL.r + S.DERS2_BL.r + S.DERS19_BL.r + S.DERS16_BL.r +
       S.DERS14_BL + S.DERS7_BL

  # Fix latent variances for identification
  G ~~ 1*G
  NonAccept ~~ 1*NonAccept
  Modulate ~~ 1*Modulate
  Awareness ~~ 1*Awareness
  Clarity ~~ 1*Clarity

  # Orthogonal latent factors (bifactor structure)
  G ~~ 0*NonAccept
  G ~~ 0*Modulate
  G ~~ 0*Awareness
  G ~~ 0*Clarity
  NonAccept ~~ 0*Modulate
  NonAccept ~~ 0*Awareness
  NonAccept ~~ 0*Clarity
  Modulate ~~ 0*Awareness
  Modulate ~~ 0*Clarity
  Awareness ~~ 0*Clarity
  
'


populationModel <- '
  # General factor
  G =~ 0.50*S.DERS8_BL + 0.50*S.DERS4_BL + 0.50*S.DERS1_BL + 
       0.50*S.DERS5_BL + 0.50*S.DERS12_BL + 0.50*S.DERS20_BL + 
       0.50*S.DERS18_BL + 0.50*S.DERS13_BL + 0.50*S.DERS17_BL + 
       0.50*S.DERS10_BL + 0.50*S.DERS3_BL + 0.50*S.DERS15_BL + 
       0.50*S.DERS21_BL + 0.50*S.DERS9_BL +
       0.50*S.DERS6_BL.r + 0.50*S.DERS11_BL.r + 0.50*S.DERS2_BL.r + 
       0.50*S.DERS19_BL.r + 0.50*S.DERS16_BL.r +
       0.50*S.DERS14_BL + 0.50*S.DERS7_BL

  # Specific factors
  NonAccept =~ 0.50*S.DERS8_BL + 0.50*S.DERS4_BL + 0.50*S.DERS1_BL + 
               0.50*S.DERS5_BL + 0.50*S.DERS12_BL + 0.50*S.DERS20_BL + 
               0.50*S.DERS18_BL
  Modulate   =~ 0.50*S.DERS13_BL + 0.50*S.DERS17_BL + 0.50*S.DERS10_BL + 
                0.50*S.DERS3_BL + 0.50*S.DERS15_BL + 0.50*S.DERS21_BL + 
                0.50*S.DERS9_BL
  Awareness  =~ 0.50*S.DERS6_BL.r + 0.50*S.DERS11_BL.r + 0.50*S.DERS2_BL.r + 
                0.50*S.DERS19_BL.r + 0.50*S.DERS16_BL.r
  Clarity    =~ 0.50*S.DERS14_BL + 0.50*S.DERS7_BL

  # Residual variances (1 - .5^2 - .5^2 = 0.5)
  S.DERS8_BL ~~ 0.5*S.DERS8_BL
  S.DERS4_BL ~~ 0.5*S.DERS4_BL
  S.DERS1_BL ~~ 0.5*S.DERS1_BL
  S.DERS5_BL ~~ 0.5*S.DERS5_BL
  S.DERS12_BL ~~ 0.5*S.DERS12_BL
  S.DERS20_BL ~~ 0.5*S.DERS20_BL
  S.DERS18_BL ~~ 0.5*S.DERS18_BL
  S.DERS13_BL ~~ 0.5*S.DERS13_BL
  S.DERS17_BL ~~ 0.5*S.DERS17_BL
  S.DERS10_BL ~~ 0.5*S.DERS10_BL
  S.DERS3_BL ~~ 0.5*S.DERS3_BL
  S.DERS15_BL ~~ 0.5*S.DERS15_BL
  S.DERS21_BL ~~ 0.5*S.DERS21_BL
  S.DERS9_BL ~~ 0.5*S.DERS9_BL
  S.DERS6_BL.r ~~ 0.5*S.DERS6_BL.r
  S.DERS11_BL.r ~~ 0.5*S.DERS11_BL.r
  S.DERS2_BL.r ~~ 0.5*S.DERS2_BL.r
  S.DERS19_BL.r ~~ 0.5*S.DERS19_BL.r
  S.DERS16_BL.r ~~ 0.5*S.DERS16_BL.r
  S.DERS14_BL ~~ 0.5*S.DERS14_BL
  S.DERS7_BL ~~ 0.5*S.DERS7_BL

  # Factor variances
  G ~~ 1*G
  NonAccept ~~ 1*NonAccept
  Modulate ~~ 1*Modulate
  Awareness ~~ 1*Awareness
  Clarity ~~ 1*Clarity

  # Factor covariances
  G ~~ 0*NonAccept
  G ~~ 0*Modulate
  G ~~ 0*Awareness
  G ~~ 0*Clarity
  NonAccept ~~ 0*Modulate
  NonAccept ~~ 0*Awareness
  NonAccept ~~ 0*Clarity
  Modulate ~~ 0*Awareness
  Modulate ~~ 0*Clarity
  Awareness ~~ 0*Clarity
'

results <- simsem::sim(n=214, generate=populationModel, model=CFAmodel_bifactor, lavaanfun="sem", nRep=10000, multicore=FALSE)

simsem::summaryConverge(results)
bifac__sim_results <- simsem::summaryParam(results, detail = TRUE, improper = FALSE)

sum(abs(bifac__sim_results$`Rel Bias`) > 0.05)

sum(abs(bifac__sim_results$`Rel SE Bias`) > 0.10)/nrow(bifac__sim_results)
median(bifac__sim_results$`Rel SE Bias`)

sum(bifac__sim_results$Coverage < 0.90)


####################
# ESEM with simsem


esem_efaPromax_Lav <- esem_efa(data=LavMat, 
                               nfactors =4,
                               fm = 'pa',
                               rotate="promax",
                               residuals = TRUE)
print(esem_efaPromax_Lav, cutoff=0.3)


esem_modelPromax_Lav <- esem_syntax(esem_efaPromax_Lav)
writeLines(esem_modelPromax_Lav)
esem_fitPromax_Lav <- cfa(model=esem_modelPromax_Lav, sample.cov=LavMat, sample.nobs=484, std.lv=TRUE, estimator = "ML")

summary(esem_fitPromax_Lav)

#population_parameters <- assign_factor_labels(parameterEstimates(esem_fitPromax_Lav))
simulation_parameters <- assign_factor_labels(parTable(esem_fitPromax_Lav))

efa_model <- '
efa("block1")*F1 + F2 + F3 + F4 =~ 
  S.DERS8_BL + S.DERS4_BL + S.DERS1_BL + S.DERS5_BL + S.DERS12_BL + S.DERS20_BL + S.DERS18_BL +
  S.DERS13_BL + S.DERS17_BL + S.DERS10_BL + S.DERS3_BL + S.DERS15_BL + S.DERS21_BL + S.DERS9_BL +
  S.DERS6_BL.r + S.DERS11_BL.r + S.DERS2_BL.r + S.DERS19_BL.r + S.DERS16_BL.r +
  S.DERS14_BL + S.DERS7_BL
'


results <- simsem::sim(n=214, generate=simulation_parameters, model=efa_model, lavaanfun="sem", nRep=10, multicore=FALSE)

simsem::summaryConverge(results)
simsem::summaryParam(results, detail = TRUE, improper = FALSE)



#####################################
# resources for later



# unit-weighted composite reliability from lavaan models:
# compRelSEM()

#This function computes Raykov's reliability coefficient (RRC) for factors from confirmatory factor analyses, 
# a measure which is commonly seen as a more accurate one than that of Cronbach's alpha which is computed based on the assumption of tau-equivalent measures. 
# It computes reliability coefficients for factors with and without correlated errors.
# relicoef(mod)

# discriminant validity in sem models: semTools::discriminantValidity.

