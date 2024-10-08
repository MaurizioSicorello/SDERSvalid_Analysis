######################################################################
# load packages and functions
library("lavaan")
library("here")

source(here::here("functions", "CFA_saveOrRead.r"))
source(here::here("functions", "esemMultilevelReliability.r"))
source(here::here("functions", "factorValidity.r"))


######################################################################
# Cross-validated psychometrics

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

ESEM4fixed_fit <- CFA_saveOrRead(ESEM4fixed, data = df, cluster = 'PARTICIPANT_ID', estimator = "MLR", filename = "ESEM4fixed_fit_pt2")
summary(ESEM4fixed_fit)

#####################################
# 8-item scale

ESEMlaiRelcustom(ESEM4fixed_fit, main_loadings_8item, clusterSize = 10)
LSTcustom(ESEM4fixed_fit, main_loadings_8item)
FactorValidityCustom(ESEM4fixed_fit, main_loadings_8item[c("NonAccept", "Awareness", "Modulate", "Clarity")])



#####################################
# 4-item scale

ESEMlaiRelcustom(ESEM4fixed_fit, main_loadings_4item)
LSTcustom(ESEM4fixed_fit, main_loadings_4item)
FactorValidityCustom(ESEM4fixed_fit, main_loadings_4item[c("NonAccept", "Awareness", "Modulate", "Clarity")])


#####################################
# Continue here: within-between correlations between old and new scales





