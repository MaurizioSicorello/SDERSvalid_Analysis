

library("here")
library("nlme")
library("psych")
library("plyr")
library("stringr")
library("flextable")
library("corrplot")
library("reshape2")
library("ggplot2")

df <- read.csv(here::here("data", "SDERSvalid_DailyLife_data_preprocessed.csv"))

source(here::here("functions", "descriptivesWeighted.r"))
source(here::here("functions", "muthenDecomposition.r"))
source(here::here("functions", "multilevelSyntaxHelper.r"))
source(here::here("functions", "efaFitMeasures.r"))
source(here::here("functions", "CFA_saveOrRead.r"))
source(here::here("functions", "esemMultilevelReliability.r"))



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
# Descriptive statistics





#####################################
# sample descriptives (cross-sectional)


names(df)

participant_counts <- as.data.frame(table(df$PARTICIPANT_ID))
participant_counts$perc <- participant_counts$Freq/60

hist(participant_counts$perc)
describe(participant_counts$perc)
describe(participant_counts$Freq)

dfcrossSec <- unique(df[, c("PARTICIPANT_ID", "SEX", "AGE", "OCCUPATION", "ACAD.DEGREE", "questDuration")])
dfcrossSec$questDuration <- dfcrossSec$questDuration/60



hist(dfcrossSec$questDuration)

#dftest <- unique(dfHealth[, c("PARTICIPANT_ID", "MENT.HEALTH")])
#round(table(dftest$MENT.HEALTH)/sum(table(dftest$MENT.HEALTH))*100, 1) # occupation

round(table(dfcrossSec$OCCUPATION)/sum(table(dfcrossSec$OCCUPATION))*100, 1) # occupation
round(table(dfcrossSec$SEX)/sum(table(dfcrossSec$SEX))*100, 1) # sex

psych::describe(dfcrossSec)

names(df)



#####################################

describe(dfSDERS)

par(mfrow = c(3, 3))
for(i in 1:7){hist(dfSDERS[,i])}
par(mfrow = c(3, 3))
for(i in 8:14){hist(dfSDERS[,i])}
par(mfrow = c(3, 3))
for(i in 15:21){hist(dfSDERS[,i])}
par(mfrow = c(1,1))

# calculate mean, SD and ranges for all variables
dfDescriptiveItems <- data.frame(
  matrix(
    nrow=length(SDERSnames),
    ncol=4,
    dimnames = list(SDERSnames, c("Mean", "SDbetween", "SDwithin", "Range"))
  )
)
for(i in 1:length(SDERSnames)){ dfDescriptiveItems[i,] <- descriptivesWeighted(x = SDERSnames[i]) }


# decomposition of covariance matrices based on Muthen 1994, published by: https://francish.netlify.app/docs/MCFAinRHUANG.pdf
mcfaInput <- mcfa.input(l2Var = "PARTICIPANT_ID", dat = dfSDERS_ID) 
combined.cov <- list(within = mcfaInput$pw.cov, between = mcfaInput$b.cov)
combined.n <- list(within = mcfaInput$n - mcfaInput$G, between = mcfaInput$G)


# ICCs and correlations
statsByGroup <- statsBy(dfSDERS_ID, group="PARTICIPANT_ID")
dfDescriptiveItems$ICC <- paste0(round(statsByGroup$ICC1[-1], 2), " [", round(statsByGroup$ci1[-1,], 2)[,1], ", ", round(statsByGroup$ci1[-1,], 2)[,2], "]")
dfcorrItems <- round(lowerUpper(lower=statsByGroup$rbg, upper=statsByGroup$rwg, diff=FALSE), 2)
dfcorrItems <- ifelse(is.na(dfcorrItems), "-", dfcorrItems)
dfDescriptiveItems <- data.frame(dfDescriptiveItems, dfcorrItems)
names(dfDescriptiveItems)[str_detect(names(dfDescriptiveItems), "S.DERS")] <- as.character(c(1:21))

flextable::save_as_docx(flextable::flextable(dfDescriptiveItems),path = here::here("manuscripts", "SDERSvalid_dailyLife", "tables", "DescriptivesByItem.docx"))


# Corrplots
corrplot(mcfaInput$pw.cor, method = "square", order = 'hclust', addrect = 5)
corrplot(mcfaInput$ab.cor, method = "square", order = 'hclust', addrect = 5)

corrplot(mcfaInput$pw.cor[itemOrder,itemOrder], method = "square")
corrplot(mcfaInput$ab.cor[itemOrder,itemOrder], method = "square")

itemOrder_small <- itemOrder[-(which(itemOrder %in% c("S.DERS10_ESM", "S.DERS9_ESM", "S.DERS15_ESM")))]

corrplot(mcfaInput$pw.cor[itemOrder_small,itemOrder_small], method = "square", order = 'hclust', addrect = 4)
corrplot(mcfaInput$ab.cor[itemOrder_small,itemOrder_small], method = "square", order = 'hclust', addrect = 4)

# within
corrM_within <- mcfaInput$pw.cor[itemOrder,itemOrder]

svg(file=here::here("manuscripts", "SDERSvalid_dailyLife","figures", "corrPlot_within.svg"))
rownames(corrM_within) <- as.numeric(unlist(str_extract_all(rownames(corrM_within), "\\d+")))
colnames(corrM_within) <- as.numeric(unlist(str_extract_all(colnames(corrM_within), "\\d+")))
corrplot(corrM_within, method = "square", tl.col="black", tl.srt = 0)
dev.off()


# between
corrM_between <- mcfaInput$ab.cor[itemOrder,itemOrder]

svg(file=here::here("manuscripts", "SDERSvalid_dailyLife","figures", "corrPlot_between.svg"))
rownames(corrM_between) <- as.numeric(unlist(str_extract_all(rownames(corrM_between), "\\d+")))
colnames(corrM_between) <- as.numeric(unlist(str_extract_all(colnames(corrM_between), "\\d+")))
corrplot(corrM_between, method = "square", tl.col="black", tl.srt = 0)
dev.off()


# mood induction

dfmoodInd <- read.csv(here::here("data", "SDERSvalid_crossSec_data_preprocessed.csv"))
dfmoodInd <- dfmoodInd[, str_replace(itemOrder, "ESM", "BL")]


corrM_moodInd <- cor(dfmoodInd)[str_replace(itemOrder, "ESM", "BL"),str_replace(itemOrder, "ESM", "BL")]

svg(file=here::here("manuscripts", "SDERSvalid_dailyLife","figures", "corrPlot_moodInd.svg"))
rownames(corrM_moodInd) <- as.numeric(unlist(str_extract_all(rownames(corrM_moodInd), "\\d+")))
colnames(corrM_moodInd) <- as.numeric(unlist(str_extract_all(colnames(corrM_moodInd), "\\d+")))
corrplot(corrM_moodInd, method = "square", tl.col="black", tl.srt = 0)
dev.off()
