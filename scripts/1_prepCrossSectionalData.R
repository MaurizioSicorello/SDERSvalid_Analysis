#########################
# load packages
library("renv")
library("here")
library("stringr")
library("psych")
library("Hmisc")
library("misty")


#########################
# load data
df <- read.csv(here("data", "raw", "SDERS_data_crossSectional_27022024.csv"),
               na.strings = "<no-response>")

# remove deprecated variables
df <- df[,1:which(names(df) == "ABSCHLUSS_RT")]


#########################
# subset cases with consent and remove duplicates

# compute questionnare durations
orig_locale <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "C")
df$COMPLETED_TS <- as.POSIXct(df$COMPLETED_TS, format = "%d-%b-%Y %H:%M")
df$STARTED_TS <- as.POSIXct(df$STARTED_TS, format = "%d-%b-%Y %H:%M")
Sys.setlocale("LC_TIME", orig_locale)
df$questDuration <- df$COMPLETED_TS - df$STARTED_TS

# number of unique IDs
length(unique(df$PARTICIPANT_ID)) #232 unique registrations

# subset consent
df <- df[!is.na(df$EINWILLIGUNG), ]
length(unique(df$PARTICIPANT_ID)) #221 unique consents

# check missingness in S-DERS
table(apply(df[,which(names(df)=="S.DERS1_BL"):which(names(df)=="S.DERS21_BL_RT")],
            MARGIN=1,function(x){sum(is.na(x))})) # participants have either all complete or all missing data on S-DERS (232 vs 88)
sum(is.na(df$S.DERS21_BL)) # the last SDERS item can be used as a missingness indicator

# remove participants with missings
df <- df[!is.na(df$S.DERS21_BL), ] 

# inspect duplicates
sum(duplicated(df$PARTICIPANT_ID))
dupInd <- duplicated(df$PARTICIPANT_ID) | duplicated(df$PARTICIPANT_ID, fromLast = TRUE)
dfcheck <- df[dupInd, ]
dupIDs <- unique(dfcheck$PARTICIPANT_ID)
dfcheck[order(dfcheck$PARTICIPANT_ID), c("PARTICIPANT_ID", "questDuration")]

# remove duplicates
# using the duplicate function is not optimal, as some retained duplicates have unlogical questionnaire durations (e.g. = 1min).
# the following loop picks the questionnaire with the lowest difference to the average questionnaire duration
retainInd <- logical(nrow(df))
meanDur <- mean(df$questDuration)

for(i in 1:nrow(df)){
  
  if(dupInd[i] == FALSE){
    retainInd[i] <- TRUE
  }else{
    
    questDurNowDiff <- abs(df[i, "questDuration"] - meanDur)
    dfsub <- df[df$PARTICIPANT_ID == df[i,1], ]
    questDurMinDiff <- min(abs(dfsub$questDuration - meanDur))
    
    if(questDurNowDiff <= questDurMinDiff){
      retainInd[i] <- TRUE
    }else{
      retainInd[i] <- FALSE
    }
    
  }
  
}
sum(retainInd)

df <- df[retainInd, ]


# convert variables to numeric where appropriate
for(i in 1:ncol(df)){ df[,i] <- all.is.numeric(df[,i], what = "vector", extras=c('NA'))}

# compute SDERS-Duration
S.DERS_duration <- rowSums(df[, str_detect(names(df), "S.DERS") & str_detect(names(df), "_RT")])/60000

# compute Mood-Induction duration
MOOD_INDUCTION_duration <- (df$MOOD_INDUCTION_RT)/60000

# remove RT variables
df <- df[, !str_detect(names(df), "_RT")]



#########################
# score questionnaires


############
# FFAF

df <- item.reverse(FFAF12, FFAF16, FFAF22, FFAF5, FFAF8, FFAF13, FFAF18, FFAF23, FFAF28, FFAF34, FFAF38,
             data = df, min = 1, max = 5)
df$FFAF_Describe_sum <- rowSums(df[, c("FFAF2", "FFAF7", "FFAF12.r", "FFAF16.r", "FFAF22.r", "FFAF27", "FFAF32", "FFAF37")])
df$FFAF_Describe_mean <- rowMeans(df[, c("FFAF2", "FFAF7", "FFAF12.r", "FFAF16.r", "FFAF22.r", "FFAF27", "FFAF32", "FFAF37")])
df$FFAF_Awareness_sum <- rowSums(df[, c("FFAF5.r", "FFAF8.r", "FFAF13.r", "FFAF18.r", "FFAF23.r", "FFAF28.r", "FFAF34.r", "FFAF38.r")])
df$FFAF_Awareness_mean <- rowMeans(df[, c("FFAF5.r", "FFAF8.r", "FFAF13.r", "FFAF18.r", "FFAF23.r", "FFAF28.r", "FFAF34.r", "FFAF38.r")])


############
# NEO

df <- item.reverse(NEO1, NEO4, NEO7, NEO10,
                   data = df, min = 0, max = 4)
df$NEO_N_sum <- rowSums(df[, c('NEO1.r','NEO2','NEO3','NEO4.r','NEO5','NEO6','NEO7.r','NEO8','NEO9','NEO10.r','NEO11','NEO12')])
df$NEO_N_mean <- rowMeans(df[, c('NEO1.r','NEO2','NEO3','NEO4.r','NEO5','NEO6','NEO7.r','NEO8','NEO9','NEO10.r','NEO11','NEO12')])

range(df$NEO_N_mean)


############
# DERS

df <- item.reverse(DERS1, DERS2, DERS6, DERS7, DERS8, DERS10, DERS17, DERS20, DERS22, DERS24, DERS34,
                   data = df, min = 1, max = 5)
df$DERS_Nonacceptance_sum <- rowSums(df[, c('DERS11', 'DERS12', 'DERS21', 'DERS23', 'DERS25', 'DERS29')])
df$DERS_Nonacceptance_mean <- rowMeans(df[, c('DERS11', 'DERS12', 'DERS21', 'DERS23', 'DERS25', 'DERS29')])
df$DERS_Impulse_sum <- rowSums(df[, c('DERS3', 'DERS14', 'DERS19', 'DERS24.r', 'DERS27', 'DERS32')])
df$DERS_Impulse_mean <- rowMeans(df[, c('DERS3', 'DERS14', 'DERS19', 'DERS24.r', 'DERS27', 'DERS32')])
df$DERS_Awareness_sum <- rowSums(df[, c('DERS2.r','DERS6.r','DERS8.r','DERS10.r','DERS17.r','DERS34.r')])
df$DERS_Awareness_mean <- rowMeans(df[, c('DERS2.r','DERS6.r','DERS8.r','DERS10.r','DERS17.r','DERS34.r')])
df$DERS_Clarity_sum <- rowSums(df[, c('DERS1.r','DERS4','DERS5','DERS7.r','DERS9')])
df$DERS_Clarity_mean <- rowMeans(df[, c('DERS1.r','DERS4','DERS5','DERS7.r','DERS9')])
df$DERS_Strategies_sum <- rowSums(df[, c('DERS15','DERS16','DERS22.r','DERS28','DERS30','DERS31', 'DERS35', 'DERS36')])
df$DERS_Strategies_mean <- rowMeans(df[, c('DERS15','DERS16','DERS22.r','DERS28','DERS30','DERS31', 'DERS35', 'DERS36')])
df$DERS_Goals_sum <- rowSums(df[, c('DERS13','DERS18','DERS20.r','DERS26','DERS33')])
df$DERS_Goals_mean <- rowMeans(df[, c('DERS13','DERS18','DERS20.r','DERS26','DERS33')])

df$DERS_Total_sum <- rowSums(df[, c('DERS1.r','DERS2.r','DERS3','DERS4','DERS5','DERS6.r','DERS7.r','DERS8.r',
                                    'DERS9','DERS10.r','DERS11','DERS12','DERS13','DERS14','DERS15','DERS16',
                                    'DERS17.r','DERS18','DERS19','DERS20.r','DERS21','DERS22.r','DERS23','DERS24.r',
                                    'DERS25','DERS26','DERS27','DERS28','DERS29','DERS30','DERS31','DERS32',
                                    'DERS33','DERS34.r','DERS35','DERS36')])
df$DERS_Total_mean <- rowMeans(df[, c('DERS1.r','DERS2.r','DERS3','DERS4','DERS5','DERS6.r','DERS7.r','DERS8.r',
                                    'DERS9','DERS10.r','DERS11','DERS12','DERS13','DERS14','DERS15','DERS16',
                                    'DERS17.r','DERS18','DERS19','DERS20.r','DERS21','DERS22.r','DERS23','DERS24.r',
                                    'DERS25','DERS26','DERS27','DERS28','DERS29','DERS30','DERS31','DERS32',
                                    'DERS33','DERS34.r','DERS35','DERS36')])
df$DERS_Total_meanUnweighted <- rowMeans(df[, c('DERS_Nonacceptance_mean', 'DERS_Impulse_mean', 'DERS_Awareness_mean',
                                                'DERS_Clarity_mean', 'DERS_Strategies_mean', 'DERS_Goals_mean')])


############
# IERQ

df$IERQ_EnhancePA_sum <- rowSums(df[, c('IERQ13','IERQ18','IERQ8','IERQ3','IERQ6')])
df$IERQ_EnhancePA_mean <- rowMeans(df[, c('IERQ13','IERQ18','IERQ8','IERQ3','IERQ6')])
df$IERQ_Perspective_sum <- rowSums(df[, c('IERQ7','IERQ10','IERQ14','IERQ2','IERQ17')])
df$IERQ_Perspective_mean <- rowMeans(df[, c('IERQ7','IERQ10','IERQ14','IERQ2','IERQ17')])
df$IERQ_Soothing_sum <- rowSums(df[, c('IERQ4','IERQ19','IERQ16','IERQ12','IERQ9')])
df$IERQ_Soothing_mean <- rowMeans(df[, c('IERQ4','IERQ19','IERQ16','IERQ12','IERQ9')])
df$IERQ_Model_sum <- rowSums(df[, c('IERQ5','IERQ11','IERQ20','IERQ15','IERQ1')])
df$IERQ_Model_mean <- rowMeans(df[, c('IERQ5','IERQ11','IERQ20','IERQ15','IERQ1')])

df$IERQ_Total_sum <- rowSums(df[, c('IERQ1','IERQ2','IERQ3','IERQ4','IERQ5','IERQ6','IERQ7','IERQ8','IERQ9','IERQ10',
                                    'IERQ11','IERQ12','IERQ13','IERQ14','IERQ15','IERQ16','IERQ17','IERQ18','IERQ19','IERQ20')])
df$IERQ_Total_mean <- rowMeans(df[, c('IERQ1','IERQ2','IERQ3','IERQ4','IERQ5','IERQ6','IERQ7','IERQ8','IERQ9','IERQ10',
                                    'IERQ11','IERQ12','IERQ13','IERQ14','IERQ15','IERQ16','IERQ17','IERQ18','IERQ19','IERQ20')])
df$IERQ_Total_meanUnweighted <- rowMeans(df[, c("IERQ_EnhancePA_mean", "IERQ_Perspective_mean", "IERQ_Soothing_mean", "IERQ_Model_mean")])


############
# DASS-21

df$DASS_Depression_sum <- rowSums(df[, c('DASS3','DASS5','DASS10','DASS13','DASS16','DASS17','DASS21')])
df$DASS_Depression_mean <- rowMeans(df[, c('DASS3','DASS5','DASS10','DASS13','DASS16','DASS17','DASS21')])
df$DASS_Anxiety_sum <- rowSums(df[, c('DASS2','DASS4','DASS7','DASS9','DASS15','DASS19','DASS20')])
df$DASS_Anxiety_mean <- rowMeans(df[, c('DASS2','DASS4','DASS7','DASS9','DASS15','DASS19','DASS20')])
df$DASS_Stress_sum <- rowSums(df[, c('DASS1','DASS6','DASS8','DASS11','DASS12','DASS14','DASS18')])
df$DASS_Stress_mean <- rowMeans(df[, c('DASS1','DASS6','DASS8','DASS11','DASS12','DASS14','DASS18')])

df$DASS_Total_sum <- rowSums(df[, c('DASS1','DASS2','DASS3','DASS4','DASS5','DASS6','DASS7',
                                 'DASS8','DASS9','DASS10','DASS11','DASS12','DASS13','DASS14',
                                 'DASS15','DASS16','DASS17','DASS18','DASS19','DASS20','DASS21')])
df$DASS_Total_mean <- rowMeans(df[, c('DASS1','DASS2','DASS3','DASS4','DASS5','DASS6','DASS7',
                                 'DASS8','DASS9','DASS10','DASS11','DASS12','DASS13','DASS14',
                                 'DASS15','DASS16','DASS17','DASS18','DASS19','DASS20','DASS21')])


############
# CTQ

df <- item.reverse(CTQ5, CTQ7, CTQ13, CTQ19, CTQ28, CTQ2, CTQ26,
                   data = df, min = 1, max = 5)

df$CTQ_EmotionAbuse_sum <- rowSums(df[, c('CTQ3','CTQ8','CTQ14','CTQ18','CTQ25')])
df$CTQ_EmotionAbuse_mean <- rowMeans(df[, c('CTQ3','CTQ8','CTQ14','CTQ18','CTQ25')])
df$CTQ_PhysicalAbuse_sum <- rowSums(df[, c('CTQ9','CTQ11','CTQ12','CTQ15','CTQ17')])
df$CTQ_PhysicalAbuse_mean <- rowMeans(df[, c('CTQ9','CTQ11','CTQ12','CTQ15','CTQ17')])
df$CTQ_SexualAbuse_sum <- rowSums(df[, c('CTQ20','CTQ21','CTQ23','CTQ24','CTQ27')])
df$CTQ_SexualAbuse_mean <- rowMeans(df[, c('CTQ20','CTQ21','CTQ23','CTQ24','CTQ27')])
df$CTQ_EmotionNeglect_sum <- rowSums(df[, c('CTQ5.r','CTQ7.r','CTQ13.r','CTQ19.r','CTQ28.r')])
df$CTQ_EmotionNeglect_mean <- rowMeans(df[, c('CTQ5.r','CTQ7.r','CTQ13.r','CTQ19.r','CTQ28.r')])
df$CTQ_PhysicalNeglect_sum <- rowSums(df[, c('CTQ1','CTQ2.r','CTQ4','CTQ6','CTQ26.r')])
df$CTQ_PhysicalNeglect_mean <- rowMeans(df[, c('CTQ1','CTQ2.r','CTQ4','CTQ6','CTQ26.r')])

df$CTQ_Total_sum <- rowSums(df[, c("CTQ_EmotionAbuse_sum", "CTQ_PhysicalAbuse_sum", "CTQ_SexualAbuse_sum",
                                   "CTQ_EmotionNeglect_sum", "CTQ_PhysicalNeglect_sum")])
df$CTQ_Total_mean <- rowMeans(df[, c("CTQ_EmotionAbuse_mean", "CTQ_PhysicalAbuse_mean", "CTQ_SexualAbuse_mean",
                                   "CTQ_EmotionNeglect_mean", "CTQ_PhysicalNeglect_mean")])



############
# BIS

df <- item.reverse(BIS1, BIS4, BIS5, BIS7, BIS8, BIS15,
                   data = df, min = 1, max = 4)

df$BIS_NonPlanning_sum <- rowSums(df[, c('BIS1.r','BIS5.r','BIS7.r','BIS8.r','BIS15.r')])
df$BIS_NonPlanning_mean <- rowMeans(df[, c('BIS1.r','BIS5.r','BIS7.r','BIS8.r','BIS15.r')])
df$BIS_Motor_sum <- rowSums(df[, c('BIS2','BIS9','BIS10','BIS12','BIS13')])
df$BIS_Motor_mean <- rowMeans(df[, c('BIS2','BIS9','BIS10','BIS12','BIS13')])
df$BIS_Attention_sum <- rowSums(df[, c('BIS3','BIS4.r','BIS6','BIS11','BIS14')])
df$BIS_Attention_mean <- rowMeans(df[, c('BIS3','BIS4.r','BIS6','BIS11','BIS14')])

df$BIS_Total_sum <- rowSums(df[, c('BIS1.r','BIS2','BIS3','BIS4.r','BIS5.r',
                                   'BIS6','BIS7.r','BIS8.r','BIS9','BIS10',
                                   'BIS11','BIS12','BIS13','BIS14','BIS15.r')])
df$BIS_Total_mean <- rowMeans(df[, c('BIS1.r','BIS2','BIS3','BIS4.r','BIS5.r',
                                   'BIS6','BIS7.r','BIS8.r','BIS9','BIS10',
                                   'BIS11','BIS12','BIS13','BIS14','BIS15.r')])


############
# FAH

df$FAH_Total_sum <- rowSums(df[, c('FAH1','FAH2','FAH3','FAH4','FAH5','FAH6','FAH7')])
df$FAH_Total_Mean <- rowMeans(df[, c('FAH1','FAH2','FAH3','FAH4','FAH5','FAH6','FAH7')])


############
# S-DERS

df <- item.reverse(S.DERS2_BL, S.DERS6_BL, S.DERS11_BL, S.DERS16_BL, S.DERS19_BL,
                   data = df, min = 1, max = 5)

df$S.DERS_NonAccept_sum <- rowSums(df[, c('S.DERS8_BL','S.DERS4_BL','S.DERS1_BL','S.DERS5_BL','S.DERS12_BL','S.DERS20_BL','S.DERS18_BL')])
df$S.DERS_NonAccept_mean <- rowMeans(df[, c('S.DERS8_BL','S.DERS4_BL','S.DERS1_BL','S.DERS5_BL','S.DERS12_BL','S.DERS20_BL','S.DERS18_BL')])
df$S.DERS_Modulate_sum <- rowSums(df[, c('S.DERS13_BL','S.DERS17_BL','S.DERS10_BL','S.DERS3_BL','S.DERS15_BL','S.DERS21_BL','S.DERS9_BL')])
df$S.DERS_Modulate_mean <- rowMeans(df[, c('S.DERS13_BL','S.DERS17_BL','S.DERS10_BL','S.DERS3_BL','S.DERS15_BL','S.DERS21_BL','S.DERS9_BL')])
df$S.DERS_Awareness_sum <- rowSums(df[, c('S.DERS6_BL.r','S.DERS11_BL.r','S.DERS2_BL.r','S.DERS19_BL.r','S.DERS16_BL.r')])
df$S.DERS_Awareness_mean <- rowMeans(df[, c('S.DERS6_BL.r','S.DERS11_BL.r','S.DERS2_BL.r','S.DERS19_BL.r','S.DERS16_BL.r')])
df$S.DERS_Clarity_sum <- rowSums(df[, c('S.DERS14_BL','S.DERS7_BL')])
df$S.DERS_Clarity_mean <- rowSums(df[, c('S.DERS14_BL','S.DERS7_BL')])

df$S.DERS_Total_sum <- rowSums(df[, c('S.DERS1_BL','S.DERS2_BL.r','S.DERS3_BL','S.DERS4_BL','S.DERS5_BL','S.DERS6_BL.r','S.DERS7_BL',
                                      'S.DERS8_BL','S.DERS9_BL','S.DERS10_BL','S.DERS11_BL.r','S.DERS12_BL','S.DERS13_BL','S.DERS14_BL',
                                      'S.DERS15_BL','S.DERS16_BL.r','S.DERS17_BL','S.DERS18_BL','S.DERS19_BL.r','S.DERS20_BL','S.DERS21_BL')])
df$S.DERS_Total_mean <- rowMeans(df[, c('S.DERS1_BL','S.DERS2_BL.r','S.DERS3_BL','S.DERS4_BL','S.DERS5_BL','S.DERS6_BL.r','S.DERS7_BL',
                                      'S.DERS8_BL','S.DERS9_BL','S.DERS10_BL','S.DERS11_BL.r','S.DERS12_BL','S.DERS13_BL','S.DERS14_BL',
                                      'S.DERS15_BL','S.DERS16_BL.r','S.DERS17_BL','S.DERS18_BL','S.DERS19_BL.r','S.DERS20_BL','S.DERS21_BL')])

#########################
# remove all objects except df
rm(list=setdiff(ls(), "df"))



#########################
# save data

if(file.exists(here("data", "SDERSvalid_crossSec_data_preprocessed.csv"))){
  warning("file already exists!")
}else{
  write.csv(df, here("data", "SDERSvalid_crossSec_data_preprocessed.csv"), row.names = FALSE)
}






