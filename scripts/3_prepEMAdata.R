############################################################
# read packages

library("here")
library("plyr")
library("stringr")
library("Hmisc")
library("misty")

############################################################
# read in files

files <- dir(here::here("data", "raw", "ema"))
varNames <- names(read.csv(here::here("data", "raw", "ema", files[1])))
readAll <- setNames(data.frame(matrix(ncol = length(varNames), nrow = 0)), varNames)

for(i in 1:length(files)){
  
  readTemp <- read.csv(here::here("data", "raw", "ema", files[i]), na.strings = "<no-response>")
  
  # tests
  print(paste("Dataframe", i))
  
  # correct number of columns
  if(ncol(readTemp) == ncol(readAll)){
    print("---equal number of columns")
  }else{
    print("---unequal number of columns!")
  }
  
  # n unique IDs per df
  print(paste0("---", length(unique(readTemp$PARTICIPANT_ID)), "participants!"))

  
  # duplicates between dfs
  if(sum(unique(readTemp$PARTICIPANT_ID) %in% unique(readAll$PARTICIPANT_ID)) == 0 ){
    print("---no duplicates")
  }else{
    print("---duplicates in participants!")
  }
  
  readAll <- rbind(readAll, readTemp)
  
}


############################################################
# remove variables


# variables with NAs
apply(readAll, 2, function(x){sum(is.na(x))})

# remove deprecated variables
readAll <- readAll[, -(which(names(readAll) == "S.DERS.TEXT"):ncol(readAll))]


# compute questionnare durations
orig_locale <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "C")
readAll$COMPLETED_TS <- as.POSIXct(readAll$COMPLETED_TS, format = "%d-%b-%Y %H:%M")
readAll$STARTED_TS <- as.POSIXct(readAll$STARTED_TS, format = "%d-%b-%Y %H:%M")
Sys.setlocale("LC_TIME", orig_locale)
readAll$questDuration <- as.numeric(readAll$COMPLETED_TS - readAll$STARTED_TS)


# remove RT and other useless variables
readAll <- readAll[, !str_detect(names(readAll), "_RT")]
readAll <- readAll[, !(names(readAll) %in% c("ESM_EINLEITUNG", "S.DERS.INSTRUKTION", "IER_INSTRUKTION", "IER_FRAGE", "ESM_ABSCHLUSS", "STUDY_ID", "STUDY_NAME", "STUDY_VERSION", "SURVEY_ID", "ESM", "TRIGGER", "START_END", "RAND_PROB", "SURVEY_NAME"))]



############################################################
# remove questionnaires

# number of unique participants
length(unique(readAll$PARTICIPANT_ID)) # 240
nrow(readAll) # 14335

# remove missed questionnaires
readAll <- readAll[readAll$EXPIRED_TS == "", ]
length(unique(readAll$PARTICIPANT_ID)) # 229
nrow(readAll) # 9226

# remove questionnaires with missings on S-DERS
readAll <- readAll[complete.cases(readAll[, str_detect(names(readAll), "S.DERS")]), ]
length(unique(readAll$PARTICIPANT_ID)) # 229
nrow(readAll) # 8710

# remove aborted questionnaires
readAll <- readAll[!is.na(readAll$COMPLETED_TS), ]

# sort dataframe
readAll <- readAll[base::order(readAll$PARTICIPANT_ID, readAll$COMPLETED_TS), ]

# day and occasion indicators
readAll$Date <- as.Date(readAll$COMPLETED_TS)
readAll$Time <- format(readAll$COMPLETED_TS, "%H:%M:%S")
readAll <- plyr::ddply(readAll, .(PARTICIPANT_ID), mutate, DayInd = as.numeric(as.factor(Date)))
readAll <- plyr::ddply(readAll, .(PARTICIPANT_ID, DayInd), mutate, OccInd = as.numeric(as.factor(Time)))


# create lagged completed time
readAll <- ddply(readAll, .(PARTICIPANT_ID, DayInd), mutate, COMPLETED_TS_lag = dplyr::lag(COMPLETED_TS))
readAll$timeDiffToPrevious <- readAll$COMPLETED_TS - readAll$COMPLETED_TS_lag
readAll <- readAll[!(!is.na(readAll$timeDiffToPrevious) & readAll$timeDiffToPrevious < 15), ] # exclude two questionnaires filled in 1 and 3 minutes apart



############################################################
# Compute Scale values

# convert to numeric
str(readAll)
for(i in 1:ncol(readAll)){ readAll[,i] <- all.is.numeric(readAll[,i], what = "vector", extras=c('NA'))}
str(readAll)

# reverse score items
readAll <- item.reverse(S.DERS2_ESM, S.DERS6_ESM, S.DERS11_ESM, S.DERS16_ESM, S.DERS19_ESM,
                   data = readAll, min = 1, max = 5)

# composite score items
readAll$S.DERS_NonAccept_EMAsum <- rowSums(readAll[, c('S.DERS8_ESM','S.DERS4_ESM','S.DERS1_ESM','S.DERS5_ESM','S.DERS12_ESM','S.DERS20_ESM','S.DERS18_ESM')])
readAll$S.DERS_NonAccept_EMAmean <- rowMeans(readAll[, c('S.DERS8_ESM','S.DERS4_ESM','S.DERS1_ESM','S.DERS5_ESM','S.DERS12_ESM','S.DERS20_ESM','S.DERS18_ESM')])
readAll$S.DERS_Modulate_EMAsum <- rowSums(readAll[, c('S.DERS13_ESM','S.DERS17_ESM','S.DERS10_ESM','S.DERS3_ESM','S.DERS15_ESM','S.DERS21_ESM','S.DERS9_ESM')])
readAll$S.DERS_Modulate_EMAmean <- rowMeans(readAll[, c('S.DERS13_ESM','S.DERS17_ESM','S.DERS10_ESM','S.DERS3_ESM','S.DERS15_ESM','S.DERS21_ESM','S.DERS9_ESM')])
readAll$S.DERS_Awareness_EMAsum <- rowSums(readAll[, c('S.DERS6_ESM.r','S.DERS11_ESM.r','S.DERS2_ESM.r','S.DERS19_ESM.r','S.DERS16_ESM.r')])
readAll$S.DERS_Awareness_EMAmean <- rowMeans(readAll[, c('S.DERS6_ESM.r','S.DERS11_ESM.r','S.DERS2_ESM.r','S.DERS19_ESM.r','S.DERS16_ESM.r')])
readAll$S.DERS_Clarity_EMAsum <- rowSums(readAll[, c('S.DERS14_ESM','S.DERS7_ESM')])
readAll$S.DERS_Clarity_EMAmean <- rowSums(readAll[, c('S.DERS14_ESM','S.DERS7_ESM')])

readAll$S.DERS_Total_EMAsum <- rowSums(readAll[, c('S.DERS1_ESM','S.DERS2_ESM.r','S.DERS3_ESM','S.DERS4_ESM','S.DERS5_ESM','S.DERS6_ESM.r','S.DERS7_ESM',
                                      'S.DERS8_ESM','S.DERS9_ESM','S.DERS10_ESM','S.DERS11_ESM.r','S.DERS12_ESM','S.DERS13_ESM','S.DERS14_ESM',
                                      'S.DERS15_ESM','S.DERS16_ESM.r','S.DERS17_ESM','S.DERS18_ESM','S.DERS19_ESM.r','S.DERS20_ESM','S.DERS21_ESM')])
readAll$S.DERS_Total_EMAmean <- rowMeans(readAll[, c('S.DERS1_ESM','S.DERS2_ESM.r','S.DERS3_ESM','S.DERS4_ESM','S.DERS5_ESM','S.DERS6_ESM.r','S.DERS7_ESM',
                                        'S.DERS8_ESM','S.DERS9_ESM','S.DERS10_ESM','S.DERS11_ESM.r','S.DERS12_ESM','S.DERS13_ESM','S.DERS14_ESM',
                                        'S.DERS15_ESM','S.DERS16_ESM.r','S.DERS17_ESM','S.DERS18_ESM','S.DERS19_ESM.r','S.DERS20_ESM','S.DERS21_ESM')])



############################################################
# merge with cross-sectional df

# load data and subset variables
dfCrossSec <- read.csv(here::here("data", "SDERSvalid_crossSec_data_preprocessed.csv"))
dfCrossSec <- dfCrossSec[, 
                          c(
                            which(names(dfCrossSec) %in% c("PARTICIPANT_ID", "SEX", "AGE", "OCCUPATION", "ACAD.DEGREE", 
                                                         "FAM.STATUS", "MENTAL.HEALTH", "MENT.AGE", "MENT.MEDICINE", 
                                                         "MENT.TREATMENT", "AFFECT_PRE_X", "AFFECT_PRE_Y", "AFFECT_POST_X",
                                                         "AFFECT_POST_Y")), 
                           which(str_detect(names(dfCrossSec), "_sum")),
                           which(str_detect(names(dfCrossSec), "_mean")))]

# check overlap in baseline and EMA
length(unique(readAll$PARTICIPANT_ID)) # N of EMA
sum((unique(readAll$PARTICIPANT_ID) %in% dfCrossSec$PARTICIPANT_ID)) # EMA with baseline
sum(!(unique(readAll$PARTICIPANT_ID) %in% dfCrossSec$PARTICIPANT_ID)) # EMA without baseline
sum(!(dfCrossSec$PARTICIPANT_ID %in% unique(readAll$PARTICIPANT_ID))) # baseline without ema

# inspect participants with missings (most contributed a normal amount of data and looks legitimate)
dfInspect <- readAll[!(readAll$PARTICIPANT_ID %in% dfCrossSec$PARTICIPANT_ID), ]
dplyr::count(x = dfInspect, PARTICIPANT_ID)

# merge dataframes
dfMerge <- merge(readAll, dfCrossSec, by = "PARTICIPANT_ID", all.x = TRUE)


if(file.exists(here::here("data", "SDERSvalid_DailyLife_data_preprocessed.csv"))){
  warning("file already exists!")
}else{
  write.csv(dfMerge, here::here("data", "SDERSvalid_DailyLife_data_preprocessed.csv"), row.names = FALSE)
}






      