################################################
# Load packages and data

library("here")
library("tidyverse")
library("Hmisc")
library("nlme")
library("lme4")
library("lmerTest")
library("psych")
library("RSA")
library("lubridate")
library("cowplot")
library("effects")
library("yardstick")
library("reshape2")
library("ctsem")

source(here::here("functions", "MultilevelRSA_25082019.R"))

df <- read.csv(here::here("data", "SDERSvalid_DailyLife_data_preprocessed.csv"))
df <- df[order(df$PARTICIPANT_ID, df$DayInd, df$Time), ]


###############################################
# prepare variables

df$S.DERS_NonAccept_mean18 <- rowMeans(df[, c('S.DERS8_ESM','S.DERS4_ESM','S.DERS1_ESM','S.DERS5_ESM','S.DERS12_ESM','S.DERS20_ESM','S.DERS18_ESM')])
df$S.DERS_Modulate_mean18 <- rowMeans(df[, c('S.DERS13_ESM','S.DERS17_ESM','S.DERS3_ESM','S.DERS21_ESM')])
df$S.DERS_Awareness_mean18 <- rowMeans(df[, c('S.DERS6_ESM.r','S.DERS11_ESM.r','S.DERS2_ESM.r','S.DERS19_ESM.r','S.DERS16_ESM.r')])
df$S.DERS_Clarity_mean18 <- rowMeans(df[, c('S.DERS14_ESM','S.DERS7_ESM')])

df$S.DERS_Total_mean18 <- rowMeans(df[, c('S.DERS1_ESM','S.DERS2_ESM.r','S.DERS3_ESM','S.DERS4_ESM','S.DERS5_ESM','S.DERS6_ESM.r','S.DERS7_ESM',
                                          'S.DERS8_ESM', 'S.DERS11_ESM.r','S.DERS12_ESM','S.DERS13_ESM','S.DERS14_ESM',
                                          'S.DERS16_ESM.r','S.DERS17_ESM','S.DERS18_ESM','S.DERS19_ESM.r','S.DERS20_ESM','S.DERS21_ESM')])

df <- df %>%
  group_by(PARTICIPANT_ID) %>% 
  mutate(
    S.DERS_Total_PM = mean(S.DERS_Total_mean18, na.rm = TRUE),
    S.DERS_NonAccept_PM = mean(S.DERS_NonAccept_mean18, na.rm = TRUE),
    S.DERS_Modulate_PM = mean(S.DERS_Modulate_mean18, na.rm = TRUE),
    S.DERS_Awareness_PM = mean(S.DERS_Awareness_mean18, na.rm = TRUE),
    S.DERS_Clarity_PM = mean(S.DERS_Clarity_mean18, na.rm = TRUE),
    
    affect_valence_PM = mean(AFFECT_ESM_X, na.rm = TRUE),
    affect_arousal_PM = mean(AFFECT_ESM_Y, na.rm = TRUE),
    stressors_PM = mean(NE.EVENT, na.rm = TRUE),
  ) %>%
  ungroup() %>% 
  as.data.frame()

df$S.DERS_Total_scaled <- scale(df$S.DERS_Total_mean18)
df$S.DERS_NonAccept_scaled = scale(df$S.DERS_NonAccept_mean18)
df$S.DERS_Modulate_scaled = scale(df$S.DERS_Modulate_mean18)
df$S.DERS_Awareness_scaled = scale(df$S.DERS_Awareness_mean18)
df$S.DERS_Clarity_scaled = scale(df$S.DERS_Clarity_mean18)

df$S.DERS_Total_PMcen <- df$S.DERS_Total_mean18 - df$S.DERS_Total_PM
df$S.DERS_NonAccept_PMcen <- df$S.DERS_NonAccept_mean18 - df$S.DERS_NonAccept_PM
df$S.DERS_Modulate_PMcen  <- df$S.DERS_Modulate_mean18 - df$S.DERS_Modulate_PM
df$S.DERS_Awareness_PMcen <- df$S.DERS_Awareness_mean18 - df$S.DERS_Awareness_PM
df$S.DERS_Clarity_PMcen   <- df$S.DERS_Clarity_mean18 - df$S.DERS_Clarity_PM
df$S.DERS_Total_PMcen_scaled <- scale(df$S.DERS_Total_PMcen)
df$S.DERS_NonAccept_PMcen_scaled <- scale(df$S.DERS_NonAccept_PMcen)
df$S.DERS_Modulate_PMcen_scaled  <- scale(df$S.DERS_Modulate_PMcen)
df$S.DERS_Awareness_PMcen_scaled <- scale(df$S.DERS_Awareness_PMcen)
df$S.DERS_Clarity_PMcen_scaled   <- scale(df$S.DERS_Clarity_PMcen)


df$affect_valence_PMcen <- df$AFFECT_ESM_X - df$affect_valence_PM
df$affect_valence_PMcen_scaled <- scale(df$affect_valence_PMcen)
df$affect_valence_PM_scaled <- scale(df$affect_valence_PM)

df$affect_arousal_PMcen <- df$AFFECT_ESM_Y - df$affect_arousal_PM
df$affect_arousal_PMcen_scaled <- scale(df$affect_arousal_PMcen)
df$affect_arousal_PM_scaled <- scale(df$affect_arousal_PM)

df$stressors_PMcen <- df$NE.EVENT - df$stressors_PM
df$stressors_PMcen_scaled <- scale(df$stressors_PMcen)
df$stressors_PM_scaled <- scale(df$stressors_PM)

df$S.DERS_Total_PM_scaled <- scale(df$S.DERS_Total_PM)
df$DERS_scaled <- scale(df$DERS_Total_mean)
df$S.DERS_scaled <- scale(df$S.DERS_Total_mean)
df$NEO_scaled <- scale(df$NEO_N_mean)
df$DASS_scaled <- scale(df$DASS_Total_mean)
df$CTQ_scaled <- scale(df$CTQ_Total_mean)

df$Time_dayHours <- hour(hms(df$Time))

df <- df %>%
  mutate(
    # Use the actual completed timestamp — already in full datetime format
    COMPLETED_TS_parsed = ymd_hms(COMPLETED_TS, tz = "Europe/Berlin"),
    id = as.numeric(as.factor(PARTICIPANT_ID))
  ) %>%
  arrange(id, COMPLETED_TS_parsed) %>%
  group_by(id) %>%
  mutate(
    time_since_start_hours = as.numeric(difftime(COMPLETED_TS_parsed, first(COMPLETED_TS_parsed), units = "hours"))
  ) %>%
  ungroup()



###############################################
# H4: state-trait correspondance

# simple correlations
dfcrossSecDERS <- unique(df[, c("PARTICIPANT_ID", "DERS_Total_sum", "S.DERS_Total_sum", "S.DERS_Total_PM")])
dfcrossSecDERS <- dfcrossSecDERS[complete.cases(dfcrossSecDERS), ]

rcorr(as.matrix(dfcrossSecDERS[,-1]))
cor.test(dfcrossSecDERS[,"DERS_Total_sum"], dfcrossSecDERS[,"S.DERS_Total_PM"])
cor.test(dfcrossSecDERS[,"S.DERS_Total_sum"], dfcrossSecDERS[,"S.DERS_Total_PM"])

# correlations as a function of n datapoints
corrOut <- as.data.frame(
  matrix(nrow=25, ncol=3, dimnames = list(NULL, c("corr", "CIlow", "CIupp")))
)

set.seed(100)
for(i in 1:25){
  
  # randomly sample i rows from each participant
  sampled_df <- df %>%
    group_by(PARTICIPANT_ID) %>%
    slice_sample(n = i) %>%
    ungroup() %>%
    as.data.frame()
  
  # person means
  sampled_df <- sampled_df %>%
    group_by(PARTICIPANT_ID) %>% 
    mutate(
      S.DERS_Total_PM = mean(S.DERS_Total_mean18, na.rm = TRUE), 
    ) %>%
    ungroup() %>% 
    as.data.frame()
  
  # cross-sectional dataframe
  sampled_df <- unique(sampled_df[, c("DERS_Total_sum", "S.DERS_Total_sum", "S.DERS_Total_PM")])
  sampled_df <- sampled_df[complete.cases(sampled_df), ]
  
  # correlations
  corrTemp <- cor.test(sampled_df[,"DERS_Total_sum"], sampled_df[,"S.DERS_Total_PM"])
  corrOut[i,1] <- corrTemp$estimate
  corrOut[i,2] <- corrTemp$conf.int[1]
  corrOut[i,3] <- corrTemp$conf.int[2]
  
}


ggplot(corrOut, aes(x = 1:nrow(corrOut), y = corr)) + 
  geom_ribbon(aes(ymin = CIlow, ymax = CIupp), alpha = 0.2) + 
  geom_smooth(se = FALSE, color = "black", size = 1) +
  # geom_line() +
  
  scale_y_continuous(breaks = seq(0, 1, by = 0.1), limits=c(0,1)) +
  theme_classic() +
  xlab("# Time points") +
  ylab("State-trait correlation")

ggsave(here::here("manuscripts", "SDERSvalid_dailyLife","figures", "stateTraitCorrelationByN.svg"), device="svg")



# correlations as a function of stressors

dfhighstress <- df[df$NE.EVENT >= 4, ]

dfcrossSecDERS <- unique(dfhighstress[, c("PARTICIPANT_ID", "DERS_Total_sum", "S.DERS_Total_sum", "S.DERS_Total_PM")])
dfcrossSecDERS <- dfcrossSecDERS[complete.cases(dfcrossSecDERS), ]

rcorr(as.matrix(dfcrossSecDERS[,-1]))
cor.test(dfcrossSecDERS[,"DERS_Total_sum"], dfcrossSecDERS[,"S.DERS_Total_PM"])
cor.test(dfcrossSecDERS[,"S.DERS_Total_sum"], dfcrossSecDERS[,"S.DERS_Total_PM"])




###############################################
# H5: relation to affective states

nullModel_day <- lme(data=df, S.DERS_Total_scaled ~ 1, random=~ 1 | PARTICIPANT_ID/DayInd)
nullModel_noday <- lme(data=df, S.DERS_Total_scaled ~ 1, random=~ 1 | PARTICIPANT_ID)

anova(nullModel_noday, nullModel_day)

affectModel_simple <- lme(data=df, S.DERS_Total_scaled ~ affect_valence_PMcen_scaled + affect_valence_PM_scaled, 
                   random = list(PARTICIPANT_ID =~ affect_valence_PMcen_scaled, DayInd =~1), na.action=na.exclude, control=lmeControl(msMaxIter = 100, opt = "optim"))
summary(affectModel_simple)

#affectModel <- lmer(data=df, S.DERS_Total_scaled ~ 
#                      affect_valence_PMcen_scaled + affect_arousal_PMcen_scaled + 
#                      I(scale(affect_valence_PMcen_scaled^2)) + affect_valence_PMcen_scaled:affect_arousal_PMcen_scaled + I(scale(affect_arousal_PMcen_scaled^2)) +
#                      affect_valence_PM_scaled + affect_arousal_PM_scaled + 
#                      I(scale(affect_valence_PM_scaled^2)) + affect_valence_PM_scaled:affect_arousal_PM_scaled + I(scale(affect_arousal_PM_scaled^2)) +
#                      (1|PARTICIPANT_ID) + (1|PARTICIPANT_ID:DayInd))


df$affect_valence_PMcen_scaled2 <- scale(df$affect_valence_PMcen_scaled^2)
df$affect_arousal_PMcen_scaled2 <- scale(df$affect_arousal_PMcen_scaled^2)
df$affect_arousalValence_PMcen <- scale(df$affect_arousal_PMcen_scaled * df$affect_valence_PMcen_scaled)
df$affect_valence_PM_scaled2 <- scale(df$affect_valence_PM_scaled^2)
df$affect_arousal_PM_scaled2 <- scale(df$affect_arousal_PM_scaled^2)
df$affect_arousalValence_PM <- scale(df$affect_arousal_PM_scaled * df$affect_valence_PM_scaled)

affectModel <- lme(data=df, S.DERS_Total_scaled ~ 
                     affect_valence_PMcen_scaled + affect_arousal_PMcen_scaled + 
                     affect_valence_PMcen_scaled2 + affect_arousalValence_PMcen + affect_arousal_PMcen_scaled2 +
                     affect_valence_PM_scaled + affect_arousal_PM_scaled + 
                     affect_valence_PM_scaled2 + affect_arousalValence_PM + affect_arousal_PM_scaled2,
                   random = list(PARTICIPANT_ID =~ affect_valence_PMcen_scaled + affect_arousal_PMcen_scaled + affect_valence_PM_scaled2 + affect_arousal_PM_scaled2, DayInd =~1),
                   na.action = na.exclude,
                   control=lmeControl(msMaxIter = 100, opt = "optim"))
summary(affectModel)


# tried different rotations, but generally this looks awful. create interaction plot instead
plotRSA(x=fixef(affectModel)["affect_valence_PMcen_scaled"],
        y=fixef(affectModel)["affect_arousal_PMcen_scaled"],
        rotation = list(x = -63, y = 120, z = 15))


predict_affect <- function(model, valence_range, arousal_range) {
  # Create a grid of values for the predictors
  grid <- expand.grid(
    affect_valence_PMcen_scaled = valence_range,
    affect_arousal_PMcen_scaled = arousal_range
  )
  
  # Compute quadratic and interaction terms for the predictors
  grid$affect_valence_PMcen_scaled2 <- grid$affect_valence_PMcen_scaled^2
  grid$affect_arousal_PMcen_scaled2 <- grid$affect_arousal_PMcen_scaled^2
  grid$affect_arousalValence_PMcen <- grid$affect_valence_PMcen_scaled * grid$affect_arousal_PMcen_scaled
  
  # Set non-centered variables to 0 (as specified)
  grid$affect_valence_PM_scaled <- 0
  grid$affect_arousal_PM_scaled <- 0
  grid$affect_valence_PM_scaled2 <- 0
  grid$affect_arousal_PM_scaled2 <- 0
  grid$affect_arousalValence_PM <- 0
  
  # Predict using the model
  predictions <- predict(model, newdata = grid, level = 0)
  
  # Add predictions to the grid
  grid$predicted_S_DERS_Total_scaled <- predictions
  
  return(grid)
}

# Usage Example
valence_range <- seq(-2, 2, length.out = 50)  
arousal_range <- seq(-1, 1, length.out = 3) 
predicted_df <- predict_affect(affectModel, valence_range, arousal_range)

predicted_df$affect_arousal_PMcen_scaled <- factor(ifelse(predicted_df$affect_arousal_PMcen_scaled == -1, "-1\u00b7SD", ifelse(predicted_df$affect_arousal_PMcen_scaled == 0, "Mean", "+1\u00b7SD")), levels=c("-1\u00b7SD", "Mean", "+1\u00b7SD"))


affectPlot_within <- ggplot(data=predicted_df, aes(y=predicted_S_DERS_Total_scaled, x=affect_valence_PMcen_scaled, colour=factor(affect_arousal_PMcen_scaled))) +
  geom_line() +
  theme_bw() +
  
  labs(color = "Arousal")  +
  ylab("S-DERS predicted value") +
  xlab("Valence") +
  ylim(-0.45, 0.6) +
    
  theme(legend.position = "top", legend.direction = "horizontal") +
  ggtitle("Within-person")


ggsave(here::here("manuscripts", "SDERSvalid_dailyLife","figures", "arousalValenceInteraction_within.svg"), device="svg")






predict_affect_between <- function(model, valence_range, arousal_range) {
  # Create a grid of values for the predictors
  grid <- expand.grid(
    affect_valence_PM_scaled = valence_range,
    affect_arousal_PM_scaled = arousal_range
  )
  
  # Compute quadratic and interaction terms for the predictors
  grid$affect_valence_PM_scaled2 <- grid$affect_valence_PM_scaled^2
  grid$affect_arousal_PM_scaled2 <- grid$affect_arousal_PM_scaled^2
  grid$affect_arousalValence_PM <- grid$affect_valence_PM_scaled * grid$affect_arousal_PM_scaled
  
  # Set non-centered variables to 0 (as specified)
  grid$affect_valence_PMcen_scaled <- 0
  grid$affect_arousal_PMcen_scaled <- 0
  grid$affect_valence_PMcen_scaled2 <- 0
  grid$affect_arousal_PMcen_scaled2 <- 0
  grid$affect_arousalValence_PMcen <- 0
  
  # Predict using the model
  predictions <- predict(model, newdata = grid, level = 0)
  
  # Add predictions to the grid
  grid$predicted_S_DERS_Total_scaled <- predictions
  
  return(grid)
}

# Usage Example
valence_range <- seq(-2, 2, length.out = 50)  
arousal_range <- seq(-1, 1, length.out = 3) 
predicted_df_between <- predict_affect_between(affectModel, valence_range, arousal_range)

predicted_df_between$affect_arousal_PM_scaled <- factor(ifelse(predicted_df_between$affect_arousal_PM_scaled == -1, "-1\u00b7SD", ifelse(predicted_df_between$affect_arousal_PM_scaled == 0, "Mean", "+1\u00b7SD")), levels=c("-1\u00b7SD", "Mean", "+1\u00b7SD"))


affectPlot_between <- ggplot(data=predicted_df_between, aes(y=predicted_S_DERS_Total_scaled, x=affect_valence_PM_scaled, colour=factor(affect_arousal_PM_scaled))) +
  geom_line() +
  theme_bw() +
  
  labs(color = "Arousal")  +
  ylab("S-DERS predicted value") +
  xlab("Valence") +
  ylim(-0.45, 0.6) +
  
  theme(legend.position = "top", legend.direction = "horizontal") +
  ggtitle("Between-person")

ggsave(here::here("manuscripts", "SDERSvalid_dailyLife","figures", "arousalValenceInteraction_between.svg"), device="svg")



plot_grid(affectPlot_within, affectPlot_between, nrow=1)
ggsave(here::here("manuscripts", "SDERSvalid_dailyLife","figures", "arousalValenceInteraction_panelPlot.svg"), device="svg")







###############################################
# relation to stressors (H6)

stressModel <- lme(data=df, S.DERS_Total_scaled ~ stressors_PMcen_scaled + stressors_PM_scaled, 
                   random = list(PARTICIPANT_ID =~ stressors_PMcen_scaled, DayInd =~1))
summary(stressModel)
intervals(stressModel)
#confint(stressModel)

# plot
ef1 <- effect(term = "stressors_PMcen_scaled", mod = stressModel)
efdata1 <- as.data.frame(ef1)


stressPlot <- ggplot(data = efdata1, aes(x = stressors_PMcen_scaled, y =  fit)) + 
  
  geom_point(data = df, 
             mapping = aes(x = stressors_PMcen_scaled, y = S.DERS_Total_scaled),
             size = 0.15, alpha = 0.4, position = "jitter") +
  
  geom_line(size = 0.8) +
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se), alpha=0.2, colour = NA) +
  
  labs(x= "Stressors", y="S-DERS") + 
  
  theme_classic()

ggsave(here::here("manuscripts", "SDERSvalid_dailyLife","figures", "stressors.svg"), device="svg")



###############################################
# persistence of negative affect (post hoc)

#################
# linear attempts

df <- df %>%
  group_by(PARTICIPANT_ID, DayInd) %>%
  arrange(PARTICIPANT_ID, DayInd, OccInd) %>%
  mutate(AFFECT_ESM_X_lag_minus1 = lead(AFFECT_ESM_X)) %>%
  ungroup()

persModel <- lme(data=df, AFFECT_ESM_X_lag_minus1 ~ affect_valence_PMcen_scaled*S.DERS_Total_PMcen_scaled, 
                   random = list(PARTICIPANT_ID =~ affect_valence_PMcen_scaled*S.DERS_Total_PMcen_scaled, DayInd =~1), na.action = "na.exclude", control=lmeControl(msMaxIter = 100, opt = "optim"))

summary(persModel)

persModel <- lme(data=df, AFFECT_ESM_X_lag_minus1 ~ affect_valence_PMcen_scaled*S.DERS_Modulate_PMcen, 
                 random = list(PARTICIPANT_ID =~ affect_valence_PMcen_scaled*S.DERS_Modulate_PMcen, DayInd =~1), na.action = "na.exclude", control=lmeControl(msMaxIter = 100, opt = "optim"))

summary(persModel)



df$S.DERS_Total_PMcen_scaled <- scale(df$S.DERS_Total_PMcen)
df$S.DERS_NonAccept_PMcen_scaled <- scale(df$S.DERS_NonAccept_PMcen)
df$S.DERS_Modulate_PMcen_scaled  <- scale(df$S.DERS_Modulate_PMcen)
df$S.DERS_Awareness_PMcen_scaled <- scale(df$S.DERS_Awareness_PMcen)
df$S.DERS_Clarity_PMcen_scaled   <- scale(df$S.DERS_Clarity_PMcen)


#######################
# ctsem attempts


df <- df %>%
  mutate(
    id = as.numeric(as.factor(PARTICIPANT_ID))  # convert to numeric id
  ) %>%
  arrange(id, time_since_start_hours)


df %>%
  group_by(id, time_since_start_hours) %>%
  filter(n() > 1)

df$time <- df$time_since_start_hours

df$SDERS_Total_PMcen_scaled <- df$S.DERS_Total_PMcen_scaled

df_ctSEM <- df[,c("id", "time", "affect_valence_PMcen_scaled", "SDERS_Total_PMcen_scaled", "stressors_PMcen_scaled")]


persModel_ct <- ctModel(type="ct",
                        n.latent=3, n.manifest=3,
                        manifestNames=c("affect_valence_PMcen_scaled", "SDERS_Total_PMcen_scaled", "stressors_PMcen_scaled"),
                        latentNames=c("affect", "SDERS", "stressors"),
                        LAMBDA=diag(3),
                        DRIFT=matrix(c(
                          "drift_Affect_Affect",    "drift_Regulation_Affect",    "drift_Stressors_Affect",
                          "drift_Affect_Regulation","drift_Regulation_Regulation","drift_Stressors_Regulation",
                          "drift_Affect_Stressors", "drift_Regulation_Stressors", "drift_Stressors_Stressors"
                        ), 3, 3, byrow = TRUE)) 


persModel_ct_fit <- ctFit(dat=df_ctSEM,
                          ctmodelobj=persModel_ct,
                          df_ctSEM)

summary(persModel_ct_fit)

ctGenerate(persModel_ct)



###############################################
# trait moderators (H7)

createInteractionPlot <- function(model, maineffectTerm = "stressors_PMcen_scaled", interactionTerm, interactionLabel) {
  
  # prepare predictions with SEs
  interactionTermName <- deparse(substitute(interactionTerm))
  xlevels_list <- setNames(list(c(-1, 0, 1)), interactionTermName)
  
  ef <- effect(
    term = paste0(maineffectTerm, ":", interactionTermName),
    xlevels = xlevels_list,
    mod = model
  )
  
  ef_df <- as.data.frame(ef)
  
  ef_df[[interactionTermName]] <- factor(
    ifelse(ef_df[[interactionTermName]] == -1, "-1·SD", 
           ifelse(ef_df[[interactionTermName]] == 0, "Mean", "+1·SD")),
    levels = c("-1·SD", "Mean", "+1·SD")
  )
  
  # Create plot
  ggplot(ef_df, aes(x = get(maineffectTerm), y = fit, color = factor(get(interactionTermName)), group = factor(get(interactionTermName)))) + 
    geom_line(size = 0.8) +
    geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=factor(get(interactionTermName))), alpha=0.2, colour = NA) +
    
    labs(x= "Stressors", y="S-DERS", color=interactionLabel, fill=interactionLabel) + 
    theme_classic() + 
    
    ylim(-2, 2) + 
    theme(axis.line = element_line(size = 0.5)) +
    theme(axis.text=element_text(size=6), axis.title=element_text(size=8), axis.ticks = element_line(colour = "black"),
          legend.title=element_text(size=6), legend.text=element_text(size=5), legend.key.size = unit(0.3, "cm"),
          legend.justification = c(1, 0), legend.position = c(0.95, 0.02)) +
    
    scale_color_manual(values = c("darkred", "darkblue", "darkgreen")) +
    scale_fill_manual(values = c("darkred", "darkblue", "darkgreen"))
  
  
}


# trait DERS
stressModel_traitDERS <- lme(data=df, S.DERS_Total_scaled ~ stressors_PMcen_scaled*DERS_scaled + stressors_PM_scaled*DERS_scaled, 
                   random = list(PARTICIPANT_ID =~ stressors_PMcen_scaled, DayInd =~1), na.action=na.exclude, control=lmeControl(msMaxIter = 100, opt = "optim"))
summary(stressModel_traitDERS)
DERSplot <- createInteractionPlot(stressModel_traitDERS, interactionTerm = DERS_scaled, interactionLabel = "DERS")

# S-DERS after mood induction
stressModel_SDERSmoodInd <- lme(data=df, S.DERS_Total_scaled ~ stressors_PMcen_scaled*S.DERS_scaled + stressors_PM_scaled*S.DERS_scaled, 
                             random = list(PARTICIPANT_ID =~ stressors_PMcen_scaled, DayInd =~1), na.action=na.exclude, control=lmeControl(msMaxIter = 100, opt = "optim"))
summary(stressModel_SDERSmoodInd)
SDERSplot <- createInteractionPlot(stressModel_SDERSmoodInd, interactionTerm = S.DERS_scaled, interactionLabel = "S-DERS (Mood induction)")

# Neuroticism
stressModel_NEO <- lme(data=df, S.DERS_Total_scaled ~ stressors_PMcen_scaled*NEO_scaled + stressors_PM_scaled*NEO_scaled, 
                                random = list(PARTICIPANT_ID =~ stressors_PMcen_scaled, DayInd =~1), na.action=na.exclude, control=lmeControl(msMaxIter = 100, opt = "optim"))
summary(stressModel_NEO)
NEOplot <- createInteractionPlot(stressModel_NEO, interactionTerm = NEO_scaled, interactionLabel = "Neuroticism")

# DASS
stressModel_DASS <- lme(data=df, S.DERS_Total_scaled ~ stressors_PMcen_scaled*DASS_scaled + stressors_PM_scaled*DASS_scaled, 
                       random = list(PARTICIPANT_ID =~ stressors_PMcen_scaled, DayInd =~1), na.action=na.exclude, control=lmeControl(msMaxIter = 100, opt = "optim"))
summary(stressModel_DASS)
DASSplot <- createInteractionPlot(stressModel_DASS, interactionTerm = DASS_scaled, interactionLabel = "DASS")

plot_grid(DERSplot, SDERSplot, NEOplot, DASSplot, nrow=2)
ggsave(here::here("manuscripts", "SDERSvalid_dailyLife","figures", "crossLevelInteraction_panelPlot.svg"), device="svg")


# DASS subscore analysis

dfCrossSec <- read.csv(here::here("data", "SDERSvalid_crossSec_data_preprocessed.csv"))

dfCrossSec <- dfCrossSec[, 
                         c(
                           which(names(dfCrossSec) == "PARTICIPANT_ID"), 
                           which(str_detect(names(dfCrossSec), "DASS")))]

dfCrossSec$DASS_Depression_sum <- rowSums(dfCrossSec[, c('DASS3','DASS5','DASS10','DASS13','DASS16','DASS17','DASS21')])
dfCrossSec$DASS_Depression_mean <- rowMeans(dfCrossSec[, c('DASS3','DASS5','DASS10','DASS13','DASS16','DASS17','DASS21')])
dfCrossSec$DASS_Anxiety_sum <- rowSums(dfCrossSec[, c('DASS2','DASS4','DASS7','DASS9','DASS15','DASS19','DASS20')])
dfCrossSec$DASS_Anxiety_mean <- rowMeans(dfCrossSec[, c('DASS2','DASS4','DASS7','DASS9','DASS15','DASS19','DASS20')])
dfCrossSec$DASS_Stress_sum <- rowSums(dfCrossSec[, c('DASS1','DASS6','DASS8','DASS11','DASS12','DASS14','DASS18')])
dfCrossSec$DASS_Stress_mean <- rowMeans(dfCrossSec[, c('DASS1','DASS6','DASS8','DASS11','DASS12','DASS14','DASS18')])

dfDASS <- merge(df, dfCrossSec, by = "PARTICIPANT_ID", all.x = TRUE)

dfDASS$DASS_Anxiety_scaled <- scale(dfDASS$DASS_Anxiety_mean)
dfDASS$DASS_Depression_scaled <- scale(dfDASS$DASS_Depression_mean)
dfDASS$DASS_Stress_scaled <- scale(dfDASS$DASS_Stress_mean)

stressModel_DASS_Anxiety <- lme(data=dfDASS, S.DERS_Total_scaled ~ stressors_PMcen_scaled*DASS_Anxiety_scaled + stressors_PM_scaled*DASS_Anxiety_scaled, 
                        random = list(PARTICIPANT_ID =~ stressors_PMcen_scaled, DayInd =~1), na.action=na.exclude, control=lmeControl(msMaxIter = 100, opt = "optim"))
summary(stressModel_DASS_Anxiety)
DASSplot <- createInteractionPlot(stressModel_DASS_Anxiety, interactionTerm = DASS_Anxiety_scaled, interactionLabel = "DASS: Anxiety")

stressModel_DASS_Depression <- lme(data=dfDASS, S.DERS_Total_scaled ~ stressors_PMcen_scaled*DASS_Depression_scaled + stressors_PM_scaled*DASS_Depression_scaled, 
                                random = list(PARTICIPANT_ID =~ stressors_PMcen_scaled, DayInd =~1), na.action=na.exclude, control=lmeControl(msMaxIter = 100, opt = "optim"))
summary(stressModel_DASS_Depression)
DASSplot <- createInteractionPlot(stressModel_DASS_Depression, interactionTerm = DASS_Depression_scaled, interactionLabel = "DASS: Depression")

stressModel_DASS_Stress <- lme(data=dfDASS, S.DERS_Total_scaled ~ stressors_PMcen_scaled*DASS_Stress_scaled + stressors_PM_scaled*DASS_Stress_scaled, 
                                   random = list(PARTICIPANT_ID =~ stressors_PMcen_scaled, DayInd =~1), na.action=na.exclude, control=lmeControl(msMaxIter = 100, opt = "optim"))
summary(stressModel_DASS_Stress)
DASSplot <- createInteractionPlot(stressModel_DASS_Stress, interactionTerm = DASS_Stress_scaled, interactionLabel = "DASS: Stress")




############################################
# incremental validity

balanced_accuracy <- function(logModel, outcomeName="MENT.HEALTH"){
  
  trueVals <- logModel$data[,outcomeName]
  predVals <- ifelse(logModel$fitted.values >= 0.5, 1, 0)
  
  sens <- sum(predVals[trueVals == 1])/length(predVals[trueVals == 1])
  spec <- sum(predVals[trueVals == 0] == 0)/length(predVals[trueVals == 0])
  
  unbalAcc <- sum(trueVals == predVals)/length(trueVals)
  balAcc <- (sens + spec)/2
  
  return(balAcc)
  
}

dfCrossSec <- read.csv(here::here("data", "SDERSvalid_crossSec_data_preprocessed.csv"))
dfCrossSec <- dfCrossSec[, c("PARTICIPANT_ID", "MENT.HEALTH")]
dfHealth <- merge(df, dfCrossSec, by = "PARTICIPANT_ID", all.x = TRUE)

dfHealth <- dfHealth[, c("PARTICIPANT_ID", "S.DERS_Total_PM_scaled", "DERS_scaled", "MENT.HEALTH", "MENT.AGE", "MENT.MEDICINE", "MENT.TREATMENT")]
dfHealth <- unique(dfHealth)
dfHealth <- dfHealth[complete.cases(dfHealth), ]

logModel_DERS <- glm(MENT.HEALTH ~ DERS_scaled, data = dfHealth, family = binomial)
summary(logModel_DERS)
balanced_accuracy(logModel_DERS)

logModel_SDERS <- glm(MENT.HEALTH ~ S.DERS_Total_PM_scaled, data = dfHealth, family = binomial)
summary(logModel_SDERS)
balanced_accuracy(logModel_SDERS)

logModel_incr <- glm(MENT.HEALTH ~ DERS_scaled + S.DERS_Total_PM_scaled, data = dfHealth, family = binomial)
summary(logModel_incr)
balanced_accuracy(logModel_incr)

anova(logModel_DERS, logModel_incr, test = "Chisq")
anova(logModel_SDERS, logModel_incr, test = "Chisq")







############################################
# CTQ analysis (should create the results in a loop, best case scenario: seperately for all S-DERS subscales)
# Scale the CTQ subscales


stressModel_CTQ <- lme(data=df, S.DERS_Total_scaled ~ stressors_PMcen_scaled*CTQ_scaled + stressors_PM_scaled*CTQ_scaled, 
                        random = list(PARTICIPANT_ID =~ stressors_PMcen_scaled, DayInd =~1), na.action=na.exclude, control=lmeControl(msMaxIter = 100, opt = "optim"))
summary(stressModel_CTQ)
intervals(stressModel_CTQ)
#CTQplot <- createInteractionPlot(stressModel_CTQ, interactionTerm = CTQ_scaled, interactionLabel = "CTQ")



# Scale the CTQ subscales
df$CTQ_EmotionAbuse_mean_scaled <- scale(df$CTQ_EmotionAbuse_mean)
df$CTQ_PhysicalAbuse_mean_scaled <- scale(df$CTQ_PhysicalAbuse_mean)
df$CTQ_SexualAbuse_mean_scaled <- scale(df$CTQ_SexualAbuse_mean)
df$CTQ_EmotionNeglect_mean_scaled <- scale(df$CTQ_EmotionNeglect_mean)
df$CTQ_PhysicalNeglect_mean_scaled <- scale(df$CTQ_PhysicalNeglect_mean)
df$CTQ_Total_mean_scaled <- scale(df$CTQ_Total_mean)

ggplot(data=melt(unique(df[, str_detect(names(df), "CTQ_") & str_detect(names(df), "_sum")])), aes(x=variable, y=value)) +
         geom_boxplot()

CTQnames <- c("CTQ_EmotionAbuse_mean_scaled", "CTQ_PhysicalAbuse_mean_scaled", "CTQ_SexualAbuse_mean_scaled", "CTQ_EmotionNeglect_mean_scaled", "CTQ_PhysicalNeglect_mean_scaled")
SDERSnames <- c("S.DERS_NonAccept_scaled", "S.DERS_Modulate_scaled", "S.DERS_Awareness_scaled", "S.DERS_Clarity_scaled")


# prep output dfs
CTQ_est_out <- as.data.frame(
  matrix(nrow=length(SDERSnames), ncol=length(CTQnames),
         dimnames = list(SDERSnames, CTQnames))
)

CTQ_CIlow_out <- CTQ_est_out
CTQ_CIhigh_out <- CTQ_est_out
CTQ_p_out <- CTQ_est_out

# extract values from loop
for(i in 1:length(SDERSnames)){
  
  for(j in 1:length(CTQnames)){
    
    f <- as.formula(paste(SDERSnames[i], "~ stressors_PMcen_scaled *", CTQnames[j], "+ stressors_PM_scaled *", CTQnames[j]))
    
    tempModel <- lme(data = df, 
                     fixed = f, 
                     random = list(PARTICIPANT_ID =~ stressors_PMcen_scaled, DayInd =~1),
                     na.action = na.exclude,
                     control = lmeControl(msMaxIter = 100, opt = "optim"))
    
    CTQ_p_out[i,j] <- summary(tempModel)$tTable[, "p-value"][paste0("stressors_PMcen_scaled:", CTQnames[j])]
    
    CTQ_est_out[i,j] <- tempModel$coefficients$fixed[paste0("stressors_PMcen_scaled:", CTQnames[j])]
    
    estTemp <- intervals(tempModel, which="fixed")$fixed
    estTemp_interaction <- estTemp[row.names(estTemp)==paste0("stressors_PMcen_scaled:", CTQnames[j]), ]
    CTQ_est_out[i,j] <- estTemp_interaction["est."]
    CTQ_CIlow_out[i,j] <- estTemp_interaction["lower"]
    CTQ_CIhigh_out[i,j] <- estTemp_interaction["upper"]
    
  }
  
}



# plot results
dfplotCTQ <- melt(cbind(row.names(CTQ_est_out), CTQ_est_out))
dfplotCTQ <- cbind(dfplotCTQ,
                   melt(cbind(row.names(CTQ_CIlow_out), CTQ_CIlow_out))[,"value"],
                   melt(cbind(row.names(CTQ_CIhigh_out), CTQ_CIhigh_out))[,"value"])
dfplotCTQ$p_uncorr <- melt(CTQ_p_out)$value
dfplotCTQ$p_uncorr_ind <- ifelse(dfplotCTQ$p_uncorr <= 0.05, 1, 0)
dfplotCTQ$asterisk_uncorr  <- ifelse(dfplotCTQ$p_uncorr < 0.001, "***", ifelse(dfplotCTQ$p_uncorr < 0.01, "**", ifelse(dfplotCTQ$p_uncorr <= 0.05, "*", "")))
dfplotCTQ$p_corr <- p.adjust(melt(CTQ_p_out)$value, method="fdr")
dfplotCTQ$p_corr_ind <- ifelse(dfplotCTQ$p_corr <= 0.05, 1, 0)
dfplotCTQ$asterisk_corr <- ifelse(dfplotCTQ$p_corr < 0.001, "***", ifelse(dfplotCTQ$p_corr < 0.01, "**", ifelse(dfplotCTQ$p_corr <= 0.05, "*", "")))

names(dfplotCTQ) <- c("SDERS", "CTQ", "Estimate", "CIupper", "CIlower", "p_uncorr", "p_uncorr_ind", "asterisk_uncorr", "p_corr", "p_corr_ind", "asterisk_corr")
dfplotCTQ$SDERS <- factor(dfplotCTQ$SDERS, levels=dfplotCTQ$SDERS[1:4])

ggplot(dfplotCTQ, aes(x=SDERS, y=Estimate, fill=CTQ)) +
  geom_bar(stat="Identity", position="dodge") +
  
  ylim(-0.03, 0.05) +
  xlab(NULL) +
  ylab("Trauma-Stress Interaction") +
  theme_classic() +
  
  scale_x_discrete(labels = c("Non-Acceptance", "Modulate", "Awareness", "Clarity")) +
  scale_fill_manual(  values = c("CTQ_EmotionAbuse_mean_scaled" = "#1f77b4",
                                 "CTQ_PhysicalAbuse_mean_scaled" = "blue",
                                 "CTQ_SexualAbuse_mean_scaled" = "darkblue",
                                 "CTQ_EmotionNeglect_mean_scaled" = "#d62728",
                                 "CTQ_PhysicalNeglect_mean_scaled" = "darkred"),labels = c("Emotional Abuse", "Physical Abuse", "Sexual Abuse", "Emotional Neglect", "Physical Neglect")) +
  theme(legend.title = element_blank()) +
  
  geom_text(aes(label = asterisk_uncorr), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            color = "lightgrey") +
  
  # Add black asterisks for p_corr
  geom_text(aes(label = asterisk_corr), 
            position = position_dodge(width = 0.9), 
            vjust = -1.5, 
            color = "black")


ggsave(here::here("manuscripts", "SDERSvalid_dailyLife", "figures", "CTQcrossLevelInteraction.svg"), device="svg")


