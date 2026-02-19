library(tidyverse)
library(ggpubr)
library(ggplot2)
library(rstatix)
library(afex)
library(dplyr)
library(car)
require(gridExtra)

#### Data Preparation HBO ###########
# Load in Data
speech_masker_data_hbo <- read.csv("/Users/benrichardson/Documents/GitHub/Broadband-ILD-fNIRS/ANALYSIS SCRIPTS/Eli Analysis/all_subjects_mean_during_stim_speech_masker.csv")
noise_masker_data_hbo <- read.csv("/Users/benrichardson/Documents/GitHub/Broadband-ILD-fNIRS/ANALYSIS SCRIPTS/Eli Analysis/all_subjects_mean_during_stim_noise_masker.csv")

colnames(speech_masker_data_hbo) <- c("S","Channel","ITD50","ITD500","ILD70n","ILD10")
colnames(noise_masker_data_hbo) <- c("S","Channel","ITD50","ITD500","ILD70n","ILD10")
speech_masker_data_hbo <- pivot_longer(speech_masker_data_hbo, cols=c("ITD50","ITD500","ILD70n","ILD10"), names_to = "Spatialization", values_to = "MeanHb")
noise_masker_data_hbo <- pivot_longer(noise_masker_data_hbo, cols=c("ITD50","ITD500","ILD70n","ILD10"), names_to = "Spatialization", values_to = "MeanHb")


# Combine speech and noise
speech_masker_data_hbo$Masker<-"speech"
noise_masker_data_hbo$Masker<-"noise"
all_data_hbo<-rbind(speech_masker_data_hbo,noise_masker_data_hbo)

# Add ROI information
all_data_hbo$Roi<- "NA"
right_pfc_channels <- c(0,1,2,3)
left_pfc_channels <- c(4,5)
right_stg_channels <- c(6,7,8,9)
left_stg_channels <- c(10,11,12,13)
all_data_hbo$Roi[which(all_data_hbo$Channel %in% right_pfc_channels)] <- "right_pfc"
all_data_hbo$Roi[which(all_data_hbo$Channel %in% left_pfc_channels)] <- "left_pfc"
all_data_hbo$Roi[which(all_data_hbo$Channel %in% right_stg_channels)] <- "right_stg"
all_data_hbo$Roi[which(all_data_hbo$Channel %in% left_stg_channels)] <- "left_stg"

# Organize Factors
to.factor <- c('S', 'Roi','Spatialization')
all_data_hbo[, to.factor] <- lapply(all_data_hbo[, to.factor], as.factor)

all_data_hbo$Masker <- as.factor(all_data_hbo$Masker)
all_data_cleaned_hbo <- na.omit(all_data_hbo)
all_data_cleaned_hbo$Spatialization <- factor(
  all_data_cleaned_hbo$Spatialization,
  levels = c("ITD50", "ITD500", "ILD70n", "ILD10")
)


all_data_cleaned_hbo %>% group_by(Spatialization, Masker,Roi) %>% shapiro_test(MeanHb)
all_data_cleaned_pfc_speech_hbo <- subset(all_data_cleaned_hbo, Roi %in% c("right_pfc","left_pfc") & Masker == "speech")
all_data_cleaned_pfc_noise_hbo <- subset(all_data_cleaned_hbo, Roi %in% c("right_pfc","left_pfc") & Masker == "noise")
all_data_cleaned_stg_speech_hbo <- subset(all_data_cleaned_hbo, Roi %in% c("right_stg","left_stg") & Masker == "speech")
all_data_cleaned_stg_noise_hbo <- subset(all_data_cleaned_hbo, Roi %in% c("right_stg","left_stg") & Masker == "noise")

#### Data Preparaion HBR #####
# Load in Data
speech_masker_data_hbr <- read.csv("/Users/benrichardson/Documents/GitHub/Broadband-ILD-fNIRS/ANALYSIS SCRIPTS/Eli Analysis/all_subjects_mean_during_stim_speech_masker_hbr.csv")
noise_masker_data_hbr <- read.csv("/Users/benrichardson/Documents/GitHub/Broadband-ILD-fNIRS/ANALYSIS SCRIPTS/Eli Analysis/all_subjects_mean_during_stim_noise_masker_hbr.csv")

colnames(speech_masker_data_hbr) <- c("S","Channel","ITD50","ITD500","ILD70n","ILD10")
colnames(noise_masker_data_hbr) <- c("S","Channel","ITD50","ITD500","ILD70n","ILD10")
speech_masker_data_hbr <- pivot_longer(speech_masker_data_hbr, cols=c("ITD50","ITD500","ILD70n","ILD10"), names_to = "Spatialization", values_to = "MeanHb")
noise_masker_data_hbr <- pivot_longer(noise_masker_data_hbr, cols=c("ITD50","ITD500","ILD70n","ILD10"), names_to = "Spatialization", values_to = "MeanHb")


# Combine speech and noise
speech_masker_data_hbr$Masker<-"speech"
noise_masker_data_hbr$Masker<-"noise"
all_data_hbr<-rbind(speech_masker_data_hbr,noise_masker_data_hbr)

# Add ROI information
all_data_hbr$Roi<-NA
all_data_hbr$Roi[which(all_data_hbr$Channel %in% right_pfc_channels)] <- "right_pfc"
all_data_hbr$Roi[which(all_data_hbr$Channel %in% left_pfc_channels)] <- "left_pfc"
all_data_hbr$Roi[which(all_data_hbr$Channel %in% right_stg_channels)] <- "right_stg"
all_data_hbr$Roi[which(all_data_hbr$Channel %in% left_stg_channels)] <- "left_stg"
# Change ROI information

#all_data$Roi[all_data$Roi == "0"] <- "pfc"
#all_data$Roi[all_data$Roi == "1"] <- "stg"

# Organize Factors
to.factor <- c('S', 'Roi','Spatialization')
all_data_hbr[, to.factor] <- lapply(all_data_hbr[, to.factor], as.factor)

all_data_hbr$Masker <- as.factor(all_data_hbr$Masker)
all_data_cleaned_hbr <- na.omit(all_data_hbr)
all_data_cleaned_hbr$Spatialization <- factor(
  all_data_cleaned_hbr$Spatialization,
  levels = c("ITD50", "ITD500", "ILD70n", "ILD10")
)

all_data_cleaned_hbr %>% group_by(Spatialization, Masker,Roi) %>% shapiro_test(MeanHb)
all_data_cleaned_pfc_speech_hbr <- subset(all_data_cleaned_hbr, Roi %in% c("right_pfc","left_pfc") & Masker == "speech")
all_data_cleaned_pfc_noise_hbr <- subset(all_data_cleaned_hbr, Roi %in% c("right_pfc","left_pfc") & Masker == "noise")
all_data_cleaned_stg_speech_hbr <- subset(all_data_cleaned_hbr, Roi %in% c("right_stg","left_stg") & Masker == "speech")
all_data_cleaned_stg_noise_hbr <- subset(all_data_cleaned_hbr, Roi %in% c("right_stg","left_stg") & Masker == "noise")


##### Combining Data #####
all_data_cleaned_hbo$chromophore <- "HbO"
all_data_cleaned_hbr$chromophore <- "HbR"
all_data <- rbind(all_data_cleaned_hbo,all_data_cleaned_hbr)
all_data_pfc_speech <- subset(all_data, Roi %in% c("right_pfc","left_pfc") & Masker == "speech")
all_data_pfc_noise <- subset(all_data, Roi %in% c("right_pfc","left_pfc") & Masker == "noise")
all_data_stg_speech <- subset(all_data, Roi %in% c("right_stg","left_stg") & Masker == "speech")
all_data_stg_noise <- subset(all_data, Roi %in% c("right_stg","left_stg") & Masker == "noise")

### Summary SE function ########
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}


# Check for normality
all_data_cleaned_hbo %>% group_by(Spatialization, Masker,Roi) %>% shapiro_test(MeanHb)




##### PFC Plot HbO ##########
pfc_se_data_speechhbo <- summarySE(
  all_data_cleaned_pfc_speech_hbo,
  measurevar = "MeanHb",
  groupvars = c("S","Spatialization"),
  na.rm = TRUE)

pfc_se_data_speechhbo <- summarySE(
  pfc_se_data_speechhbo,
  measurevar = "MeanHb",
  groupvars = c("Spatialization"),
  na.rm = TRUE)


plotspeechhbo <- ggplot() +
  
  # --- Individual subject means ---
  geom_point(
    data = aggregate(
      MeanHb ~ S + Spatialization,
      data = all_data_cleaned_pfc_speech_hbo,
      FUN = mean),
    aes(x = Spatialization,
        y = MeanHb),
    color = "red",
    size = 2,
    alpha = 0.3,
    position = position_jitter(width = 0.08)
  ) +
  
  # --- Error bars ---
  geom_errorbar(
    data = pfc_se_data_speechhbo,
    aes(x = Spatialization,
        y = MeanHb,
        ymin = MeanHb - se,
        ymax = MeanHb + se),
    width = 0.5,
    linewidth = 0.8
  ) +
  
  # --- Mean point ---
  geom_point(
    data = pfc_se_data_speechhbo,
    aes(x = Spatialization,
        y = MeanHb),
    size = 4,
    shape = 21,
    fill = "red"
  ) +
  
  ggtitle("Speech Masker") +
  labs(x = "", y = "Mean \u0394HbO (\u03BCM)") +
  ylim(-0.25,0.4) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 18),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 12),
    legend.position = "none"
  ) +
  scale_x_discrete(labels=c("ITD50" = "", "ITD500" = "",
                            "ILD70n" = "", "ILD10" = ""))


# Noise HbO 
pfc_se_data_noisehbo <- summarySE(
  all_data_cleaned_pfc_noise_hbo,
  measurevar="MeanHb",
  groupvars=c("S","Spatialization"),
  na.rm=TRUE)

pfc_se_data_noisehbo <- summarySE(
  pfc_se_data_noisehbo,
  measurevar="MeanHb",
  groupvars=c("Spatialization"),
  na.rm=TRUE)


plotnoisehbo <- ggplot() +
  geom_point(
    data = aggregate(
      MeanHb ~ S + Spatialization,
      data = all_data_cleaned_pfc_noise_hbo,
      FUN = mean),
    aes(x = Spatialization, y = MeanHb),
    color = "red",
    size = 2,
    alpha = 0.3,
    position = position_jitter(width = 0.08)
  ) +
  geom_errorbar(
    data = pfc_se_data_noisehbo,
    aes(x = Spatialization,
        y = MeanHb,
        ymin = MeanHb - se,
        ymax = MeanHb + se),
    width = 0.5,
    linewidth = 0.8
  ) +
  geom_point(
    data = pfc_se_data_noisehbo,
    aes(x = Spatialization, y = MeanHb),
    size = 4,
    shape = 21,
    fill = "red"
  ) +
  ggtitle("Noise Masker") +
  labs(x = "", y = "Mean \u0394HbO (\u03BCM)") +
  ylim(-0.25,0.4) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 18),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 12),
    legend.position = "none"
  ) +
  scale_x_discrete(labels=c("ITD50" = "", "ITD500" = "",
                            "ILD70n" = "", "ILD10" = ""))


# Speech HbR
# Speech HbR
pfc_se_data_speechhbr <- summarySE(
  all_data_cleaned_pfc_speech_hbr,
  measurevar = "MeanHb",
  groupvars = c("S","Spatialization"),
  na.rm = TRUE)

pfc_se_data_speechhbr <- summarySE(
  pfc_se_data_speechhbr,
  measurevar = "MeanHb",
  groupvars = c("Spatialization"),
  na.rm = TRUE)


plotspeechhbr <- ggplot() +

  # --- Individual subject means ---
  geom_point(
    data = aggregate(
      MeanHb ~ S + Spatialization,
      data = all_data_cleaned_pfc_speech_hbr,
      FUN = mean),
    aes(x = Spatialization,
        y = MeanHb),
    color = "blue",
    size = 2,
    alpha = 0.3,
    position = position_jitter(width = 0.08)
  ) +
  
  # --- Error bars ---
  geom_errorbar(
    data = pfc_se_data_speechhbr,
    aes(x = Spatialization,
        y = MeanHb,
        ymin = MeanHb - se,
        ymax = MeanHb + se),
    width = 0.5,
    linewidth = 0.8
  ) +
  
  # --- Mean point ---
  geom_point(
    data = pfc_se_data_speechhbr,
    aes(x = Spatialization,
        y = MeanHb),
    size = 4,
    shape = 21,
    fill = "blue"
  ) +
  labs(x = "", y = "Mean \u0394HbR (\u03BCM)") +
  ylim(-0.2,0.1) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 18),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 12),
    legend.position = "none"
  ) +
  scale_x_discrete(labels=c("ITD50" = "Small\nITD",
                            "ITD500" = "Large\nITD",
                            "ILD70n" = "Natural\nILD",
                            "ILD10" = "Broadband\nILD"))

# Noise HbR
# Noise HbR
pfc_se_data_noisehbr <- summarySE(
  all_data_cleaned_pfc_noise_hbr,
  measurevar = "MeanHb",
  groupvars = c("S","Spatialization"),
  na.rm = TRUE)

pfc_se_data_noisehbr <- summarySE(
  pfc_se_data_noisehbr,
  measurevar = "MeanHb",
  groupvars = c("Spatialization"),
  na.rm = TRUE)


plotnoisehbr <- ggplot() +
  
  # --- Individual subject means ---
  geom_point(
    data = aggregate(
      MeanHb ~ S + Spatialization,
      data = all_data_cleaned_pfc_noise_hbr,
      FUN = mean),
    aes(x = Spatialization,
        y = MeanHb),
    color = "blue",
    size = 2,
    alpha = 0.3,
    position = position_jitter(width = 0.08)
  ) +
  
  # --- Error bars ---
  geom_errorbar(
    data = pfc_se_data_noisehbr,
    aes(x = Spatialization,
        y = MeanHb,
        ymin = MeanHb - se,
        ymax = MeanHb + se),
    width = 0.5,
    linewidth = 0.8
  ) +
  
  # --- Mean point ---
  geom_point(
    data = pfc_se_data_noisehbr,
    aes(x = Spatialization,
        y = MeanHb),
    size = 4,
    shape = 21,
    fill = "blue"
  ) +
  
  labs(x = "", y = "Mean \u0394HbR (\u03BCM)") +
  ylim(-0.2,0.1) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 18),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 12),
    legend.position = "none"
  ) +
  scale_x_discrete(labels=c("ITD50" = "Small\nITD",
                            "ITD500" = "Large\nITD",
                            "ILD70n" = "Natural\nILD",
                            "ILD10" = "Broadband\nILD"))


plot_mean_hb_pfc_raw <- grid.arrange(plotspeechhbo, plotnoisehbo, plotspeechhbr, plotnoisehbr, ncol=2,  widths = c(1,1), heights = c(4,2))
ggsave("/Users/benrichardson/Documents/GitHub/Broadband-ILD-fNIRS/PAPER FIGURES/mean_hb_pfc_raw.svg", plot = plot_mean_hb_pfc_raw, width = 10, height = 8, units = "in")

















##### STAT MODELS HBO #####

# PFC, Speech Masker Model #
model_pfc_speech_hbo <- mixed(MeanHb ~ Spatialization + (1|S) + (1|Channel),
                              data= all_data_cleaned_pfc_speech_hbo, 
                              control = lmerControl(optimizer = "bobyqa"), method = 'LRT')
model_pfc_speech_hbo 

# Pairwise Comparisons (treatment coding)

# ITD50 as reference
all_data_cleaned_pfc_speech_hbo$Spatialization <- relevel(all_data_cleaned_pfc_speech_hbo$Spatialization, "ITD50")
pfc_speech_lmer_itd50 <- lmer(MeanHb ~ Spatialization + (1|S) + (1|Channel),
                              data= all_data_cleaned_pfc_speech_hbo,
                              control = lmerControl(optimizer = "bobyqa"))#
summary(pfc_speech_lmer_itd50)

# ITD500 as reference
all_data_cleaned_pfc_speech_hbo$Spatialization <- relevel(all_data_cleaned_pfc_speech_hbo$Spatialization, "ITD500")
pfc_speech_lmer_itd500 <- lmer(MeanHb ~ Spatialization + (1|S) + (1|Channel),
                               data= all_data_cleaned_pfc_speech_hbo,
                               control = lmerControl(optimizer = "bobyqa"))#
summary(pfc_speech_lmer_itd500)

# ILD70n as reference
all_data_cleaned_pfc_speech_hbo$Spatialization <- relevel(all_data_cleaned_pfc_speech_hbo$Spatialization, "ILD70n")
pfc_speech_lmer_ild70n <- lmer(MeanHb ~ Spatialization + (1|S) + (1|Channel),
                               data= all_data_cleaned_pfc_speech_hbo,
                               control = lmerControl(optimizer = "bobyqa"))#
summary(pfc_speech_lmer_ild70n)

# ILD10 as reference
all_data_cleaned_pfc_speech_hbo$Spatialization <- relevel(all_data_cleaned_pfc_speech_hbo$Spatialization, "ILD10")
pfc_speech_lmer_ild10 <- lmer(MeanHb ~ Spatialization + (1|S) + (1|Channel),
                              data= all_data_cleaned_pfc_speech_hbo,
                              control = lmerControl(optimizer = "bobyqa"))#
summary(pfc_speech_lmer_ild10)



#PFC, Noise Masker Model #
model_pfc_noise_hbo <- mixed(MeanHb ~ Spatialization + (1|S) + (1|Channel),
                             data= all_data_cleaned_pfc_noise_hbo, 
                             control = lmerControl(optimizer = "bobyqa"), method = 'LRT')
model_pfc_noise_hbo

# Pairwise Comparisons (treatment coding)

# ITD50 as reference
all_data_cleaned_pfc_noise_hbo$Spatialization <- relevel(all_data_cleaned_pfc_noise_hbo$Spatialization, "ITD50")
pfc_noise_lmer_itd50 <- lmer(MeanHb ~ Spatialization + (1|S) + (1|Channel),
                             data= all_data_cleaned_pfc_noise_hbo,
                             control = lmerControl(optimizer = "bobyqa"))#
#summary(pfc_noise_lmer_itd50)

# ITD500 as reference
all_data_cleaned_pfc_noise_hbo$Spatialization <- relevel(all_data_cleaned_pfc_noise_hbo$Spatialization, "ITD500")
pfc_noise_lmer_itd500 <- lmer(MeanHb ~ Spatialization + (1|S) + (1|Channel),
                              data= all_data_cleaned_pfc_noise_hbo,
                              control = lmerControl(optimizer = "bobyqa"))#
#summary(pfc_noise_lmer_itd500)

# ILD70n as reference
all_data_cleaned_pfc_noise_hbo$Spatialization <- relevel(all_data_cleaned_pfc_noise_hbo$Spatialization, "ILD70n")
pfc_noise_lmer_ild70n <- lmer(MeanHb ~ Spatialization + (1|S) + (1|Channel),
                              data= all_data_cleaned_pfc_noise_hbo,
                              control = lmerControl(optimizer = "bobyqa"))#
#summary(pfc_noise_lmer_ild70n)

# ILD10 as reference
all_data_cleaned_pfc_noise_hbo$Spatialization <- relevel(all_data_cleaned_pfc_noise_hbo$Spatialization, "ILD10")
pfc_noise_lmer_ild10 <- lmer(MeanHb ~ Spatialization + (1|S) + (1|Channel),
                             data= all_data_cleaned_pfc_noise_hbo,
                             control = lmerControl(optimizer = "bobyqa"))#
#summary(pfc_lmer_noise_ild10)


# STG, Speech Masker Model #
model_stg_speech_hbo <- mixed(MeanHb ~ Spatialization + (1|S) + (1|Channel),
                              data= all_data_cleaned_stg_speech_hbo, 
                              control = lmerControl(optimizer = "bobyqa"), method = 'LRT')
model_stg_speech_hbo

# Pairwise Comparisons (treatment coding)

# ITD50 as reference
all_data_cleaned_stg_speech_hbo$Spatialization <- relevel(all_data_cleaned_stg_speech_hbo$Spatialization, "ITD50")
stg_speech_lmer_itd50 <- lmer(MeanHb ~ Spatialization + (1|S) + (1|Channel),
                              data= all_data_cleaned_stg_speech_hbo,
                              control = lmerControl(optimizer = "bobyqa"))#
#summary(stg_speech_lmer_itd50)

# ITD500 as reference
all_data_cleaned_stg_speech_hbo$Spatialization <- relevel(all_data_cleaned_stg_speech_hbo$Spatialization, "ITD500")
stg_speech_lmer_itd500 <- lmer(MeanHb ~ Spatialization + (1|S) + (1|Channel),
                               data= all_data_cleaned_stg_speech_hbo,
                               control = lmerControl(optimizer = "bobyqa"))#
#summary(stg_speech_lmer_itd500)

# ILD70n as reference
all_data_cleaned_stg_speech_hbo$Spatialization <- relevel(all_data_cleaned_stg_speech_hbo$Spatialization, "ILD70n")
stg_speech_lmer_ild70n <- lmer(MeanHb ~ Spatialization + (1|S) + (1|Channel),
                               data= all_data_cleaned_stg_speech_hbo,
                               control = lmerControl(optimizer = "bobyqa"))#
#summary(stg_speech_lmer_ild70n)

# ILD10 as reference
all_data_cleaned_stg_speech_hbo$Spatialization <- relevel(all_data_cleaned_stg_speech_hbo$Spatialization, "ILD10")
stg_speech_lmer_ild10 <- lmer(MeanHb ~ Spatialization + (1|S) + (1|Channel),
                              data= all_data_cleaned_stg_speech_hbo,
                              control = lmerControl(optimizer = "bobyqa"))#
#summary(stg_speech_lmer_ild10)


# STG, Noise Masker Model #
model_stg_noise_hbo <- mixed(MeanHb ~ Spatialization + (1|S) + (1|Channel),
                             data= all_data_cleaned_stg_noise_hbo, 
                             control = lmerControl(optimizer = "bobyqa"), method = 'LRT')
model_stg_noise_hbo



#### STAT MODELS HBR ####

# PFC Speech
model_pfc_speech_hbr <- mixed(MeanHb ~ Spatialization + (1|S) + (1|Channel),
                              data= all_data_cleaned_pfc_speech_hbr, 
                              control = lmerControl(optimizer = "bobyqa"), method = 'LRT')
model_pfc_speech_hbr


# PFC Noise
model_pfc_noise_hbr <- mixed(MeanHb ~ Spatialization + (1|S) + (1|Channel),
                             data= all_data_cleaned_pfc_noise_hbr, 
                             control = lmerControl(optimizer = "bobyqa"), method = 'LRT')
model_pfc_noise_hbr

# STG Speech
model_stg_speech_hbr <- mixed(MeanHb ~ Spatialization + (1|S) + (1|Channel),
                              data= all_data_cleaned_stg_speech_hbr, 
                              control = lmerControl(optimizer = "bobyqa"), method = 'LRT')
model_stg_speech_hbr

# STG Noise
model_stg_noise_hbr <- mixed(MeanHb ~ Spatialization + (1|S) + (1|Channel),
                             data= all_data_cleaned_stg_noise_hbr, 
                             control = lmerControl(optimizer = "bobyqa"), method = 'LRT')
model_stg_noise_hbr








