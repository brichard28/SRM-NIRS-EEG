
library(tidyverse)
library(ggpubr)
library(ggplot2)
library(rstatix)
library(afex)
library(dplyr)
library(car)
library(emmeans)
library(gridExtra)

# Data Preparation ####
# Load in Data
attend_right_speech_data_hbo <- read.csv("/Users/benrichardson/Documents/GitHub/Broadband-ILD-fNIRS/ANALYSIS SCRIPTS/Eli Analysis/all_subjects_mean_during_stim_lateralization_target_right_speech_masker.csv")
attend_left_speech_data_hbo <- read.csv("/Users/benrichardson/Documents/GitHub/Broadband-ILD-fNIRS/ANALYSIS SCRIPTS/Eli Analysis/all_subjects_mean_during_stim_lateralization_target_left_speech_masker.csv")
attend_right_noise_data_hbo <- read.csv("/Users/benrichardson/Documents/GitHub/Broadband-ILD-fNIRS/ANALYSIS SCRIPTS/Eli Analysis/all_subjects_mean_during_stim_lateralization_target_right_noise_masker.csv")
attend_left_noise_data_hbo <- read.csv("/Users/benrichardson/Documents/GitHub/Broadband-ILD-fNIRS/ANALYSIS SCRIPTS/Eli Analysis/all_subjects_mean_during_stim_lateralization_target_left_noise_masker.csv")

attend_right_speech_data_hbr <- read.csv("/Users/benrichardson/Documents/GitHub/Broadband-ILD-fNIRS/ANALYSIS SCRIPTS/Eli Analysis/all_subjects_mean_during_stim_lateralization_target_right_speech_masker_hbr.csv")
attend_left_speech_data_hbr <- read.csv("/Users/benrichardson/Documents/GitHub/Broadband-ILD-fNIRS/ANALYSIS SCRIPTS/Eli Analysis/all_subjects_mean_during_stim_lateralization_target_left_speech_masker_hbr.csv")
attend_right_noise_data_hbr <- read.csv("/Users/benrichardson/Documents/GitHub/Broadband-ILD-fNIRS/ANALYSIS SCRIPTS/Eli Analysis/all_subjects_mean_during_stim_lateralization_target_right_noise_masker_hbr.csv")
attend_left_noise_data_hbr <- read.csv("/Users/benrichardson/Documents/GitHub/Broadband-ILD-fNIRS/ANALYSIS SCRIPTS/Eli Analysis/all_subjects_mean_during_stim_lateralization_target_left_noise_masker_hbr.csv")

colnames(attend_right_speech_data_hbo) <- c("S","Channel","ITD50","ITD500","ILD70n","ILD10")
colnames(attend_right_noise_data_hbo) <- c("S","Channel","ITD50","ITD500","ILD70n","ILD10")
colnames(attend_left_speech_data_hbo) <- c("S","Channel","ITD50","ITD500","ILD70n","ILD10")
colnames(attend_left_noise_data_hbo) <- c("S","Channel","ITD50","ITD500","ILD70n","ILD10")

colnames(attend_right_speech_data_hbr) <- c("S","Channel","ITD50","ITD500","ILD70n","ILD10")
colnames(attend_right_noise_data_hbr) <- c("S","Channel","ITD50","ITD500","ILD70n","ILD10")
colnames(attend_left_speech_data_hbr) <- c("S","Channel","ITD50","ITD500","ILD70n","ILD10")
colnames(attend_left_noise_data_hbr) <- c("S","Channel","ITD50","ITD500","ILD70n","ILD10")

attend_right_speech_data_hbo$chromophore <- "HbO"
attend_right_speech_data_hbr$chromophore <- "HbR"
attend_left_speech_data_hbo$chromophore <- "HbO"
attend_left_speech_data_hbr$chromophore <- "HbR"

attend_right_noise_data_hbo$chromophore <- "HbO"
attend_right_noise_data_hbr$chromophore <- "HbR"
attend_left_noise_data_hbo$chromophore <- "HbO"
attend_left_noise_data_hbr$chromophore <- "HbR"


attend_right_speech_data <- rbind(attend_right_speech_data_hbo,attend_right_speech_data_hbr)
attend_left_speech_data <- rbind(attend_left_speech_data_hbo,attend_left_speech_data_hbr)
attend_right_noise_data <- rbind(attend_right_noise_data_hbo,attend_right_noise_data_hbr)
attend_left_noise_data <- rbind(attend_left_noise_data_hbo,attend_left_noise_data_hbr)

attend_right_speech_data <- pivot_longer(attend_right_speech_data, cols=c("ITD50","ITD500","ILD70n","ILD10"), names_to = "Spatialization", values_to = "MeanHb")
attend_right_noise_data <- pivot_longer(attend_right_noise_data, cols=c("ITD50","ITD500","ILD70n","ILD10"), names_to = "Spatialization", values_to = "MeanHb")
attend_left_speech_data <- pivot_longer(attend_left_speech_data, cols=c("ITD50","ITD500","ILD70n","ILD10"), names_to = "Spatialization", values_to = "MeanHb")
attend_left_noise_data <- pivot_longer(attend_left_noise_data, cols=c("ITD50","ITD500","ILD70n","ILD10"), names_to = "Spatialization", values_to = "MeanHb")

attend_right_speech_data <- attend_right_speech_data[!attend_right_speech_data$S == '2',]
attend_right_speech_data <- attend_right_speech_data[!attend_right_speech_data$S == '1',]
attend_right_noise_data <- attend_right_noise_data[!attend_right_noise_data$S == '2',]
attend_right_noise_data <- attend_right_noise_data[!attend_right_noise_data$S == '1',]

attend_left_speech_data <- attend_left_speech_data[!attend_left_speech_data$S == '2',]
attend_left_speech_data <- attend_left_speech_data[!attend_left_speech_data$S == '1',]
attend_left_noise_data <- attend_left_noise_data[!attend_left_noise_data$S == '2',]
attend_left_noise_data <- attend_left_noise_data[!attend_left_noise_data$S == '1',]



attend_right_speech_data$Masker <- "speech"
attend_left_speech_data$Masker <- "speech"
attend_right_noise_data$Masker <- "noise"
attend_left_noise_data$Masker <- "noise"

attend_right_data <-rbind(attend_right_speech_data,attend_right_noise_data)
attend_left_data <-rbind(attend_left_speech_data,attend_left_noise_data)


# Add Ipsilateral/Contralateral Information
left_hemisphere_channels <- c(0,1,2,3,10,11,12,13)
right_hemisphere_channels <- c(4,5,6,7,8,9)

attend_right_data$Hemisphere <- NA
attend_left_data$Hemisphere <- NA

attend_right_data$Hemisphere[which(attend_right_data$Channel %in% right_hemisphere_channels)] <- "Ipsilateral"
attend_right_data$Hemisphere[which(attend_right_data$Channel %in% left_hemisphere_channels)] <- "Contralateral"

attend_left_data$Hemisphere[which(attend_left_data$Channel %in% right_hemisphere_channels)] <- "Contralateral"
attend_left_data$Hemisphere[which(attend_left_data$Channel %in% left_hemisphere_channels)] <- "Ipsilateral"

attend_right_data <- na.omit(attend_right_data)
attend_left_data <- na.omit(attend_left_data)

# Combine attend left and right
attend_right_data$Attend<-"right"
attend_left_data$Attend<-"left"
all_data<-rbind(attend_right_data,attend_left_data)

# Add ROI information
all_data$Roi<-NA
pfc_channels <- c(0,1,2,3,4,5)
stg_channels <- c(6,7,8,9,10,11,12,13)
all_data$Roi[which(all_data$Channel %in% pfc_channels)] <- "pfc"
all_data$Roi[which(all_data$Channel %in% stg_channels)] <- "stg"

# Organize Factors
to.factor <- c('S', 'Roi', 'Hemisphere', 'Masker', 'Attend', 'Spatialization')
all_data[, to.factor] <- lapply(all_data[, to.factor], as.factor)

all_data_cleaned <- na.omit(all_data)

all_data_cleaned$Spatialization <- factor(
  all_data_cleaned$Spatialization,
  levels = c("ITD50", "ITD500", "ILD70n", "ILD10")
)


# Check for normality, remove outliers
#shapiro.test(all_data_cleaned$MeanHb)


# Summary SE Function ####
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


# STG, Speech Masker Model ####

all_data_cleaned_stg_speechhbo <-  subset(all_data_cleaned, chromophore == "HbO" &  Roi == "stg" & Masker == "speech")
all_data_cleaned_stg_speechhbr <-  subset(all_data_cleaned, chromophore == "HbR" &  Roi == "stg" & Masker == "speech")

z2_stg_speech <- mixed(MeanHb ~ Spatialization*Hemisphere + (1|S) + (1|Channel),
                       data= all_data_cleaned_stg_speechhbo, 
                       control = lmerControl(optimizer = "bobyqa"), method = 'LRT')

z2_stg_speech

# Significant interaction between spatialization and hemisphere
# Pairwise Comparisons

EMM_stg_speech <- emmeans(z2_stg_speech, ~ Spatialization * Hemisphere)
pairs(EMM_stg_speech, simple = "Spatialization", adjust = "bonferroni")
pairs(EMM_stg_speech, simple = "Hemisphere", adjust = "bonferroni")


# STG, Noise Masker Model ####

all_data_cleaned_stg_noisehbo <-  subset(all_data_cleaned, chromophore == "HbO" & Roi == "stg" & Masker == "noise")
all_data_cleaned_stg_noisehbr <-  subset(all_data_cleaned, chromophore == "HbR" & Roi == "stg" & Masker == "noise")

z2_stg_noise <- mixed(MeanHb ~ Spatialization*Hemisphere + (1|S) + (1|Channel),
                       data= all_data_cleaned_stg_noisehbo, 
                       control = lmerControl(optimizer = "bobyqa"), method = 'LRT')

z2_stg_noise

# Significant interaction between spatialization and hemisphere
EMM_stg_noise <- emmeans(z2_stg_noise, ~ Spatialization * Hemisphere)
pairs(EMM_stg_noise, simple = "Spatialization", adjust = "bonferroni")
pairs(EMM_stg_noise, simple = "Hemisphere", adjust = "bonferroni")




# IF INCLUDING BOTH SPEECH AND NOISE
# HbO
all_data_cleaned_stg_hbo <- subset(all_data_cleaned, chromophore == "HbO" &  Roi == "stg")
model_stg_hbo <- mixed(MeanHb ~ Spatialization*Hemisphere*Masker + (1|S) + (1|Channel), data = all_data_cleaned_stg_hbo, control = lmerControl(optimizer = "bobyqa"), method = 'LRT')

all_data_cleaned_stg_hbo$Hemisphere <- relevel(all_data_cleaned_stg_hbo$Hemisphere, "Contralateral")
posthoc_stg_itd50_hbo <- lmer(MeanHb ~ Hemisphere + (1|S) + (1|Channel),
                                  data= subset(all_data_cleaned_stg_hbo, Spatialization == "ITD50"), 
                                  control = lmerControl(optimizer = "bobyqa"))

summary(posthoc_stg_itd50_hbo)


posthoc_stg_itd500_hbo <- lmer(MeanHb ~ Hemisphere + (1|S) + (1|Channel),
                              data= subset(all_data_cleaned_stg_hbo, Spatialization == "ITD500"), 
                              control = lmerControl(optimizer = "bobyqa"))

summary(posthoc_stg_itd500_hbo)


posthoc_stg_ild70n_hbo <- lmer(MeanHb ~ Hemisphere + (1|S) + (1|Channel),
                               data= subset(all_data_cleaned_stg_hbo, Spatialization == "ILD70n"), 
                               control = lmerControl(optimizer = "bobyqa"))

summary(posthoc_stg_ild70n_hbo)

posthoc_stg_ild10_hbo <- lmer(MeanHb ~ Hemisphere + (1|S) + (1|Channel),
                               data= subset(all_data_cleaned_stg_hbo, Spatialization == "ILD10"), 
                               control = lmerControl(optimizer = "bobyqa"))

summary(posthoc_stg_ild10_hbo)

# HbR
all_data_cleaned_stg_hbr <- subset(all_data_cleaned, chromophore == "HbR" &  Roi == "stg")
model_stg_hbr <- mixed(MeanHb ~ Spatialization*Hemisphere*Masker + (1|S) + (1|Channel), data = all_data_cleaned_stg_hbr, control = lmerControl(optimizer = "bobyqa"), method = 'LRT')




# STG Plot ####
stg_se_data_speechhbo <- summarySE(all_data_cleaned_stg_speechhbo, measurevar="MeanHb", groupvars=c("S","Hemisphere","Spatialization"), na.rm = TRUE)
stg_se_data_speechhbo <- summarySE(stg_se_data_speechhbo, measurevar="MeanHb", groupvars=c("Hemisphere","Spatialization"), na.rm = TRUE)
pd <- position_dodge(width = 0.6)

plotspeechhbo <- ggplot() +
  
  # --- Individual subject means ---
  geom_point(
    data = aggregate(
      MeanHb ~ S + Hemisphere + Spatialization,
      data = all_data_cleaned_stg_speechhbo,
      FUN = mean
    ),
    aes(x = Spatialization,
        y = MeanHb,
        shape = Hemisphere,
        fill = Hemisphere),
    color = "black",
    size = 2,
    stroke = 0.3,
    position = position_jitterdodge(
      jitter.width = 0.08,
      dodge.width = 0.6
    ),
    alpha = 0.3
  ) +
  
  # --- Error bars (group mean ± SE) ---
  geom_errorbar(
    data = stg_se_data_speechhbo,
    aes(x = Spatialization,
        y = MeanHb,
        ymin = MeanHb - se,
        ymax = MeanHb + se,
        group = Hemisphere),
    width = 0.5,
    linewidth = 0.8,
    position = pd
  ) +
  
  # --- Mean symbols ---
  geom_point(
    data = stg_se_data_speechhbo,
    aes(x = Spatialization,
        y = MeanHb,
        shape = Hemisphere,
        fill = Hemisphere),
    size = 4,
    position = pd
  ) +
  
  scale_shape_manual(values = c("Contralateral" = 24,
                                "Ipsilateral" = 22)) +
  
  scale_fill_manual(values = c("Contralateral" = "red",
                               "Ipsilateral" = "darkred")) +
  
  ggtitle("Speech Masker") +
  labs(x = "", y = "Mean \u0394HbO (\u03BCM)") +
  ylim(-0.375,0.5) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 18),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 12),
    legend.position = "none"
  ) + 
  scale_x_discrete(labels=c("ITD50" = "", "ITD500" = "","ILD70n" = "","ILD10" = ""))
  



stg_se_data_noisehbo <- summarySE(all_data_cleaned_stg_noisehbo, measurevar="MeanHb", groupvars=c("S","Hemisphere","Spatialization"), na.rm = TRUE)
stg_se_data_noisehbo <- summarySE(stg_se_data_noisehbo, measurevar="MeanHb", groupvars=c("Hemisphere","Spatialization"), na.rm = TRUE)
plotnoisehbo <- ggplot() +
  # --- Individual subject means ---
  geom_point(
    data = aggregate(
      MeanHb ~ S + Hemisphere + Spatialization,
      data = all_data_cleaned_stg_noisehbo,
      FUN = mean
    ),
    aes(x = Spatialization,
        y = MeanHb,
        shape = Hemisphere,
        fill = Hemisphere),
    color = "black",
    size = 2,
    stroke = 0.3,
    position = position_jitterdodge(
      jitter.width = 0.08,
      dodge.width = 0.6
    ),
    alpha = 0.3
  ) +
  
  # --- Error bars (group mean ± SE) ---
  geom_errorbar(
    data = stg_se_data_noisehbo,
    aes(x = Spatialization,
        y = MeanHb,
        ymin = MeanHb - se,
        ymax = MeanHb + se,
        group = Hemisphere),
    width = 0.5,
    linewidth = 0.8,
    position = pd
  ) +
  
  # --- Mean symbols ---
  geom_point(
    data = stg_se_data_noisehbo,
    aes(x = Spatialization,
        y = MeanHb,
        shape = Hemisphere,
        fill = Hemisphere),
    size = 4,
    position = pd
  ) +
  
  scale_shape_manual(values = c("Contralateral" = 24,
                                "Ipsilateral" = 22)) +
  
  scale_fill_manual(values = c("Contralateral" = "red",
                               "Ipsilateral" = "darkred")) +
  
  ggtitle("Noise Masker") +
  labs(x = "", y = "") +
  ylim(-0.375,0.5) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 18),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 12),
    legend.position = "none"
  ) + 
  scale_x_discrete(labels=c("ITD50" = "", "ITD500" = "","ILD70n" = "","ILD10" = ""))

  


stg_se_data_speechhbr <- summarySE(all_data_cleaned_stg_speechhbr, measurevar="MeanHb", groupvars=c("S","Hemisphere","Spatialization"), na.rm = TRUE)
stg_se_data_speechhbr <- summarySE(stg_se_data_speechhbr, measurevar="MeanHb", groupvars=c("Hemisphere","Spatialization"), na.rm = TRUE)

plotspeechhbr <- ggplot() +
  
  # --- Individual subject means ---
  geom_point(
    data = aggregate(
      MeanHb ~ S + Hemisphere + Spatialization,
      data = all_data_cleaned_stg_speechhbr,
      FUN = mean
    ),
    aes(x = Spatialization,
        y = MeanHb,
        shape = Hemisphere,
        fill = Hemisphere),
    color = "black",
    size = 2,
    stroke = 0.3,
    position = position_jitterdodge(
      jitter.width = 0.08,
      dodge.width = 0.6
    ),
    alpha = 0.3
  ) +
  
  # --- Error bars (group mean ± SE) ---
  geom_errorbar(
    data = stg_se_data_speechhbr,
    aes(x = Spatialization,
        y = MeanHb,
        ymin = MeanHb - se,
        ymax = MeanHb + se,
        group = Hemisphere),
    width = 0.5,
    linewidth = 0.8,
    position = pd
  ) +
  
  # --- Mean symbols ---
  geom_point(
    data = stg_se_data_speechhbr,
    aes(x = Spatialization,
        y = MeanHb,
        shape = Hemisphere,
        fill = Hemisphere),
    size = 4,
    position = pd
  ) +
  
  scale_shape_manual(values = c("Contralateral" = 24,
                                "Ipsilateral" = 22)) +
  
  scale_fill_manual(values = c("Contralateral" = "blue",
                               "Ipsilateral" = "darkblue")) +
  
  labs(x = "", y = "Mean \u0394HbO (\u03BCM)") +
  ylim(-0.25,0.15) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 18),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 12),
    legend.position = "none"
  ) +
  scale_x_discrete(labels=c("ITD50" = "Small\nITD", "ITD500" = "Large\nITD","ILD70n" = "Natural\nILD","ILD10" = "Broadband\nILD"))
  


stg_se_data_noisehbr <- summarySE(all_data_cleaned_stg_noisehbr, measurevar="MeanHb", groupvars=c("S","Hemisphere","Spatialization"), na.rm = TRUE)
stg_se_data_noisehbr <- summarySE(stg_se_data_noisehbr, measurevar="MeanHb", groupvars=c("Hemisphere","Spatialization"), na.rm = TRUE)


plotnoisehbr <- ggplot() +
  
  # --- Individual subject means ---
  geom_point(
    data = aggregate(
      MeanHb ~ S + Hemisphere + Spatialization,
      data = all_data_cleaned_stg_noisehbr,
      FUN = mean
    ),
    aes(x = Spatialization,
        y = MeanHb,
        shape = Hemisphere,
        fill = Hemisphere),
    color = "black",
    size = 2,
    stroke = 0.3,
    position = position_jitterdodge(
      jitter.width = 0.08,
      dodge.width = 0.6
    ),
    alpha = 0.3
  ) +
  
  # --- Error bars (group mean ± SE) ---
  geom_errorbar(
    data = stg_se_data_noisehbr,
    aes(x = Spatialization,
        y = MeanHb,
        ymin = MeanHb - se,
        ymax = MeanHb + se,
        group = Hemisphere),
    width = 0.5,
    linewidth = 0.8,
    position = pd
  ) +
  
  # --- Mean symbols ---
  geom_point(
    data = stg_se_data_noisehbr,
    aes(x = Spatialization,
        y = MeanHb,
        shape = Hemisphere,
        fill = Hemisphere),
    size = 4,
    position = pd
  ) +
  
  scale_shape_manual(values = c("Contralateral" = 24,
                                "Ipsilateral" = 22)) +
  
  scale_fill_manual(values = c("Contralateral" = "blue",
                               "Ipsilateral" = "darkblue")) +
  
  labs(x = "", y = "") +
  ylim(-0.25,0.15) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 18),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 12),
    legend.position = "none"
  ) + 
  scale_x_discrete(labels=c("ITD50" = "Small\nITD", "ITD500" = "Large\nITD","ILD70n" = "Natural\nILD","ILD10" = "Broadband\nILD"))


plot_mean_hb_stg_raw <- grid.arrange(plotspeechhbo, plotnoisehbo, plotspeechhbr, plotnoisehbr, ncol=2,  widths = c(1,1), heights = c(4,2))
ggsave("/Users/benrichardson/Documents/GitHub/Broadband-ILD-fNIRS/PAPER FIGURES/mean_hb_stg_raw.svg", plot = plot_mean_hb_stg_raw, width = 10, height = 8, units = "in")

