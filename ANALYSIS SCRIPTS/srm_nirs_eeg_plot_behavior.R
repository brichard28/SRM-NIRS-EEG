library(ggplot2)
library(tidyr)
library(dplyr)
library(gridExtra)

# --- Load your data ---
# Assuming you've saved CSVs from MATLAB as described
lead_hit <- read.csv("/Users/benrichardson/Documents/GitHub/Broadband-ILD-fNIRS/RESULTS DATA/SRM-NIRS-EEG-1_Lead_Hit_Rates_THROWBOTHWINDOWSOUT.csv", row.names = 1)
lag_hit  <- read.csv("/Users/benrichardson/Documents/GitHub/Broadband-ILD-fNIRS/RESULTS DATA/SRM-NIRS-EEG-1_Lag_Hit_Rates_THROWBOTHWINDOWSOUT.csv", row.names = 1)
lead_FA  <- read.csv("/Users/benrichardson/Documents/GitHub/Broadband-ILD-fNIRS/RESULTS DATA/SRM-NIRS-EEG-1_Lead_FA_Rates_THROWBOTHWINDOWSOUT.csv", row.names = 1)
lag_FA   <- read.csv("/Users/benrichardson/Documents/GitHub/Broadband-ILD-fNIRS/RESULTS DATA/SRM-NIRS-EEG-1_Lag_FA_Rates_THROWBOTHWINDOWSOUT.csv", row.names = 1)

lead_hit_noise <- lead_hit[,c(1,2,3,4)]
lead_hit_speech <- lead_hit[,c(5,6,7,8)]

lag_hit_noise <- lag_hit[,c(1,2,3,4)]
lag_hit_speech <- lag_hit[,c(5,6,7,8)]


lead_FA_speech <- lead_FA
lag_FA_speech <- lag_FA

# Example for lead_hit_noise
conditions <- c("Small\nITDs", "Large\nITDs", "Natural\nILDs", "Broadband\nILDs")

colnames(lead_hit_noise) <- conditions
colnames(lag_hit_noise)  <- conditions
colnames(lead_hit_speech) <- conditions
colnames(lag_hit_speech)  <- conditions
colnames(lead_FA_speech) <- conditions
colnames(lag_FA_speech)  <- conditions


# --- Helper: convert to long format ---
library(tidyr)
library(dplyr)

library(tidyverse)

make_long_df <- function(lead_df, lag_df, masker) {
  
  # Add subject ID
  lead_df <- lead_df %>%
    mutate(subject = row_number())
  
  lag_df <- lag_df %>%
    mutate(subject = row_number())
  
  # Pivot longer
  lead_long <- lead_df %>%
    pivot_longer(
      cols = everything() & !matches("subject"),  # all columns except 'subject'
      names_to = "Condition",
      values_to = "Rate"
    ) %>%
    mutate(Position = "Lead",
           Masker = masker)
  
  lag_long <- lag_df %>%
    pivot_longer(
      cols = everything() & !matches("subject"),
      names_to = "Condition",
      values_to = "Rate"
    ) %>%
    mutate(Position = "Lag",
           Masker = masker)
  
  # Combine lead and lag
  long_df <- bind_rows(lead_long, lag_long)
  
  return(long_df)
}



# --- Prepare data frames ---
# Hit rates
hit_noise_df  <- make_long_df(lead_hit_noise, lag_hit_noise, "Noise")
hit_speech_df <- make_long_df(lead_hit_speech, lag_hit_speech, "Speech")

# FA rates
FA_speech_df   <- make_long_df(lead_FA_speech, lag_FA_speech, "Speech")

# --- Helper: compute mean + SEM ---
summarize_sem <- function(df) {
  df %>%
    group_by(Condition, Type) %>%
    summarise(
      Mean = mean(Rate, na.rm=TRUE),
      SEM  = sd(Rate, na.rm=TRUE) / sqrt(length(Rate[!is.na(Rate)])),
      .groups="drop"
    )
}

# --- Plot function ---
make_plot <- function(df, y_label, plot_title) {
  
  df$Condition <- factor(
    df$Condition,
    levels = c("Small\nITDs", "Large\nITDs", "Natural\nILDs", "Broadband\nILDs")
  )
  
  df$Position <- factor(df$Position, levels = c("Lead","Lag"))
  
  dodge_width <- 0.8
  pd <- position_dodge(width = dodge_width)
  
  # Summary stats: mean + SEM
  summary_df <- df %>%
    group_by(Condition, Position) %>%
    summarise(
      Mean = mean(Rate, na.rm = TRUE),
      SEM  = sd(Rate, na.rm = TRUE)/sqrt(length(Rate[!is.na(Rate)])),
      .groups = "drop"
    )
  
  ggplot() +
    # Violin of individual subjects
    geom_violin(data = df, aes(x=Condition, y=Rate, fill=Position), 
                position = pd, width=0.7, alpha=0.3, show.legend = FALSE) +
    # Individual subject points
    geom_point(data = df, aes(x=Condition, y=Rate, color=Position, shape=Position),
               position = pd, size=5, alpha=0.5) +
    # Mean + SEM
    geom_errorbar(data = summary_df, 
                  aes(x=Condition, ymin=Mean-SEM, ymax=Mean+SEM, group=Position), 
                  color="black", width=1.0, linewidth = 2.0, position=pd) +
    geom_point(data = summary_df, aes(x=Condition, y=Mean, color=Position, shape=Position),
               position = pd, size=10) +
    labs(x="", y="", title="") +
    ylim(0,1.0) + 
    theme_classic() +
    theme(
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.x  = element_text(size = 14),
      axis.text.y  = element_text(size = 14), # 36
      plot.title   = element_text(size = 14),
      legend.title = element_text(size = 14),
      legend.text  = element_text(size = 14)
    ) +
    scale_fill_manual(values=c("Lead"="#0072B2", "Lag"="#d95f02")) +
    scale_color_manual(values=c("Lead"="#0072B2", "Lag"="#d95f02")) +
    scale_shape_manual(values=c("Lead"=16, "Lag"=17)) +
    guides(fill = guide_legend(title="Position"),
           color = guide_legend(title="Position"),
           shape = guide_legend(title="Position"))
}




# --- Create plots ---
p1 <- make_plot(hit_speech_df, "Speech Masker", "Hit Rate")
p2 <- make_plot(hit_noise_df,  "Noise Masker", "")
p3 <- make_plot(FA_speech_df,  "",   "False Alarm Rate")

# --- Empty plot for bottom-right ---
# Make a placeholder plot the same size as others
# Correct placeholder plot
p4 <- ggplot(data = data.frame(x = 1:4, y = 0:1)) +
  geom_blank(aes(x = x, y = y)) +
  theme_void()


# --- Arrange 2x2 grid ---
library(patchwork)

# Combine plots
behavior_plot <- (p1 | p3) / (p2 | p4)  # 2x2 layout

# Save as SVG
ggsave(
  filename = "/Users/benrichardson/Documents/GitHub/Broadband-ILD-fNIRS/PAPER FIGURES/behavior_raw_throwbothwindows_out.svg",
  plot = behavior_plot,
  width = 20,
  height = 14,
  units = "in",
  device = "svg"
)

behavior_plot
