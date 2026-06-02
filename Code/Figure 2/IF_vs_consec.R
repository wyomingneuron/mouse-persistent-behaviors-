library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)
library(lubridate)
library(ggpubr)
library(gridExtra)
df_FR  <- data.frame(consec_Other = numeric(0))
df_22  <- data.frame(consec_Other = numeric(0))
df_55  <- data.frame(consec_Other = numeric(0))
df_RPR <- data.frame(consec_Other = numeric(0))

# Folder containing all CSV files
data_folder <- "C:/Users/sunlab/OneDrive - University of Wyoming/Reversal paper/Reversal paper/Data/Reversal data final"

# List all CSV files
all_files <- list.files(data_folder, pattern = "\\.CSV$", full.names = TRUE)

# Loop over all files
for (file in all_files) {
  
  # --- Smart filename parsing ---
  file_name <- basename(file)                 # e.g., "1FR_1.CSV"
  file_name_no_ext <- sub("\\.CSV$", "", file_name)
  
  # Extract mouse number (after last _)
  mouse_num <- as.integer(sub(".*_(\\d+)$", "\\1", file_name_no_ext))
  
  # Extract paradigm number (leading digit(s))
  paradigm_num <- as.integer(sub("^(\\d+).*", "\\1", file_name_no_ext))
  
  # Extract paradigm name (everything between leading digit(s) and last _mouse)
  paradigm_name <- sub("^\\d+(.*)_\\d+$", "\\1", file_name_no_ext)
  
  # Remove leading underscore
  paradigm_name <- sub("^_", "", paradigm_name)
  
  # --- Begin single-mouse code ---
  
  my_data <- read.csv(file)
  
  my_data$Event <- gsub("RightWithPellet", "Right_WP", my_data$Event)
  my_data$Event <- gsub("RightDuringDispense", "Right_DD", my_data$Event)
  my_data$Event <- gsub("LeftWithPellet", "Left_WP", my_data$Event)
  my_data$Event <- gsub("LeftDuringDispense", "Left_DD", my_data$Event)
  my_data$Event <- gsub("RightinTimeout", "Right_TO", my_data$Event)
  my_data$Event <- gsub("RightinTimeOut", "Right_TO", my_data$Event)
  my_data$Event <- gsub("LeftinTimeOut", "Left_TO", my_data$Event)
  my_data$Event <- gsub("LeftinTimeout", "Left_TO", my_data$Event)
  
  Event_cleaned <- my_data$Event
  df <- data.frame(
    TimePoint = 1:length(Event_cleaned),
    Value = Event_cleaned
  )
  
  df$TimePoint <- my_data[,2]
  df2 <- df[df$Value != "Pellet", ]
  time_diff_seconds <- diff(df2$TimePoint) / 1000
  filtered_rows <- which(df$Value != "Pellet")
  
  df3 <- df[df$Value == "Pellet", ]
  time_diff_seconds_p <- diff(df3$TimePoint) / 1000
  filtered_rows_p <- which(df$Value == "Pellet")
  
  instant_freq <- 1 / time_diff_seconds
  instant_freq_p <- 1 / time_diff_seconds_p
  
  freq_df <- data.frame(
    EventIndex = filtered_rows[2:length(filtered_rows)],
    InstantFrequency = instant_freq
  )
  
  freq_dfp <- data.frame(
    EventIndex = filtered_rows_p[2:length(filtered_rows_p)],
    InstantFrequency = instant_freq_p
  )
  
  IF_poke_average <- mean(instant_freq, na.rm = TRUE)
  IF_pellet_average <- mean(instant_freq_p, na.rm = TRUE)
  
  if (paradigm_num == 1) {
    nXn <- 0
  } else if (paradigm_num == 2) {
    nXn <- 2
  } else if (paradigm_num == 3) {
    nXn <- 5
  } else if (paradigm_num == 4) {
    nXn <- "inf"
  } 
  
  # --- Activate column logic ---
  
  df <- data.frame(
    TimePoint = 1:length(Event_cleaned),
    Value = Event_cleaned
  )
  
  if (nXn == 0) {
    
    df$activate <- NA
    df$activate[1] <- "C"
    first_pellet <- which(df$Value == "Pellet")[1]
    if (first_pellet > 2) {
      df$activate[2:(first_pellet-1)] <- "rep"
    }
    df$activate[df$Value == "Pellet"] <- "P"
    pellet_rows <- which(df$Value == "Pellet")
    next_rows <- pellet_rows + 1
    next_rows <- next_rows[next_rows <= nrow(df)]
    df$activate[next_rows] <- "C"
    df$activate[is.na(df$activate)] <- "rep"
    
    fullraw <- df[,1:3]
    fullraw$Time <- my_data[,2]/1000
    
  } else if (nXn == "inf") {
    
    df$activate <- NA
    rep_set <- c("Left_WP", "Right_WP", "Left_DD", "Right_DD")
    pellet_indices <- which(df$Value == "Pellet")
    
    for (idx in pellet_indices) {
      df$activate[idx] <- "P"
      i <- idx - 1
      while (i > 0) {
        if (df$Value[i] %in% rep_set) {
          df$activate[i] <- "rep"
        } else {
          df$activate[i] <- "C"
          break
        }
        i <- i - 1
      }
    }
    df$activate[is.na(df$activate)] <- "I"
    fullraw <- df[,1:3]
    fullraw$Time <- my_data[,2]/1000
    
  } else {
    
    Pcol <- df$TimePoint[df$Value=='Pellet']
    num_blocks <- ceiling(length(Pcol) / nXn)
    index <- rep(c("L", "R"), length.out = num_blocks)
    index_vector <- rep(index, each = nXn)[1:length(Pcol)]
    df$activate[df$Value=='Pellet'] <- index_vector
    
    last_L_index <- NA
    last_R_index <- NA
    
    for (i in 1:nrow(df)) {
      if (!is.na(df$activate[i]) && df$activate[i] == 'L') {
        if (!is.na(last_L_index) || !is.na(last_R_index)) {
          start_index <- max(last_L_index, last_R_index, na.rm = TRUE) + 1
          first_left_match_index <- which(df$Value[start_index:(i-1)] %in% c('Left', 'Left_DD', 'Left_WP','Left_TO'))[1] + start_index - 1
        } else {
          first_left_match_index <- which(df$Value[1:(i-1)] %in% c('Left', 'Left_DD', 'Left_WP','Left_TO'))[1]
        }
        if (!is.na(first_left_match_index)) {
          if (df$Value[first_left_match_index] == 'Left') df$activate[first_left_match_index] <- 'C'
          else if (df$Value[first_left_match_index] == 'Left_DD') df$activate[first_left_match_index] <- 'C_DD'
          else if (df$Value[first_left_match_index] == 'Left_WP') df$activate[first_left_match_index] <- 'C_WP'
          else if (df$Value[first_left_match_index] == 'Left_TO') df$activate[first_left_match_index] <- 'C_TO'
        }
        last_L_index <- i
      }
      if (!is.na(df$activate[i]) && df$activate[i] == 'R') {
        if (!is.na(last_L_index) || !is.na(last_R_index)) {
          start_index <- max(last_L_index, last_R_index, na.rm = TRUE) + 1
          first_right_match_index <- which(df$Value[start_index:(i-1)] %in% c('Right', 'Right_DD', 'Right_WP', 'Right_TO'))[1] + start_index - 1
        } else {
          first_right_match_index <- which(df$Value[1:(i-1)] %in% c('Right', 'Right_DD', 'Right_WP','Right_TO'))[1]
        }
        if (!is.na(first_right_match_index)) {
          if (df$Value[first_right_match_index] == 'Right') df$activate[first_right_match_index] <- 'C'
          else if (df$Value[first_right_match_index] == 'Right_DD') df$activate[first_right_match_index] <- 'C_DD'
          else if (df$Value[first_right_match_index] == 'Right_WP') df$activate[first_right_match_index] <- 'C_WP'
          else if (df$Value[first_right_match_index] == 'Right_TO') df$activate[first_right_match_index] <- 'C_TO'
        }
        last_R_index <- i
      }
    }
    
    # Repeat handling between C and L/R
    for (i in 1:nrow(df)) {
      k <- 1
      if ((df$activate[i] %in% c('C','C_DD','C_WP','C_TO')) & !is.na(df$activate[i])) {
        while (is.na(df$activate[i+k])) k <- k+1
      }
      if (df$activate[i+k]=='L' && k>1) {
        for (l in 1:(k-1)) {
          if (df$Value[i+l]=='Left') df$activate[i+l]='crep'
          else if (df$Value[i+l]=='Left_WP') df$activate[i+l]='crep_WP'
          else if (df$Value[i+l]=='Left_DD') df$activate[i+l]='crep_DD'
          else if (df$Value[i+l]=='Left_TO') df$activate[i+l]='crep_TO'
          else if (df$Value[i+l]=='Right') df$activate[i+l]='irep'
          else if (df$Value[i+l]=='Right_DD') df$activate[i+l]='irep_DD'
          else if (df$Value[i+l]=='Right_WP') df$activate[i+l]='irep_WP'
          else if (df$Value[i+l]=='Right_TO') df$activate[i+l]='irep_TO'
        }
      }
      if (df$activate[i+k]=='R' && k>1) {
        for (l in 1:(k-1)) {
          if (df$Value[i+l]=='Right') df$activate[i+l]='crep'
          else if (df$Value[i+l]=='Right_WP') df$activate[i+l]='crep_WP'
          else if (df$Value[i+l]=='Right_DD') df$activate[i+l]='crep_DD'
          else if (df$Value[i+l]=='Right_TO') df$activate[i+l]='crep_TO'
          else if (df$Value[i+l]=='Left') df$activate[i+l]='irep'
          else if (df$Value[i+l]=='Left_DD') df$activate[i+l]='irep_DD'
          else if (df$Value[i+l]=='Left_WP') df$activate[i+l]='irep_WP'
          else if (df$Value[i+l]=='Left_TO') df$activate[i+l]='irep_TO'
        }
      }
    }
    
    last_pellet <- tail(which(df$Value == "Pellet"), 1)
    df$activate[is.na(df$activate)] <- "I"
    fullraw <- df[,1:3]
    fullraw$Time <- my_data[,2]/1000
  }
  
  plot_df <- freq_df %>%
    left_join(fullraw %>% dplyr::select(TimePoint, activate),
              by = c("EventIndex" = "TimePoint"))
  
  plot_df$ColorGroup <- ifelse(plot_df$activate == "C", "C", "Other")

 
  count <- 0
  plot_df$consec_Other<-0
  for(i in 1:nrow(plot_df)) {
    if(plot_df$ColorGroup[i] == "Other") {
      count <- count + 1
      plot_df$consec_Other[i] <- count
    } else {
      count <- 0
    }
  }

  avg_df <- aggregate(
    InstantFrequency ~ consec_Other,
    data = plot_df,
    FUN = mean
  )
  colnames(avg_df)[2]<-mouse_num
  
  if (paradigm_num==1){
    df_FR<-merge(df_FR, avg_df, by = "consec_Other", all = TRUE)
  } else if (paradigm_num==2){
    df_22<-merge(df_22, avg_df, by = "consec_Other", all = TRUE)
  } else if (paradigm_num==3){
    df_55<-merge(df_55, avg_df, by = "consec_Other", all = TRUE)
  } else {
    df_RPR<-merge(df_RPR, avg_df, by = "consec_Other", all = TRUE)
  }
}


df_FR_long <- df_FR %>%
  pivot_longer(
    cols = -consec_Other, 
    names_to = "Mouse", 
    values_to = "Value"
  )
df_22_long  <- pivot_longer(df_22,  cols = -consec_Other, names_to = "Mouse", values_to = "Value")
df_55_long  <- pivot_longer(df_55,  cols = -consec_Other, names_to = "Mouse", values_to = "Value")
df_RPR_long <- pivot_longer(df_RPR, cols = -consec_Other, names_to = "Mouse", values_to = "Value")




plot_panel <- function(df_long, title) {
  ggplot(df_long, aes(x = consec_Other, y = Value)) +
    geom_boxplot(aes(group = consec_Other), outlier.shape = NA, alpha = 0.3) +  # boxplot
    geom_point(aes(color = Mouse), position = position_jitter(width = 0.1, height = 0)) +  # dots
    geom_line(aes(group = Mouse, color = Mouse), alpha = 0.6) +  # connect dots per mouse
    labs(
      x = "Number of Consecutive Other Pokes",
      y = "Average Instantaneous Frequency",
      title = title
    ) +
    theme_classic() +
    theme(legend.position = "none")  # hide legend if too many mice
}
  
p_FR  <- plot_panel(df_FR_long,  "FR")
p_22  <- plot_panel(df_22_long,  "2×2")
p_55  <- plot_panel(df_55_long,  "5×5")
p_RPR <- plot_panel(df_RPR_long, "RPR")

grid.arrange(p_FR, p_22, p_55, p_RPR, ncol = 2)
  
  
#statistics
library(nlme)
df_FR_long_trim  <- subset(df_FR_long,  consec_Other < 20)
df_22_long_trim  <- subset(df_22_long,  consec_Other < 20)
df_55_long_trim  <- subset(df_55_long,  consec_Other < 20)
df_RPR_long_trim <- subset(df_RPR_long, consec_Other < 20)



# Fit quadratic LME models with heterogeneous residuals per x
model_FR  <- lme(Value ~ as.numeric(consec_Other) + I(as.numeric(consec_Other)^2),
                 random = ~1 | Mouse,
                 data = df_FR_long_trim,
                 weights = varIdent(form = ~1 | consec_Other),
                 na.action = na.omit)

model_22  <- lme(Value ~ as.numeric(consec_Other) + I(as.numeric(consec_Other)^2),
                 random = ~1 | Mouse,
                 data = df_22_long_trim,
                 weights = varIdent(form = ~1 | consec_Other),
                 na.action = na.omit)

model_55  <- lme(Value ~ as.numeric(consec_Other) + I(as.numeric(consec_Other)^2),
                 random = ~1 | Mouse,
                 data = df_55_long_trim,
                 weights = varIdent(form = ~1 | consec_Other),
                 na.action = na.omit)

model_RPR <- lme(Value ~ as.numeric(consec_Other) + I(as.numeric(consec_Other)^2),
                 random = ~1 | Mouse,
                 data = df_RPR_long_trim,
                 weights = varIdent(form = ~1 | consec_Other),
                 na.action = na.omit)





# View summaries with p-values
summary(model_FR)
summary(model_22)
summary(model_55)
summary(model_RPR)


### BH
pvals <- c(
  summary(model_FR)$tTable["as.numeric(consec_Other)", "p-value"],
  summary(model_FR)$tTable["I(as.numeric(consec_Other)^2)", "p-value"],
  
  summary(model_22)$tTable["as.numeric(consec_Other)", "p-value"],
  summary(model_22)$tTable["I(as.numeric(consec_Other)^2)", "p-value"],
  
  summary(model_55)$tTable["as.numeric(consec_Other)", "p-value"],
  summary(model_55)$tTable["I(as.numeric(consec_Other)^2)", "p-value"],
  
  summary(model_RPR)$tTable["as.numeric(consec_Other)", "p-value"],
  summary(model_RPR)$tTable["I(as.numeric(consec_Other)^2)", "p-value"]
)


pvals_BH <- p.adjust(pvals, method = "BH")


format(pvals_BH, scientific = FALSE)


# =========================
# Plotting with quadratic curves
# =========================
plot_panel_quad <- function(df_long, model, title) {
  # Create a smooth sequence of X for curve
  newdata <- data.frame(
    consec_Other = seq(min(df_long$consec_Other, na.rm = TRUE),
                       max(df_long$consec_Other, na.rm = TRUE),
                       length.out = 100)
  )
  
  # Predict population-level (fixed effect) curve
  newdata$pred <- predict(model, newdata, level = 0)  # level = 0 = fixed effects only in nlme
  
  # Plot
  ggplot(df_long, aes(x = consec_Other, y = Value)) +
    geom_boxplot(aes(group = consec_Other), outlier.shape = NA, alpha = 0.3) +  # boxplot
    geom_point(aes(color = Mouse), position = position_jitter(width = 0.1, height = 0)) +  # dots
    geom_line(aes(group = Mouse, color = Mouse), alpha = 0.6) +  # connect mouse points
    geom_line(data = newdata, aes(x = consec_Other, y = pred), color = "black", size = 1.2) +  # quadratic fit
    labs(
      x = "Number of Consecutive Other Pokes",
      y = "Average Instantaneous Frequency",
      title = title
    ) +
    theme_classic() +
    theme(legend.position = "none")
}

# Generate each panel with quadratic fit
p_FR  <- plot_panel_quad(df_FR_long_trim,  model_FR,  "FR")
p_22  <- plot_panel_quad(df_22_long_trim,  model_22,  "2×2")
p_55  <- plot_panel_quad(df_55_long_trim,  model_55,  "5×5")
p_RPR <- plot_panel_quad(df_RPR_long_trim, model_RPR, "RPR")

# Combine 4 panels in a 2x2 layout
grid.arrange(p_FR, p_22, p_55, p_RPR, ncol = 2)
