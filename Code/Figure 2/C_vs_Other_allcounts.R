library(ggplot2)
library(dplyr)
library(zoo)
library(lubridate)
library(ggpubr)
library(tidyr)
library(lme4)
library(performance)
library(gridExtra)

#install.packages("reformulas")
#install.packages("glmmTMB")
library(glmmTMB)
# Folder containing all CSV files
setwd("C:/Users/sunlab/OneDrive - University of Wyoming/Reversal paper/Reversal paper/all pictures")
data_folder <- "C:/Users/sunlab/OneDrive - University of Wyoming/Reversal paper/Reversal paper/Data/Reversal data final"

# List all CSV files
all_files <- list.files(data_folder, pattern = "\\.CSV$", full.names = TRUE)

# Initialize results dataframe
results <- data.frame(
  Paradigm = integer(),
  ParadigmName = character(),
  Mouse = integer(),
  Poke_IF_Avg = numeric(),
  Pellet_IF_Avg = numeric(),
  Mean_IF_Poke_C = numeric(),
  Mean_IF_Poke_Other =numeric(),
  other_counts=numeric(),
  poke_counts=numeric(),
  pellet_counts=numeric(),
  stringsAsFactors = FALSE
)

ttest_results <- list()

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
  # --- Begin your single-mouse code ---
  
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
  dfp <- df
  
  # Real time in ms
  df$TimePoint <- my_data[,2]
  df2 <- df[df$Value != "Pellet", ]
  time_diff_seconds <- diff(df2$TimePoint) / 1000
  filtered_rows <- which(df$Value != "Pellet")
  
  df3 <- df[df$Value == "Pellet", ]
  time_diff_seconds_p <- diff(df3$TimePoint) / 1000
  filtered_rows_p <- which(df$Value == "Pellet")
  
  instant_freq <- 1 / time_diff_seconds
  instant_freq_p <- 1 / time_diff_seconds_p

  
  
  # Create data frame for plotting: event index corresponds to the second event in each pair onward
  freq_df <- data.frame(
    EventIndex = filtered_rows[2:length(filtered_rows)],
    InstantFrequency = instant_freq
  )
  
  
  
  # Create data frame for plotting: event index corresponds to the second event in each pair onward
  
  freq_dfp <- data.frame(
    EventIndex = filtered_rows_p[2:length(filtered_rows_p)],
    InstantFrequency = instant_freq_p
  )
  
  
  IF_poke_average <- mean(instant_freq, na.rm = TRUE)
  IF_pellet_average <- mean(instant_freq_p, na.rm = TRUE)
  pellet_counts=nrow(freq_dfp)+1
  poke_counts=nrow(freq_df)+1
  
  
  
  if (paradigm_num == 1) {
    nXn <- 0
  } else if (paradigm_num == 2) {
    nXn <- 2
  } else if (paradigm_num == 3) {
    nXn <- 5
  } else if (paradigm_num == 4) {
    nXn <- "inf"
  } 
  
  
  my_data <- read.csv(file)
  
  my_data$Event <- gsub("RightWithPellet", "Right_WP", my_data$Event)
  my_data$Event <- gsub("RightDuringDispense", "Right_DD", my_data$Event)
  my_data$Event <- gsub("LeftWithPellet", "Left_WP", my_data$Event)
  my_data$Event <- gsub("LeftDuringDispense", "Left_DD", my_data$Event)
  my_data$Event <- gsub("RightinTimeout", "Right_TO", my_data$Event)
  my_data$Event <- gsub("RightinTimeOut", "Right_TO", my_data$Event)
  my_data$Event <- gsub("LeftinTimeOut", "Left_TO", my_data$Event)
  my_data$Event <- gsub("LeftinTimeout", "Left_TO", my_data$Event)
  
  
  
  Event_cleaned=my_data$Event
  # Convert vector to a data frame
  df <- data.frame(
    TimePoint = 1:length(Event_cleaned),  # Time points as the index of the vector
    Value = Event_cleaned
  )
  
  
  if (nXn==0){
    # initialize activate as NA
    df$activate <- NA
    
    # 1. First row always C
    df$activate[1] <- "C"
    
    # 2. Find first Pellet
    first_pellet <- which(df$Value == "Pellet")[1]
    
    # 3. Rows between first row and first Pellet (exclusive) → rep
    if (first_pellet > 2) {
      df$activate[2:(first_pellet-1)] <- "rep"
    }
    
    # 4. Pellet rows → P
    df$activate[df$Value == "Pellet"] <- "P"
    
    # 5. Row after Pellet → C (careful of last row)
    pellet_rows <- which(df$Value == "Pellet")
    next_rows <- pellet_rows + 1
    next_rows <- next_rows[next_rows <= nrow(df)]
    df$activate[next_rows] <- "C"
    
    # 6. Remaining NA → rep
    df$activate[is.na(df$activate)] <- "rep"
    
    fullraw=df[,1:3]
    
    
    ###### ADD real time (ms)
    fullraw$Time<-my_data[,2]/1000
    
    
  }else if (nXn=="inf"){
    
    # initialize activate as NA
    df$activate <- NA
    
    # define rep set
    rep_set <- c("Left_WP", "Right_WP", "Left_DD", "Right_DD")
    
    # find Pellet indices
    pellet_indices <- which(df$Value == "Pellet")
    
    for (idx in pellet_indices) {
      # mark Pellet as P
      df$activate[idx] <- "P"
      
      # look backward
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
    
    # replace remaining NA with I
    df$activate[is.na(df$activate)] <- "I"
    fullraw=df[,1:3]
    
    
    ###### ADD real time (ms)
    fullraw$Time<-my_data[,2]/1000
    
    
  }else {
    
    
    Pcol=df$TimePoint[df$Value=='Pellet']
    
    
    num_blocks <- ceiling(length(Pcol) / nXn)
    
    # Create the index vector
    index <- rep(c("L", "R"), length.out = num_blocks)
    
    # Ensure that the index vector has the correct length to match data
    index_vector <- rep(index, each = nXn)[1:length(Pcol)]
    length(index_vector)
    df$activate[df$Value=='Pellet']=index_vector
    df
    
    # Initialize variables to track the most recent 'L' and 'R' indices
    last_L_index <- NA
    last_R_index <- NA
    
    # Iterate over the rows in the data frame
    for (i in 1:nrow(df)) {
      # Case when 'L' is in 'activate' column and it's not NA
      if (!is.na(df$activate[i]) && df$activate[i] == 'L') {
        # Find the first 'Left', 'Left_DD', or 'Left_WP' in 'Value' before the current 'L',
        # but after the last 'L' or 'R'
        if (!is.na(last_L_index) || !is.na(last_R_index)) {
          start_index <- max(last_L_index, last_R_index, na.rm = TRUE) + 1
          first_left_match_index <- which(df$Value[start_index:(i-1)] %in% c('Left', 'Left_DD', 'Left_WP', 'Left_TO'))[1] + start_index - 1
        } else {
          # If no last L or R, just find the first matching (Left, Left_DD, Left_WP) before that 'L'
          first_left_match_index <- which(df$Value[1:(i-1)] %in% c('Left', 'Left_DD', 'Left_WP','Left_TO'))[1]
        }
        
        if (!is.na(first_left_match_index)) {
          # Mark them with 'C', 'C_DD', or 'C_WP'
          if (df$Value[first_left_match_index] == 'Left') {
            df$activate[first_left_match_index] <- 'C'
          } else if (df$Value[first_left_match_index] == 'Left_DD') {
            df$activate[first_left_match_index] <- 'C_DD'
          } else if (df$Value[first_left_match_index] == 'Left_WP') {
            df$activate[first_left_match_index] <- 'C_WP'
          } else if (df$Value[first_left_match_index] == 'Left_TO') {
            df$activate[first_left_match_index] <- 'C_TO'
          }
        }
        
        # Update the last L index
        last_L_index <- i
      }
      
      # Case when 'R' is in 'activate' column and it's not NA
      if (!is.na(df$activate[i]) && df$activate[i] == 'R') {
        # Find the first 'Right', 'Right_DD', or 'Right_WP' in 'Value' before the current 'R',
        # but after the last 'L' or 'R'
        if (!is.na(last_L_index) || !is.na(last_R_index)) {
          start_index <- max(last_L_index, last_R_index, na.rm = TRUE) + 1
          first_right_match_index <- which(df$Value[start_index:(i-1)] %in% c('Right', 'Right_DD', 'Right_WP', 'Right_TO'))[1] + start_index - 1
        } else {
          # If no last L or R, just find the first matching (Right, Right_DD, Right_WP) before that 'R'
          first_right_match_index <- which(df$Value[1:(i-1)] %in% c('Right', 'Right_DD', 'Right_WP', 'Right_TO'))[1]
        }
        
        if (!is.na(first_right_match_index)) {
          # Mark them with 'C', 'C_DD', or 'C_WP'
          if (df$Value[first_right_match_index] == 'Right') {
            df$activate[first_right_match_index] <- 'C'
          } else if (df$Value[first_right_match_index] == 'Right_DD') {
            df$activate[first_right_match_index] <- 'C_DD'
          } else if (df$Value[first_right_match_index] == 'Right_WP') {
            df$activate[first_right_match_index] <- 'C_WP'
          } else if (df$Value[first_right_match_index] == 'Right_TO') {
            df$activate[first_right_match_index] <- 'C_TO'
          }
        }
        
        # Update the last R index
        last_R_index <- i
      }
    }
    df
    
    
    
    #between C and L: #if Left: crep (correct repeat)
    #if Left_WP: crep_WP (correct repeat with pellet) 
    #if Left_DD: crep_DD (correct repeat during dispense).
    #if Right: irep (incorrect repeat)
    #if Right_WP: irep_WP (incorrect with pellet) 
    #if Right_DD: irep_DD (incorrect during dispense).
    #if Right_TO: irep_TO (incorrect in Timeout)
    
    #between C and R: #if Right: crep (correct repeat)
    #if Right_WP: crep_WP (correct repeat with pellet) 
    #if Right_DD: crep_DD (correct repeat during dispense).
    #if Left: irep (incorrect repeat)
    #if Left_WP: irep_WP (incorrect with pellet) 
    #if Left_DD: irep_DD (incorrect during dispense).
    #if Left_TO: irep_TO (incorrect in Timeout)
    
    for (i in 1:nrow(df)){
      k=1
      if ((df$activate[i]=='C'|df$activate[i]=='C_DD'|df$activate[i]=='C_WP'|df$activate[i]=='C_TO')&&!is.na(df$activate[i])){
        while (is.na(df$activate[i+k])){
          k=k+1
        }
      }
      if (df$activate[i+k]=='L'&&k>1){
        for (l in 1:(k-1)){
          if (df$Value[i+l]=='Left'){
            df$activate[i+l]='crep'
          }else if (df$Value[i+l]=='Left_WP'){
            df$activate[i+l]='crep_WP'
          }else if (df$Value[i+l]=='Left_DD'){
            df$activate[i+l]='crep_DD' 
          }else if (df$Value[i+l]=='Left_TO'){
            df$activate[i+l]='crep_TO' 
          }else if (df$Value[i+l]=='Right'){
            df$activate[i+l]='irep' 
          }else if (df$Value[i+l]=='Right_DD'){
            df$activate[i+l]='irep_DD' 
          }else if (df$Value[i+l]=='Right_WP'){
            df$activate[i+l]='irep_WP' 
          }else if (df$Value[i+l]=='Right_TO'){
            df$activate[i+l]='irep_TO' 
          }
        }
      }
      if (df$activate[i+k]=='R'&&k>1){
        for (l in 1:(k-1)){
          if (df$Value[i+l]=='Right'){
            df$activate[i+l]='crep'
          }else if (df$Value[i+l]=='Right_WP'){
            df$activate[i+l]='crep_WP'
          }else if (df$Value[i+l]=='Right_DD'){
            df$activate[i+l]='crep_DD' 
          }else if (df$Value[i+l]=='Right_TO'){
            df$activate[i+l]='crep_TO' 
          }else if (df$Value[i+l]=='Left'){
            df$activate[i+l]='irep' 
          }else if (df$Value[i+l]=='Left_DD'){
            df$activate[i+l]='irep_DD' 
          }else if (df$Value[i+l]=='Left_WP'){
            df$activate[i+l]='irep_WP' 
          }else if (df$Value[i+l]=='Left_TO'){
            df$activate[i+l]='irep_TO' 
          }
        }
      }
    }
    
    
    last_pellet=tail(which(df$Value == "Pellet"), 1)
    
    if (last_pellet<nrow(df)){
      if (num_blocks %% 2 == 0){
        k=1
        while (df$Value[last_pellet+k]!='Left' && df$Value[last_pellet+k]!='Left_WP' && df$Value[last_pellet+k]!='Left_DD' && df$Value[last_pellet+k]!='Left_TO' && last_pellet+k<nrow(df)){
          k=k+1
        }
        if (df$Value[last_pellet+k]=='Left'){
          df$activate[last_pellet+k]='C'
        }else if (df$Value[last_pellet+k]=='Left_WP'){
          df$activate[last_pellet+k]='C_WP'
        } else if (df$Value[last_pellet+k]=='Left_DD'){
          df$activate[last_pellet+k]='C_DD'
        } else if (df$Value[last_pellet+k]=='Left_TO'){
          df$activate[last_pellet+k]='C_TO'
        }
        if (nrow(df)-last_pellet-k>0){
          for (i in 1:(nrow(df)-last_pellet-k))
            if (df$Value[last_pellet+k+i]=='Left'){
              df$activate[last_pellet+k+i]='crep'
            }else if (df$Value[last_pellet+k+i]=='Left_WP'){
              df$activate[last_pellet+k+i]='crep_WP'
            }else if (df$Value[last_pellet+k+i]=='Left_DD'){
              df$activate[last_pellet+k+i]='crep_DD' 
            }else if (df$Value[last_pellet+k+i]=='Left_TO'){
              df$activate[last_pellet+k+i]='crep_TO' 
            }else if (df$Value[last_pellet+k+i]=='Right'){
              df$activate[last_pellet+k+i]='irep' 
            }else if (df$Value[last_pellet+k+i]=='Right_DD'){
              df$activate[last_pellet+k+i]='irep_DD' 
            }else if (df$Value[last_pellet+k+i]=='Right_WP'){
              df$activate[last_pellet+k+i]='irep_WP' 
            }else if (df$Value[last_pellet+k+i]=='Right_TO'){
              df$activate[last_pellet+k+i]='irep_TO' 
            }
        }
        
      }else{
        k=1
        while (df$Value[last_pellet+k]!='Right'&& df$Value[last_pellet+k]!='Right_WP' && df$Value[last_pellet+k]!='Right_DD' && df$Value[last_pellet+k]!='Right_TO' && last_pellet+k<nrow(df)){
          k=k+1
        }
        if (df$Value[last_pellet+k]=='Right'){
          df$activate[last_pellet+k]='C'
        }else if (df$Value[last_pellet+k]=='Right_WP'){
          df$activate[last_pellet+k]='C_WP'
        } else if (df$Value[last_pellet+k]=='Right_DD'){
          df$activate[last_pellet+k]='C_DD'
        } else if (df$Value[last_pellet+k]=='Right_TO'){
          df$activate[last_pellet+k]='C_TO'
        }
        
        if (nrow(df)-last_pellet-k>0){
          for (i in 1:(nrow(df)-last_pellet-k))
            if (df$Value[last_pellet+k+i]=='Left'){
              df$activate[last_pellet+k+i]='irep'
            }else if (df$Value[last_pellet+k+i]=='Left_WP'){
              df$activate[last_pellet+k+i]='irep_WP'
            }else if (df$Value[last_pellet+k+i]=='Left_DD'){
              df$activate[last_pellet+k+i]='irep_DD' 
            }else if (df$Value[last_pellet+k+i]=='Left_TO'){
              df$activate[last_pellet+k+i]='irep_TO' 
            }else if (df$Value[last_pellet+k+i]=='Right'){
              df$activate[last_pellet+k+i]='crep' 
            }else if (df$Value[last_pellet+k+i]=='Right_DD'){
              df$activate[last_pellet+k+i]='crep_DD' 
            }else if (df$Value[last_pellet+k+i]=='Right_WP'){
              df$activate[last_pellet+k+i]='crep_WP' 
            }else if (df$Value[last_pellet+k+i]=='Right_TO'){
              df$activate[last_pellet+k+i]='crep_TO' 
            }
        } 
      }
    }
    
    df$activate[is.na(df$activate)] <- "I"
    
    fullraw=df[,1:3]
    
    
    ###### ADD real time (ms)
    fullraw$Time<-my_data[,2]/1000
    
  }
  
  
  plot_df <- freq_df %>%
    left_join(fullraw %>% dplyr::select(TimePoint, activate),
              by = c("EventIndex" = "TimePoint"))
  
  
  
  
  # make color variable based on activate
  #plot_df$ColorGroup <- ifelse(plot_df$activate %in% c("C", "C_WP"), "C", "Other")
  
  plot_df$ColorGroup <- ifelse(plot_df$activate == "C", "C", "Other")
  mean_IF_poke_c=mean(plot_df$InstantFrequency[plot_df$ColorGroup=="C"])
  mean_IF_poke_other=mean(plot_df$InstantFrequency[plot_df$ColorGroup=="Other"])
  other_counts=sum(I(plot_df$ColorGroup=="Other"))
  
  
  
  
  
  # --- End of your single-mouse code ---
  
  # Save results
  results <- rbind(results, data.frame(
    Paradigm = paradigm_num,
    ParadigmName = paradigm_name,
    Mouse = mouse_num,
    Poke_IF_Avg = IF_poke_average,
    Pellet_IF_Avg = IF_pellet_average,
    Mean_IF_Poke_C = mean_IF_poke_c,
    Mean_IF_Poke_Other = mean_IF_poke_other,
    other_counts=other_counts,
    poke_counts=poke_counts,
    pellet_counts=pellet_counts,
    stringsAsFactors = FALSE
  ))
}

# Optional: sort by paradigm and mouse
results <- results %>% arrange(Paradigm, Mouse)

# View results
print(results)






###################################################################################
##### ensure correct ordering (keeps your real names)
results$ParadigmName <- factor(results$ParadigmName,
                               levels = unique(results$ParadigmName))

##### storage
plot_list <- list()
ttest_results <- list()

##### use REAL labels consistently
paradigms <- unique(results$ParadigmName)

##### ---- loop over paradigms ----
for (p in paradigms) {
  
  df <- results %>% filter(ParadigmName == p)
  
  # paired t-test
  tt <- t.test(df$Mean_IF_Poke_C,
               df$Mean_IF_Poke_Other,
               paired = TRUE)
  
  # store p-value using REAL name
  ttest_results[[p]] <- list(
    statistic = as.numeric(tt$statistic),
    p_value   = tt$p.value,
    cohens_d  = as.numeric(mean(df$Mean_IF_Poke_C - df$Mean_IF_Poke_Other) / sd(df$Mean_IF_Poke_C - df$Mean_IF_Poke_Other))
  )
  
  # reshape for plotting
  df_long <- df %>%
    select(Mouse, Mean_IF_Poke_C, Mean_IF_Poke_Other) %>%
    pivot_longer(cols = c(Mean_IF_Poke_C, Mean_IF_Poke_Other),
                 names_to = "Type",
                 values_to = "InstantFreq")
  
  df_long$Type <- ifelse(df_long$Type == "Mean_IF_Poke_C",
                         "Correct Pokes",
                         "Other Pokes")
  
  jit <- position_jitter(width = 0.1)
  
  # plot
  p_plot <- ggplot(df_long, aes(x = Type, y = InstantFreq, fill = Type)) +
    geom_boxplot(width = 0.6, alpha = 0.5) +
    geom_line(aes(group = Mouse),
              color = "gray",
              alpha = 0.7,
              position = jit) +
    geom_jitter(size = 2, position = jit) +
    scale_fill_manual(values = c("Correct Pokes" = "blue",
                                 "Other Pokes" = "red")) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      axis.text.x  = element_text(size = 15),
      axis.text.y  = element_text(size = 15),
      plot.title   = element_text(size = 15),
      legend.position = "none",
      axis.line = element_line(color = "black", linewidth = 0.8)
    ) +
    labs(title = paste0("Paradigm: ", p),
         y = "Instantaneous Poking Frequency",
         x = "")
  
  plot_list[[p]] <- p_plot
}

##### ---- BH correction ----
pvals <- sapply(ttest_results, function(x) x$p_value)

pvals_adj <- p.adjust(pvals, method = "BH")

names(pvals_adj) <- names(ttest_results)




##### ---- add BH annotations ----
for (name in names(plot_list)) {
  
  df_plot <- results %>% filter(ParadigmName == name)
  
  y_max <- max(df_plot$Mean_IF_Poke_C,
               df_plot$Mean_IF_Poke_Other,
               na.rm = TRUE)
  
  plot_list[[name]] <- plot_list[[name]] +
    annotate("text",
             x = 1.5,
             y = y_max * 1.1,
             label = paste0("Adjusted p = ", signif(pvals_adj[name], 3)),
             size = 4)
}

##### ---- view one plot ----
plot_list[[as.character(paradigms[1])]]

##### ---- all plots ----
grid.arrange(grobs = plot_list, ncol = 2)

##### ---- outputs ----
results
ttest_results
pvals_adj









results$other_counts

# Convert data to long format for ggplot
df_long1 <- results %>%
  pivot_longer(cols = c(Poke_IF_Avg, Pellet_IF_Avg),
               names_to = "Measure",
               values_to = "Value")

# Make Measure a factor with readable names
df_long1$Measure <- factor(df_long1$Measure, levels = c("Poke_IF_Avg", "Pellet_IF_Avg"),
                           labels = c("Poke IF Avg", "Pellet IF Avg"))

# Set ParadigmName order
df_long1$ParadigmName <- factor(df_long1$ParadigmName, levels = c("FR", "2x2", "5x5", "RPR"))

# Plot
ggplot(df_long1, aes(x = ParadigmName, y = Value, fill = Measure)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.6, outlier.shape = NA) +
  geom_jitter(aes(color = Measure), 
              position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8),
              size = 2, alpha = 0.8) +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e"), labels = c("Poke", "Pellet")) +
  scale_color_manual(values = c("#1f77b4", "#ff7f0e"), labels = c("Poke", "Pellet")) +
  labs(x = "Paradigm", y = "Average Instantaneous Frequency", fill = "", color = "") +
  theme_classic() +
  theme(legend.position = "top")





# Add the scaled other_counts as a new column
results <- results %>%
  mutate(Other_Counts_Scaled = other_counts / 6000)

# Convert to long format including the third measure
df_long1 <- results %>%
  pivot_longer(cols = c(poke_counts, other_counts, pellet_counts),
               names_to = "Measure",
               values_to = "Value")

# Make Measure a factor with readable labels and correct order
df_long1$Measure <- factor(df_long1$Measure, 
                           levels = c("poke_counts", "other_counts", "pellet_counts"),
                           labels = c("Poke Counts", "Other Poke Counts", "Pellet Counts"))

# Set Paradigm order
df_long1$ParadigmName <- factor(df_long1$ParadigmName, levels = c("FR", "2x2", "5x5", "RPR"))

# Plot
ggplot(df_long1, aes(x = ParadigmName, y = Value, fill = Measure)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.6, outlier.shape = NA) +
  geom_jitter(aes(color = Measure), 
              position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8),
              size = 2, alpha = 0.8) +
  scale_fill_manual(values = c("#1f77b4", "red", "green"), labels = c("Poke Counts", "Other Poke Counts", "Pellet Counts")) +  # blue, green, orange
  scale_color_manual(values = c("#1f77b4", "red", "green"), labels = c("Poke Counts", "Other Poke Counts", "Pellet Counts")) +
  labs(x = "Paradigm", y = "Counts", fill = "Measure", color = "Measure") +
  theme_classic() +
  theme(legend.position = "top")


# Compute mean and SD per Paradigm × Measure
summary_stats <- df_long1 %>%
  group_by(ParadigmName, Measure) %>%
  summarise(
    mean_value = mean(Value, na.rm = TRUE),
    sd_value   = sd(Value, na.rm = TRUE),
    max_value = max(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    label = paste0(round(mean_value, 3), " ± ", round(sd_value, 3))
  )

# Plot with annotations
ggplot(df_long1, aes(x = ParadigmName, y = Value, fill = Measure)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.6, outlier.shape = NA) +
  geom_jitter(aes(color = Measure), 
              position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8),
              size = 2, alpha = 0.8) +
  # Add mean ± SD text above each box
  geom_text(
    data = summary_stats,
    aes(x = ParadigmName, y = max_value + 50 ,  # adjust vertical position
        label = label, group = Measure),
    position = position_dodge(width = 0.8),
    size = 3
  ) +
  scale_fill_manual(values = c("#1f77b4", "red", "green")) +
  scale_color_manual(values = c("#1f77b4", "red", "green")) +
  labs(x = "Paradigm", y = "Counts",
       fill = "Measure", color = "Measure") +
  theme_classic() +
  theme(legend.position = "top")



###GLMM
model_nb <- glmmTMB(
  poke_counts ~ ParadigmName ,
  family = nbinom2,   # most common choice
  data = results
)

check_overdispersion(model_nb)

summary(model_nb)

model_nb2 <- glmmTMB(
  other_counts ~ ParadigmName,
  family = nbinom2,   # most common choice
  data = results
)

check_overdispersion(model_nb2)

summary(model_nb2)



model_nb3 <- glmmTMB(
  pellet_counts ~ ParadigmName,
  family = nbinom2,   # most common choice
  data = results
)

check_overdispersion(model_nb3)

summary(model_nb3)