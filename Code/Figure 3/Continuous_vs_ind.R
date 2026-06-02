library(ggplot2)
library(dplyr)
library(zoo)
library(lubridate)
library(ggpubr)
library(tidyr)
library(lme4)
library(performance)
#install.packages("reformulas")
#install.packages("glmmTMB")
library(glmmTMB)
# Folder containing all CSV files
setwd("C:/Users/sunlab/OneDrive - University of Wyoming/Reversal paper/Reversal paper/all pictures")
data_folder <- "C:/Users/sunlab/OneDrive - University of Wyoming/Reversal paper/Reversal paper/Data/Reversal_normal_disease_alldata"

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
results_con<-results


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
results_ind<-results



results_con$Mode <- "continuous"
results_ind$Mode  <- "independent"
results_all <- rbind(results_con, results_ind)
results_all


# Make sure Paradigm is a factor
results_all <- results_all %>%
  mutate(
    Paradigm = factor(Paradigm, levels = 1:4, labels = c("FR", "2x2", "5x5", "RPR"))
  )
results_all$Mode <- factor(results_all$Mode, levels = c("independent", "continuous"))

# Function to make the plot for any column
plot_counts <- function(data, count_col, y_label) {
  ggplot(data, aes(x = Paradigm, y = .data[[count_col]], fill = Mode)) +
    geom_boxplot(position = position_dodge(width = 0.8), width = 0.6, alpha = 0.5, outlier.shape = NA) +
    geom_jitter(color = "black", 
                position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8),
                size = 2) +
    labs(y = y_label, x = "Paradigm") +
    theme_minimal(base_size = 14)+
    theme(
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      axis.text.y = element_text(size = 15),
      axis.text.x  = element_text(size = 15),
      plot.title   = element_text(size = 15),   # picture title
      legend.title = element_text(size = 15),   # legend title
      legend.text  = element_text(size = 15),    # legend labels
      axis.line = element_line(color = "black", linewidth = 0.8),
      panel.grid = element_blank()
    )
  
}





plot_counts <- function(data, count_col, y_label) {
  
  # Prepare comparisons per paradigm
  comparisons <- lapply(unique(data$Paradigm), function(p) {
    subset_data <- data[data$Paradigm == p, ]
    list(c("Mode", "Mode"))  # placeholder; ggpubr handles grouping internally
  })
  
  # Basic plot
  p <- ggplot(data, aes(x = Paradigm, y = .data[[count_col]], fill = Mode)) +
    geom_boxplot(position = position_dodge(width = 0.8), width = 0.6, alpha = 0.5, outlier.shape = NA) +
    geom_jitter(color = "black", 
                position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8),
                size = 2) +
    scale_fill_manual(values = c(
      continuous = "red",
      independent = "blue"))+
    labs(y = y_label, x = "Paradigm") +
    theme_minimal(base_size = 14) +
    theme(
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      axis.text.y = element_text(size = 15),
      axis.text.x = element_text(size = 15),
      plot.title = element_text(size = 15),
      legend.title = element_text(size = 15),
      legend.text = element_text(size = 15),
      axis.line = element_line(color = "black", linewidth = 0.8),
      panel.grid = element_blank()
    ) +
    # Add p-values automatically
    stat_compare_means(
      aes(group = Mode),
      method = "wilcox.test",      # Mann–Whitney U test
      label = "p.format",           # show formatted p-values
      label.y = max(data[[count_col]]) * 1.05  # slightly above the top
    )
  
  return(p)
}


# Create the three plots
poke_plot <- plot_counts(results_all, "poke_counts", "Poke Counts")
pellet_plot <- plot_counts(results_all, "pellet_counts", "Pellet Counts")
other_plot <- plot_counts(results_all, "other_counts", "Other Counts")

# Show plots
poke_plot
pellet_plot
other_plot


#BH 
## exact U-test
manual_pvals <- results_all %>%
  group_by(Paradigm) %>%
  summarise(
    p_poke = wilcox.test(
      poke_counts ~ Mode,
      exact = TRUE   # IMPORTANT: matches ggpubr typical fallback behavior
    )$p.value,
    
    p_pellet = wilcox.test(
      pellet_counts ~ Mode,
      exact = TRUE
    )$p.value,
    
    p_other = wilcox.test(
      other_counts ~ Mode,
      exact = TRUE
    )$p.value
  )

manual_pvals



p_raw <- c(
  # p_poke
  0.438, 0.0372, 0.0195, 0.310,
  
  # p_pellet
  0.699, 0.768, 0.593, 0.759,
  
  # p_other
  1.000, 0.0400, 0.0112, 0.310
)

p_adj <- p.adjust(p_raw, method = "BH")
p_adj





# 1. compute ALL raw p-values (no BH yet)
# ----------------------------
get_p_table <- function(data, count_col, measure_name) {
  
  data %>%
    group_by(Paradigm) %>%
    summarise(
      p = wilcox.test(
        .data[[count_col]] ~ Mode,
        exact = TRUE
      )$p.value,
      .groups = "drop"
    ) %>%
    mutate(measure = measure_name)
}

# ----------------------------
# 2. collect ALL tests and apply ONE BH (n = 12)
# ----------------------------
p_all <- bind_rows(
  get_p_table(results_all, "poke_counts",   "poke"),
  get_p_table(results_all, "pellet_counts", "pellet"),
  get_p_table(results_all, "other_counts",  "other")
) %>%
  arrange(p) %>%   # optional but safer
  mutate(p_adj = p.adjust(p, method = "BH"))

# ----------------------------
# 3. split back for plotting
# ----------------------------
p_poke   <- p_all %>% filter(measure == "poke") %>%
  mutate(y = max(results_all$poke_counts, na.rm = TRUE) * 1.05)

p_pellet <- p_all %>% filter(measure == "pellet") %>%
  mutate(y = max(results_all$pellet_counts, na.rm = TRUE) * 1.05)

p_other  <- p_all %>% filter(measure == "other") %>%
  mutate(y = max(results_all$other_counts, na.rm = TRUE) * 1.05)

# ----------------------------
# 4. plotting function
# ----------------------------
plot_counts <- function(data, count_col, y_label, p_table) {
  
  ggplot(data, aes(x = Paradigm, y = .data[[count_col]], fill = Mode)) +
    
    geom_boxplot(position = position_dodge(width = 0.8),
                 width = 0.6, alpha = 0.5, outlier.shape = NA) +
    
    geom_jitter(color = "black",
                position = position_jitterdodge(jitter.width = 0.15,
                                                dodge.width = 0.8),
                size = 2) +
    
    scale_fill_manual(values = c(
      continuous = "red",
      independent = "blue"
    )) +
    
    labs(y = y_label, x = "Paradigm") +
    
    theme_minimal(base_size = 14) +
    theme(
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      axis.text.y = element_text(size = 15),
      axis.text.x = element_text(size = 15),
      legend.title = element_text(size = 15),
      legend.text = element_text(size = 15),
      axis.line = element_line(color = "black", linewidth = 0.8),
      panel.grid = element_blank()
    ) +
    
    geom_text(
      data = p_table,
      aes(x = Paradigm, y = y,
          label = paste0("Adjusted p = ", signif(p_adj, 3))),
      inherit.aes = FALSE,
      size = 4
    )
}

# ----------------------------
# 5. plots
# ----------------------------
poke_plot   <- plot_counts(results_all, "poke_counts",   "Poke Counts",   p_poke)
pellet_plot <- plot_counts(results_all, "pellet_counts", "Pellet Counts", p_pellet)
other_plot  <- plot_counts(results_all, "other_counts",  "Other Counts",  p_other)

# ----------------------------
# 6. display
# ----------------------------
poke_plot
pellet_plot
other_plot
