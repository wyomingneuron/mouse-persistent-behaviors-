library(ggplot2)
library(dplyr)
library(flexmix)
library(tidyr)
library(fmsb)
library(stringr)
#install.packages("reshape2")
library(reshape2)

# Folder containing your CSV files
input_dir <- "C:/Users/sunlab/OneDrive - University of Wyoming/Reversal paper/Reversal paper/Data/Reversal data final"

setwd("C:/Users/sunlab/OneDrive - University of Wyoming/Reversal paper/Reversal paper/all pictures")
# List all CSV files in the folder
files <- list.files(input_dir, pattern = "\\.CSV$", full.names = TRUE)




file_info <- data.frame(
  file = files,
  # Extract group number: the FIRST number before underscore
  group = as.integer(str_extract(basename(files), "^[0-9]+")),
  
  # Extract mouse number: the LAST number before ".CSV"
  mouse = as.integer(str_extract(basename(files), "[0-9]+(?=\\.CSV$)")),
  
  stringsAsFactors = FALSE
)

# Sort by group then mouse
file_info <- file_info[order(file_info$group, file_info$mouse), ]

file_info



# Step 2: Create output df
# -----------------------------------------
df_PERCS <- data.frame(
  group = file_info$group,
  mouse = paste0("M", file_info$mouse),
  P = NA, E = NA, R = NA, C = NA, S = NA, Pellet = NA, Poke=NA
)






# -----------------------------------------
# Step 3: Loop through files and compute metrics
# -----------------------------------------
for(a in 1:nrow(file_info)){
  
  raw <- read.csv(file_info$file[a])
  
  
  # --- Step 1: Read CSV
  my_data <- raw
  
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
  
  dfp<-df
  
  
  ###### ADD real time (s)
  #df$TimePoint<-my_data[,1]
  #df2<-df[df$Value!="Pellet",]
  #time_durations <- mdy_hms(df2$TimePoint)
  #time_diff_seconds <- as.numeric(diff(time_durations), units = "secs")
  
  ###### ADD real time (ms)
  df$TimePoint<-my_data[,2]
  df2<-df[df$Value!="Pellet",]
  time_diff_seconds <- diff(df2$TimePoint) / 1000
  
  
  
  ###### plot time interval
  # Get original row indices of filtered events (to keep event numbering consistent)
  filtered_rows <- which(df$Value != "Pellet")
  
  
  ###### ADD real time (ms) for pellet only
  
  df3<-df[df$Value=="Pellet",]
  time_diff_seconds_p <- diff(df3$TimePoint) / 1000
  filtered_rows_p <- which(df$Value == "Pellet")
  
  
  
  
  
  # Create data frame for plotting: event index corresponds to the second event in each pair onward
  freq_df <- data.frame(
    EventIndex = filtered_rows[2:length(filtered_rows)],
    Timeinterval = time_diff_seconds
  )
  
  
  # Create data frame for plotting: event index corresponds to the second event in each pair onward
  freq_dfp <- data.frame(
    EventIndex = filtered_rows_p[2:length(filtered_rows_p)],
    Timeinterval = time_diff_seconds_p
  )
  
  
  
  
  
  
  
  
  
  # Calculate frequency (Hz = events per second)
  instant_freq <- 1 / time_diff_seconds
  
  
  # Create data frame for plotting: event index corresponds to the second event in each pair onward
  freq_df <- data.frame(
    EventIndex = filtered_rows[2:length(filtered_rows)],
    InstantFrequency = instant_freq
  )
  
  
  
  # Calculate frequency (Hz = events per second)
  instant_freq_p <- 1 / time_diff_seconds_p
  
  
  # Create data frame for plotting: event index corresponds to the second event in each pair onward
  freq_dfp <- data.frame(
    EventIndex = filtered_rows_p[2:length(filtered_rows_p)],
    InstantFrequency = instant_freq_p
  )
  
  
  
  
  
  if(a<=9){
    
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
    
    fullraw=df
    
    
    ###### ADD real time (ms)
    fullraw$Time<-my_data[,2]/1000
    fullraw$TimePoint<-1:length(fullraw$TimePoint)
    
    # --- Step 1: 简化状态 ---
    fullraw2 <- fullraw %>%
      mutate(State = case_when(
        grepl("^Left", Value)  ~ "Left",
        grepl("^Right", Value) ~ "Right",
        Value == "Pellet"      ~ "Pellet"
      )) %>%
      filter(!is.na(State))
    
    df_PERCS$Pellet[a]<-sum(I(fullraw2$State=="Pellet"))
    df_PERCS$Poke[a]<-sum(I(fullraw2$State!="Pellet"))
    P_val <- 0      #
    E_val <- 0      # 
    R_val <- 0      #  
    C_val <- 0      # 
  
    
    ### Calculate S
  
    
    
    # --- Step 2: 构建 transition count matrix ---
    states <- c("Left", "Right", "Pellet")
    T_count <- matrix(0, nrow=3, ncol=3, dimnames=list(states, states))
    
    for(i in 1:(nrow(fullraw2)-1)){
      from <- fullraw2$State[i]
      to   <- fullraw2$State[i+1]
      T_count[from, to] <- T_count[from, to] + 1
    }
    
    
    # --- Step 3: 转换为 transition probability matrix ---
    T_mat <- T_count / rowSums(T_count)
    T_mat[is.na(T_mat)] <- 0   # 避免某一行没有transition
    
    
    # --- Step 4: 计算 p(i)（起点概率）---
    start_states <- fullraw2$State[-nrow(fullraw2)]
    p_i <- table(start_states) / length(start_states)
    
    # 保证顺序一致
    p_i <- p_i[states]
    p_i[is.na(p_i)] <- 0
    
    
    # --- Step 5: 计算每一行的 entropy ---
    row_entropy <- apply(T_mat, 1, function(row){
      row <- row[row > 0]
      if(length(row) == 0) return(0)
      -sum(row * log2(row))
    })
    
    
    # --- Step 6: 计算总 entropy（Markov entropy rate）---
    H <- sum(p_i * row_entropy)
    
    
    # --- Step 7: 标准化得到 S ---
    N <- 3
    S_val <- 1 - H / log2(N)
    
    
    
    
    df_PERCS$P[a] <- P_val
    df_PERCS$E[a] <- E_val
    df_PERCS$R[a] <- R_val
    df_PERCS$C[a] <- C_val
    df_PERCS$S[a] <- S_val
    
    
    
    
  } else if (a<=19) {
    nXn=2
    
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
    
    
    
    
    fullraw=df
    
    
    ###### ADD real time (ms)
    fullraw$Time<-my_data[,2]/1000
    fullraw$TimePoint<-1:length(fullraw$TimePoint)
    
    plot_df <- freq_df %>%
      left_join(fullraw %>% dplyr::select(TimePoint, activate, Time),
                by = c("EventIndex" = "TimePoint"))
    
    # make color variable based on activate
    #plot_df$ColorGroup <- ifelse(plot_df$activate %in% c("C", "C_WP"), "C", "Other")
    
    plot_df$ColorGroup <- ifelse(plot_df$activate == "C", "C", "Other")
    
    
    
    
    
    # Prepare data: remove NA's and create data frame
    freq_clean <- na.omit(freq_df$InstantFrequency)
    df <- data.frame(y = freq_clean)
    
    # Fit 2-component normal mixture model
    set.seed(123)  # For reproducibility
    model <- flexmix(y ~ 1, data = df, k = 2, model = FLXMRglm(family = "gaussian"))
    
    
    
    # Assign clusters to each data point
    df$cluster <- clusters(model)
    print(table(df$cluster))
    
    
    
    ###### find threshod 
    # Extract cluster means (component means)
    params <- parameters(model)  # matrix with means and maybe other params
    
    # For FLXMRglm with gaussian, params is matrix with means in first row:
    comp_means <- params[1, ]
    
    # Identify which cluster is lower and which is higher
    lower_comp <- which.min(comp_means)
    higher_comp <- which.max(comp_means)
    
    # Get max y of lower cluster and min y of higher cluster
    max_lower <- max(df$y[df$cluster == lower_comp])
    min_higher <- min(df$y[df$cluster == higher_comp])
    
    # Calculate threshold as midpoint between max_lower and min_higher
    threshold <- (max_lower + min_higher) / 2
    
    threshold
    
    
    lower=mean(df$y[df$cluster == lower_comp]) 
    
    
    ###################compare with average
    mean(freq_df$InstantFrequency)
    
    
    clustered_freq=df
    clustered_freq$cluster[clustered_freq$cluster == lower_comp]="L"
    clustered_freq$cluster[clustered_freq$cluster == higher_comp]="H"
    
    clustered_freq
    
    
    
    
    #Find persistence periods
    
    
    
    # Set minimum band length (in number of rows/events)
    min_band_length <- 4
    
    # Initialize state
    in_persistence <- FALSE
    start_row <- NA
    persistence_ranges <- data.frame(start = numeric(), end = numeric())
    
    for (i in seq_len(nrow(freq_df))) {
      freq <- freq_df$InstantFrequency[i]
      
      if (!in_persistence && freq > threshold) {
        in_persistence <- TRUE
        start_row <- i
      } else if (in_persistence && freq < lower) {
        end_row <- i - 1
        if (!is.na(start_row) && (end_row - start_row + 1) >= min_band_length) {
          # Convert row index to EventIndex for plotting
          start_event <- freq_df$EventIndex[start_row]
          end_event <- freq_df$EventIndex[end_row]
          persistence_ranges <- rbind(persistence_ranges, data.frame(start = start_event, end = end_event))
          cat("Band added from row", start_row, "to", end_row, "\n")
        } else {
          cat("Band ignored from row", start_row, "to", end_row, "\n")
        }
        in_persistence <- FALSE
        start_row <- NA
      }
    }
    
    # Final open band
    if (in_persistence && !is.na(start_row)) {
      end_row <- nrow(freq_df)
      if ((end_row - start_row + 1) >= min_band_length) {
        start_event <- freq_df$EventIndex[start_row]
        end_event <- freq_df$EventIndex[end_row]
        persistence_ranges <- rbind(persistence_ranges, data.frame(start = start_event, end = end_event))
        cat("Final band added from row", start_row, "to", end_row, "\n")
      } else {
        cat("Final band ignored from row", start_row, "to", end_row, "\n")
      }
    }
    
    # Double-check output
    print("Final persistence ranges:")
    print(persistence_ranges)
    
    
    # --- Step 1: 简化状态 ---
    fullraw2 <- fullraw %>%
      mutate(State = case_when(
        grepl("^Left", Value)  ~ "Left",
        grepl("^Right", Value) ~ "Right",
        Value == "Pellet"      ~ "Pellet"
      )) %>%
      filter(!is.na(State))
    
    df_PERCS$Pellet[a]<-sum(I(fullraw2$State=="Pellet"))
    df_PERCS$Poke[a]<-sum(I(fullraw2$State!="Pellet"))
    
  
    
    
   
    
    
    
    
    ### Calculate R
    
    # --- Step 1: 定义时间范围（用 0 作为起点） ---
    t_min <- 0
    t_max <- max(fullraw$Time, na.rm = TRUE)
    t_mid <- t_max / 2
    
    
    # --- Step 2: 把 start / end 映射到真实时间 ---
    persistence_ranges$start_time <- fullraw$Time[
      match(persistence_ranges$start, fullraw$TimePoint)
    ]
    
    persistence_ranges$end_time <- fullraw$Time[
      match(persistence_ranges$end, fullraw$TimePoint)
    ]
    
    
    # --- Step 3: 按 start_time 划分前后半 ---
    # （跨中点的 bout 自动归到 first，因为看 start）
    persistence_ranges$half <- ifelse(
      persistence_ranges$start_time <= t_mid,
      "first",
      "second"
    )
    
    
    res <- persistence_ranges %>%
      mutate(length = end_time - start_time) %>%
      group_by(half) %>%
      summarise(total_length = sum(length)) %>%
      tidyr::pivot_wider(names_from = half, values_from = total_length) %>%
      mutate(ratio = second / first)
    
    R_val <- res$ratio
    
    
    ### Calculate E
    # 所有 bout 的起点
    starts<-persistence_ranges$start_time
    ends<-persistence_ranges$end_time
    
    # 对每个 Time，找到它落在哪个 start 区间位置
    idx <- findInterval(fullraw2$Time, starts)
    
    # idx = 0 说明在所有区间之前 → 肯定不在 bout 里
    # 否则检查是否 <= 对应的 end_time
    in_bout <- idx > 0 & fullraw2$Time <= ends[pmax(idx, 1)]
    
    # 再筛 Left / Right
    keep <- in_bout & fullraw2$activate %in% c("C")
    keep_P<-in_bout & fullraw2$State %in% c("Left","Right")
    
    # 最终结果
    result <- fullraw2[keep, ]
    result_P<-fullraw2[keep_P, ]
    # 总数
    nrow(result)
    
    E_val=nrow(result)/sum(I(fullraw2$activate!="P"))
    
    
    ### calculate P
    P_val <-nrow(result_P)  
    
    
    
    ### Calculate C  
    # --- Step 1: 计算 bout lengths ---
    bout_lengths <- persistence_ranges$end_time - persistence_ranges$start_time
    
    # --- Step 2: 计算 non-bout lengths ---
    # 非 bout 时间 = 上一个 bout 结束到下一个 bout 开始
    # 如果只有一个 bout，non-bout length = NA
    if(nrow(persistence_ranges) > 1){
      non_bout_lengths <- persistence_ranges$start_time[-1] - persistence_ranges$end_time[-nrow(persistence_ranges)]
    } else {
      non_bout_lengths <- NA
    }
    
    # --- Step 3: 计算 CV ---
    # CV = 标准差 / 平均值
    cv_bout <- sd(bout_lengths, na.rm = TRUE) / mean(bout_lengths, na.rm = TRUE)
    cv_non_bout <- sd(non_bout_lengths, na.rm = TRUE) / mean(non_bout_lengths, na.rm = TRUE)
    
    # --- Step 4: 计算新指标 ---
    C_val <- 1 / (cv_bout + cv_non_bout)
    
    
    
    ### Calculate S
    
    
    
    # --- Step 2: 构建 transition count matrix ---
    states <- c("Left", "Right", "Pellet")
    T_count <- matrix(0, nrow=3, ncol=3, dimnames=list(states, states))
    
    for(i in 1:(nrow(fullraw2)-1)){
      from <- fullraw2$State[i]
      to   <- fullraw2$State[i+1]
      T_count[from, to] <- T_count[from, to] + 1
    }
    
    
    # --- Step 3: 转换为 transition probability matrix ---
    T_mat <- T_count / rowSums(T_count)
    T_mat[is.na(T_mat)] <- 0   # 避免某一行没有transition
    
    
    # --- Step 4: 计算 p(i)（起点概率）---
    start_states <- fullraw2$State[-nrow(fullraw2)]
    p_i <- table(start_states) / length(start_states)
    
    # 保证顺序一致
    p_i <- p_i[states]
    p_i[is.na(p_i)] <- 0
    
    
    # --- Step 5: 计算每一行的 entropy ---
    row_entropy <- apply(T_mat, 1, function(row){
      row <- row[row > 0]
      if(length(row) == 0) return(0)
      -sum(row * log2(row))
    })
    
    
    # --- Step 6: 计算总 entropy（Markov entropy rate）---
    H <- sum(p_i * row_entropy)
    
    
    # --- Step 7: 标准化得到 S ---
    N <- 3
    S_val <- 1 - H / log2(N)
    
    
    
    
    df_PERCS$P[a] <- P_val
    df_PERCS$E[a] <- E_val
    df_PERCS$R[a] <- R_val
    df_PERCS$C[a] <- C_val
    df_PERCS$S[a] <- S_val
    
    
    
    
    
    
  } else if(a<=28){
    nXn=5
    
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
    
    
    
    fullraw=df
    
    
    ###### ADD real time (ms)
    fullraw$Time<-my_data[,2]/1000
    fullraw$TimePoint<-1:length(fullraw$TimePoint)
    
    plot_df <- freq_df %>%
      left_join(fullraw %>% dplyr::select(TimePoint, activate, Time),
                by = c("EventIndex" = "TimePoint"))
    
    # make color variable based on activate
    #plot_df$ColorGroup <- ifelse(plot_df$activate %in% c("C", "C_WP"), "C", "Other")
    
    plot_df$ColorGroup <- ifelse(plot_df$activate == "C", "C", "Other")
    
    
    
    
    
    # Prepare data: remove NA's and create data frame
    freq_clean <- na.omit(freq_df$InstantFrequency)
    df <- data.frame(y = freq_clean)
    
    # Fit 2-component normal mixture model
    set.seed(123)  # For reproducibility
    model <- flexmix(y ~ 1, data = df, k = 2, model = FLXMRglm(family = "gaussian"))
    
    
    
    # Assign clusters to each data point
    df$cluster <- clusters(model)
    print(table(df$cluster))
    
    
    
    ###### find threshod 
    # Extract cluster means (component means)
    params <- parameters(model)  # matrix with means and maybe other params
    
    # For FLXMRglm with gaussian, params is matrix with means in first row:
    comp_means <- params[1, ]
    
    # Identify which cluster is lower and which is higher
    lower_comp <- which.min(comp_means)
    higher_comp <- which.max(comp_means)
    
    # Get max y of lower cluster and min y of higher cluster
    max_lower <- max(df$y[df$cluster == lower_comp])
    min_higher <- min(df$y[df$cluster == higher_comp])
    
    # Calculate threshold as midpoint between max_lower and min_higher
    threshold <- (max_lower + min_higher) / 2
    
    threshold
    
    
    lower=mean(df$y[df$cluster == lower_comp]) 
    
    
    ###################compare with average
    mean(freq_df$InstantFrequency)
    
    
    clustered_freq=df
    clustered_freq$cluster[clustered_freq$cluster == lower_comp]="L"
    clustered_freq$cluster[clustered_freq$cluster == higher_comp]="H"
    
    clustered_freq
    
    
    
    
    #Find persistence periods
    
    
    
    # Set minimum band length (in number of rows/events)
    min_band_length <- 4
    
    # Initialize state
    in_persistence <- FALSE
    start_row <- NA
    persistence_ranges <- data.frame(start = numeric(), end = numeric())
    
    for (i in seq_len(nrow(freq_df))) {
      freq <- freq_df$InstantFrequency[i]
      
      if (!in_persistence && freq > threshold) {
        in_persistence <- TRUE
        start_row <- i
      } else if (in_persistence && freq < lower) {
        end_row <- i - 1
        if (!is.na(start_row) && (end_row - start_row + 1) >= min_band_length) {
          # Convert row index to EventIndex for plotting
          start_event <- freq_df$EventIndex[start_row]
          end_event <- freq_df$EventIndex[end_row]
          persistence_ranges <- rbind(persistence_ranges, data.frame(start = start_event, end = end_event))
          cat("Band added from row", start_row, "to", end_row, "\n")
        } else {
          cat("Band ignored from row", start_row, "to", end_row, "\n")
        }
        in_persistence <- FALSE
        start_row <- NA
      }
    }
    
    # Final open band
    if (in_persistence && !is.na(start_row)) {
      end_row <- nrow(freq_df)
      if ((end_row - start_row + 1) >= min_band_length) {
        start_event <- freq_df$EventIndex[start_row]
        end_event <- freq_df$EventIndex[end_row]
        persistence_ranges <- rbind(persistence_ranges, data.frame(start = start_event, end = end_event))
        cat("Final band added from row", start_row, "to", end_row, "\n")
      } else {
        cat("Final band ignored from row", start_row, "to", end_row, "\n")
      }
    }
    
    # Double-check output
    print("Final persistence ranges:")
    print(persistence_ranges)
    
    
    
    
    
    
    #ggsave("filtered_bands_strict.png", plot = p, width = 80, height = 5, units = "in", dpi = 300, limitsize = FALSE, bg = "white")
    #print(p)
    # --- Step 1: 简化状态 ---
    fullraw2 <- fullraw %>%
      mutate(State = case_when(
        grepl("^Left", Value)  ~ "Left",
        grepl("^Right", Value) ~ "Right",
        Value == "Pellet"      ~ "Pellet"
      )) %>%
      filter(!is.na(State))
    
    df_PERCS$Pellet[a]<-sum(I(fullraw2$State=="Pellet"))
    df_PERCS$Poke[a]<-sum(I(fullraw2$State!="Pellet"))
    
    
    
    
   
    
    
    
    ### Calculate R
    
    # --- Step 1: 定义时间范围（用 0 作为起点） ---
    t_min <- 0
    t_max <- max(fullraw$Time, na.rm = TRUE)
    t_mid <- t_max / 2
    
    
    # --- Step 2: 把 start / end 映射到真实时间 ---
    persistence_ranges$start_time <- fullraw$Time[
      match(persistence_ranges$start, fullraw$TimePoint)
    ]
    
    persistence_ranges$end_time <- fullraw$Time[
      match(persistence_ranges$end, fullraw$TimePoint)
    ]
    
    
    # --- Step 3: 按 start_time 划分前后半 ---
    # （跨中点的 bout 自动归到 first，因为看 start）
    persistence_ranges$half <- ifelse(
      persistence_ranges$start_time <= t_mid,
      "first",
      "second"
    )
    
    
    res <- persistence_ranges %>%
      mutate(length = end_time - start_time) %>%
      group_by(half) %>%
      summarise(total_length = sum(length)) %>%
      tidyr::pivot_wider(names_from = half, values_from = total_length) %>%
      mutate(ratio = second / first)
    
    R_val <- res$ratio
   
  
    ### Calculate E
    # 所有 bout 的起点
    starts<-persistence_ranges$start_time
    ends<-persistence_ranges$end_time
    
    # 对每个 Time，找到它落在哪个 start 区间位置
    idx <- findInterval(fullraw2$Time, starts)
    
    # idx = 0 说明在所有区间之前 → 肯定不在 bout 里
    # 否则检查是否 <= 对应的 end_time
    in_bout <- idx > 0 & fullraw2$Time <= ends[pmax(idx, 1)]
    
    # 再筛 Left / Right
    keep <- in_bout & fullraw2$activate %in% c("C")
    keep_P<-in_bout & fullraw2$State %in% c("Left","Right")
    
    # 最终结果
    result <- fullraw2[keep, ]
    result_P<-fullraw2[keep_P, ]
    # 总数
    nrow(result)
    
    E_val=nrow(result)/sum(I(fullraw2$activate!="P"))
    
    
    ### calculate P
    P_val <-nrow(result_P)  
    
    
   
  
    ### Calculate C  
    # --- Step 1: 计算 bout lengths ---
    bout_lengths <- persistence_ranges$end_time - persistence_ranges$start_time
    
    # --- Step 2: 计算 non-bout lengths ---
    # 非 bout 时间 = 上一个 bout 结束到下一个 bout 开始
    # 如果只有一个 bout，non-bout length = NA
    if(nrow(persistence_ranges) > 1){
      non_bout_lengths <- persistence_ranges$start_time[-1] - persistence_ranges$end_time[-nrow(persistence_ranges)]
    } else {
      non_bout_lengths <- NA
    }
    
    # --- Step 3: 计算 CV ---
    # CV = 标准差 / 平均值
    cv_bout <- sd(bout_lengths, na.rm = TRUE) / mean(bout_lengths, na.rm = TRUE)
    cv_non_bout <- sd(non_bout_lengths, na.rm = TRUE) / mean(non_bout_lengths, na.rm = TRUE)
    
    # --- Step 4: 计算新指标 ---
    C_val <- 1 / (cv_bout + cv_non_bout)
    
    
    
    ### Calculate S
    # --- Step 1: 简化状态 ---
    fullraw2 <- fullraw %>%
      mutate(State = case_when(
        grepl("^Left", Value)  ~ "Left",
        grepl("^Right", Value) ~ "Right",
        Value == "Pellet"      ~ "Pellet"
      )) %>%
      filter(!is.na(State))
    
    
    # --- Step 2: 构建 transition count matrix ---
    states <- c("Left", "Right", "Pellet")
    T_count <- matrix(0, nrow=3, ncol=3, dimnames=list(states, states))
    
    for(i in 1:(nrow(fullraw2)-1)){
      from <- fullraw2$State[i]
      to   <- fullraw2$State[i+1]
      T_count[from, to] <- T_count[from, to] + 1
    }
    
    
    # --- Step 3: 转换为 transition probability matrix ---
    T_mat <- T_count / rowSums(T_count)
    T_mat[is.na(T_mat)] <- 0   # 避免某一行没有transition
    
    
    # --- Step 4: 计算 p(i)（起点概率）---
    start_states <- fullraw2$State[-nrow(fullraw2)]
    p_i <- table(start_states) / length(start_states)
    
    # 保证顺序一致
    p_i <- p_i[states]
    p_i[is.na(p_i)] <- 0
    
    
    # --- Step 5: 计算每一行的 entropy ---
    row_entropy <- apply(T_mat, 1, function(row){
      row <- row[row > 0]
      if(length(row) == 0) return(0)
      -sum(row * log2(row))
    })
    
    
    # --- Step 6: 计算总 entropy（Markov entropy rate）---
    H <- sum(p_i * row_entropy)
    
    
    # --- Step 7: 标准化得到 S ---
    N <- 3
    S_val <- 1 - H / log2(N)
    
    
    
    
    
    df_PERCS$P[a] <- P_val
    df_PERCS$E[a] <- E_val
    df_PERCS$R[a] <- R_val
    df_PERCS$C[a] <- C_val
    df_PERCS$S[a] <- S_val
    
    
    
    
    
  }else {
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
    
    
    
    fullraw=df
    
    
    ###### ADD real time (ms)
    fullraw$Time<-my_data[,2]/1000
    fullraw$TimePoint<-1:length(fullraw$TimePoint)
    
    plot_df <- freq_df %>%
      left_join(fullraw %>% dplyr::select(TimePoint, activate, Time),
                by = c("EventIndex" = "TimePoint"))
    
    # make color variable based on activate
    #plot_df$ColorGroup <- ifelse(plot_df$activate %in% c("C", "C_WP"), "C", "Other")
    
    plot_df$ColorGroup <- ifelse(plot_df$activate == "C", "C", "Other")
    
    
    
    
    
    # Prepare data: remove NA's and create data frame
    freq_clean <- na.omit(freq_df$InstantFrequency)
    df <- data.frame(y = freq_clean)
    
    # Fit 2-component normal mixture model
    set.seed(123)  # For reproducibility
    model <- flexmix(y ~ 1, data = df, k = 2, model = FLXMRglm(family = "gaussian"))
    
    
    
    # Assign clusters to each data point
    df$cluster <- clusters(model)
    print(table(df$cluster))
    
    # Plot histogram colored by cluster assignment
    ggplot(df, aes(x = y, fill = factor(cluster))) +
      geom_histogram(bins = 50, position = "identity", alpha = 0.6) +
      labs(
        title = "Mixture of Two Normals (via flexmix)",
        x = "Instantaneous Frequency",
        fill = "Component"
      ) +
      theme_minimal()
    
    ###### find threshod 
    # Extract cluster means (component means)
    params <- parameters(model)  # matrix with means and maybe other params
    
    # For FLXMRglm with gaussian, params is matrix with means in first row:
    comp_means <- params[1, ]
    
    # Identify which cluster is lower and which is higher
    lower_comp <- which.min(comp_means)
    higher_comp <- which.max(comp_means)
    
    # Get max y of lower cluster and min y of higher cluster
    max_lower <- max(df$y[df$cluster == lower_comp])
    min_higher <- min(df$y[df$cluster == higher_comp])
    
    # Calculate threshold as midpoint between max_lower and min_higher
    threshold <- (max_lower + min_higher) / 2
    
    threshold
    
    
    lower=mean(df$y[df$cluster == lower_comp]) 
    
    
    ###################compare with average
    mean(freq_df$InstantFrequency)
    
    
    clustered_freq=df
    clustered_freq$cluster[clustered_freq$cluster == lower_comp]="L"
    clustered_freq$cluster[clustered_freq$cluster == higher_comp]="H"
    
    clustered_freq
    
    
    
    
    #Find persistence periods
    
    
    
    # Set minimum band length (in number of rows/events)
    min_band_length <- 4
    
    # Initialize state
    in_persistence <- FALSE
    start_row <- NA
    persistence_ranges <- data.frame(start = numeric(), end = numeric())
    
    for (i in seq_len(nrow(freq_df))) {
      freq <- freq_df$InstantFrequency[i]
      
      if (!in_persistence && freq > threshold) {
        in_persistence <- TRUE
        start_row <- i
      } else if (in_persistence && freq < lower) {
        end_row <- i - 1
        if (!is.na(start_row) && (end_row - start_row + 1) >= min_band_length) {
          # Convert row index to EventIndex for plotting
          start_event <- freq_df$EventIndex[start_row]
          end_event <- freq_df$EventIndex[end_row]
          persistence_ranges <- rbind(persistence_ranges, data.frame(start = start_event, end = end_event))
          cat("Band added from row", start_row, "to", end_row, "\n")
        } else {
          cat("Band ignored from row", start_row, "to", end_row, "\n")
        }
        in_persistence <- FALSE
        start_row <- NA
      }
    }
    
    # Final open band
    if (in_persistence && !is.na(start_row)) {
      end_row <- nrow(freq_df)
      if ((end_row - start_row + 1) >= min_band_length) {
        start_event <- freq_df$EventIndex[start_row]
        end_event <- freq_df$EventIndex[end_row]
        persistence_ranges <- rbind(persistence_ranges, data.frame(start = start_event, end = end_event))
        cat("Final band added from row", start_row, "to", end_row, "\n")
      } else {
        cat("Final band ignored from row", start_row, "to", end_row, "\n")
      }
    }
    
    # Double-check output
    print("Final persistence ranges:")
    print(persistence_ranges)
    
    
    
    
    
    
    #ggsave("filtered_bands_strict.png", plot = p, width = 80, height = 5, units = "in", dpi = 300, limitsize = FALSE, bg = "white")
    #print(p)
    # --- Step 1: 简化状态 ---
    
    
    fullraw2 <- fullraw %>%
      mutate(State = case_when(
        grepl("^Left", Value)  ~ "Left",
        grepl("^Right", Value) ~ "Right",
        Value == "Pellet"      ~ "Pellet"
      )) %>%
      filter(!is.na(State))
    
    df_PERCS$Pellet[a]<-sum(I(fullraw2$State=="Pellet"))
    df_PERCS$Poke[a]<-sum(I(fullraw2$State!="Pellet"))
    

    
    
    
    
    
    
    
    ### Calculate R
    
    # --- Step 1: 定义时间范围（用 0 作为起点） ---
    t_min <- 0
    t_max <- max(fullraw$Time, na.rm = TRUE)
    t_mid <- t_max / 2
    
    
    # --- Step 2: 把 start / end 映射到真实时间 ---
    persistence_ranges$start_time <- fullraw$Time[
      match(persistence_ranges$start, fullraw$TimePoint)
    ]
    
    persistence_ranges$end_time <- fullraw$Time[
      match(persistence_ranges$end, fullraw$TimePoint)
    ]
    
    
    # --- Step 3: 按 start_time 划分前后半 ---
    # （跨中点的 bout 自动归到 first，因为看 start）
    persistence_ranges$half <- ifelse(
      persistence_ranges$start_time <= t_mid,
      "first",
      "second"
    )
    
    
    res <- persistence_ranges %>%
      mutate(length = end_time - start_time) %>%
      group_by(half) %>%
      summarise(total_length = sum(length)) %>%
      tidyr::pivot_wider(names_from = half, values_from = total_length) %>%
      mutate(ratio = second / first)
    
    R_val <- res$ratio
    
    
    
    ### Calculate E
    # 所有 bout 的起点
    starts<-persistence_ranges$start_time
    ends<-persistence_ranges$end_time
    
    # 对每个 Time，找到它落在哪个 start 区间位置
    idx <- findInterval(fullraw2$Time, starts)
    
    # idx = 0 说明在所有区间之前 → 肯定不在 bout 里
    # 否则检查是否 <= 对应的 end_time
    in_bout <- idx > 0 & fullraw2$Time <= ends[pmax(idx, 1)]
    
    # 再筛 Left / Right
    keep <- in_bout & fullraw2$activate %in% c("C")
    keep_P<-in_bout & fullraw2$State %in% c("Left","Right")
    
    # 最终结果
    result <- fullraw2[keep, ]
    result_P<-fullraw2[keep_P, ]
    # 总数
    nrow(result)
    
    E_val=nrow(result)/sum(I(fullraw2$activate!="P"))
    
    
    ### calculate P
    P_val <-nrow(result_P)  
    
    
    
    
    ### Calculate C  
    # --- Step 1: 计算 bout lengths ---
    bout_lengths <- persistence_ranges$end_time - persistence_ranges$start_time
    
    # --- Step 2: 计算 non-bout lengths ---
    # 非 bout 时间 = 上一个 bout 结束到下一个 bout 开始
    # 如果只有一个 bout，non-bout length = NA
    if(nrow(persistence_ranges) > 1){
      non_bout_lengths <- persistence_ranges$start_time[-1] - persistence_ranges$end_time[-nrow(persistence_ranges)]
    } else {
      non_bout_lengths <- NA
    }
    
    # --- Step 3: 计算 CV ---
    # CV = 标准差 / 平均值
    cv_bout <- sd(bout_lengths, na.rm = TRUE) / mean(bout_lengths, na.rm = TRUE)
    cv_non_bout <- sd(non_bout_lengths, na.rm = TRUE) / mean(non_bout_lengths, na.rm = TRUE)
    
    # --- Step 4: 计算新指标 ---
    C_val <- 1 / (cv_bout + cv_non_bout)
    
    
    
    ### Calculate S
    # --- Step 1: 简化状态 ---
    fullraw2 <- fullraw %>%
      mutate(State = case_when(
        grepl("^Left", Value)  ~ "Left",
        grepl("^Right", Value) ~ "Right",
        Value == "Pellet"      ~ "Pellet"
      )) %>%
      filter(!is.na(State))
    
    
    # --- Step 2: 构建 transition count matrix ---
    states <- c("Left", "Right", "Pellet")
    T_count <- matrix(0, nrow=3, ncol=3, dimnames=list(states, states))
    
    for(i in 1:(nrow(fullraw2)-1)){
      from <- fullraw2$State[i]
      to   <- fullraw2$State[i+1]
      T_count[from, to] <- T_count[from, to] + 1
    }
    
    
    # --- Step 3: 转换为 transition probability matrix ---
    T_mat <- T_count / rowSums(T_count)
    T_mat[is.na(T_mat)] <- 0   # 避免某一行没有transition
    
    
    # --- Step 4: 计算 p(i)（起点概率）---
    start_states <- fullraw2$State[-nrow(fullraw2)]
    p_i <- table(start_states) / length(start_states)
    
    # 保证顺序一致
    p_i <- p_i[states]
    p_i[is.na(p_i)] <- 0
    
    
    # --- Step 5: 计算每一行的 entropy ---
    row_entropy <- apply(T_mat, 1, function(row){
      row <- row[row > 0]
      if(length(row) == 0) return(0)
      -sum(row * log2(row))
    })
    
    
    # --- Step 6: 计算总 entropy（Markov entropy rate）---
    H <- sum(p_i * row_entropy)
    
    
    # --- Step 7: 标准化得到 S ---
    N <- 3
    S_val <- 1 - H / log2(N)
    
    
    
    df_PERCS$P[a] <- P_val
    df_PERCS$E[a] <- E_val
    df_PERCS$R[a] <- R_val
    df_PERCS$C[a] <- C_val
    df_PERCS$S[a] <- S_val
    
    
    
    
    
    
  }
  
  
}




df_PERCS


# 设置画布为 5行1列
par(mfrow = c(5,1), mar = c(4,4,2,1))

# P
boxplot(P ~ group, data = df_PERCS,
        main = "P",
        xlab = "Paradigm",
        ylab = "P")

# E
boxplot(E ~ group, data = df_PERCS,
        main = "E",
        xlab = "Paradigm",
        ylab = "E")

# R
boxplot(R ~ group, data = df_PERCS,
        main = "R",
        xlab = "Paradigm",
        ylab = "R")

# C
boxplot(C ~ group, data = df_PERCS,
        main = "C",
        xlab = "Paradigm",
        ylab = "C")

# S
boxplot(S ~ group, data = df_PERCS,
        main = "S",
        xlab = "Paradigm",
        ylab = "S")


df_norm<-df_PERCS

df_ind<-df_PERCS


df_norm$P<-(df_norm$P-min(df_norm$P))/(max(df_norm$P)-min(df_norm$P))
df_norm$E<-(df_norm$E-min(df_norm$E))/(max(df_norm$E)-min(df_norm$E))
df_norm$R<-(df_norm$R-min(df_norm$R))/(max(df_norm$R)-min(df_norm$R))
df_norm$C<-(df_norm$C-min(df_norm$C))/(max(df_norm$C)-min(df_norm$C))
df_norm$S<-(df_norm$S-min(df_norm$S))/(max(df_norm$S)-min(df_norm$S))

df<-df_norm





#write.csv(df_norm, "PERCS_IND.csv", row.names = FALSE)



####plot
# -----------------------------------------------------
global_max <- max(df[, c("P","E","R","C","S")])
global_min <- min(df[, c("P","E","R","C","S")])

# radar max/min rows
radar_limits <- data.frame(
  P = c(global_max, global_min),
  E = c(global_max, global_min),
  R = c(global_max, global_min),
  C = c(global_max, global_min),
  S = c(global_max, global_min)
)

# -----------------------------------------------------
# 3. PLOT SETTINGS: 4 plots in a 2x2 grid
# -----------------------------------------------------



groups <- unique(df$group)

# -----------------------------------------------------
# 4. DRAW RADAR FOR EACH GROUP
# -----------------------------------------------------
group_names <- c(
  "1" = "FR",
  "2" = "2x2",
  "3" = "5x5",
  "4" = "RPR"
)

par(mfrow = c(2, 2))

for (g in groups) {
  
  group_data <- df[df$group == g, c("P","E","R","C","S")]
  # rename dimensions here
  colnames(group_data) <- c("Perseverance", "Endurance", "Resistance", "Consistency", "Stability")
  
  colnames(radar_limits) <- colnames(group_data)
  # combine max/min rows + real data
  radar_df <- rbind(radar_limits, group_data)
  
  # compute axis labels
  axis_labels <- round(seq(global_min, global_max, length.out = 6), 2)
 
  radarchart(
    radar_df,                                                                                                                                                                                                                                                                       
    axistype = 1,
    seg = 5,                         # how many rings
    caxislabels = axis_labels,       # real numeric scale
    pcol = rainbow(nrow(group_data)),
    plwd = 2,
    plty = 1,
    title = paste("Group", group_names[as.character(g)]),
    cglcol = "grey",
    cglty = 1,
    cglwd = 0.8,
    axislabcol = "grey30"
    
  )
  if (g!=1){
  df_real <- radar_df[3:nrow(radar_df), ]  # remove first 2 rows (max/min)
  
  cor_matrix <- cor(df_real, method = "spearman")  # or method = "pearson"
  print(round(cor_matrix, 2))
  # Convert to long format for ggplot
  cor_long <- melt(cor_matrix)
  colnames(cor_long) <- c("Var1","Var2","value")
  #print(
  # Plot with ggplot2
  ggplot(cor_long, aes(x=Var1, y=Var2, fill=value)) +
    geom_tile(color="white") +
    geom_text(aes(label=round(value,2)), color="black") +
    scale_fill_gradient2(low="blue", mid="white", high="red", midpoint=0, limits=c(-1,1)) +
    scale_y_discrete(limits=rev(levels(cor_long$Var2))) +  # reverse y-axis
    theme_minimal() +
    theme(axis.text.x = element_text(angle=45, hjust=1)) +
    labs(title="Correlation Heatmap", fill="Correlation")
  #)
  }
}



library(MASS)



df_norm$group <- factor(df_norm$group)

Cmodel<-lm(C ~ group + P + E + R  + S,
           data=df_norm)
summary(Cmodel)


m_intercept_only <- glm.nb(
  Pellet ~ group + P + E + R + C + S,
  data = df_norm
)

summary(m_intercept_only)



library(car)
vif(m_intercept_only)

m_xC <- glm.nb(
  Pellet ~ group + P + E + R  + S,
  data = df_norm
)

summary(m_xC)
vif(m_xC)

m_E <- glm.nb(
  Pellet ~ group + P + E + R + C + S + group:E,
  data = df_norm
)

anova(m_intercept_only, m_E, test = "Chisq")
AIC(m_intercept_only, m_E)



m_C <- glm.nb(
  Pellet ~ group + P + E + R + C + S + group:C,
  data = df_norm
)

anova(m_intercept_only, m_E, test = "Chisq")
AIC(m_intercept_only, m_C)


m_S <- glm.nb(
  Pellet ~ group + P + E + R + C + S + group:S,
  data = df_norm
)

anova(m_intercept_only, m_E, test = "Chisq")
AIC(m_intercept_only, m_S)

#install.packages("MuMIn")
library(MuMIn)
m_full <- glm.nb(
  Pellet ~ group + P + E + R  + S,
  data = df_norm,
  na.action = na.fail
)

model_set <- dredge(m_full, rank = "AIC")

model_set

m_best <- glm.nb(
  Pellet ~ E + S,
  data = df_norm
)

summary(m_best)


m_poke <- glm.nb(
  Poke ~ group + P + E + R  + S,
  data = df_norm,
  na.action = na.fail
)

summary(m_poke)



model_set <- dredge(m_poke, rank = "AIC")

model_set





