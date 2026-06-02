library(ggplot2)
library(dplyr)
library(zoo)
library(lubridate)
my_data <- read.csv("Read some mice data.CSV")

my_data$Event <- gsub("RightWithPellet", "Right_WP", my_data$Event)
my_data$Event <- gsub("RightDuringDispense", "Right_DD", my_data$Event)
my_data$Event <- gsub("LeftWithPellet", "Left_WP", my_data$Event)
my_data$Event <- gsub("LeftDuringDispense", "Left_DD", my_data$Event)
my_data$Event <- gsub("RightinTimeout", "Right_TO", my_data$Event)
my_data$Event <- gsub("RightinTimeOut", "Right_TO", my_data$Event)
my_data$Event <- gsub("LeftinTimeOut", "Left_TO", my_data$Event)
my_data$Event <- gsub("LeftinTimeout", "Left_TO", my_data$Event)


nXn=0   # Set nXn according to Paradigm----0: FR; 2: 2x2; 5:5x5; "inf": RPR

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








# Plot with colored rectangles (C = blue, F = red)
ggplot(df, aes(x = TimePoint, y = 1, fill = activate)) +
  geom_tile(height = 1) +  # This makes it a continuous rectangle
  scale_fill_manual(values = c("C" = "blue", "C_WP" = "blue", "C_DD" = "blue", "C_TO" = "blue", "L"= "blue","R"= "blue","I" = "red","crep" = "red","irep" = "red","crep_DD" = "red","crep_WP" = "red","irep_DD" = "red","irep_WP" = "red", "irep_TO" = "red", "crep_TO" = "red")) +  # Set colors for C and F
  labs(
    x = "Number of Behaviors",
    y = "Correctness of Behaviors",
    title = "Correct/Incorrect Behavior Vs. Time points"
  ) +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),  # Remove the y-axis title
    axis.text.y = element_blank(),   # Remove the y-axis labels
    axis.ticks.y = element_blank()   # Remove the y-axis ticks
  ) +
  theme(legend.position = "none")  # Remove legend if not needed




###### ADD real time (s)
#df$ExactTime<-my_data[,1]

# Parse using parse_date_time for flexibility
#df$ExactTime <- parse_date_time(df$ExactTime, orders = "mdy HMS")

# Create new column for seconds since the first time point
#df$ElapsedSec <- as.numeric(difftime(df$ExactTime, df$ExactTime[1], units = "secs"))






# Calculate the cumulative number of "C"s up to each time point
cumulative_C <- cumsum(df$activate == "C"|df$activate == "C_DD"|df$activate == "C_WP"|df$activate == "C_TO"|df$activate == "L"|df$activate == "R")
cumulative_I <- cumsum(df$activate == "I")
cumulative_rep <- cumsum(df$activate == "irep"|df$activate == "crep"|df$activate == "irep_DD"|df$activate == "irep_WP"|df$activate == "crep_DD"|df$activate == "crep_WP"|df$activate == "crep_TO"|df$activate == "irep_TO")



# Calculate the correction proportion for each time point
correction_proportion <- cumulative_C / seq_along(df$activate)
I_proportion<-cumulative_I/ seq_along(df$activate)
rep_proportion<-cumulative_rep / seq_along(df$activate)

# Create a data frame to use with ggplot
df_correction <- data.frame(
  TimePoint = seq_along(df$activate),  # Time points as the index of the vector
  CorrectionProportion = correction_proportion,
  I_Proportion = I_proportion,
  Rep_Proportion = rep_proportion
)

# Reshape the data into long format for ggplot
df_long <- reshape(df_correction, 
                   varying = c("CorrectionProportion", "I_Proportion", "Rep_Proportion"),
                   v.names = "Proportion",
                   timevar = "Type",
                   times = c("Correct", "Mistake", "Repeat"),
                   direction = "long")

# Plot all three proportions vs. time points
ggplot(df_long, aes(x = TimePoint, y = Proportion, color = Type)) +
  geom_line(size = 1) +  # Line to show the proportions over time
  scale_color_manual(values = c("Correct" = "blue", "Mistake" = "red", "Repeat" = "green"))+
  labs(
    x = "Number of behaviors",
    y = "Proportion",
    title = "Proportion vs. Number of Behaviors",
    color = "Proportion Type"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +  # Adjust legend position
  coord_cartesian(ylim = c(0, 1))   # Set y-axis limits between 0 and 1




# moving average
# Set the window size k
k <- 10  # You can change this to any number you want

# Create logical vectors for each type
is_C <- df$activate %in% c("C", "C_DD", "C_WP", "C_TO", "L", "R")
is_I <- df$activate == "I"
is_rep <- df$activate %in% c("irep", "crep", "irep_DD", "irep_WP", "crep_DD", "crep_WP", "crep_TO", "irep_TO")

# Moving proportions (rolling mean of logical values gives proportions)
moving_C <- rollapply(is_C, width = k, FUN = mean, align = "right", fill = NA)
moving_I <- rollapply(is_I, width = k, FUN = mean, align = "right", fill = NA)
moving_rep <- rollapply(is_rep, width = k, FUN = mean, align = "right", fill = NA)

# Total correct behavior rate (you can sum the three components or define it differently)
moving_CCBR <- moving_C  # or e.g., (moving_C + moving_I + moving_rep) if that’s your definition

# Prepare the data for plotting
df_moving <- data.frame(
  TimePoint = seq_along(df$activate),
  MovingCCBR = moving_CCBR
)

# Plot
ggplot(df_moving, aes(x = TimePoint, y = MovingCCBR)) +
  geom_line(color = "darkgreen", size = 1) +
  labs(
    x = "Number of Behaviors",
    y = paste0("Proportion of Correct Behavior (Recent ", k, "-timesteps)"),
    title = "Moving proportion of Correct Behavior (N2, DAY 3, k=10, First 100 timesteps)"
  ) +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 1),xlim = c(0, 100))



fullraw=df[,1:3]


###### ADD real time (ms)
fullraw$Time<-my_data[,2]/1000

}

























