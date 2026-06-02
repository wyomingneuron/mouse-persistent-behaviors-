library(ggplot2)
library(dplyr)
library(zoo)
library(lubridate)
library(MASS)
setwd("set your own workspace")
my_data <- read.csv("Read the same mice data as in event label.R.CSV")

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



# Plot the time interval
ggplot(freq_df, aes(x = EventIndex, y = Timeinterval)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Time Interval between Poking",
    x = "Event Index (Filtered)",
    y = "Time Interval between Poking (Seconds)"
  ) +
  theme_minimal()





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



#####fix upper

##### auto upper
upper=max(freq_df$InstantFrequency)+0.2
upper_p=max(freq_dfp$InstantFrequency)+0.2


# Plot the instantaneous frequency
p=ggplot(freq_dfp, aes(x = EventIndex, y = InstantFrequency)) +
  geom_line() +
  geom_point() +
  labs(
   title = "Instantaneous Frequency for Pellet (RPR)",
   x = "Event Index (Filtered)",
    y = "Frequency (Hz)"
  ) +
  ylim(0, 0.1) + #xlim(0,2000)
  theme_minimal()+
  theme(
    panel.grid = element_blank(),        # remove all grid lines
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    axis.text.x  = element_text(size = 25),
    axis.text.y  = element_text(size = 25),
    plot.title   = element_text(size = 25),   # picture title
    legend.title = element_text(size = 25),   # legend title
    legend.text  = element_text(size = 25)    # legend labels
  )

print(p)
ggsave("long_freq_p.png", plot = p, width = 20, height = 5, units = "in", dpi = 300, limitsize = FALSE,bg = "white") # get location of this saved plot by getwd()






























### nee fullraw from CCBR file
# join freq_df with fullraw by EventIndex / TimePoint
plot_df <- freq_df %>%
  left_join(fullraw %>%dplyr::select(TimePoint, activate),
            by = c("EventIndex" = "TimePoint"))




# make color variable based on activate
#plot_df$ColorGroup <- ifelse(plot_df$activate %in% c("C", "C_WP"), "C", "Other")

plot_df$ColorGroup <- ifelse(plot_df$activate == "C", "C", "Other")


# plot
p <- ggplot(plot_df, aes(x = EventIndex, y = InstantFrequency)) +
  geom_line() +
  geom_point(aes(color = ColorGroup)) +
  scale_color_manual(values = c("C" = "blue", "Other" = "red")) +
  labs(
    title = "Instantaneous Frequency for Poking (FR)",
    x = "Event Index ",
    y = "Frequency (Hz)",
    color = "Color_group"
  ) +
  ylim(0, 2.1) +
 
  theme_minimal()

print(p)
ggsave("long_freq.png", plot = p, width = 20, height = 5, units = "in", dpi = 300, limitsize = FALSE,bg = "white") # get location of this saved plot by getwd()







############check if instant frequency relates to consecutive mistakes.

plot_df$consec_Other <- 0  # initialize column
count <- 0

for(i in 1:nrow(plot_df)) {
  if(plot_df$ColorGroup[i] == "Other") {
    count <- count + 1
    plot_df$consec_Other[i] <- count
  } else {
    count <- 0
  }
}




plot_df



# Linear regression
lm_model <- lm(InstantFrequency ~ consec_Other, data = plot_df)
summary(lm_model)  # see coefficients and R-squared

mean(plot_df$InstantFrequency[plot_df$consec_Other==15])


avg_df <- aggregate(
  InstantFrequency ~ consec_Other,
  data = plot_df,
  FUN = mean
)

lm_model_avg <- lm(InstantFrequency ~ consec_Other, data = avg_df)
summary(lm_model_avg)


par(mfrow = c(2, 2))  # show 4 plots together
plot(lm_model_avg)

# Basic scatter plot with regression line
ggplot(avg_df, aes(x = consec_Other, y = InstantFrequency)) +
  geom_point(size = 3, color = "blue") +              # points for the means
  geom_smooth(method = "lm", se = TRUE, color = "red") +  # linear regression line with confidence interval
  labs(
    x = "Number of Consecutive Other Pokes",
    y = "Mean Instantaneous Frequency",
    title = "Relationship Between Consecutive Other Pokes and Mean Instantaneous Frequency"
  ) +
  theme_minimal(base_size = 14)                        # clean minimal theme



# Fit robust regression
rlm_model <- rlm(InstantFrequency ~ consec_Other, data = avg_df)

# Create a new data frame with predicted values
avg_df$rlm_fit <- predict(rlm_model)

# Plot with ggplot2
ggplot(avg_df, aes(x = consec_Other, y = InstantFrequency)) +
  geom_point(size = 3, color = "blue") +            # scatter points
  geom_line(aes(y = rlm_fit), color = "red", size = 1) +  # fitted robust regression line
  labs(
    x = "Number of Consecutive Other Pokes",
    y = "Mean Instantaneous Frequency",
    title = "Robust Regression of Mean Instantaneous Frequency vs Consecutive Other Pokes"
  ) +
  theme_minimal(base_size = 14)

residuals_rlm <- resid(rlm_model)  # or rlm_model if linear

# Plot autocorrelation function
acf(residuals_rlm, main = "ACF of Residuals from Robust Quadratic Regression")



# Quadratic robust regression
rlm_model_poly <- rlm(InstantFrequency ~ consec_Other + I(consec_Other^2), data = avg_df)

# Predicted values for plotting
avg_df$rlm_fit_poly <- predict(rlm_model_poly)


ggplot(avg_df, aes(x = consec_Other, y = InstantFrequency)) +
  geom_point(size = 3, color = "blue") +                      # mean IF points
  geom_line(aes(y = rlm_fit_poly), color = "red", size = 1) + # quadratic robust fit
  labs(
    x = "Number of Consecutive Other Pokes",
    y = "Mean Instantaneous Frequency",
    title = "Quadratic Robust Regression of Mean Instantaneous Frequency"
  ) +
  theme_minimal(base_size = 14)





ggplot(plot_df, aes(x = consec_Other, y = InstantFrequency)) +
  geom_point(color = "blue") +                 # scatter points
  geom_smooth(method = "lm", color = "red") +  # regression line
  labs(x = "Consecutive Other Count",
       y = "Instant Frequency",
       title = "Instant Frequency vs Consecutive Other") +  ylim(0, 4)+
  theme_minimal()



ggplot(plot_df, aes(x = EventIndex, y = consec_Other)) +
  geom_point(size=0.6)









library(plotly)

# Make a 3D scatter/mesh plot
plot_ly(
  data = plot_df,
  x = ~EventIndex,
  y = ~consec_Other,
  z = ~InstantFrequency,
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 4, color = ~InstantFrequency, colorscale = "Viridis", showscale = TRUE)
)






#library(plotly)
#library(akima)

# Interpolate data to a grid
#interp_res <- with(plot_df, interp(x = EventIndex, y = consec_Other, z = InstantFrequency, duplicate = "mean"))

# 3D surface plot
#plot_ly(x = interp_res$x, 
#        y = interp_res$y, 
#        z = interp_res$z, 
#        type = "surface") %>%
#  layout(scene = list(
#    xaxis = list(title = "EventIndex"),
#    yaxis = list(title = "consec_Other"),
#    zaxis = list(title = "InstantFrequency")
#  ),
#  title = "Surface: InstantFrequency ~ EventIndex + consec_Other")


# Run two-sample t-test
t_result <- t.test(InstantFrequency ~ ColorGroup, data = plot_df)

# See results
t_result



# Extract group means
means <- plot_df %>%
  group_by(ColorGroup) %>%
  summarise(mean_val = mean(InstantFrequency))

# Annotation text for t-test result
annot_text <- paste0(
  "t = ", round(t_result$statistic, 2),
  ", df = ", round(t_result$parameter, 1),
  "\np = ", signif(t_result$p.value, 3)
)

# Plot
ggplot(plot_df, aes(x = InstantFrequency, fill = ColorGroup)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
  scale_fill_manual(values = c("C" = "blue", "Other" = "red")) +
  geom_vline(data = means, aes(xintercept = mean_val, color = ColorGroup),
             linetype = "dashed", size = 1) +
  # Add text labels at mean lines
  geom_text(data = means,
            aes(x = mean_val, y = 0, 
                label = paste0("Mean = ", round(mean_val, 3)),
                color = ColorGroup),
            angle = 90, vjust = -0.5, hjust = 0, show.legend = FALSE) +
  labs(
    title = "Histogram of InstantFrequency by Group",
    x = "InstantFrequency",
    y = "Count"
  ) +
  theme_minimal() +
  annotate("text", x = Inf, y = Inf, label = annot_text,
           hjust = 1.1, vjust = 1.5, size = 4, color = "black")


#### moving average for instant frequency 
# Set your moving average window size
k <- 64  # for example, adjust this as needed

# Compute the moving average
freq_df$SmoothedFrequency <- rollmean(freq_df$InstantFrequency, k = k, fill = NA, align = "center")

# Plot both original and smoothed frequencies
ggplot(freq_df, aes(x = EventIndex)) +
  geom_line(aes(y = InstantFrequency), color = "blue", alpha = 0.4, size = 1) +
  geom_point(aes(y = InstantFrequency), color = "blue", alpha = 0.4) +
  geom_line(aes(y = SmoothedFrequency), color = "red", size = 1) +
  labs(
    title = paste("Instantaneous Frequency and", k, "-point Moving Average"),
    x = "Event Index (Filtered)",
    y = "Frequency (Hz)"
  ) +
  ylim(0, upper) +
  theme_minimal()
#### moving average only
# Create a new data frame with non-NA smoothed values
smoothed_df <- data.frame(
  EventIndex = freq_df$EventIndex,
  SmoothedFrequency = freq_df$SmoothedFrequency
)

# Remove rows with NA (due to windowing at edges)
smoothed_df <- na.omit(smoothed_df)

# Plot the smoothed frequency only
ggplot(smoothed_df, aes(x = EventIndex, y = SmoothedFrequency)) +
  geom_line(color = "red", size = 1) +
  geom_point(color = "red") +
  labs(
    title = paste("Smoothed Instantaneous Frequency (", k, "-point Moving Average)", sep = ""),
    x = "Event Index (Filtered)",
    y = "Frequency (Hz)"
  ) +
  ylim(0, upper) +
  theme_minimal()



hist(freq_df$InstantFrequency,breaks = 50)















########################Mixture model for freq
###################################mixture of two normal
# install.packages("flexmix")
library(flexmix)

# Prepare data: remove NA's and create data frame
freq_clean <- na.omit(freq_df$InstantFrequency)
df <- data.frame(y = freq_clean)

# Fit 2-component normal mixture model
set.seed(123)  # For reproducibility
model <- flexmix(y ~ 1, data = df, k = 2, model = FLXMRglm(family = "gaussian"))

# Show summary and parameters
summary(model)
print(parameters(model))

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




bins <- 50

# 计算每个簇的均值和标准差
params <- parameters(model)
comp_means <- params[1, ]
comp_sds <- sapply(1:2, function(i) sd(df$y[df$cluster == i]))

ggplot(df, aes(x = y, fill = factor(cluster))) +
  geom_histogram(aes(y = ..density..), bins = bins, position = "identity", alpha = 0.4, color = "black") +
  stat_function(fun = function(x) dnorm(x, mean = comp_means[lower_comp], sd = comp_sds[lower_comp]),
                color = "blue", size = 1.2) +
  stat_function(fun = function(x) dnorm(x, mean = comp_means[higher_comp], sd = comp_sds[higher_comp]),
                color = "red", size = 1.2) +
  labs(
    title = "Mixture of Two Normals (via flexmix) with Gaussian Curves",
    x = "Instantaneous Frequency",
    fill = "Component"
  ) +
  theme_minimal()


# Parameters from your model
params <- parameters(model)
comp_means <- params[1, ]
comp_sds   <- params[2, ]  # sigma from flexmix

# Example: which is lower and higher mean
lower_comp  <- which.min(comp_means)
higher_comp <- which.max(comp_means)


# Histogram + mixture curves + annotation
ggplot(df, aes(x = y, fill = factor(cluster))) +
  geom_histogram(aes(y = ..density..), bins = bins, 
                 position = "identity", alpha = 0.4, color = "black") +
  # Gaussian curves
  stat_function(fun = function(x) dnorm(x, mean = comp_means[lower_comp], sd = comp_sds[lower_comp]),
                color = "blue", size = 1.2) +
  stat_function(fun = function(x) dnorm(x, mean = comp_means[higher_comp], sd = comp_sds[higher_comp]),
                color = "red", size = 1.2) + 
  # Vertical lines at means
  geom_vline(xintercept = comp_means[lower_comp], color = "blue", linetype = "dashed", size = 1) +
  geom_vline(xintercept = comp_means[higher_comp], color = "red", linetype = "dashed", size = 1) +
  # Add mean & sd text labels
  annotate("text", x = 0.2, 
           y = 18,
           label = sprintf("μ = %.3f\nσ = %.3f", comp_means[lower_comp], comp_sds[lower_comp]),
           color = "blue", hjust = 0.5, vjust = 0) +
  annotate("text", x = 0.5, 
           y = max(dnorm(df$y, mean = comp_means[higher_comp], sd = comp_sds[higher_comp])) * 1.05,
           label = sprintf("μ = %.3f\nσ = %.3f", comp_means[higher_comp], comp_sds[higher_comp]),
           color = "red", hjust = 0.5, vjust = 0) +
  labs(
    title = "Mixture of Two Normals (via flexmix) with Gaussian Curves",
    x = "Instantaneous Frequency",
    fill = "Component"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # remove all grid lines
   axis.line = element_line(color = "black", linewidth = 0.8)
  )




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




# Plot
p <- ggplot(plot_df, aes(x = EventIndex, y = InstantFrequency)) +
  geom_rect(data = persistence_ranges, inherit.aes = FALSE,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            fill = "gray", alpha = 0.3) +
  geom_line() +
  geom_point(aes(color = ColorGroup)) +
  scale_color_manual(values = c("C" = "blue", "Other" = "red")) +
  geom_hline(yintercept = threshold, linetype = "dashed", color = "red", size = 1) +
  geom_hline(yintercept = lower, linetype = "dashed", color = "blue", size = 1) +
  labs(
    title = "Instantaneous Frequency for Poking (RPR)",
    x = "Event Index",
    y = "Frequency (Hz)"
  ) +
  ylim(0, 2.1) +
  theme_minimal()+
  theme(
    panel.grid = element_blank(),        # remove all grid lines
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    axis.text.x  = element_text(size = 25),
    axis.text.y  = element_text(size = 25),
    plot.title   = element_text(size = 25),   # picture title
    legend.title = element_text(size = 25),   # legend title
    legend.text  = element_text(size = 25)    # legend labels
  )

ggsave("filtered_bands_strict.png", plot = p, width = 20, height = 5, units = "in", dpi = 300, limitsize = FALSE, bg = "white")
print(p)






































############################### put all data together

instant_freq_full <- c(0, instant_freq)

# Combine back into df2
df2$InstantFrequency <- instant_freq_full



# Make sure event number is accessible
df2$event_num <- as.numeric(rownames(df2))  # from row names

# Start with N for all
df2$persistence <- "N"

# Loop over each range and mark Y
for (i in 1:nrow(persistence_ranges)) {
  start_evt <- persistence_ranges$start[i]
  end_evt   <- persistence_ranges$end[i]
  
  df2$persistence[df2$event_num >= start_evt & df2$event_num <= end_evt] <- "Y"
}

df2[50:150,]

df2$TimePoint<-df2$TimePoint/1000
df2


# Calculate duration for each persistence range
persistence_ranges$duration_ms <- mapply(function(s, e) {
  start_time <- df2$TimePoint[df2$event_num == s]
  end_time   <- df2$TimePoint[df2$event_num == e]
  end_time - start_time
}, persistence_ranges$start, persistence_ranges$end)

colnames(persistence_ranges)[3]<-"duration_s"


# Find the longest period
longest_idx <- which.max(persistence_ranges$duration_s)
longest_period <- persistence_ranges[longest_idx, ]
colnames(longest_period)[3]<-"duration_s"

# Show results

persistence_ranges
longest_period
longest_time=longest_period$duration_s






# --- Parameters
longest_time <- max(persistence_ranges$duration_s, na.rm = TRUE)
pre_time <- longest_time / 5
total_x_length <- longest_time * (1 + 1/5)

# --- Prepare and align data
# Make sure event_num and start/end are numeric
df2 <- df2 %>% mutate(event_num = as.numeric(event_num))
persistence_ranges <- persistence_ranges %>% 
  mutate(
    start = as.numeric(start),
    end = as.numeric(end),
    period_id = row_number()
  )

# Join persistence_ranges with df2 to get start and end TimePoints
start_times <- df2 %>% select(event_num, start_time = TimePoint)
end_times <- df2 %>% select(event_num, end_time = TimePoint)

p_ranges <- persistence_ranges %>%
  left_join(start_times, by = c("start" = "event_num")) %>%
  left_join(end_times, by = c("end" = "event_num"))

# Optional: check mappings
print(p_ranges %>% select(period_id, start, start_time, end, end_time))

# --- Build aligned event points per period
aligned_list <- lapply(seq_len(nrow(p_ranges)), function(i) {
  pr <- p_ranges[i, ]
  if (is.na(pr$start_time)) {
    return(NULL)  # skip periods with missing start time
  }
  
  s_time <- pr$start_time
  window_start <- s_time - pre_time
  window_end <- s_time + longest_time
  
  df2 %>%
    filter(TimePoint >= window_start, TimePoint <= window_end) %>%
    filter(!is.na(Value) & Value != "") %>%
    mutate(
      period_id = pr$period_id,
      x_aligned = TimePoint - s_time + pre_time
    )
})

aligned_points <- bind_rows(aligned_list)

aligned_points <- aligned_points %>%
  left_join(fullraw %>% select(TimePoint, activate),
            by = c("event_num" = "TimePoint"))

aligned_points <- aligned_points %>%
  mutate(color_group = ifelse(activate %in% c("C", "C_WP"), "C", "Other"))

# --- Prepare persistence period segments for visualization  ##### mark persistence period only once
p_segments <- p_ranges %>%
  filter(!is.na(start_time) & !is.na(end_time)) %>%
  mutate(
    x_start = pre_time,
    x_end = pre_time + (end_time - start_time)
  )





###### mark all p-period including overlapping
# --- Prepare persistence period segments for contiguous Y blocks
p_segments <- aligned_points %>%
  arrange(period_id, x_aligned) %>%
  group_by(period_id) %>%
  mutate(run = cumsum(lag(persistence, default = first(persistence)) != persistence)) %>%
  filter(persistence == "Y") %>%                 # keep only Y blocks
  group_by(period_id, run) %>%
  summarise(
    x_start = first(x_aligned),                  # start of this Y-run
    x_end   = last(x_aligned),                   # end of this Y-run
    .groups = "drop"
  )




# --- Plot
dotplot<-ggplot() +
  # Horizontal grey bar shows the persistence period duration per period
  geom_segment(data = p_segments, aes(x = x_start, xend = x_end, y = period_id, yend = period_id),
               color = "grey80", size = 2) +
  # Dots for all events around each start (within pre/post window)
  geom_point(data = aligned_points, aes(x = x_aligned, y = period_id, color = color_group), size = 2) +
  # Vertical dashed line marks the aligned start time (pre_time)
  geom_vline(xintercept = pre_time, linetype = "dashed", color = "red") +
  # Reverse Y axis so Period 1 is at top
  scale_y_reverse(breaks = p_ranges$period_id,
                  labels = paste0("Period ", p_ranges$period_id)) +
  # Fix X limits to full window
  coord_cartesian(xlim = c(0, total_x_length)) +
  scale_color_manual(values = c("C" = "blue", "Other" = "red")) +
  labs(
    x = sprintf("Aligned Time (s) "),
    y = NULL,
    title = "Persistence period (2x2)"
  ) +
  theme_minimal()+
  theme(
    axis.text.y = element_blank(),   # <-- remove "Period 1, 2, ..."
    axis.ticks.y = element_blank(),   # optional, but usually desired
    panel.grid = element_blank(),        # remove all grid lines
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    axis.text.x  = element_text(size = 25),
    plot.title   = element_text(size = 25),   # picture title
    legend.title = element_text(size = 25),   # legend title
    legend.text  = element_text(size = 25)    # legend labels
  )
print(dotplot)

ggsave("dotplot.png", plot = dotplot, width = 10, height = 10, units = "in", dpi = 300, limitsize = FALSE, bg = "white")






# empty plot

total_x_length <- 600
pre_time <- total_x_length /5       # adjust if needed
num_rows <- 60

# Data frame just for Y-axis rows
p_ranges <- data.frame(period_id = 1:num_rows)

# Dotplot (empty, formatted the same)
dotplot <- ggplot() +
  # Vertical dashed line at pre_time
  geom_vline(xintercept = pre_time, linetype = "dashed", color = "red") +
  # Reverse Y axis to have Period 1 on top
  scale_y_reverse(breaks = p_ranges$period_id,
                  labels = paste0("Period ", p_ranges$period_id)) +
  # Fix X limits
  coord_cartesian(xlim = c(0, total_x_length)) +
  labs(
    x = sprintf("Aligned Time (s) "),
    y = NULL,
    title = "Persistence period (FR)"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    axis.text.x  = element_text(size = 25),
    plot.title   = element_text(size = 25),
    legend.title = element_text(size = 25),
    legend.text  = element_text(size = 25)
  )

print(dotplot)


# Parameters

longest_time <- max(persistence_ranges$duration_s, na.rm = TRUE)
total_x_length <- longest_time * (1 + 1/5)

# Prepare df2
df2 <- df2 %>% arrange(TimePoint)

# Add is_N and block_id as before
df2 <- df2 %>%
  mutate(
    is_N = persistence == "N",
    block_id = cumsum(is_N != lag(is_N, default = first(is_N)))
  )

# Initialize list for non-persistence subperiods
non_per_subperiods <- list()
period_counter <- 1

# Process each block of consecutive N events
blocks <- split(df2 %>% filter(is_N), df2$block_id[df2$is_N])

for (block_id in names(blocks)) {
  block_df <- blocks[[block_id]]
  
  start_idx <- 1
  n_rows <- nrow(block_df)
  while (start_idx <= n_rows && period_counter <= nrow(persistence_ranges)) {
    start_time <- block_df$TimePoint[start_idx]
    
    # Select events from start_idx until event where time difference >= total_x_length
    end_idx <- start_idx
    while (end_idx < n_rows && (block_df$TimePoint[end_idx + 1] - start_time) < total_x_length) {
      end_idx <- end_idx + 1
    }
    
    # Extract the subperiod events
    subperiod_events <- block_df[start_idx:end_idx, ] %>%
      mutate(
        period_id = period_counter,
        relative_time = TimePoint - start_time
      )
    
    non_per_subperiods[[length(non_per_subperiods) + 1]] <- subperiod_events
    
    period_counter <- period_counter + 1
    start_idx <- end_idx + 1  # next subperiod starts at next event
    
    if (period_counter > nrow(persistence_ranges)) {
      break
    }
  }
}

# Combine all subperiods
non_points <- bind_rows(non_per_subperiods)

non_points <- non_points %>%
  left_join(fullraw %>% dplyr::select(TimePoint, activate),
            by = c("event_num" = "TimePoint"))

non_points <- non_points %>%
  mutate(color_group = ifelse(activate %in% c("C", "C_WP"), "C", "Other"))

# Plot
dotplot_np<-ggplot(non_points, aes(x = relative_time, y = period_id, color = color_group)) +
  geom_point(size = 2) +
  scale_y_reverse(
    breaks = unique(non_points$period_id),
    labels = paste0("Non-persistence ", unique(non_points$period_id))
  ) +
  coord_cartesian(xlim = c(0, total_x_length)) +
  scale_color_manual(values = c("C" = "blue", "Other" = "red")) +
  labs(
    x = "Relative Time (s) within Non-persistence Period",
    y = NULL,
    title = "Non-persistence periods (FR)"
  ) +
  theme_minimal()+
  theme(
    axis.text.y = element_blank(),   # <-- remove "Period 1, 2, ..."
    axis.ticks.y = element_blank(),   # optional, but usually desired
    panel.grid = element_blank(),        # remove all grid lines
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    axis.text.x  = element_text(size = 25),
    plot.title   = element_text(size = 25),   # picture title
    legend.title = element_text(size = 25),   # legend title
    legend.text  = element_text(size = 25)    # legend labels
  )


print(dotplot_np)

ggsave("dotplot_np.png", plot = dotplot_np, width = 10, height = 10, units = "in", dpi = 300, limitsize = FALSE, bg = "white")

df2 
