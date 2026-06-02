library(dplyr)
library(tidyr)
library(fmsb)
library(ggplot2)

# Read data
bird_raw <- read.csv("C:/Users/sunlab/OneDrive - University of Wyoming/Reversal paper/Reversal paper/all_events_long_format.csv", 
                     header = TRUE,
                     sep = ",",
                     stringsAsFactors = FALSE,
                     na.strings = c("", "NA"))

head(bird_raw)

# Step 1: Remove recache events (EventType == 1)
bird_without_recache <- bird_raw %>% 
  filter(EventType != 1)

cat("Original rows:", nrow(bird_raw), "\n")
cat("After removing recaches:", nrow(bird_without_recache), "\n")

## PERCS

# Trial-level summaries
trial_summary <- bird_without_recache %>%
  group_by(Bird, Day, Trial) %>%
  summarise(
    total_events = n(),
    retrieval_events = sum(EventType == 2),
    check_events = sum(EventType == 3),
    .groups = 'drop'
  )

# Calculate S per trial
calculate_S_per_trial <- function(trial_data, n_sites = 64) {
  # trial_data is data for a single trial (one Bird, Day, Trial combination)
  site_sequence <- trial_data %>%
    arrange(Time) %>%
    pull(Site)
  
  if(length(site_sequence) < 2) return(NA_real_)
  
  T_count <- matrix(0, nrow = n_sites, ncol = n_sites)
  
  for(i in 1:(length(site_sequence) - 1)) {
    from <- site_sequence[i]
    to <- site_sequence[i + 1]
    if(from >= 1 && from <= n_sites && to >= 1 && to <= n_sites) {
      T_count[from, to] <- T_count[from, to] + 1
    }
  }
  
  T_mat <- T_count / rowSums(T_count)
  T_mat[is.na(T_mat)] <- 0
  
  start_sites <- site_sequence[-length(site_sequence)]
  p_i <- table(factor(start_sites, levels = 1:n_sites)) / length(start_sites)
  
  row_entropy <- apply(T_mat, 1, function(row) {
    row <- row[row > 0]
    if(length(row) == 0) return(0)
    -sum(row * log2(row))
  })
  
  H <- sum(p_i * row_entropy)
  S_val <- 1 - H / log2(n_sites)
  
  return(S_val)
}

# Calculate S for each trial
trial_S <- bird_without_recache %>%
  group_by(Bird, Day, Trial) %>%
  summarise(
    S_trial = calculate_S_per_trial(cur_data(), n_sites = 64),
    .groups = 'drop'
  )

# Merge S back to trial_summary
trial_summary <- trial_summary %>%
  left_join(trial_S, by = c("Bird", "Day", "Trial"))

# Now calculate bird-level metrics
bird_metrics <- trial_summary %>%
  group_by(Bird) %>%
  summarise(
    # P: Average number of events (type 2+3) per trial
    P = mean(total_events),
    
    # E: Proportion of event type 2 across all trials
    E = sum(retrieval_events) / sum(total_events),
    
    # S: Average S across trials
    S = mean(S_trial, na.rm = TRUE),
    
    # C: 1 / Coefficient of variation of total events per trial
    C = {
      cv <- sd(total_events) / mean(total_events)
      if(is.na(cv) | cv == 0) NA else 1/cv
    },
    .groups = 'drop'
  )

# Calculate R separately (needs proper numerical trial ordering)
bird_R <- trial_summary %>%
  group_by(Bird) %>%
  arrange(Day, Trial, .by_group = TRUE) %>%
  summarise(
    events_list = list(total_events),
    n_trials = n(),
    .groups = 'drop'
  ) %>%
  rowwise() %>%
  mutate(
    R = {
      events <- events_list
      n <- length(events)
      
      if(n <= 1) {
        NA_real_
      } else if(n %% 2 == 0) {
        mid <- n/2
        sum(events[(mid+1):n]) / sum(events[1:mid])
      } else {
        mid <- ceiling(n/2)
        sum(events[(mid+1):n]) / sum(events[1:(mid-1)])
      }
    }
  ) %>%
  ungroup() %>%
  select(Bird, R)

# Merge R back to bird_metrics
bird_metrics <- bird_metrics %>%
  left_join(bird_R, by = "Bird")

# Select final 7x6 dataframe (Bird + 5 metrics)
final_results <- bird_metrics %>%
  select(Bird, P, E, R, C, S)

# View final results
print(final_results)
cat("\nDimensions:", nrow(final_results), "x", ncol(final_results), "\n")

# -----------------------------------------------------
# NORMALIZE AND RADAR PLOT
# -----------------------------------------------------
df_PERCS <- final_results

df_norm <- df_PERCS

# Normalize each metric to [0,1]
df_norm$P <- (df_norm$P - min(df_norm$P, na.rm = TRUE)) / (max(df_norm$P, na.rm = TRUE) - min(df_norm$P, na.rm = TRUE))
df_norm$E <- (df_norm$E - min(df_norm$E, na.rm = TRUE)) / (max(df_norm$E, na.rm = TRUE) - min(df_norm$E, na.rm = TRUE))
df_norm$R <- (df_norm$R - min(df_norm$R, na.rm = TRUE)) / (max(df_norm$R, na.rm = TRUE) - min(df_norm$R, na.rm = TRUE))
df_norm$C <- (df_norm$C - min(df_norm$C, na.rm = TRUE)) / (max(df_norm$C, na.rm = TRUE) - min(df_norm$C, na.rm = TRUE))
df_norm$S <- (df_norm$S - min(df_norm$S, na.rm = TRUE)) / (max(df_norm$S, na.rm = TRUE) - min(df_norm$S, na.rm = TRUE))

df <- df_norm

# Radar plot limits
global_max <- max(df[, c("P","E","R","C","S")], na.rm = TRUE)
global_min <- min(df[, c("P","E","R","C","S")], na.rm = TRUE)

radar_limits <- data.frame(
  P = c(global_max, global_min),
  E = c(global_max, global_min),
  R = c(global_max, global_min),
  C = c(global_max, global_min),
  S = c(global_max, global_min)
)

# Prepare data
bird_data <- df[, c("P","E","R","C","S")]
colnames(bird_data) <- c("Perseverance", "Endurance", "Resistance", 
                         "Consistency", "Stability")
colnames(radar_limits) <- colnames(bird_data)

radar_df <- rbind(radar_limits, bird_data)
rownames(radar_df) <- c("max", "min", df$Bird)

# Axis labels
axis_labels <- round(seq(global_min, global_max, length.out = 6), 2)

# Draw radar chart
bird_colors <- rainbow(nrow(df))

radarchart(
  radar_df,
  axistype = 1,
  seg = 5,
  caxislabels = axis_labels,
  pcol = bird_colors,
  plwd = 2,
  plty = 1,
  title = "PERCS Profiles - Individual Birds",
  cglcol = "grey",
  cglty = 1,
  cglwd = 0.8,
  axislabcol = "grey30"
)

legend(
  x = "topright",
  legend = df$Bird,
  bty = "n",
  pch = 20,
  col = bird_colors,
  text.col = "black",
  cex = 0.8,
  pt.cex = 1.5
)




####### basix statisc

# Step 1: Calculate instant frequency for each event in each trial
bird_IF <- bird_without_recache %>%
  group_by(Bird, Day, Trial) %>%
  arrange(Time, .by_group = TRUE) %>%
  mutate(
    # Calculate time difference to previous event
    time_diff = Time - lag(Time),
    # Instant frequency = 1 / time difference
    IF = 1 / time_diff,
    # First event of each trial is NA (already by default from lag)
  ) %>%
  ungroup()

# Step 2: Calculate average IF for each bird by event type
bird_IF_summary <- bird_IF %>%
  group_by(Bird, EventType) %>%
  summarise(
    avg_IF = mean(IF, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    EventType = factor(EventType, 
                       levels = c(2, 3),
                       labels = c("Rewarded Check - Retrieval(Type 2)", "Unrewarded Check (Type 3)"))
  )

# View the summary data
print(bird_IF_summary)

# Step 3: Create box plot
ggplot(bird_IF_summary, aes(x = EventType, y = avg_IF)) +
  geom_boxplot(fill = c("#E69F00", "#56B4E9"), alpha = 0.7) +
  geom_jitter(width = 0.1, size = 3, alpha = 0.8) +
  labs(
    title = "Average Instant Frequency by Event Type",
    subtitle = "Each point represents one bird (n = 7)",
    x = "Event Type",
    y = "Average Instant Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11)
  )

# Optional: Print individual bird values for verification
cat("\nIndividual bird average IF values:\n")
bird_IF_summary %>%
  pivot_wider(
    names_from = EventType,
    values_from = avg_IF
  ) %>%
  print()

library(ggpubr)  


# Step 3: Create box plot with paired t-test
ggplot(bird_IF_summary, aes(x = EventType, y = avg_IF)) +
  geom_boxplot(aes(fill = EventType), alpha = 0.7, width = 0.5) +
  geom_jitter(width = 0.1, size = 3, alpha = 0.8) +
  geom_line(aes(group = Bird), color = "grey50", alpha = 0.5, linewidth = 0.8) +  # add paired lines
  scale_fill_manual(values = c("#E69F00", "#56B4E9")) +
  stat_compare_means(
    paired = TRUE,
    method = "t.test",
    label = "p.format",           # shows exact p-value
    label.x = 1.5,                # position the label between the two boxes
    label.y = max(bird_IF_summary$avg_IF) * 1.05,  # slightly above the highest point
    size = 5
  ) +
  labs(
    title = "Average Instant Frequency by Event Type",
    subtitle = "Paired t-test: Each point represents one bird (n = 7)",
    x = "Event Type",
    y = "Average Instant Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    legend.position = "none"      # remove legend since x-axis already labels it
  )

# Also print the paired t-test results for reporting
cat("\n=== Paired t-test results ===\n")
bird_IF_wide <- bird_IF_summary %>%
  pivot_wider(
    names_from = EventType,
    values_from = avg_IF
  )

t_test_result <- t.test(
  bird_IF_wide$`Rewarded Check - Retrieval(Type 2)`, 
  bird_IF_wide$`Unrewarded Check (Type 3)`, 
  paired = TRUE
)
print(t_test_result)

cat("\n=== Individual bird values ===\n")
print(bird_IF_wide)




library(dplyr)
library(nlme)
library(ggplot2)

# =========================
# STEP 1: streak (correct, within trial)
# =========================
bird_IF2 <- bird_IF %>%
  arrange(Bird, Day, Trial, Time) %>%
  group_by(Bird, Day, Trial) %>%
  mutate(
    consec = {
      s <- 0
      out <- numeric(n())
      
      for (i in seq_along(EventType)) {
        
        if (EventType[i] == 2) {
          s <- 0
          out[i] <- 0
          
        } else if (EventType[i] == 3) {
          s <- s + 1
          out[i] <- s
          
        } else {
          out[i] <- NA
        }
      }
      out
    }
  ) %>%
  ungroup()

# =========================
# STEP 2: aggregate (KEEP 0)
# =========================
bird_summary <- bird_IF2 %>%
  filter(!is.na(consec), !is.na(IF)) %>%
  group_by(Bird, consec) %>%
  summarise(
    meanIF = mean(IF),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(consec <= 5)

# =========================
# STEP 3: nlme model (stable version)
# =========================
model_bird <- lme(
  meanIF ~ consec + I(consec^2),
  random = ~1 | Bird,
  data = bird_summary
)

# =========================
# STEP 4: MODEL OUTPUT (WITH P-VALUES)
# =========================
cat("\n================ FULL MODEL SUMMARY ================\n")
print(summary(model_bird))   # includes p-values

cat("\n================ FIXED EFFECTS TABLE ================\n")
print(summary(model_bird)$tTable)  # clean p-values table

cat("\n================ RANDOM EFFECTS ================\n")
print(VarCorr(model_bird))

cat("\n================ AIC / BIC ================\n")
print(AIC(model_bird))
print(BIC(model_bird))






# =========================
# STEP 1: QUADRATIC MODEL (already done)
# =========================
model_quad <- lme(
  meanIF ~ consec + I(consec^2),
  random = ~1 | Bird,
  data = bird_summary
)

# =========================
# STEP 2: CUBIC MODEL
# =========================
model_cubic <- lme(
  meanIF ~ consec + I(consec^2) + I(consec^3),
  random = ~1 | Bird,
  data = bird_summary
)

# =========================
# STEP 3: MODEL COMPARISON
# =========================
comparison <- data.frame(
  model = c("quadratic", "cubic"),
  AIC = c(AIC(model_quad), AIC(model_cubic)),
  BIC = c(BIC(model_quad), BIC(model_cubic))
)

print(comparison)

# =========================
# STEP 4: FULL SUMMARY OUTPUTS
# =========================
cat("\n================ QUADRATIC MODEL ================\n")
print(summary(model_quad)$tTable)

cat("\n================ CUBIC MODEL ================\n")
print(summary(model_cubic)$tTable)




# =========================
# PREDICTION GRID
# =========================
newdata <- data.frame(
  consec = seq(0, 5, by = 0.1)
)

newdata$pred_quad <- predict(model_quad, newdata, level = 0)
newdata$pred_cubic <- predict(model_cubic, newdata, level = 0)

# =========================
# PLOT
# =========================
ggplot(bird_summary, aes(x = consec, y = meanIF)) +
  geom_point(aes(color = Bird), size = 2, alpha = 0.8) +
  geom_line(aes(group = Bird, color = Bird), alpha = 0.4) +
  
  # quadratic curve
  geom_line(data = newdata,
            aes(x = consec, y = pred_quad),
            color = "black",
            linewidth = 1.2) +
  
  # cubic curve
  geom_line(data = newdata,
            aes(x = consec, y = pred_cubic),
            color = "red",
            linewidth = 1.2) +
  
  labs(
    x = "Consecutive mistakes",
    y = "Mean IF",
    title = "Quadratic (black) vs Cubic (red)"
  ) +
  theme_classic() +
  theme(legend.position = "none")







# =========================
# LINEAR MODEL
# =========================
model_lin <- lme(
  meanIF ~ consec,
  random = ~1 | Bird,
  data = bird_summary
)
plot(fitted(model_lin), resid(model_lin))
abline(h = 0)
# =========================
# MODEL COMPARISON (AIC / BIC)
# =========================
comparison <- data.frame(
  model = c("linear", "quadratic", "cubic"),
  AIC = c(AIC(model_lin), AIC(model_quad), AIC(model_cubic)),
  BIC = c(BIC(model_lin), BIC(model_quad), BIC(model_cubic))
)

print(comparison)

# =========================
# MODEL ESTIMATES (fixed effects)
# =========================
cat("\n================ LINEAR MODEL =================\n")
print(summary(model_lin)$tTable)

cat("\n================ QUADRATIC MODEL ================\n")
print(summary(model_quad)$tTable)

cat("\n================ CUBIC MODEL ================\n")
print(summary(model_cubic)$tTable)

# =========================
# PREDICTION GRID
# =========================
newdata <- data.frame(
  consec = seq(0, 5, by = 0.1)
)

newdata$lin <- predict(model_lin, newdata, level = 0)
newdata$quad <- predict(model_quad, newdata, level = 0)
newdata$cubic <- predict(model_cubic, newdata, level = 0)

# =========================
# FINAL PLOT (3 CURVES)
# =========================
ggplot(bird_summary, aes(x = consec, y = meanIF)) +
  geom_point(aes(color = Bird), size = 2, alpha = 0.8) +
  geom_line(aes(group = Bird, color = Bird), alpha = 0.4) +
  
  geom_line(data = newdata, aes(x = consec, y = lin),
            color = "blue", linewidth = 1.1) +
  
  #geom_line(data = newdata, aes(x = consec, y = quad),
   #         color = "black", linewidth = 1.2) +
  
  #geom_line(data = newdata, aes(x = consec, y = cubic),
   #         color = "red", linewidth = 1.1) +
  
  labs(
    x = "Consecutive mistakes",
    y = "Average Instantaneous Frequency",
    title = "Relationship Between Bird Checking Frequency and Consecutive Unrewarded Checks "
  ) +
  theme_classic() +
  theme(legend.position = "none")