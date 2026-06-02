library(ggplot2)
library(dplyr)
library(flexmix)
library(tidyr)
library(fmsb)
library(stringr)
library(vegan)
library(plotly)





df_con$condition <- "con"
df_ind$condition <- "ind"
df_ind <- df_ind %>%
  dplyr::select(-Poke)

df_conind <- rbind(df_con, df_ind)
df_norm<-df_conind

df_norm$P<-(df_norm$P-min(df_norm$P))/(max(df_norm$P)-min(df_norm$P))
df_norm$E<-(df_norm$E-min(df_norm$E))/(max(df_norm$E)-min(df_norm$E))
df_norm$R<-(df_norm$R-min(df_norm$R))/(max(df_norm$R)-min(df_norm$R))
df_norm$C<-(df_norm$C-min(df_norm$C))/(max(df_norm$C)-min(df_norm$C))
df_norm$S<-(df_norm$S-min(df_norm$S))/(max(df_norm$S)-min(df_norm$S))

df_conind<-df_norm

# -----------------------------------------------------
group_names <- c(
  "1" = "FR",
  "2" = "2x2",
  "3" = "5x5",
  "4" = "RPR"
)

par(mfrow = c(2, 2))




for (g in groups) {
  
  group_data <- df_conind[df_conind$group == g, c("P","E","R","C","S")]
  # rename dimensions here
  colnames(group_data) <- c("Persistence", "Endurance", "Resistance", "Consistency", "Stability")
  
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
}



for (g in groups) {
  
  group_data <- df_conind[df_conind$group == g, ]
  
  # keep only radar columns
  radar_data <- group_data[, c("P","E","R","C","S")]
  
  colnames(radar_data) <- c("Perseverance", "Endurance", "Resistance", "Consistency", "Stability")
  
  colnames(radar_limits) <- colnames(radar_data)
  
  radar_df <- rbind(radar_limits, radar_data)
  
  axis_labels <- round(seq(global_min, global_max, length.out = 6), 2)
  
  # 🎯 color per mouse based on condition
  point_colors <- ifelse(group_data$condition == "con", "red", "blue")
  
  radarchart(
    radar_df,
    axistype = 1,
    seg = 5,
    caxislabels = axis_labels,
    
    # IMPORTANT: color per line (mouse)
    pcol = point_colors,
    plwd = 2,
    plty = 1,
    
    title = paste("Group", group_names[as.character(g)]),
    
    cglcol = "grey",
    cglty = 1,
    cglwd = 0.8,
    axislabcol = "grey30"
  )
}

par(xpd = NA)  # allow drawing outside panels

legend("bottom",
       inset = c(0, -0.15),
       legend = c("Continuous", "Independent"),
       col = c("red", "blue"),
       lty = 1,
       lwd = 2,
       horiz = TRUE,
       bty = "n")







df_PERCS_allmice<-df_conind

df_dispersion=df_PERCS_allmice[21:58,]


#### variance test
dist_matrix <- dist(df_dispersion[, c("P","E","R","C","S")], method = "euclidean")

# -------------------------------
# 3️⃣ Compute distance-based multivariate dispersion
# -------------------------------
bd <- betadisper(dist_matrix, df_dispersion$group)

# Inspect distances to centroid (optional)
bd$distances

# -------------------------------
# 4️⃣ Permutation test for group differences in dispersion
# -------------------------------
set.seed(123)  # reproducible
perm_test <- permutest(bd, permutations = 999)

# View results
perm_test

par(mfrow = c(1, 1))

# 1️⃣ Create a dataframe with distances and group
df_bd <- data.frame(
  mouse = df_dispersion$mouse,
  group = factor(df_dispersion$group, 
                 levels = 1:4, 
                 labels = c("FR", "2x2", "5x5", "RPR")),  # rename groups
  distance_to_centroid = bd$distances
)

# 2️⃣ Boxplot with ggplot2
ggplot(df_bd, aes(x = group, y = distance_to_centroid, fill = group)) +
  geom_boxplot() +
  geom_jitter(width = 0.15, size = 2, alpha = 0.7) +  # show individual points
  labs(
    title = "Multivariate Dispersion of PERCS Features Across Paradigms (distance to centroid)",
    x = "Paradigm",
    y = "Distance to centroid",
    fill = "Paradigm"  # rename legend
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")



# Define the comparisons you want: all vs RPR
comparisons <- list(
  c(1, 4),  # FR vs RPR
  c(2, 4),  # 2x2 vs RPR
  c(3, 4)   # 5x5 vs RPR
)

# Prepare a results list
posthoc_results <- list()

# Loop over each comparison
for (comp in comparisons) {
  
  # Subset data for the two groups
  df_sub <- df_dispersion %>% filter(group %in% comp)
  
  # Compute Euclidean distance for the subset
  dist_sub <- dist(df_sub[, c("P","E","R","C","S")])
  
  # Map numeric to readable labels
  labels_map <- c("FR","2x2","5x5","RPR")
  group_labels <- labels_map[comp]
  
  # Create betadisper object
  bd_sub <- betadisper(dist_sub, factor(df_sub$group, labels = group_labels))
  
  # Run permutation test
  set.seed(123)
  perm_res <- permutest(bd_sub, permutations = 999)
  
  # Store results with a descriptive name
  comp_name <- paste0(group_labels[1], " vs ", group_labels[2])
  posthoc_results[[comp_name]] <- perm_res
}

# -----------------------
# View results
# -----------------------
posthoc_results


# raw p-values (manual vector)
p_raw <- c(
  0.003,  # FR vs RPR
  0.026,  # 2x2 vs RPR
  0.001   # 5x5 vs RPR
)

# BH correction
p_adj <- p.adjust(p_raw, method = "BH")

# check result
p_adj



#### k-means cluster

# -------------------------------
features <- df_PERCS_allmice[, c("P","E","R","C","S")]

# -------------------------------
# 2️⃣ Scale the features (recommended for k-means)
# -------------------------------
features_scaled <- scale(features)

# -------------------------------
# 3️⃣ Run k-means
#    - k = number of clusters you want
#    - nstart = number of random initializations
# -------------------------------
set.seed(123)  # for reproducibility
k <- 4  # you can change based on how many clusters you expect
kmeans_result <- kmeans(features_scaled, centers = k, nstart = 25)

# -------------------------------
# 4️⃣ Inspect results
# -------------------------------
kmeans_result$cluster      # cluster assignment for each mouse
kmeans_result$centers      # cluster centroids in scaled space
kmeans_result$tot.withinss # total within-cluster sum of squares

# -------------------------------
# 5️⃣ Optional: add cluster assignments to your dataframe
# -------------------------------
df_PERCS_allmice$kmeans_cluster <- kmeans_result$cluster

# -------------------------------
# 1️⃣ Create contingency table
# -------------------------------
heat_df <- df_PERCS_allmice %>%
  group_by(group, kmeans_cluster) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(
    group = factor(group, labels = c("FR","2x2","5x5","RPR")),
    kmeans_cluster = factor(kmeans_cluster)
  )

# -------------------------------
# 2️⃣ Convert to numeric for continuous heat
# -------------------------------
heat_df$x <- as.numeric(heat_df$kmeans_cluster)
heat_df$y <- as.numeric(heat_df$group)

# -------------------------------
# 3️⃣ Plot continuous heat map
# -------------------------------
ggplot(heat_df, aes(x = x, y = y, fill = count)) +
  geom_raster(interpolate = TRUE) +  # interpolate makes it smooth
  scale_fill_gradient(low = "white", high = "steelblue") +
  scale_x_continuous(breaks = 1:4, labels = levels(heat_df$kmeans_cluster)) +
  scale_y_continuous(breaks = 1:4, labels = levels(heat_df$group)) +
  labs(
    title = "Continuous Heatmap: Original Group vs K-means Cluster",
    x = "K-means Cluster",
    y = "Original Group",
    fill = "Number of Mice"
  ) +
  theme_minimal()

# 1️⃣ Prepare contingency table
heat_df <- df_PERCS_allmice %>%
  group_by(group, kmeans_cluster) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(
    group = factor(group, labels = c("FR","2x2","5x5","RPR")),
    kmeans_cluster = factor(kmeans_cluster)
  )

# 2️⃣ Plot heatmap with discrete steps
ggplot(heat_df, aes(x = kmeans_cluster, y = group, fill = count)) +
  geom_tile(color = "white") +   # each cell is solid color
  geom_text(aes(label = count), color = "black", size = 4) +
  scale_fill_gradient2(low = "white", mid = "lightblue", high = "steelblue", 
                       midpoint = 5, limits = c(0, max(heat_df$count))) +
  labs(
    title = "Heatmap: Original Group vs K-means Cluster",
    x = "K-means Cluster",
    y = "Original Group",
    fill = "Number of Mice"
  ) +
  theme_minimal()




# Example in R
d <- dist(features_scaled, method = "euclidean")
hc <- hclust(d, method = "ward.D2")
plot(hc, labels = df_PERCS_allmice$mouse, main = "Hierarchical Clustering Dendrogram")
rect.hclust(hc, k = 3, border = "red")  # optional: highlight 4 clusters






levels(df_PERCS_allmice$group)
df_PERCS_allmice$group = factor(df_PERCS_allmice$group, levels = 1:4)
# ---------- PCA ----------
percs <- df_PERCS_allmice[, c("P","E","R","C","S")]
pca <- prcomp(percs, center = TRUE, scale. = TRUE)

# ---------- PCA dataframe ----------
pca_df <- df_PERCS_allmice %>%
  mutate(
    PC1 = pca$x[,1],
    PC2 = pca$x[,2],
    PC3 = pca$x[,3],
    group = factor(group, levels = 1:4,  labels = c("FR", "2x2", "5x5", "RPR")),  # group determines shape
    multi_group = ifelse(mouse %in% names(table(mouse)[table(mouse)>1]), "Cross-group", "Single-group")  # optional color
  )

# ---------- 3D PCA plot ----------
plot_ly() %>%
  # Lines connecting same mouse across groups
  add_trace(
    data = pca_df,
    x = ~PC1, y = ~PC2, z = ~PC3,
    type = "scatter3d",
    mode = "lines",
    split = ~mouse,
    line = list(color = 'gray', width = 2),
    showlegend = FALSE
  ) %>%
  # Points: shape = group, color = cross-group
  add_trace(
    data = pca_df,
    x = ~PC1, y = ~PC2, z = ~PC3,
    type = "scatter3d",
    mode = "markers",
    color = ~group,  # optional: color by cross-group
    colors = c("Cross-group" = "red", "Single-group" = "blue"),
    symbols = ~group,       # shape = group
    text = ~mouse,
    marker = list(size = 5)
  ) %>%
  layout(
    scene = list(
      xaxis = list(title = paste0("PC1 (", round(summary(pca)$importance[2,1]*100,1), "%)")),
      yaxis = list(title = paste0("PC2 (", round(summary(pca)$importance[2,2]*100,1), "%)")),
      zaxis = list(title = paste0("PC3 (", round(summary(pca)$importance[2,3]*100,1), "%)"))
    )
  )


# ---------- 3D PCA plot (NO lines) ----------
plot_ly(
  data = pca_df,
  x = ~PC1, y = ~PC2, z = ~PC3,
  type = "scatter3d",
  mode = "markers",
  color = ~group,
  colors = c("Cross-group" = "red", "Single-group" = "blue"),
  symbol = ~group,
  text = ~mouse,
  marker = list(size = 5)
) %>%
  layout(
    scene = list(
      xaxis = list(title = paste0("PC1 (", round(summary(pca)$importance[2,1]*100,1), "%)")),
      yaxis = list(title = paste0("PC2 (", round(summary(pca)$importance[2,2]*100,1), "%)")),
      zaxis = list(title = paste0("PC3 (", round(summary(pca)$importance[2,3]*100,1), "%)"))
    )
  )


pca_df$group <- as.factor(pca_df$group)

plot_ly(
  data = pca_df,
  x = ~PC1, y = ~PC2, z = ~PC3,
  type = "scatter3d",
  mode = "markers",
  color = ~group,
  text = ~mouse,
  marker = list(size = 5)
) %>%
  layout(
    scene = list(
      xaxis = list(title = paste0("PC1 (", round(summary(pca)$importance[2,1]*100,1), "%)")),
      yaxis = list(title = paste0("PC2 (", round(summary(pca)$importance[2,2]*100,1), "%)")),
      zaxis = list(title = paste0("PC3 (", round(summary(pca)$importance[2,3]*100,1), "%)"))
    )
  )
