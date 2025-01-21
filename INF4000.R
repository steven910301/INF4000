getwd()
setwd("C:/Users/ASUS/桌面/learning/6027introtodatascience")
spotifydata= read.csv('dataset.csv')
death_metal_tracks= read.csv('death_metal_tracks.csv')
View(spotifydata)
View(death_metal_tracks)
death_metal_df <- read.csv("death_metal_tracks.csv")
# Fill outliers with median
Q1 <- quantile(death_metal_df$duration_ms, 0.25)
Q3 <- quantile(death_metal_df$duration_ms, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
outliers <- death_metal_df$duration_ms[death_metal_df$duration_ms < lower_bound | death_metal_df$duration_ms > upper_bound]
print(outliers)
death_metal_df$duration_ms[death_metal_df$duration_ms < lower_bound | death_metal_df$duration_ms > upper_bound] <- median(death_metal_df$duration_ms, na.rm = TRUE)
outliers_after <- death_metal_df$duration_ms[death_metal_df$duration_ms < lower_bound | death_metal_df$duration_ms > upper_bound]
print(outliers_after)

# standardize
standardize <- function(x) {
  return((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
}
death_metal_df_standardized <- as.data.frame(lapply(death_metal_df, function(col) {
  if (is.numeric(col)) {
    return(standardize(col))
  } else {
    return(col)
  }
}))
print(head(death_metal_df_standardized))


# correlation matrix heatmap
library(ggplot2)
correlation_matrix <- cor(death_metal_df[, c("duration_ms", "danceability", "loudness",
                                             "speechiness",  "instrumentalness", "tempo", "popularity", "key")])
print(correlation_matrix)
install.packages('reshape2')
library(reshape2)
correlation_melted <- melt(correlation_matrix)
ggplot(correlation_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "black") + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), 
                       name = "Correlation") +
  labs(title = "Correlation Matrix Heatmap", x = "Variables", y = "Variables") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

# PCA
df_scaled <- scale(death_metal_df[, c("duration_ms", "danceability", "loudness", "speechiness",
                                      "instrumentalness", "tempo", "popularity", "key")])
pca_result <- prcomp(df_scaled, center = TRUE, scale. = TRUE)
summary(pca_result)
pca_data <- as.data.frame(pca_result$x)
ggplot(pca_data, aes(x = PC1, y = PC2)) +
  geom_point() +
  labs(title = "PCA Result", x = "PC 1", y = "PC 2") +
  theme_minimal()
install.packages("car", type = "binary")
install.packages("leaps", type = "binary")
install.packages("FactoMineR")
install.packages("factoextra")
install.packages("plotly")
library(FactoMineR)
library(factoextra)
library(plotly)
str(death_metal_tracks)
death_metal_tracks$duration_ms <- as.numeric(as.character(death_metal_tracks$duration_ms))
death_metal_tracks$popularity <- as.numeric(as.character(death_metal_tracks$popularity))
numeric_data <- death_metal_tracks[, c("danceability", "tempo", "duration_ms", 
                                       "loudness", "speechiness", "instrumentalness", 
                                       "popularity", "key")]
numeric_data_scaled <- scale(numeric_data)

pca_result <- PCA(numeric_data_scaled, graph = FALSE)
summary(pca_result)
# PCA scree plot
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 100)) +
  labs(title = "Scree Plot of Explained Variance", x = "Principal Components", y = "Percentage of Explained Variance") +
  theme_minimal()

#PCA biplot
fviz_pca_biplot(pca_result, 
                geom.ind = FALSE,   
                col.var = "contrib", 
                gradient.cols = c("steelblue", "orange", "darkgreen"),
                repel = TRUE,         
                label = "var")      



library(GGally)
library(patchwork)
# violin plot
p1 <- ggplot(death_metal_tracks, aes(x = track_genre, y = popularity, fill = track_genre)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, outlier.shape = NA, color = "black") +
  labs(title = "popularity of Death Metal Tracks", x = "Genre", y = "popularity") +
  theme_minimal()
p2 <- ggplot(death_metal_tracks, aes(x = track_genre, y = duration_ms, fill = track_genre)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, outlier.shape = NA, color = "black") +
  labs(title = "duration_ms of Death Metal Tracks", x = "Genre", y = "duration_ms") +
  theme_minimal()
p3 <- ggplot(death_metal_tracks, aes(x = track_genre, y = danceability, fill = track_genre)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, outlier.shape = NA, color = "black") +
  labs(title = "Danceability of Death Metal Tracks", x = "Genre", y = "danceability") +
  theme_minimal()
p4 <- ggplot(death_metal_tracks, aes(x = track_genre, y = key, fill = track_genre)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, outlier.shape = NA, color = "black") +
  labs(title = "key of Death Metal Tracks", x = "Genre", y = "key") +
  theme_minimal()
p5 <- ggplot(death_metal_tracks, aes(x = track_genre, y = loudness, fill = track_genre)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, outlier.shape = NA, color = "black") +
  labs(title = "volume of Death Metal Tracks", x = "Genre", y = "volume") +
  theme_minimal()
p6 <- ggplot(death_metal_tracks, aes(x = track_genre, y = speechiness, fill = track_genre)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, outlier.shape = NA, color = "black") +
  labs(title = "speechiness of Death Metal Tracks", x = "Genre", y = "speechiness") +
  theme_minimal()
p7 <- ggplot(death_metal_tracks, aes(x = track_genre, y = instrumentalness, fill = track_genre)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, outlier.shape = NA, color = "black") +
  labs(title = "instrumentalness of Death Metal Tracks", x = "Genre", y = "instrumentalness") +
  theme_minimal()
p8 <- ggplot(death_metal_tracks, aes(x = track_genre, y = tempo, fill = track_genre)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, outlier.shape = NA, color = "black") +
  labs(title = "tempo of Death Metal Tracks", x = "Genre", y = "tempo") +
  theme_minimal()
combined_plot1 <- (p1 | p2 ) / ( p3 | p4)
combined_plot2 <- (p5 | p6 ) / ( p7 | p8)
print(combined_plot1)
print(combined_plot2)




# Linear Regression Between Popularity & Duration in Scatter plot

ggplot(death_metal_tracks, aes(x = duration_ms, y = popularity)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", color = "red") + 
  labs(title = "Popularity vs Duration with Linear Regression", 
       x = "Duration (ms)", 
       y = "Popularity") +
  theme_minimal()
# Quadratic Polynomial Regression Between Popularity & Duration in Scatter plot
ggplot(death_metal_tracks, aes(x = duration_ms, y = popularity)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red") +  
  labs(title = "Popularity vs Duration with Quadratic Polynomial Regression", 
       x = "Duration (ms)", 
       y = "Popularity") +
  theme_minimal()
# LOESS Between Popularity & Duration in Scatter plot
ggplot(death_metal_tracks, aes(x = duration_ms, y = popularity)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "loess", color = "red") + 
  labs(title = "Popularity vs Duration with Local Regression (LOESS)", 
       x = "Duration (ms)", 
       y = "Popularity") +
  theme_minimal()


