rm(list=ls())

# Library
library(quantmod)
library(dplyr)
library(ggplot2)
library(GGally)

# Define the tickers for gold and silver
tickers <- c("GC=F", "SI=F")

# Download historical data for closing prices only
data <- lapply(tickers, function(ticker) {
  getSymbols(ticker,
             from = Sys.Date() - 6*365,  # 6 years before today's date
             to = Sys.Date(),
             periodicity = "daily",
             auto.assign = FALSE)  # Do not auto-assign to environment
})

# Datasets
gold_df = data[[1]]$`GC=F.Close`
silver_df = data[[2]]$`SI=F.Close`

# Merge datasets by Date column
merged_df <- as.data.frame(cbind(gold_df, silver_df))

names(merged_df) <- c('Gold','Silver')

# Chart using ggpairs()
ggpairs(merged_df, 
        lower = list(continuous = wrap("points", color = "darkblue", alpha = 0.6)),
        upper = list(continuous = wrap("cor", corSize = 3)),
        diag = list(continuous = wrap("densityDiag", fill = "darkgray")),
        title = "Correlation Matrix")

# Scatter Plot
merged_df$Date <- as.Date(row.names(merged_df))
scale_factor = 70

# Stylish colors
gold_color <- "#FFC107"  # Amber
silver_color <- "#607D8B"  # Blue Grey
shade_color <- "#A8F1F9"

shaded_start_2020 <- as.Date("2020-03-01")
shaded_end_2020 <- as.Date("2020-05-31")

shaded_start_2023 <- as.Date("2022-12-10")
shaded_end_2023 <- as.Date("2023-02-01")

# Assuming merged_df is your data frame containing Date, Gold, and Silver columns
ggplot(merged_df, aes(x=Date)) +
  # First y-axis (Gold)
  geom_line(aes(y=Gold, color = "Gold")) + 
  # Second y-axis (Silver)
  geom_line(aes(y=Silver * scale_factor, color = "Silver")) +
  # Shaded area for specific months
  geom_rect(
    xmin = shaded_start_2020, xmax = shaded_end_2020,
    ymin = -Inf, ymax = Inf, fill = shade_color, alpha = 0.01,
    color = NA
  ) +
  geom_rect(
    xmin = shaded_start_2023, xmax = shaded_end_2023,
    ymin = -Inf, ymax = Inf, fill = shade_color, alpha = 0.01,
    color = NA
  ) +
  # Scale for the primary y-axis (Gold)
  scale_y_continuous(
    name = "Gold (USD/Oz.)",
    # Adding secondary axis
    sec.axis = sec_axis(~./scale_factor, name="Silver (USD/Oz.)")
  ) +
  # Add labels and color legend
  labs(
    x = "Date",
    color = "Precious Metals"
  ) +
  # Set stylish colors for the lines
  scale_color_manual(values = c("Gold" = gold_color, "Silver" = silver_color)) +
  # Improve theme for better readability
  theme_minimal() +
  theme(
    axis.title.y.left = element_text(color = 'black'),
    axis.title.y.right = element_text(color = "black"),
    legend.position = "bottom"
  )