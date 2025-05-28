# R Script for Empirical Assignment - RCT25 
# Muhammad Mujtaba Sarwar
# Matriculation Number: 643797

----------------------------------------------------------------------------------------------------------------------
# Step 4 : Find the Largest Firm in Orbis for Your Postal Code
  
# Step 4.1 - Load Orbis Data
orbis_panel_berlin <- readRDS("C:/Users/Mujtaba Sarwar/Downloads/rct25/data/generated/orbis_panel_berlin.rds")

# Step 4.2 - Filter Orbis Data for 2021
orbis_2021 <- subset(orbis_panel_berlin, year ==2021)

# Step 4.3 - Filter Orbis Data for 2021 + Postal Code = 10405
orbis_2021_10405 <- subset(orbis_2021, postcode == 10405)

# Step 5 : Compare Firms from Your Postal Code to the Berlin Firm Population

# Step 5.1 - Load Necessary Packages
library(dplyr)
library(xtable)
# Step 5.2 - Renaming for clarity
berlin_firms_2021 <- orbis_2021
berlin_10405_firms_2021 <- orbis_2021_10405

# Step 5.3 Filter out both subsets for missing or zero total assets / equity
berlin_firms_2021_clean <- subset(berlin_firms_2021,
                                  !is.na(toas) & toas > 0 &
                                    !is.na(shfd))

berlin_10405_firms_2021_clean <- subset(berlin_10405_firms_2021,
                                        !is.na(toas) & toas > 0 &
                                          !is.na(shfd))

# Step 5.4 - Calculate equity ratio for each firm (Total equity / total assets)
berlin_firms_2021_clean$equity_ratio <- berlin_firms_2021$shfd / berlin_firms_2021$toas
berlin_10405_firms_2021_clean$equity_ratio <- berlin_10405_firms_2021$shfd / berlin_10405_firms_2021$toas

# Step 5.5 - Add distinct group labels and then combine both datasets
berlin_firms_2021_clean$group <- "Berlin"
berlin_10405_firms_2021_clean$group <- "10405"
combined_data <- rbind(berlin_firms_2021_clean, berlin_10405_firms_2021_clean)

# Step 5.6 - Compute Summary Statistics
summary_stats <- combined_data %>%
  group_by(group) %>%
  summarise(
    # 5.6.1 --- Total Assets ---
    N = n(),
    Mean_Total_Assets = mean(toas, na.rm = TRUE),
    SD_Total_Assets = sd(toas, na.rm = TRUE),
    Median_Total_Assets = median(toas, na.rm = TRUE),
    IQR_Total_Assets = IQR(toas, na.rm = TRUE),
    Min_Total_Assets = min(toas, na.rm = TRUE),
    Max_Total_Assets = max(toas, na.rm = TRUE),
    Mean_Log_Total_Assets = mean(log(toas), na.rm = TRUE),
    SD_Log_Total_Assets = sd(log(toas), na.rm = TRUE),
    
    # 5.6.2 --- Equity Ratio ---
    Mean_Equity_Ratio = mean(equity_ratio, na.rm = TRUE),
    SD_Equity_Ratio = sd(equity_ratio, na.rm = TRUE),
    Median_Equity_Ratio = median(equity_ratio, na.rm = TRUE),
    IQR_Equity_Ratio = IQR(equity_ratio, na.rm = TRUE),
    Min_Equity_Ratio = min(equity_ratio, na.rm = TRUE),
    Max_Equity_Ratio = max(equity_ratio, na.rm = TRUE)
  )
print(summary_stats)

# Step 5.7 - Perform Statistical Testing
cat("=== Hypothesis Testing ===\n\n")

# 5.7.1 - T-test for Total Assets
t_test_assets <- t.test(toas ~ group, data = combined_data)
cat("T-test: Total Assets (10405 vs Berlin)\n")
print(t_test_assets)

# 5.7.2 - T-test for Equity Ratio
t_test_equity <- t.test(equity_ratio ~ group, data = combined_data)
cat("\nT-test: Equity Ratio (10405 vs Berlin)\n")
print(t_test_equity)

# Step 5.8 – Prepare a comprehensive Tables

# 5.8.1 - Create Summary Statistics Table
summary_table_display <- data.frame(
  Statistic = c(
    "N",
    "Mean Total Assets (EUR)", "SD Total Assets", "Median Total Assets", "IQR Total Assets", 
    "Min Total Assets", "Max Total Assets", 
    "Mean Log(Total Assets)", "SD Log(Total Assets)",
    "Mean Equity Ratio", "SD Equity Ratio", "Median Equity Ratio", 
    "IQR Equity Ratio", "Min Equity Ratio", "Max Equity Ratio"
  ),
  Berlin = c(
    summary_stats$N[summary_stats$group == "Berlin"],
    summary_stats$Mean_Total_Assets[summary_stats$group == "Berlin"],
    summary_stats$SD_Total_Assets[summary_stats$group == "Berlin"],
    summary_stats$Median_Total_Assets[summary_stats$group == "Berlin"],
    summary_stats$IQR_Total_Assets[summary_stats$group == "Berlin"],
    summary_stats$Min_Total_Assets[summary_stats$group == "Berlin"],
    summary_stats$Max_Total_Assets[summary_stats$group == "Berlin"],
    summary_stats$Mean_Log_Total_Assets[summary_stats$group == "Berlin"],
    summary_stats$SD_Log_Total_Assets[summary_stats$group == "Berlin"],
    summary_stats$Mean_Equity_Ratio[summary_stats$group == "Berlin"],
    summary_stats$SD_Equity_Ratio[summary_stats$group == "Berlin"],
    summary_stats$Median_Equity_Ratio[summary_stats$group == "Berlin"],
    summary_stats$IQR_Equity_Ratio[summary_stats$group == "Berlin"],
    summary_stats$Min_Equity_Ratio[summary_stats$group == "Berlin"],
    summary_stats$Max_Equity_Ratio[summary_stats$group == "Berlin"]
  ),
  `10405` = c(
    summary_stats$N[summary_stats$group == "10405"],
    summary_stats$Mean_Total_Assets[summary_stats$group == "10405"],
    summary_stats$SD_Total_Assets[summary_stats$group == "10405"],
    summary_stats$Median_Total_Assets[summary_stats$group == "10405"],
    summary_stats$IQR_Total_Assets[summary_stats$group == "10405"],
    summary_stats$Min_Total_Assets[summary_stats$group == "10405"],
    summary_stats$Max_Total_Assets[summary_stats$group == "10405"],
    summary_stats$Mean_Log_Total_Assets[summary_stats$group == "10405"],
    summary_stats$SD_Log_Total_Assets[summary_stats$group == "10405"],
    summary_stats$Mean_Equity_Ratio[summary_stats$group == "10405"],
    summary_stats$SD_Equity_Ratio[summary_stats$group == "10405"],
    summary_stats$Median_Equity_Ratio[summary_stats$group == "10405"],
    summary_stats$IQR_Equity_Ratio[summary_stats$group == "10405"],
    summary_stats$Min_Equity_Ratio[summary_stats$group == "10405"],
    summary_stats$Max_Equity_Ratio[summary_stats$group == "10405"]
  ),
  check.names = FALSE
)

# 5.8.2 – Add t-test p-values
summary_table_display$`T-test p-value` <- NA
summary_table_display$`T-test p-value`[summary_table_display$Statistic == "Mean Total Assets (EUR)"] <- t_test_assets$p.value
summary_table_display$`T-test p-value`[summary_table_display$Statistic == "Mean Equity Ratio"] <- t_test_equity$p.value

# 5.8.3 – Perform Table Formatting
format_column <- function(col_vector) {
  sapply(col_vector, function(x) {
    x <- as.numeric(x)
    if (is.na(x)) {
      return("")
    } else if (abs(x) >= 1000) {
      return(format(round(x, 0), big.mark = ",", nsmall = 0))
    } else {
      return(format(round(x, 2), nsmall = 2))
    }
  })
}

summary_table_display$Berlin <- format_column(summary_table_display$Berlin)
summary_table_display$`10405` <- format_column(summary_table_display$`10405`)
summary_table_display$`T-test p-value` <- sapply(summary_table_display$`T-test p-value`, function(x) {
  x <- as.numeric(x)
  if (is.na(x)) {
    return("")
  } else {
    return(format(round(x, 4), nsmall = 4))
  }
})

summary_table_display$Berlin[7] <- format(round(as.numeric(summary_table_display$Berlin[7]), 0), big.mark = ",", scientific = FALSE)


# 5.8.4 - Exporting the Summary Statistics Table to Latex
latex_table <- xtable(summary_table_display,
                      caption = "Descriptive Statistics and T-Test Results for Berlin and 10405 Firms (2021)",
                      label = "tab:summary_stats",
                      align = c("l", "l", "c", "c", "c"))
print(latex_table, include.rownames = FALSE, sanitize.text.function = identity)







