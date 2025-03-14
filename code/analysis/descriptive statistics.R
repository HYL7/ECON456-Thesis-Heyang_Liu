source(here::here("housekeeping.R"))
options(scipen = 999)
df <- read.csv(file.path(clean_dir,"cleaned_data.csv"))
numerical_df <- df[sapply(df, is.numeric)]

desc_stats_df <- describe(numerical_df)

desc_stats_df <- cbind(Variable = rownames(desc_stats_df), desc_stats_df)

# calculate 25th, 75th
percentile_25 <- numerical_df %>%
  summarise(across(everything(), ~quantile(., 0.25, na.rm = TRUE)))

percentile_75 <- numerical_df %>%
  summarise(across(everything(), ~quantile(., 0.75, na.rm = TRUE)))

percentile_25 <- as.data.frame(t(percentile_25))
percentile_75 <- as.data.frame(t(percentile_75))

colnames(percentile_25) <- "25th Percentile"
colnames(percentile_75) <- "75th Percentile"

# merge 25th, 75th
desc_stats_df <- cbind(desc_stats_df, percentile_25, percentile_75)

# 删除不需要的列
desc_stats_df <- desc_stats_df[, !colnames(desc_stats_df) %in% c("vars", "n", "kurtosis", "se", "mad", "trimmed","skew","range")]

# makesure the names are correct
colnames(desc_stats_df) <- c("Variable", "Mean", "Standard Deviation","Median", "Minimum", "Maximum", "25th Percentile", "75th Percentile")

# to 3 digits
desc_stats_df[-1] <- lapply(desc_stats_df[-1], function(x) {
  if(is.numeric(x)) {
    round(x, 3)
  } else {
    x
  }
})

# null names
rownames(desc_stats_df) <- NULL

#change order
desc_stats_df <- desc_stats_df[, c("Variable", "Mean", "Standard Deviation", "Minimum",  "Median", "Maximum",
                                   "25th Percentile", "75th Percentile")]

# save
write.csv(desc_stats_df,file="writing/descripitive statistics table.csv",row.names = FALSE)
