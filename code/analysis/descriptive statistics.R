source(here::here("housekeeping.R"))
df <- read.csv(file.path(clean_dir,"cleaned_data.csv"))
numerical_df <- df[sapply(df, is.numeric)]

#calculate
desc_stats_df <- describe(numerical_df)

# add variable names
desc_stats_df <- cbind(Variable = rownames(desc_stats_df), desc_stats_df)

#reset row names
rownames(desc_stats_df) <- NULL

desc_stats_df <- desc_stats_df[, !colnames(desc_stats_df) %in% c("vars", "n","kurtosis", "se", "mad", "trimmed")]

options(scipen = 999)

desc_stats_df[-1] <- lapply(desc_stats_df[-1], function(x) {
  if(is.numeric(x)) {
    round(x, 3)
  } else {
    x
  }
})

