source(here::here("housekeeping.R"))

#this is for selecting economic variables that are not correlated

df <- read.csv(file.path(clean_dir,"cleaned_data.csv"))

sub_data <- df[, 8:25]

# use kendall method
cor_matrix <- cor(sub_data, method = "kendall")

#find if there is any correlation greater than 0.6

threshold <- 0.6
high_corr_indices <- which(abs(cor_matrix) > threshold & abs(cor_matrix) < 1, arr.ind = TRUE)

# delete repeated values
high_corr_indices <- high_corr_indices[high_corr_indices[,1] < high_corr_indices[,2], ]

# print high correlated variables
for(i in 1:nrow(high_corr_indices)) {
  var1 <- rownames(cor_matrix)[high_corr_indices[i, 1]]
  var2 <- colnames(cor_matrix)[high_corr_indices[i, 2]]
  corr_value <- cor_matrix[var1, var2]
  cat(sprintf("%s - %s: %.3f\n", var1, var2, corr_value))
}


#General.government.revenue - General.government.total.expenditure is highly correlated
#As a result, General.government.revenue is removed

df<-df[, -22]

#renew cleaned_data
write.csv(df,file = file.path(clean_dir,"cleaned_data.csv"), row.names = FALSE)