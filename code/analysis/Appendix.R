source(here::here("housekeeping.R"))

df <- read.csv(file.path(clean_dir,"cleaned_data.csv"))

table(df$`Country Name`)

table(df$year)
ggplot(df, aes(x = factor(1), y = year)) +
  geom_boxplot() +
  labs(title = "Boxplot of Year", x = "", y = "Year") +
  theme_minimal()

table(df$region_wb)
ggplot(df, aes(x = region_wb)) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(title = "Frequency of Regions", x = "World Bank Region", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

table(df$region_un_sub)
ggplot(df, aes(x = region_un_sub)) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(title = "Frequency of Regions", x = "United Nations Region", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

table(df$incomegroup)
ggplot(df, aes(x = incomegroup)) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(title = "Frequency of Income groups", x = "Income Groups", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(df, aes(x = Remittance_as_percent)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Remittance as Percent", x = "Remittance as Percent", y = "Frequency") +
  theme_minimal()


ggplot(df, aes(y = Remittance_as_percent)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Boxplot of Remittance as Percent", y = "Remittance as Percent") +
  theme_minimal()


ggplot(df, aes(x = Remittance_recieved)) +
  geom_histogram(binwidth = 1e9, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Remittance Received", x = "Remittance Received", y = "Frequency") +
  theme_minimal()

ggplot(df, aes(x = Remittance_paid)) +
  geom_histogram(binwidth = 1e9, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Remittance Paid", x = "Remittance Paid", y = "Frequency") +
  theme_minimal()

ggplot(df, aes(x = gini_std)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Gini Coefficient (Standardized)", x = "Gini Coefficient", y = "Frequency") +
  theme_minimal()

ggplot(df, aes(x = bottom20)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Bottom 20% Income Share", x = "Income Share of Bottom 20%", y = "Frequency") +
  theme_minimal()


ggplot(df, aes(x = top20)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Top 20% Income Share", x = "Income Share of Top 20%", y = "Frequency") +
  theme_minimal()

ggplot(df, aes(y = top20)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Boxplot of Top 20% Income Share", y = "Income Share of Top 20%") +
  theme_minimal()


ggplot(df, aes(x = school_13)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of the school expectancy of ISCED levels 1 to 3", x = "School expectancy of ISCED levels 1 to 3", y = "Frequency") +
  theme_minimal()

ggplot(df, aes(x = school_58)) +
  geom_histogram(binwidth = 0.25, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of the school expectancy of ISCED levels 5 to 8", x = "School expectancy of ISCED levels 5 to 8", y = "Frequency") +
  theme_minimal()

ggplot(df, aes(y = school_58)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Boxplot of School Expectancy (ISCED Levels 5 to 8)", 
       y = "School Expectancy (Years)") +
  theme_minimal()

ggplot(df, aes(x = Real.GDP)) +
  geom_histogram(binwidth = 1e12, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Real GDP", x = "Real GDP", y = "Frequency") +
  theme_minimal()

ggplot(df, aes(x = Real.GDP)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Boxplot of Real GDP", x = "Real GDP") +
  theme_minimal()

ggplot(df, aes(x = `GDP.per.capita`)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Boxplot of GDP per Capita", x= "GDP per Capita") +
  theme_minimal()

ggplot(df, aes(x = `GDP.per.capita`)) +
  geom_histogram(binwidth = 5000, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of GDP per Capita", x = "GDP per Capita", y = "Frequency") +
  theme_minimal()

ggplot(df, aes(x = `Total.investment`)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Boxplot of Total Investment", x = "Total Investment") +
  theme_minimal()