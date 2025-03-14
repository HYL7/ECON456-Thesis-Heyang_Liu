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