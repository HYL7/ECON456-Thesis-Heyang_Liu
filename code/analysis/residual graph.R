source(here::here("housekeeping.R"))

df <- read.csv(file.path(clean_dir,"cleaned_data.csv"))


# no Remittance/GDP
no_remit <- lm(gini_std ~ school_13 + I(school_13^2) + school_58 + I(school_58^2) + 
               Real.GDP + GDP.per.capita + Total.investment + 
               Gross.national.savings + Inflation + Unemployment.rate + 
               General.government.total.expenditure + 
               General.government.net.lending.borrowing + Current.account.balance, 
             data = df)

ggplot(df, aes(x = Remittance_as_percent, y = residuals(no_remit))) +
  stat_summary_bin(
    fun = mean,
    bins = 30, 
    geom = "point",
    color = "blue",
    size = 2
  ) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Binned Residuals vs Remit/GDP (Gini Coefficient)",
       x = "Remit/GDP",
       y = "Mean Residual in Bin") +
  theme_minimal()


ggplot(df, aes(x = Remittance_as_percent, y = residuals(no_remit))) +
  stat_summary_bin(
    fun = mean,
    bins = 30, 
    geom = "point",
    color = "blue",
    size = 2
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ poly(x, 2),  #quadratic regression
    se = FALSE,
    color = "red"
  ) +
  labs(
    title = "Binned Residuals vs Remit/GDP",
    x = "Remit/GDP",
    y = "Mean Residual in Bin"
  ) +
  theme_minimal()



#no school_13

no_school13 <- feols(gini_std ~ Remittance_as_percent + I(Remittance_as_percent^2) + 
                            school_58 + I(school_58^2) + 
                            Real.GDP + GDP.per.capita + Total.investment + 
                            Gross.national.savings + Inflation + Unemployment.rate + 
                            General.government.total.expenditure + 
                            General.government.net.lending.borrowing + Current.account.balance | 
                            period,
                          data = df)


ggplot(df, aes(x = school_13, y = residuals(no_school13))) +
  stat_summary_bin(
    fun = mean,
    bins = 30, 
    geom = "point",
    color = "blue",
    size = 2
  ) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Binned Residuals vs ISCED Level 1-3 Expectancy (Gini Coefficient)",
       x = "ISCED Level 1-3 School Life Expectancy",
       y = "Mean Residual in Bin") +
  theme_minimal()

ggplot(df, aes(x = school_13, y = residuals(no_school13))) +
  stat_summary_bin(
    fun = mean,
    bins = 30, 
    geom = "point",
    color = "blue",
    size = 2
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ poly(x, 2),  #quadratic regression
    se = FALSE,
    color = "red"
  ) +
  labs(
    title = "Binned Residuals vs ISCED Level 1-3 Expectancy (Gini Coefficient)",
    x = "ISCED Level 1-3 School Life Expectancy",
    y = "Mean Residual in Bin"
  ) +
  theme_minimal()


#no school_58

no_school58 <- feols(gini_std ~ Remittance_as_percent + I(Remittance_as_percent^2) + 
                       school_13 + I(school_13^2) + 
                       Real.GDP + GDP.per.capita + Total.investment + 
                       Gross.national.savings + Inflation + Unemployment.rate + 
                       General.government.total.expenditure + 
                       General.government.net.lending.borrowing + Current.account.balance | 
                       period,
                     data = df)


ggplot(df, aes(x = school_58, y = residuals(no_school58))) +
  stat_summary_bin(
    fun = mean,
    bins = 30, 
    geom = "point",
    color = "blue",
    size = 2
  ) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Binned Residuals vs ISCED Level 5-8 Expectancy (Gini Coefficient)",
       x = "ISCED Level 1-3 School Life Expectancy",
       y = "Mean Residual in Bin") +
  theme_minimal()


ggplot(df, aes(x = school_58, y = residuals(no_school58))) +
  stat_summary_bin(
    fun = mean,
    bins = 30, 
    geom = "point",
    color = "blue",
    size = 2
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ poly(x, 2),  #quadratic regression
    se = FALSE,
    color = "red"
  ) +
  labs(
    title = "Binned Residuals vs ISCED Level 5-8 Expectancy (Gini Coefficient)",
    x = "ISCED Level 5-8 School Life Expectancy",
    y = "Mean Residual in Bin"
  ) +
  theme_minimal()