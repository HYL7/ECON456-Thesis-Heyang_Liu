source(here::here("housekeeping.R"))

df <- read.csv(file.path(clean_dir,"cleaned_data.csv"))

#simple regression without econ data

gini <- lm(gini_std ~ Remittance_as_percent + I(Remittance_as_percent^2) + school_13 + I(school_13^2)+ school_58 + I(school_58^2), data = df)

share <- lm(I(top20-bottom20) ~ Remittance_as_percent + I(Remittance_as_percent^2) + school_13 + I(school_13^2) + school_58 + I(school_58^2), data = df)

#create a chart

models <- list(
  "Standardized Gini" = gini,
  "Top20 - Bottom20" = share
)

modelsummary(models, output = "markdown",
             coef_map = c(
               "Remittance_as_percent" = "Remittance (%)",
               "I(Remittance_as_percent^2)" = "Remittance (%)²",
               "school_13" = "ISCED 1–3 (years)",
               "I(school_13^2)" = "ISCED 1–3 (years)²",
               "school_58" = "ISCED 5–8 (years)",
               "I(school_58^2)" = "ISCED 5–8 (years)²",
               "(Intercept)" = "Constant"
             ),
             statistic = c("std.error"),
             stars = TRUE,
             gof_omit = "IC|Log.Lik")

#simple regression with economic related variables

gini_2 <- lm(gini_std ~ Remittance_as_percent + I(Remittance_as_percent^2) + 
                    school_13 + I(school_13^2) + school_58 + I(school_58^2) + 
                    Real.GDP + GDP.per.capita + Total.investment + 
                    Gross.national.savings + Inflation + Unemployment.rate + 
                     General.government.total.expenditure + 
                    General.government.net.lending.borrowing + Current.account.balance, 
                  data = df)

share_2 <- lm(I(top20 - bottom20) ~ Remittance_as_percent + I(Remittance_as_percent^2) + 
                     school_13+ I(school_13^2) + school_58 + I(school_58^2) + 
                     Real.GDP + GDP.per.capita + Total.investment + 
                     Gross.national.savings + Inflation + Unemployment.rate + General.government.total.expenditure + 
                     General.government.net.lending.borrowing + Current.account.balance, 
                   data = df)


models_2 <- list(
  "Standardized Gini" = gini_2,
  "Top20 - Bottom20" = share_2
)

modelsummary(models_2, output = "markdown",
             coef_map = c(
               "Remittance_as_percent" = "Remittance (%)",
               "I(Remittance_as_percent^2)" = "Remittance (%)²",
               "school_13" = "ISCED 1–3 (years)",
               "I(school_13^2)" = "ISCED 1–3 (years)²",
               "school_58" = "ISCED 5–8 (years)",
               "I(school_58^2)" = "ISCED 5–8 (years)²",
               "(Intercept)" = "Constant"
             ),
             coef_omit = "Real.GDP|GDP.per.capita|Total.investment|Gross.national.savings|Inflation|Unemployment.rate|General.government.total.expenditure|General.government.net.lending.borrowing|Current.account.balance",
             statistic = c("std.error"),
             stars = TRUE)

#Add Time period fixed effect
gini_3 <- feols(gini_std ~ Remittance_as_percent + I(Remittance_as_percent^2) + 
                  school_13 + I(school_13^2) + school_58 + I(school_58^2) + 
                  Real.GDP + GDP.per.capita + Total.investment + 
                  Gross.national.savings + Inflation + Unemployment.rate + 
                  General.government.total.expenditure + 
                  General.government.net.lending.borrowing + Current.account.balance | 
                  period,
                data = df)

share_3 <- feols(I(top20 - bottom20) ~ Remittance_as_percent + I(Remittance_as_percent^2) + 
                   school_13 + I(school_13^2) + school_58 + I(school_58^2) + 
                   Real.GDP + GDP.per.capita + Total.investment + 
                   Gross.national.savings + Inflation + Unemployment.rate + 
                   General.government.total.expenditure + 
                   General.government.net.lending.borrowing + Current.account.balance | 
                   period,
                 data = df)

models_3 <- list(
  "Standardized Gini" = gini_3,
  "Top20 - Bottom20" = share_3
)

modelsummary(models_3, output = "markdown",
             coef_map = c(
               "Remittance_as_percent" = "Remittance (%)",
               "I(Remittance_as_percent^2)" = "Remittance (%)²",
               "school_13" = "ISCED 1–3 (years)",
               "I(school_13^2)" = "ISCED 1–3 (years)²",
               "school_58" = "ISCED 5–8 (years)",
               "I(school_58^2)" = "ISCED 5–8 (years)²",
               "(Intercept)" = "Constant"
             ),
             coef_omit = "Real.GDP|GDP.per.capita|Total.investment|Gross.national.savings|Inflation|Unemployment.rate|General.government.total.expenditure|General.government.net.lending.borrowing|Current.account.balance",
             statistic = "std.error",
             stars = TRUE)


#identify insignificant controls
modelsummary(models_2, output = "markdown",
             coef_map = c(
               "Remittance_as_percent" = "Remittance (%)",
               "I(Remittance_as_percent^2)" = "Remittance (%)²",
               "school_13" = "ISCED 1–3 (years)",
               "I(school_13^2)" = "ISCED 1–3 (years)²",
               "school_58" = "ISCED 5–8 (years)",
               "I(school_58^2)" = "ISCED 5–8 (years)²",
               "Real.GDP" = "Real GDP",
               "GDP.per.capita" = "GDP.per.capita",
               "Total.investment" = "Total.investment",
               "Gross.national.savings" = "Gross.national.savings",
               "Inflation" = "Inflation",
               "Unemployment.rate" = "Unemployment.rate",
               "General.government.total.expenditure" = "General.government.total.expenditure",
               "General.government.net.lending.borrowing" = "General.government.net.lending.borrowing",
               "Current.account.balance" = "Current.account.balance"
             ),
             #coef_omit = "Real.GDP|GDP.per.capita|Total.investment|Gross.national.savings|Inflation|Unemployment.rate|General.government.total.expenditure|General.government.net.lending.borrowing|Current.account.balance",
             statistic = c("std.error"),
             stars = TRUE)

#with only part of economic controls

gini_4 <- lm(gini_std ~ Remittance_as_percent + I(Remittance_as_percent^2) + 
               school_13 + I(school_13^2) + school_58 + I(school_58^2) + 
               Real.GDP + GDP.per.capita + Inflation + General.government.total.expenditure +
               General.government.net.lending.borrowing, 
             data = df)

share_4 <- lm(I(top20 - bottom20) ~ Remittance_as_percent + I(Remittance_as_percent^2) + 
                school_13 + I(school_13^2) + school_58 + I(school_58^2) + 
                Real.GDP + GDP.per.capita + Inflation + General.government.total.expenditure +
                General.government.net.lending.borrowing,
              data = df)


models_4 <- list(
  "Standardized Gini" = gini_4,
  "Top20 - Bottom20" = share_4
)

modelsummary(models_4, output = "markdown",
             coef_map = c(
               "Remittance_as_percent" = "Remittance (%)",
               "I(Remittance_as_percent^2)" = "Remittance (%)²",
               "school_13" = "ISCED 1–3 (years)",
               "I(school_13^2)" = "ISCED 1–3 (years)²",
               "school_58" = "ISCED 5–8 (years)",
               "I(school_58^2)" = "ISCED 5–8 (years)²",
               "(Intercept)" = "Constant"
             ),
             coef_omit = "Real.GDP|GDP.per.capita|Total.investment|Gross.national.savings|Inflation|Unemployment.rate|General.government.total.expenditure|General.government.net.lending.borrowing|Current.account.balance",
             statistic = c("std.error"),
             stars = TRUE)

#Time period fixed effect with part of economic controls
gini_5 <- feols(gini_std ~ Remittance_as_percent + I(Remittance_as_percent^2) + 
                  school_13 + I(school_13^2) + school_58 + I(school_58^2) + 
                  Real.GDP + GDP.per.capita + Inflation + General.government.total.expenditure +
                  General.government.net.lending.borrowing| period,
                data = df)

share_5 <- feols(I(top20 - bottom20) ~ Remittance_as_percent + I(Remittance_as_percent^2) + 
                   school_13 + I(school_13^2) + school_58 + I(school_58^2) + 
                   Real.GDP + GDP.per.capita + Inflation + General.government.total.expenditure +
                   General.government.net.lending.borrowing| period,
                 data = df)

models_5 <- list(
  "Standardized Gini" = gini_5,
  "Top20 - Bottom20" = share_5
)

modelsummary(models_5, output = "markdown",
             coef_map = c(
               "Remittance_as_percent" = "Remittance (%)",
               "I(Remittance_as_percent^2)" = "Remittance (%)²",
               "school_13" = "ISCED 1–3 (years)",
               "I(school_13^2)" = "ISCED 1–3 (years)²",
               "school_58" = "ISCED 5–8 (years)",
               "I(school_58^2)" = "ISCED 5–8 (years)²",
               "(Intercept)" = "Constant"
             ),
             coef_omit = "Real.GDP|GDP.per.capita|Total.investment|Gross.national.savings|Inflation|Unemployment.rate|General.government.total.expenditure|General.government.net.lending.borrowing|Current.account.balance",
             statistic = "std.error",
             stars = TRUE)

