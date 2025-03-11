source(here::here("housekeeping.R"))

#read the international remittance recieved data
recieved<- read_csv(file.path(raw_dir, "personal_remittance_received.csv"), skip = 4)

#delete 3rd-24st column, they are the indicator code(useless) data before 1980(world aggregate data from 1977, world economic data starts from 1980)
recieved <- recieved %>% select(-c(3:24))
#delete last 8 columns, that's because of restriction of other datasets
recieved <- recieved %>% select(-c(40:47))

#These are the country codes that are need to be removed
# aggregate values and special cases
codes_to_remove <- c("AFE", "AFW","EAS", "ECS", "EMU", "EUU", "CSS", "HIC", "HPC", "IBD", "IBT", "EAP", "EAR", "ECA",
                     "IDA", "IDB", "IDX", "LIC", "LMC", "LMY", "MEA", "MIC", "NAC", "LAC", "LCN", "LDC",
                     "OSS", "PSS", "SSF", "SST", "TEA", "TEC", "TLA", "TMN", "TSA", "LTE", "MNA", "OED","PRE","PST","SAS","SSA",
                     "TSS", "UMC", "ZWE", "BRA")
recieved <- recieved %>% filter(!`Country Code` %in% codes_to_remove)

#Convert it into panel data

recieved_panel <- recieved %>%
  pivot_longer(
    cols = -c(`Country Name`, `Country Code`), # what's being converted(other than country name, code)
    names_to = "year",  #new column name
    values_to = "Remittance_recieved"  # record
  ) %>%
  mutate(year = as.integer(year)) %>%  # make sure year is a numeric
  drop_na(year)  # drop na of Year


#Repeat this process for other two datasets

paid<- read_csv(file.path(raw_dir, "personal_remittance_paid.csv"), skip = 4)

#delete 4th-11st column, they are the indicator code(useless) and empty data from 1960-1969
paid <- paid %>% select(-c(3:24))
#delete last 8 columns, that's because of restriction of other datasets
paid <- paid %>% select(-c(40:47))

#These are the country codes that are need to be removed
# aggregate values EAS, ECS, EMU, EUU, CSS, HIC, HPC, IBD, IBT, IDA, IDB, IDX, LIC, LMC, LMY, MEA, MIC, NAC, OSS, PSS, SSF, SST, TEA, TEC, TLA, TMN, TSA, TSS, UMC
# Speicial case: ZWE
paid <- paid %>% filter(!`Country Code` %in% codes_to_remove)

#Convert it into panel data

paid_panel <- paid %>%
  pivot_longer(
    cols = -c(`Country Name`, `Country Code`), # what's being converted(other than country name)
    names_to = "year",  #new column name
    values_to = "Remittance_paid"  # record
  ) %>%
  mutate(year = as.integer(year)) %>%  # make sure year is a numeric
  drop_na(year)  # drop na of Year

#-------------------

percent<- read_csv(file.path(raw_dir, "personal_remittance_received_percentage.csv"), skip = 4)

#delete 4th-11st column, they are the indicator code(useless) and empty data from 1960-1969
percent <- percent %>% select(-c(3:24))
#delete last 8 columns, that's because of restriction of other datasets
percent <- percent %>% select(-c(40:47))

#These are the country codes that are need to be removed
# aggregate values EAS, ECS, EMU, EUU, CSS, HIC, HPC, IBD, IBT, IDA, IDB, IDX, LIC, LMC, LMY, MEA, MIC, NAC, OSS, PSS, SSF, SST, TEA, TEC, TLA, TMN, TSA, TSS, UMC
# Speicial case: ZWE
percent <- percent %>% filter(!`Country Code` %in% codes_to_remove)

#Convert it into panel data

percent_panel <- percent %>%
  pivot_longer(
    cols = -c(`Country Name`, `Country Code`), # what's being converted(other than country name)
    names_to = "year",  #new column name
    values_to = "Remittance_as_percent"  # record
  ) %>%
  mutate(year = as.integer(year)) %>%  # make sure year is a numeric
  drop_na(year)  # drop na of Year

#merge three datasets

country_remittance <- full_join(percent_panel,recieved_panel,by = c("Country Name", "Country Code", "year"))
country_remittance <- full_join(country_remittance, paid_panel, by = c("Country Name", "Country Code", "year"))

country_remittance <- country_remittance %>%
  drop_na(Remittance_recieved, Remittance_as_percent, Remittance_paid)

#separate world data from overall data set

wld_remittance<- country_remittance %>% filter(`Country Code` == "WLD")
write.csv(wld_remittance, file=file.path(clean_dir, "world_remittance.csv"))
country_remittance <- country_remittance %>% filter(`Country Code` != "WLD")

#read inequality data:
wiid_data <- read_excel(file.path(raw_dir, "wiidcountry.xlsx"))
wiid_cleaned <- wiid_data %>%
  select(c3, year, region_wb, region_un_sub, incomegroup, gini_std, bottom20, top20) %>%
  rename(`Country Code` = c3)

remittance_and_inequality <- full_join(country_remittance, wiid_cleaned, by = c("Country Code", "year"))

remittance_and_inequality <- remittance_and_inequality %>% drop_na()

#reorder the variables:

remittance_and_inequality <- remittance_and_inequality %>%
  select(`Country Name`,`Country Code`, year, region_wb, region_un_sub, incomegroup, Remittance_as_percent,
          Remittance_recieved, Remittance_paid, gini_std, bottom20, top20)


#read education datasets
school_data_13 <- read_csv(file.path(raw_dir, "school_expectancy_13.csv"))

# Keep only sex=all genders
school_data_13 <-school_data_13 %>%
  filter(Sex == "All genders") %>%
  select(-Sex)  # delete this column

# keep and rename useful variables
school_data_13 <- school_data_13 %>%
  select(`Reference Area`, `Time Period`, `Observation Value`) %>%
  rename(
    `Country Name` = `Reference Area`,
    year = `Time Period`,
    school_13 = `Observation Value`
  )

#repeat this for the other dataset

school_data_58 <- read_csv(file.path(raw_dir, "school_expectancy_58.csv"))

# Keep only sex=all genders
school_data_58 <-school_data_58 %>%
  filter(Sex == "All genders") %>%
  select(-Sex)  # delete this column

# keep and rename useful variables
school_data_58 <- school_data_58 %>%
  select(`Reference Area`, `Time Period`, `Observation Value`) %>%
  rename(
    `Country Name` = `Reference Area`,
    year = `Time Period`,
    school_58 = `Observation Value`
  )

remittance_inequality_education <- full_join(remittance_and_inequality, school_data_13, by = c("Country Name", "year"))
remittance_inequality_education <- full_join(remittance_inequality_education, school_data_58, by = c("Country Name", "year"))

remittance_inequality_education<-remittance_inequality_education %>% drop_na()


#read economic status data
econ_data <- read_excel(file.path(raw_dir, "WEO_Oct2024.xlsx"), na = "n/a") #replace "n/a" with NA
econ_data <- econ_data %>% select(-c(54:60))

#keep only variables needed
variables_to_save<-c("PPPGDP",
                     "NGDP_D",
                     "NGDPRPPPPC",
                     "NID_NGDP",
                     "NGSD_NGDP",
                     "PCPI",
                     "LUR",
                     "GGR_NGDP",
                     "GGX_NGDP",
                     "GGXCNL_NGDP",
                     "GGXWDN_NGDP",
                     "BCA_NGDPD"
                     )

econ_data <- econ_data %>%
  filter(`WEO Subject Code` %in% variables_to_save)

#rename variables
econ_data$`WEO Subject Code`<-as.character(econ_data$`WEO Subject Code`)
econ_data<-econ_data%>%
  mutate(WEO_Subject_Name = recode(`WEO Subject Code`,
                                   "PPPGDP" = "GDP",
                                   "NGDP_D" = "GDP Deflator",
                                   "NGDPRPPPPC" = "GDP per capita",
                                   "NID_NGDP" = "Total investment",
                                   "NGSD_NGDP" = "Gross national savings",
                                   "PCPI" = "Inflation",
                                   "LUR" = "Unemployment rate",
                                   "GGR_NGDP" = "General government revenue",
                                   "GGX_NGDP" = "General government total expenditure",
                                   "GGXCNL_NGDP" = "General government net lending/borrowing",
                                   "GGXWDN_NGDP" = "General government net debt",
                                   "BCA_NGDPD" = "Current account balance"
  ))

econ_data <- econ_data %>%
  rename(`Country Code` = ISO)%>%
  rename(`Country Name` = `Country`)
  

#delete unwated columns
econ_data <- econ_data %>%
  select(-c(1, 3, 5, 6, 7, 8, 9))
#Make Sure all year columns are all characters
econ_data <- econ_data %>%
  mutate(across(-c(`Country Name`, `Country Code`), as.character))

#use a for loop to convert this into panel data
subject_names <- unique(econ_data$WEO_Subject_Name)

#this is out of for loop to make sure there is already one to merge
temp_data <- econ_data %>%
  filter(WEO_Subject_Name == "GDP") %>%  #first in subject names
  select(-WEO_Subject_Name)

econ_panel <- temp_data %>%
  pivot_longer(
    cols = -c(`Country Name`, `Country Code`),  # Only change Years
    names_to = "year",
    values_to = "GDP"
  ) %>%
  mutate(year = as.integer(year),
         GDP = as.numeric(GDP))

for (subject in subject_names[-1]){
  #Repeat for other variables other than GDP
  temp_data <- econ_data %>%
    filter(WEO_Subject_Name == subject) %>%
    select(-WEO_Subject_Name) 
  
  temp_data <- temp_data %>%
    mutate(across(-c(`Country Name`, `Country Code`), as.character))
  
  temp_data <- temp_data %>%
    pivot_longer(
      cols = -c(`Country Name`, `Country Code`),  # 只转换年份列
      names_to = "year",
      values_to = subject  # 动态赋值列名
    ) %>%
    mutate(year = as.integer(year),
           across(all_of(subject), ~ as.numeric(.))
           )
  #merge it with econ_panel
  econ_panel<-full_join(econ_panel, temp_data, by = c("Country Name","Country Code", "year"))
}

full_data<-full_join(remittance_inequality_education, econ_panel,by = c("Country Name","Country Code", "year"))
full_data<-full_data%>%drop_na()

#convert GDP into Real GDP, using GDP Deflator (then remove it)
full_data$GDP<-full_data$GDP/full_data$`GDP Deflator`*100

full_data<-full_data%>%
  rename(`Real GDP` = GDP)%>%
  select(-16)


write.csv(full_data,file = file.path(clean_dir,"cleaned_data.csv"), row.names = FALSE)