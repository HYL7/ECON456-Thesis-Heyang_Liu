source(here::here("housekeeping.R"))

#download inequality data
url_country_gini <- "https://www.wider.unu.edu/sites/default/files/Data/wiidcountry.xlsx"
url_global_gini  <- "https://www.wider.unu.edu/sites/default/files/Data/wiidglobal.xlsx"


download.file(url_country_gini, destfile = file.path(raw_dir, "wiidcountry.xlsx"), mode = "wb")
download.file(url_global_gini,  destfile = file.path(raw_dir, "wiidglobal.xlsx"), mode = "wb")


#dowload remittance data

url_remittance_recieved <- "https://api.worldbank.org/v2/en/indicator/BX.TRF.PWKR.CD.DT?downloadformat=csv"
download.file(url_remittance_recieved, destfile = file.path(raw_dir, "remittance_recieved.zip"), mode = "wb")
unzip(file.path(raw_dir, "remittance_recieved.zip"), exdir = raw_dir)

url_remittance_paid="https://api.worldbank.org/v2/en/indicator/BM.TRF.PWKR.CD.DT?downloadformat=csv"
download.file(url_remittance_paid, destfile = file.path(raw_dir, "remittance_paid.zip"), mode = "wb")
unzip(file.path(raw_dir, "remittance_paid.zip"), exdir = raw_dir)

url_remittance_percentage <- "https://api.worldbank.org/v2/en/indicator/BX.TRF.PWKR.DT.GD.ZS?downloadformat=csv"
download.file(url_remittance_percentage, file.path(raw_dir, "remittance_percentage.zip"), mode = "wb")
unzip(file.path(raw_dir, "remittance_percentage.zip"), exdir = raw_dir)

#economic data
download.file("https://www.imf.org/-/media/Files/Publications/WEO/WEO-Database/2021/WEOApr2021all.ashx", 
              destfile = file.path(raw_dir, "WEO_Apr2021.xls"), mode = "wb")
