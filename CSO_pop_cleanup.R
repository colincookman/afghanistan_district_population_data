library(gsheet)
library(tidyverse)
setwd("~/Google Drive/GitHub/afghanistan_district_population_data")

cso_import <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1OshMwmcHRQqiIpx4yyGPDNvaojHeyx9no3d614Q1C-I/edit?usp=sharing")

cso_pop <- cso_import

cso_pop$rural_fem <- as.numeric(cso_pop$rural_fem)
cso_pop$rural_male <- as.numeric(cso_pop$rural_male)
cso_pop$rural_total <- as.numeric(cso_pop$rural_total)
cso_pop$urban_fem <- as.numeric(cso_pop$urban_fem)
cso_pop$urban_male <- as.numeric(cso_pop$urban_male)
cso_pop$urban_total <- as.numeric(cso_pop$urban_total)
cso_pop$fem_population <- as.numeric(cso_pop$fem_population)
cso_pop$male_population <- as.numeric(cso_pop$male_population)
cso_pop$total_population <- as.numeric(cso_pop$total_population)


cso_pop$province_name[cso_pop$province_name == "Badakshan"] <- "Badakhshan"
cso_pop$province_name[cso_pop$province_name == "Daikaendi"] <- "Daykundi"
cso_pop$province_name[cso_pop$province_name == "Daikundi"] <- "Daykundi"
cso_pop$province_name[cso_pop$province_name == "Helmand"] <- "Hilmand"
cso_pop$province_name[cso_pop$province_name == "Herat"] <- "Hirat"
cso_pop$province_name[cso_pop$province_name == "Jozjan"] <- "Jawzjan"
cso_pop$province_name[cso_pop$province_name == "Jowzjan"] <- "Jawzjan"
cso_pop$province_name[cso_pop$province_name == "Konar"] <- "Kunar"
cso_pop$province_name[cso_pop$province_name == "Konoz"] <- "Kunduz"
cso_pop$province_name[cso_pop$province_name == "Kunarha"] <- "Kunar"
cso_pop$province_name[cso_pop$province_name == "Laghaman"] <- "Laghman"
cso_pop$province_name[cso_pop$province_name == "Maidan Wardak"] <- "Wardak"
cso_pop$province_name[cso_pop$province_name == "Maydan"] <- "Wardak"
cso_pop$province_name[cso_pop$province_name == "Neemroz"] <- "Nimroz"
cso_pop$province_name[cso_pop$province_name == "Nimruz"] <- "Nimroz"
cso_pop$province_name[cso_pop$province_name == "Nooristan"] <- "Nuristan"
cso_pop$province_name[cso_pop$province_name == "Noorstan"] <- "Nuristan"
cso_pop$province_name[cso_pop$province_name == "Paktia"] <- "Paktya"
cso_pop$province_name[cso_pop$province_name == "Panjshir"] <- "Panjsher"
cso_pop$province_name[cso_pop$province_name == "Sar-i-Pul"] <- "Sar-e-Pul"
cso_pop$province_name[cso_pop$province_name == "Saripul"] <- "Sar-e-Pul"
cso_pop$province_name[cso_pop$province_name == "Unlisted (Baghis?)"] <- "Badghis"
cso_pop$province_name[cso_pop$province_name == "Unlisted (Balkh?)"] <- "Balkh"
cso_pop$province_name[cso_pop$province_name == "Unlisted (Bamyan?)"] <- "Bamyan"
cso_pop$province_name[cso_pop$province_name == "Unlisted (Farah?)"] <- "Farah"
cso_pop$province_name[cso_pop$province_name == "Unlisted (Faryab?)"] <- "Faryab"
cso_pop$province_name[cso_pop$province_name == "Unlisted (Ghor?)"] <- "Ghor"
cso_pop$province_name[cso_pop$province_name == "Unlisted (Herat?)"] <- "Hirat"
cso_pop$province_name[cso_pop$province_name == "Unlisted (Hilmand?)"] <- "Hilmand"
cso_pop$province_name[cso_pop$province_name == "Unlisted (Jowzjan?)"] <- "Jawzjan"
cso_pop$province_name[cso_pop$province_name == "Unlisted (Kandahar?)"] <- "Kandahar"
cso_pop$province_name[cso_pop$province_name == "Unlisted (Nimruz?)"] <- "Nimroz"
cso_pop$province_name[cso_pop$province_name == "Unlisted (Oruzgan?)"] <- "Uruzgan"
cso_pop$province_name[cso_pop$province_name == "Unlisted (Panjshir?)"] <- "Panjsher"
cso_pop$province_name[cso_pop$province_name == "Unlisted (Samangan?)"] <- "Samangan"
cso_pop$province_name[cso_pop$province_name == "Unlisted (Sar-e-Pul?)"] <- "Sar-e-Pul"
cso_pop$province_name[cso_pop$province_name == "Unlisted (Zabul?)"] <- "Zabul"
cso_pop$province_name[cso_pop$province_name == "Urozgan"] <- "Uruzgan"

pop_totals <- cso_pop %>% filter(district_number == "TOTAL" | district_number == "Total")

cso_pop <- cso_pop %>% filter(district_number != "TOTAL" & district_number != "Total") %>% 
  rowwise %>% mutate(
    province_code = str_pad(province_number, width = 2, side = "left", pad = 0),
    district_code = paste0(province_code, str_pad(district_number, width = 2, side = "left", pad = 0)),
    units = ifelse(solar_year >= 1394, "exact", "thousands")
    ) %>% 
  dplyr::select(solar_year, gregorian_year, 
                province_code, province_number, province_name, 
                district_code, district_number, district_name_eng,
                everything()) %>%
  rename(province_name_eng = province_name) %>%
  arrange(solar_year, province_code, district_code)

cso_pop_scaled <- cso_pop %>% mutate(
  rural_fem = ifelse(units == "thousands", rural_fem * 1000, rural_fem),
  rural_male = ifelse(units == "thousands", rural_male * 1000, rural_male),
  rural_total = ifelse(units == "thousands", rural_total * 1000, rural_total),
  urban_fem = ifelse(units == "thousands", urban_fem * 1000, urban_fem),
  urban_male = ifelse(units == "thousands", urban_male * 1000, urban_male),
  urban_total = ifelse(units == "thousands", urban_total * 1000, urban_total),
  fem_population = ifelse(units == "thousands", fem_population * 1000, fem_population),
  male_population = ifelse(units == "thousands", male_population * 1000, male_population),
  total_population = ifelse(units == "thousands", total_population * 1000, total_population)
)

cso_pop_scaled$province_number <- NULL
cso_pop_scaled$district_number <- as.numeric(cso_pop_scaled$district_number)

write.csv(cso_pop_scaled, "cso_district_population_estimates_2004-2020.csv", row.names = F)

cso_codes <- cso_pop_scaled %>% 
  dplyr::select(solar_year, province_code, province_name_eng, district_code, district_name_eng)

# write.csv(cso_codes, "cso_codes.csv", row.names = F)

