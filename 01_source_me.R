# Copyright 2023 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

#load packages---------------
library(tidyverse)
library(here)
library(janitor)
library(readxl)
library(vroom)
library(XLConnect)
#constants----------
digits <- 0 #rounding to the nearest whole number
min_year <- 2012 #only report data from 2013 on
#functions--------------
source(here("R","functions.R"))
# create tidy mapping file---------------
tidy_mapping <- read_excel(here("data","mapping","2023_naics_to_lmo.xlsx"))%>%
  clean_names()%>%
  group_by(lmo_ind_code, lmo_detailed_industry)%>%
  nest()%>%
  mutate(data=map(data, seperate_naics))%>%
  unnest(data)%>%
  na.omit()%>%
  rowid_to_column()%>%
  group_by(rowid, lmo_ind_code, lmo_detailed_industry)%>%
  nest()%>%
  mutate(data=map(data,expand_naics))%>%
  unnest(data)%>%
  rename(naics=data)%>%
  mutate(naics3=str_sub(naics,1,3),
         naics2=str_sub(naics,1,2),
         across(contains("naics"), ~ as.numeric(.x)))%>%
  ungroup()%>%
  select(contains("naics"), contains("lmo"))

write_csv(tidy_mapping, here("data","mapping", "tidy_2023_naics_to_lmo.csv"))

#raw data------------------
emp_4digitnaics_regional <- vroom(here("data","rtra",list.files(here("data","rtra"), pattern = "naics")))%>%
  clean_names()%>%
  filter(!is.na(syear),
         !naics_5 %in% c(1100,2100))%>% #not sure why these are included in 4 digit data?
  mutate(count=count/12)%>%
  full_join(tidy_mapping, by=c("naics_5"="naics"))

date_range <- paste(range(emp_4digitnaics_regional$syear, na.rm=TRUE), collapse = "-")

#lmo industry aggregation-------------
lmo_regional_emp <-emp_4digitnaics_regional%>%
  group_by(bc_region)%>%
  nest()%>%
  mutate(agg_wide=map(data, aggregate_pivot))

lmo_regional_emp <- bind_rows(lmo_regional_emp, agg_north_coast_nechako(lmo_regional_emp, lmo_ind_code, lmo_detailed_industry))%>%
  mutate(bc_region=if_else(is.na(bc_region), "British Columbia", bc_region))%>%
  arrange(bc_region)

wb <- XLConnect::loadWorkbook(here("out",paste0("Employment for 64 LMO Industries,",date_range,".xlsx")), create = TRUE)

lmo_regional_emp%>%
  mutate(walk2(bc_region, agg_wide, write_sheet, "Employment for 64 LMO Industries", 5000, 15000))
saveWorkbook(wb, here::here("out", paste0("Employment for 64 LMO Industries,",date_range,".xlsx")))

# nest the recent data by region-----------------

recent <-emp_4digitnaics_regional%>%
  filter(syear>min_year)

recent_range <- paste(range(recent$syear, na.rm=TRUE), collapse = "-")

recent_nested <- recent%>%
  group_by(bc_region)%>%
  nest()

# 4 digit level---------------

four_digit_regional_emp<- recent_nested%>%
  mutate(agg_wide=map(data, aggregate_pivot2, naics_5))

four_digit_regional_emp<- bind_rows(four_digit_regional_emp, agg_north_coast_nechako(four_digit_regional_emp, naics_5))%>%
  mutate(bc_region=if_else(is.na(bc_region), "British Columbia", bc_region))%>%
  arrange(bc_region)

wb <- XLConnect::loadWorkbook(here("out",paste0("Employment for 4 digit NAICS,",recent_range,".xlsx")), create = TRUE)
four_digit_regional_emp%>%
  mutate(walk2(bc_region, agg_wide, write_sheet, "Employment for 4 digit NAICS", 5000, 3000))
saveWorkbook(wb, here::here("out", paste0("Employment for 4 digit NAICS,",recent_range,".xlsx")))

# 3 digit level---------------

three_regional_emp <-recent_nested%>%
  mutate(agg_wide=map(data, aggregate_pivot2, naics3))

three_regional_emp <- bind_rows(three_regional_emp, agg_north_coast_nechako(three_regional_emp, naics3))%>%
  mutate(bc_region=if_else(is.na(bc_region), "British Columbia", bc_region))%>%
  arrange(bc_region)

wb <- XLConnect::loadWorkbook(here("out",paste0("Employment for 3 digit NAICS,",recent_range,".xlsx")), create = TRUE)
three_regional_emp%>%
  mutate(walk2(bc_region, agg_wide, write_sheet, "Employment for 3 digit NAICS", 5000, 3000))
saveWorkbook(wb, here::here("out", paste0("Employment for 3 digit NAICS,",recent_range,".xlsx")))

# 2 digit level---------------

two_regional_emp <-recent_nested%>%
  mutate(agg_wide=map(data, aggregate_pivot2, naics2))

two_regional_emp <- bind_rows(two_regional_emp, agg_north_coast_nechako(two_regional_emp, naics2))%>%
  mutate(bc_region=if_else(is.na(bc_region), "British Columbia", bc_region))%>%
  arrange(bc_region)

wb <- XLConnect::loadWorkbook(here("out",paste0("Employment for 2 digit NAICS,",recent_range,".xlsx")), create = TRUE)
two_regional_emp%>%
  mutate(walk2(bc_region, agg_wide, write_sheet, "Employment for 2 digit NAICS", 5000, 3000))
saveWorkbook(wb, here::here("out", paste0("Employment for 2 digit NAICS,",recent_range,".xlsx")))














