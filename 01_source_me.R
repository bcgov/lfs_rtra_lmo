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

#process the data------------------
emp_4digitnaics_regional <- vroom(here("data","rtra",list.files(here("data","rtra"), pattern = "naics")))%>%
  clean_names()%>%
  filter(!is.na(syear))%>%
  mutate(count=count/12,0)

date_range <- paste(range(emp_4digitnaics_regional$syear), collapse = "-")

lmo_regional_emp <-emp_4digitnaics_regional%>%
  full_join(tidy_mapping, by=c("naics_5"="naics"))%>%
  group_by(bc_region)%>%
  nest()%>%
  mutate(agg_wide=map(data, aggregate_pivot))

lmo_regional_emp <- bind_rows(lmo_regional_emp, agg_north_coast_nechako(lmo_regional_emp))%>%
  mutate(bc_region=if_else(is.na(bc_region), "British Columbia", bc_region))%>%
  arrange(bc_region)

wb <- XLConnect::loadWorkbook(here("out",paste0("Employment for 64 LMO Industries,",date_range,".xlsx")), create = TRUE)

lmo_regional_emp%>%
  mutate(walk2(bc_region, agg_wide, write_sheet))
saveWorkbook(wb, here::here("out", paste0("Employment for 64 LMO Industries,",date_range,".xlsx")))










