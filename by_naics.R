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

######################################################################################
#NOTE THIS FILE DEPENDS ON CONSTANTS AND LIBRARIES LOADED IN THE FILE 00_source_me.R
######################################################################################

# create tidy mapping file---------------
tidy_mapping <- read_excel(here("data","mapping","2024_naics_to_lmo.xlsx"))%>%
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
         naics2=str_sub(naics,1,2))%>%
  ungroup()%>%
  select(contains("naics"), contains("lmo"))

write_csv(tidy_mapping, here("data","mapping", "tidy_2024_naics_to_lmo.csv"))

#get naics descriptions------------------

naics_descriptions <- read_csv(here("data", "mapping", "naics17descriptions.csv"),
                               col_types = cols(
                                 class_title = col_character(),
                                 naics = col_character()
                               ))

#raw data------------------
emp_4digitnaics_regional <- vroom(here("data",
                                       "rtra",
                                       "by_naics",
                                       list.files(here("data","rtra", "by_naics"),
                                                  pattern = "naics")))%>%
  clean_names()%>%
  filter(syear %in% minmin_year:max_year,
         !naics_5 %in% c("01100","02100"))%>%
  mutate(count=count/12,
         naics=str_sub(naics_5,2,5))%>%
  full_join(tidy_mapping)

#lmo industry aggregation-------------
lmo_regional_emp <-emp_4digitnaics_regional%>%
  group_by(bc_region)%>%
  nest()%>%
  mutate(agg_wide=map(data, aggregate_pivot))

lmo_regional_emp <- bind_rows(lmo_regional_emp, agg_north_coast_nechako(lmo_regional_emp, lmo_ind_code, lmo_detailed_industry))%>%
  mutate(bc_region=if_else(is.na(bc_region), "British Columbia", bc_region))%>%
  arrange(bc_region)

wb <- XLConnect::loadWorkbook(here("out", paste0("Employment for 64 LMO Industries,",date_range,".xlsx")), create = TRUE)
lmo_regional_emp%>%
  mutate(walk2(bc_region, agg_wide, write_sheet, "Employment for 64 LMO Industries", 5000, 15000, date_range))
saveWorkbook(wb, here::here("out", paste0("Employment for 64 LMO Industries",date_range,".xlsx")))

# nest the recent data by region-----------------

recent_nested <-emp_4digitnaics_regional%>%
  filter(syear %in% min_year:max_year)%>%
  group_by(bc_region)%>%
  nest()

# 4 digit level---------------

four_regional_emp<- recent_nested%>%
  mutate(agg_wide=map(data, aggregate_pivot2, naics_5))

four_regional_emp<- bind_rows(four_regional_emp, agg_north_coast_nechako(four_regional_emp, naics_5))%>%
  mutate(bc_region=if_else(is.na(bc_region), "British Columbia", bc_region))%>%
  arrange(bc_region)%>%
  mutate(agg_wide=map(agg_wide, add_naics_5),
         agg_wide=map(agg_wide, rearrange_columns)
         )


wb <- XLConnect::loadWorkbook(here("out", paste0("Employment for 4 digit NAICS",recent_range,".xlsx")), create = TRUE)
four_regional_emp%>%
  mutate(walk2(bc_region, agg_wide, write_sheet, "Employment for 4 digit NAICS", 5000, 3000, recent_range))
saveWorkbook(wb, here::here("out", paste0("Employment for 4 digit NAICS",recent_range,".xlsx")))

# 3 digit level---------------

three_regional_emp <-recent_nested%>%
  mutate(agg_wide=map(data, aggregate_pivot2, naics3))

three_regional_emp <- bind_rows(three_regional_emp, agg_north_coast_nechako(three_regional_emp, naics3))%>%
  mutate(bc_region=if_else(is.na(bc_region), "British Columbia", bc_region))%>%
  arrange(bc_region)%>%
  mutate(agg_wide=map(agg_wide, add_naics_3),
         agg_wide=map(agg_wide, rearrange_columns)
  )

wb <- XLConnect::loadWorkbook(here("out",paste0("Employment for 3 digit NAICS",recent_range,".xlsx")), create = TRUE)
three_regional_emp%>%
  mutate(walk2(bc_region, agg_wide, write_sheet, "Employment for 3 digit NAICS", 5000, 3000, recent_range))
saveWorkbook(wb, here::here("out", paste0("Employment for 3 digit NAICS",recent_range,".xlsx")))

# 2 digit level---------------

two_regional_emp <-recent_nested%>%
  mutate(agg_wide=map(data, aggregate_pivot2, naics2))

two_regional_emp <- bind_rows(two_regional_emp, agg_north_coast_nechako(two_regional_emp, naics2))%>%
  mutate(bc_region=if_else(is.na(bc_region), "British Columbia", bc_region))%>%
  arrange(bc_region)%>%
  mutate(agg_wide=map(agg_wide, add_naics_2),
         agg_wide=map(agg_wide, rearrange_columns)
  )

wb <- XLConnect::loadWorkbook(here("out",paste0("Employment for 2 digit NAICS",recent_range,".xlsx")), create = TRUE)
two_regional_emp%>%
  mutate(walk2(bc_region, agg_wide, write_sheet, "Employment for 2 digit NAICS", 5000, 3000, recent_range))
saveWorkbook(wb, here::here("out", paste0("Employment for 2 digit NAICS",recent_range,".xlsx")))








