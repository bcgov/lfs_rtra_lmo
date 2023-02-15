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

# labour force status-------------------------
status_by_noc <- vroom(here(
  "data",
  "rtra",
  "by_noc",
  list.files(here("data", "rtra", "by_noc"),
    pattern = "stat"
  )
)) %>%
  clean_names() %>%
  filter(
    syear %in% min_year:max_year,
    noc_5 != "missi"
  ) %>%
  mutate(count = round(count / 12, digits))
#nest by measure---------------------
nested <- status_by_noc %>%
  filter(!is.na(noc_5)) %>%
  pivot_wider(names_from = lf_stat, values_from = count) %>%
  clean_names() %>%
  select(-na, -unknown) %>%
  mutate(
    labour_force = employed + unemployed,
    unemployment_rate = unemployed / labour_force
  ) %>%
  pivot_longer(cols = -c(syear, noc_5), names_to = "name", values_to = "value") %>%
  group_by(name) %>%
  nest()
# for counts do not format the data, make wide, add totals---------------
no_format <- nested %>%
  filter(name != "unemployment_rate") %>%
  mutate(
    wide = map(data, pivot_wider, id_cols = noc_5, names_from = syear, values_from = value),
    wide = map(wide, adorn_totals)
  )
# for unemployment rate format as a percent, make wide, no totals-----------------
format_as_percent <- nested %>%
  filter(name == "unemployment_rate") %>%
  mutate(wide = map(data, format_pivot))
# bind the unformated and formated together-----------------
status_by_noc <- bind_rows(no_format, format_as_percent)
#save to excel-------------------------
wb <- XLConnect::loadWorkbook(here("out", paste0("Labour force status for 5 digit NOC,", date_range, ".xlsx")), create = TRUE)
status_by_noc %>%
  mutate(walk2(name, wide, write_sheet, title = NULL, 7000, 3000, date_range))
saveWorkbook(wb, here::here("out", paste0("Labour force status for 5 digit NOC,", date_range, ".xlsx")))
# average retirement age-----------------------
retire_by_noc <- vroom(
  here(
    "data",
    "rtra",
    "by_noc",
    list.files(here("data", "rtra", "by_noc"),
      pattern = "retire"
    ))
  ,
  col_types = cols(
    SYEAR = col_double(),
    NOC_5 = col_character(), #need to specify not a number so leading zeros not stripped.
    AGE = col_character(),
    `_COUNT_` = col_double()
  )
) %>%
  clean_names() %>%
  filter(
    syear %in% min_year:max_year,
    noc_5 != "missi"
  )
#nest by year and NOC, then calculate average for each nest--------------------
nested <- retire_by_noc %>%
  group_by(syear, noc_5) %>%
  nest() %>%
  mutate(retire_age = map_dbl(data, ave_retire_age))
#make wide, replace 0s with NAs
retire_wide <- nested %>%
  select(-data) %>%
  pivot_wider(id_cols = noc_5, names_from = syear, values_from = retire_age) %>%
  mutate(across(where(is.numeric), ~ if_else(near(.x, 0), NA, .x)))
#write to excel--------------------------------------
wb <- XLConnect::loadWorkbook(here("out", paste0("Average retirement age for 5 digit NOC,", date_range, ".xlsx")), create = TRUE)
write_sheet("Average Retirement Age", retire_wide, title = NULL, 7000, 3000, date_range)
saveWorkbook(wb, here::here("out", paste0("Average retirement age for 5 digit NOC,", date_range, ".xlsx")))


