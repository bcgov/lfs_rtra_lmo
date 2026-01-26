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

noc21_descriptions <- readxl::read_excel(resolve_current("noc_descriptions"),
                                         col_types = "text")|>
  mutate(noc_5=str_pad(noc_5, width=5, side="left", pad="0"))


# labour force status-------------------------

lf_status_files <- c(resolve_current("lf_status_1115_p1"),
                     resolve_current("lf_status_1115_p2"),
                     resolve_current("lf_status_1620_p1"),
                     resolve_current("lf_status_1620_p2"),
                     resolve_current("lf_status_2125_p1"),
                     resolve_current("lf_status_2125_p2")
                     )

status_by_noc <- vroom(lf_status_files,
                       col_types = vroom::cols(
                         SYEAR = vroom::col_double(),
                         NOC_5 = vroom::col_character(), #need to specify not a number so leading zeros not stripped.
                         LF_STAT = vroom::col_character(),
                         `_COUNT_` = vroom::col_double()
                       )
                       )%>%
  clean_names() %>%
  filter(noc_5 != "missi",
         syear %in% min_year:max_year) %>%
  mutate(count = round(count / 12, digits),
         noc_5=str_pad(noc_5, width = 5, pad = "0"))%>%
  full_join(noc21_descriptions)

# adjust teachers: allocate  noc 41229 to nocs 41220 41221 proportionately

allocate_to <- status_by_noc|>
  filter(noc_5 %in% c(41220, 41221))|>
  group_by(syear, lf_stat)|>
  mutate(prop=count/sum(count))

allocate_from <- status_by_noc|>
  filter(noc_5==41229)|>
  select(syear, lf_stat, to_be_allocated=count)

allocated <- full_join(allocate_to, allocate_from)|>
  mutate(count=count+prop*to_be_allocated)|>
  select(-prop, -to_be_allocated)

status_by_noc <- status_by_noc|>
  filter(!noc_5 %in% c(41220, 41221, 41229))|>
  bind_rows(allocated)|>
  arrange(noc_5, syear)

#nest by measure---------------------
nested <- status_by_noc %>%
  filter(!is.na(noc_5)) %>%
  pivot_wider(id_cols=c(noc_5, class_title, syear), names_from = lf_stat, values_from = count) %>%
  clean_names()%>%
  select(-na, -unknown)%>%
  mutate(
    labour_force = employed + unemployed,
    unemployment_rate = unemployed / labour_force
  ) %>%
  pivot_longer(cols = -c(syear, class_title, noc_5), names_to = "name", values_to = "value") %>%
  group_by(name) %>%
  nest()
# for counts do not format the data, make wide, add totals---------------
no_format <- nested %>%
  filter(name != "unemployment_rate") %>%
  mutate(
    wide = map(data, pivot_wider, id_cols = c(noc_5, class_title), names_from = syear, values_from = value),
    wide = map(wide, remove_na_column),
    wide = map(wide, adorn_totals)
  )
# for unemployment rate format as a percent, make wide, no totals-----------------
format_as_percent <- nested %>%
  filter(name == "unemployment_rate") %>%
  mutate(wide = map(data, format_pivot))
#save to excel-------------------------
wb <- XLConnect::loadWorkbook("non_existent_file.xlsx", create = TRUE)
no_format %>%
  mutate(walk2(name, wide, write_sheet, title = NULL, 7000, 10000, recent_range))
format_as_percent %>%
  mutate(walk2(name, wide, write_sheet, title = NULL, 7000, 10000, recent_range, digits=1))
saveWorkbook(wb, here::here("out", paste0("Labour force status for 5 digit NOC (41229 split)", recent_range, ".xlsx")))

# average retirement age-----------------------

retire_files <- c(resolve_current("retirement_age_1115_p1"),
                  resolve_current("retirement_age_1115_p2"),
                  resolve_current("retirement_age_1620_p1"),
                  resolve_current("retirement_age_1620_p2"),
                  resolve_current("retirement_age_2125_p1"),
                  resolve_current("retirement_age_2125_p2")
                  )

retire_by_noc <- vroom(retire_files,
  col_types = vroom::cols(
    SYEAR = vroom::col_double(),
    NOC_5 = vroom::col_character(), #need to specify not a number so leading zeros not stripped.
    AGE = vroom::col_character(),
    `_COUNT_` = vroom::col_double()
  )
) %>%
  clean_names() %>%
  filter(
    syear %in% min_year:max_year,
    noc_5 != "missi"
  )%>%
  left_join(noc21_descriptions)

#nest by year and NOC, then calculate average for each nest--------------------
nested <- retire_by_noc %>%
  group_by(syear, noc_5, class_title) %>%
  nest() %>%
  mutate(retire_age = map_dbl(data, ave_retire_age))
#make wide, replace 0s with NAs
retire_wide <- nested %>%
  select(-data) %>%
  arrange(noc_5, syear)|>
  pivot_wider(id_cols = c(noc_5, class_title), names_from = syear, values_from = retire_age) %>%
  mutate(across(where(is.numeric), ~ if_else(near(.x, 0), NA_real_, .x)))%>%
  arrange(noc_5)
#write to excel--------------------------------------
wb <- XLConnect::loadWorkbook("non_existent_file.xlsx", create = TRUE)
write_sheet("Average Retirement Age", retire_wide, title = NULL, 7000, 3000, recent_range)
saveWorkbook(wb, here::here("out", paste0("Average retirement age for 5 digit NOC (Canada)", recent_range, ".xlsx")))


