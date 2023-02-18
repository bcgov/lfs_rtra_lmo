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
library(cansim)

#check bottom lines---------------------

naics_total <- check_naics("total", "British Columbia", tol=50)
naics_subtotal <- check_naics("subtotal", "British Columbia", tol=50)

by_noc <- status_by_noc%>%
  filter(name=="employed")%>%
  pull(wide)
noc_total <- by_noc[[1]]%>%
  filter(noc_5=="Total")%>%
  pivot_longer(cols=-c(noc_5, class_title), names_to="syear", values_to="noc")%>%
  select(-noc_5)

totals <- full_join(naics_total, noc_total)%>%
  select(syear, !contains("close"), contains("close"))%>%
  mutate(noc_close=near(lmo, noc, tol=50))

cansim_dat <- cansim::get_cansim("14-10-0023-01")

cansim <- cansim_dat%>%
  clean_names()%>%
  filter(geo=="British Columbia",
         ref_date %in% min_year:max_year,
         sex=="Both sexes",
         age_group=="15 years and over",
         labour_force_characteristics=="Employment",
         north_american_industry_classification_system_naics=="Total, all industries")%>%
  select(syear=ref_date, cansim=val_norm)

all_totals <- full_join(totals, cansim)%>%
  mutate(cansim_close=near(lmo, cansim, tol=50))%>%
  select(syear, !contains("close"), contains("close"))%>%
  mutate(across(where(is.numeric), round))%>%
  select(-class_title)

write_csv(x = all_totals, file = here("out","Comparison of Totals.csv"))
write_csv(x = naics_subtotal, file = here("out","Comparison of Naics Subtotals.csv"))




