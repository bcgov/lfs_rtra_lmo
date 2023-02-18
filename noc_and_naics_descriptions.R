library(tidyverse)
library(here)
library(janitor)

fill_sequence <- function(strng){
  if(str_detect(strng, "-", negate = TRUE)){
    as_tibble(strng)
  }else{
    start <-as.numeric(str_split(strng, "-")[[1]][1])
    end <- as.numeric(str_split(strng, "-")[[1]][2])
    as_tibble(as.character(start:end))%>%
      rename(naics=value)
  }
}

read_csv(here("data","mapping", "noc_2021_version_1.0_-_classification_structure.csv"),
                 col_types = cols(
                   Level = col_double(),
                   `Hierarchical structure` = col_character(),
                   `Code - NOC 2021 V1.0` = col_character(),
                   `Class title` = col_character(),
                   `Class definition` = col_character()))%>%
  clean_names()%>%
  filter(hierarchical_structure=="Unit Group")%>%
  mutate(noc_5= str_pad(code_noc_2021_v1_0, side="left", width=5, pad="0"))%>%
  select(noc_5, class_title)%>%
  write_csv(here("data","mapping","noc21descriptions.csv"))

read_csv(here("data","mapping", "naics-scian-2017-structure-v3-eng.csv"),
                  col_types=cols(
                    Level = col_double(),
                    `Hierarchical structure` = col_character(),
                    Code = col_character(),
                    `Class title` = col_character(),
                    Superscript = col_character(),
                    `Class definition` = col_character()))%>%
  clean_names()%>%
  filter(hierarchical_structure %in% c("Sector","Subsector","Industry group"))%>%
  select(naics=code, class_title)%>%
  group_by(class_title)%>%
  nest()%>%
  mutate(data=map(data, fill_sequence))%>%
  unnest(data)%>%
  write_csv(here("data","mapping","naics17descriptions.csv"))

