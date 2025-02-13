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
# load packages---------------
library(tidyverse)
library(here)
library(janitor)
library(readxl)
library(vroom)
library(XLConnect)
library(conflicted)
conflicts_prefer(dplyr::filter)
#constants
###################################################
minmin_year <- 2000 #first year for the LMO data
max_year <- year(today())-1 #last year for everything
min_year <- max_year-9 # first year for everything else
digits <- 0 # rounding to the nearest whole number
date_range <- paste(minmin_year, max_year, sep = "-")
recent_range <- paste(min_year, max_year, sep = "-")
###################################################
source(here("R", "functions.R"))
#move old files to archive-------------------------------
files_to_archive <- list.files(here("out"))[list.files(here("out"))!="archive"]
paths_to_archive <- here("out", files_to_archive)
if(!dir.exists(here("out_archive", max_year))) dir.create(here("out_archive", max_year))
new_paths <- here("out_archive", max_year, files_to_archive)
file.copy(paths_to_archive, new_paths, overwrite=TRUE)
file.remove(paths_to_archive)
#run the scripts---------------
source("by_naics.R")
source("by_noc.R")
source("check_data.R")
