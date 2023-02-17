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

###################################################
#THINGS YOU PROBABLY NEED TO CHANGE
###################################################
minmin_year <- 2000 #first year for the LMO data
min_year <- 2013 # first year for everything else
max_year <- 2022 #last year for everything
###################################################

# load packages---------------
library(tidyverse)
library(here)
library(janitor)
library(readxl)
library(vroom)
library(XLConnect)

# functions--------------
source(here("R", "functions.R"))

# constants----------
digits <- 0 # rounding to the nearest whole number
date_range <- paste(minmin_year,max_year, sep = "-")
recent_range <- paste(min_year,max_year, sep = "-")

source("by_naics.R")
source("by_noc.R")
source("check_data.R")
