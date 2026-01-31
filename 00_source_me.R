#' INSTRUCTIONS:
#'
#' 1) upload SAS scripts to https://eft-tef.statcan.gc.ca/ (17 scripts need to be split across days or people)
#' 2) download all the .csv files (stats can zips them up)
#' 3) unzip the file, and copy the contents to data/add_to_pond.
#' 4) also put any changes to mapping files or description files in data/add_to_pond.
#' 5) source this file, results in directory "out".

# load packages---------------
library(tidyverse)
library(here)
library(janitor)
library(readxl)
library(vroom)
library(XLConnect)
library(digest)
library(fs)
library(yaml)
library(tools)
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
source(here("..","shared_functions","pond_utilities.R"))
#move old files to archive-------------------------------
files_to_archive <- list.files(here("out"))[list.files(here("out"))!="archive"]
paths_to_archive <- here("out", files_to_archive)
if(!dir.exists(here("out_archive", max_year))) dir.create(here("out_archive", max_year))
new_paths <- here("out_archive", max_year, files_to_archive)
file.copy(paths_to_archive, new_paths, overwrite=TRUE)
file.remove(paths_to_archive)
#add new files to data pond-------------------------------------

ingest_pond()

#run the scripts---------------
source("by_naics.R")
source("by_noc.R")
source("check_data.R")
