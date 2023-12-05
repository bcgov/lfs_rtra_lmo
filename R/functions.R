seperate_naics <- function(tbbl){
  #' takes a 1 x 1 tibble as an input and splits it by symbols ", &" and returns
  #' components in a n x 1 tibble.
  naics <-str_split(tbbl, ",|&")
  tibble(naics=trimws(unlist(naics)))
}
expand_naics <- function(tbbl){
  #' takes a 1 x 1 tibble as an input, converts the cells contents to a string,
  #' gets the length of the string, and returns the string if its length == 4... otherwise
  #' if string length is 3, it pastes characters 0 through 9 on the end
  #' if string length is 2, it pastes characters 00 through 99 on the end
  #' if string length is 1 it pastes characters 000 through 999 on the end
  #' returns a vector
  tmp_string <- tbbl$naics[[1]]
  tmp_length <- str_length(tmp_string)
  if(tmp_length == 4) {tmp_string}
  else if(tmp_length == 3) {paste0(tmp_string, 0:9)}
  else if(tmp_length == 2) {paste0(tmp_string, str_pad(0:99, 2, pad = "0"))}
  else if(tmp_length == 1) {paste0(tmp_string, str_pad(0:999, 3, pad = "0"))}
}
aggregate_pivot <- function(tbbl){
  #' A version of the function below, to handle the fact that LMO industry has 2 grouping variables (the code and description)
  #' input: a long tibble with columns syear, naics_5, count, naics (4digit), naics3, naics2, lmo_ind, lmo_detailed,
  #' output: a wide tibble with columns lmo_ind_code, lmo_detailed_industry, and the year values
  raw_total <- tbbl%>% #with RTRA data NA for naics_5 indicates it is the total across all naics.
    filter(is.na(naics_5))%>%
    select(syear, total=count)%>%
    na.omit()
  aggregated_long <- tbbl%>% #aggregate by LMO industries
    group_by(syear, lmo_ind_code, lmo_detailed_industry)%>%
    summarise(count=round(sum(count, na.rm=TRUE), digits))%>%
    filter(!is.na(syear),
           !is.na(lmo_ind_code))
  subtotal <- aggregated_long%>% #this is the sum of the LMO industry aggregates
    group_by(syear)%>%
    summarize(subtotal=sum(count))
  diff <- full_join(raw_total, subtotal)%>% #likely the total and subtotal do not match
    mutate(unknown=total-subtotal)%>%
    pivot_longer(cols=-syear, names_to = "name", values_to = "value")%>%
    pivot_wider(names_from = syear, values_from = value)
    diff <- diff[c(2,3,1),] # reorder rows subtotal, difference, total
  diff <- diff%>%
    mutate(lmo_ind_code=name)%>% #need to add this column for binding with data below
    rename(lmo_detailed_industry=name) #need to rename this column for binding with data below
  aggregated <- aggregated_long%>%
    pivot_wider(id_cols=c(lmo_ind_code, lmo_detailed_industry), names_from = syear, values_from = count)
  bind_rows(aggregated, diff)
}
aggregate_pivot2 <- function(tbbl, var){
  #' A version of the function above, where there is a single grouping variable (one of the naics levels) rather than 2 (for LMO)
  #' Inputs: a long tibble with columns: syear, naics_5, count, naics (4 digits), naics3, naics2, lmo_ind_code, lmo_detailed_industry
  #' AND a variable to to aggregate by: one of naics5, naics, naics3, naics2
  #' Output: a wide tibble with columns: the aggregation variable var, and year
  quoted_var <- deparse(substitute(var))
  raw_total <- tbbl%>% #with RTRA data NA for naics_5 indicates it is the total across all naics.
    filter(is.na(naics_5))%>%
    select(syear, total=count)%>%
    na.omit()
  aggregated_long <- tbbl%>% #aggregate by var
    group_by(syear, {{  var  }})%>%
    summarise(count=round(sum(count, na.rm=TRUE), digits))%>%
    filter(!is.na(syear),
           !is.na({{  var  }}))
  subtotal <- aggregated_long%>% #the total of the above aggregation
    group_by(syear)%>%
    summarize(subtotal=sum(count))
  diff <- full_join(raw_total, subtotal)%>%
    mutate(unknown=total-subtotal)%>%#likely the total and subtotal do not match
    pivot_longer(cols=-syear, names_to = "name", values_to = "value")%>%
    pivot_wider(names_from = syear, values_from = value)
  diff <- diff[c(2,3,1),] # reorder rows subtotal, difference, total
  diff <- diff%>%
    rename(!!quoted_var := name) #for binding with data below
  aggregated <- aggregated_long%>%
    pivot_wider(id_cols={{  var  }}, names_from = syear, values_from = count)%>%
    arrange({{  var  }})
  bind_rows(aggregated, diff)
}

agg_north_coast_nechako <- function(tbbl, var1, var2=NULL){
  #' Input: a nested tibble with columns "bc_region" "data" "agg_wide", where
  #' agg_wide has EITHER 1 (Naics) or 2 (LMO) id column(s) and year columns
  #' column BC region includes values (plural) "Nechako" and "North Coast"
  #'
  #' Output: a nested tibble with columns "bc_region" "data" "agg_wide", where
  #' agg_wide has either 1 (Naics) or 2 (LMO) id column(s) and year columns
  #' column BC region includes value (singular) "North Coast & Nechako"
  #'
  nechako_long <- tbbl%>%
    filter(bc_region=="Nechako")%>%
    ungroup()%>%
    select(agg_wide)%>%
    unnest(agg_wide)%>%
    pivot_longer(cols=-c({{  var1  }},{{  var2  }}), names_to = "year", values_to = "nechako")
  north_coast_long <- tbbl%>%
    filter(bc_region=="North Coast")%>%
    ungroup()%>%
    select(agg_wide)%>%
    unnest(agg_wide)%>%
    pivot_longer(cols=-c({{  var1  }},{{  var2  }}), names_to = "year", values_to = "north_coast")
  full_join(nechako_long, north_coast_long)%>%
    mutate(value=nechako+north_coast)%>% #add the two regions together
    select(-nechako, -north_coast)%>% #get rid of the two regions
    pivot_wider(id_cols = c({{  var1  }},{{  var2  }}), names_from = year, values_from = value)%>%
    nest(data=everything())%>%
    rename(agg_wide = data)%>%
    mutate(bc_region = "North Coast & Nechako",
           data=NA) #so the resulting nested tibble has the same format as the input tibble
}

write_sheet <- function(long_name, tbbl, title, width1, width2, date_range, digits=0) {
  # a wrapper function to write multiple things to an excel worksheet.
  colnames(tbbl) <- wrapR::make_title(colnames(tbbl))
  tbbl <- tbbl%>%
    mutate(across(where(is.numeric), ~round(.x, digits=digits)))
  title <- paste(title, long_name, date_range, sep=" ")%>%
    str_replace_all("_"," ")%>%
    str_to_title()
  subtitle <- "Source: LFS via RTRA"
  sheet_name <- str_trunc(long_name, width = 31)%>% # excel cant handle sheet names longer than this
    str_replace_all("_"," ")%>%
    str_to_title()
  createSheet(wb, sheet_name)
  setColumnWidth(wb, sheet = sheet_name, column = 1:2, width = c(width1,width2))

  writeWorksheet( # add the title
    wb,
    title,
    sheet_name,
    startRow = 1,
    startCol = 1,
    header = FALSE,
    rownames = FALSE
  )

  writeWorksheet( # add the subtitle
    wb,
    subtitle,
    sheet_name,
    startRow = 2,
    startCol = 1,
    header = FALSE,
    rownames = FALSE
  )

  writeWorksheet( # add the data
    wb,
    tbbl,
    sheet_name,
    startRow = 4,
    startCol = 1,
    header = TRUE,
    rownames = FALSE
  )
}
format_pivot <- function(tbbl){
  #' Input: a long tibble with columns "noc_5","class_title","syear","value": (values are percentages).
  #' Output: a wide tibble with columns "noc_5", "class_title" and the years.
  tbbl%>%
    mutate(value=100*round(value, digits=3))%>% #percentages with 1 decimal place
    pivot_wider(id_cols=c(noc_5, class_title), names_from = syear, values_from = value)
}

ave_retire_age <- function(tbbl){
  #' Input: a tibble with columns age and count
  #' Output: a numeric vector of length 1: the average retirement age
  total <- tbbl[is.na(tbbl$age),"count"][["count"]] #total number of retirees
  tbbl%>%
    filter(!is.na(age))%>%
    mutate(age=as.numeric(age),
           weight=count/total,
           age_weight=age*weight)%>%
    summarize(retire_age=round(sum(age_weight, na.rm=TRUE), digits))%>%
    pull()
}

get_quoted_thing <- function(tbbl, quoted_thing, quoted_name, index1, index2){
  #'Inputs: a tibble with 2 index columns plus years
  #' a quoted thing like "total",
  #' a quoted name like "lmo" or "four",
  #' index1 like lmo_ind_code, or naics_5 and
  #' index2 like lmo_detailed_industry or class_title

  #' Output: a tibble with two columns: syear and quoted_name
  tbbl%>%
    filter({{  index1  }}==quoted_thing)%>% #e.g. lmo_ind_code=="total"
    pivot_longer(cols=-c({{  index1  }},{{  index2  }}), names_to="syear", values_to=quoted_name)%>%
    filter(syear %in% min_year:max_year)%>%
    ungroup()%>%
    select(-{{  index1  }}, -{{  index2  }})
}

check_naics <- function(quoted_thing, region, tol){
  #' Inputs: a quoted thing like "total", a region like "British Columbia" and tolerance level e.g. 50
  #' Nested dataframes ****_regional_emp are drawn from environment that have columns bc_region, data, agg_wide
  lmo_dat <- lmo_regional_emp[lmo_regional_emp$bc_region==region, "agg_wide"][[1]][[1]]
  four_dat <- four_regional_emp[four_regional_emp$bc_region==region, "agg_wide"][[1]][[1]]
  three_dat <- three_regional_emp[three_regional_emp$bc_region==region, "agg_wide"][[1]][[1]]
  two_dat <- two_regional_emp[two_regional_emp$bc_region==region, "agg_wide"][[1]][[1]]

  lmo <- get_quoted_thing(lmo_dat, quoted_thing, "lmo", lmo_ind_code, lmo_detailed_industry)
  four <- get_quoted_thing(four_dat, quoted_thing, "four", naics_5, class_title)
  three <- get_quoted_thing(three_dat, quoted_thing,"three", naics3, class_title)
  two <- get_quoted_thing(two_dat, quoted_thing, "two", naics2, class_title)
  temp <- lmo%>%
    full_join(four)%>%
    full_join(three)%>%
    full_join(two)%>%
    mutate(four_close=near(lmo, four, tol=tol),
           three_close=near(lmo, three, tol=tol),
           two_close=near(lmo, two, tol=tol)
    )
}
rearrange_columns <- function(tbbl){
  #' Input: a tibble with columns like "naics something or other", class_title, and other columns
  #' Output: a tibble with the same columns, but rearranged
  tbbl%>%
    select(contains("naics"), class_title, everything())
}
add_naics_5 <- function(tbbl){
  #'Input: a tibble with columns that include naics_5.
  #'Output: a tibble where the naics description has been joined in.
  tbbl%>%
    mutate(naics_5=str_replace(naics_5, "0",""))%>%
    left_join(naics_descriptions, by=c("naics_5"="naics"))
}
add_naics_3 <- function(tbbl){
  #'Input: a tibble with columns that include naics_3.
  #'Output: a tibble where the naics description has been joined in.
  tbbl%>%
    left_join(naics_descriptions, by=c("naics3"="naics"))
}
add_naics_2 <- function(tbbl){
  #'Input: a tibble with columns that include naics_2.
  #'Output: a tibble where the naics description has been joined in.
  tbbl%>%
    left_join(naics_descriptions, by=c("naics2"="naics"))
}






