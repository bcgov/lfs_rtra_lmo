seperate_naics <- function(my_string){
  naics <-str_split(my_string, ",|&")
  my_df <- tibble(naics=trimws(unlist(naics)))
}
expand_naics <- function(tbbl){
  tmp_string <- tbbl$naics[[1]]
  tmp_length <- str_length(tmp_string)
  if(tmp_length == 4) {tmp_string}
  else if(tmp_length == 3) {paste0(tmp_string, 0:9)}
  else if(tmp_length == 2) {paste0(tmp_string, str_pad(0:99, 2, pad = "0"))}
  else if(tmp_length == 1) {paste0(tmp_string, str_pad(0:999, 3, pad = "0"))}
}
aggregate_pivot <- function(tbbl){
  raw_total <- tbbl%>%
    filter(is.na(naics_5))%>%
    select(syear, total=count)%>%
    na.omit()

  aggregated_long <- tbbl%>%
    group_by(syear, lmo_ind_code, lmo_detailed_industry)%>%
    summarise(count=round(sum(count, na.rm=TRUE) ,digits))%>%
    filter(!is.na(syear),
           !is.na(lmo_ind_code))

  subtotal <- aggregated_long%>%
    group_by(syear)%>%
    summarize(subtotal=sum(count))

  diff <- full_join(raw_total, subtotal)%>%
    mutate(unknown=total-subtotal)%>%
    pivot_longer(cols=-syear, names_to = "name", values_to = "value")%>%
    pivot_wider(names_from = syear, values_from = value)
  diff <- diff[c(2,3,1),] # reorder rows subtotal, difference, total
  diff <- diff%>%
    mutate(lmo_ind_code=name)%>% #for binding with data
    rename(lmo_detailed_industry=name) #for binding with data below

  aggregated <- aggregated_long%>%
    pivot_wider(id_cols=c(lmo_ind_code, lmo_detailed_industry), names_from = syear, values_from = count)

  bind_rows(aggregated, diff)
}

aggregate_pivot2 <- function(tbbl, var){
  quoted_var <- deparse(substitute(var))

  raw_total <- tbbl%>%
    filter(is.na(naics_5))%>%
    select(syear, total=count)%>%
    na.omit()

  aggregated_long <- tbbl%>%
    group_by(syear, {{  var  }})%>%
    summarise(count=round(sum(count, na.rm=TRUE),digits))%>%
    filter(!is.na(syear),
           !is.na({{  var  }}))

  subtotal <- aggregated_long%>%
    group_by(syear)%>%
    summarize(subtotal=sum(count))

  diff <- full_join(raw_total, subtotal)%>%
    mutate(unknown=total-subtotal)%>%
    pivot_longer(cols=-syear, names_to = "name", values_to = "value")%>%
    pivot_wider(names_from = syear, values_from = value)
  diff <- diff[c(2,3,1),] # reorder rows subtotal, difference, total
  diff <- diff%>%
    rename(!!quoted_var := name)

  aggregated <- aggregated_long%>%
    pivot_wider(id_cols={{  var  }}, names_from = syear, values_from = count)%>%
    arrange({{  var  }})

  bind_rows(aggregated, diff)
}

agg_north_coast_nechako <- function(tbbl, var1, var2=NULL){
  nechako <- tbbl%>%
    filter(bc_region=="Nechako")%>%
    ungroup()%>%
    select(agg_wide)%>%
    unnest(agg_wide)%>%
    pivot_longer(cols=-c({{  var1  }},{{  var2  }}), names_to = "year", values_to = "nechako")

  north_coast <- tbbl%>%
    filter(bc_region=="North Coast")%>%
    ungroup()%>%
    select(agg_wide)%>%
    unnest(agg_wide)%>%
    pivot_longer(cols=-c({{  var1  }},{{  var2  }}), names_to = "year", values_to = "north_coast")

  full_join(nechako, north_coast)%>%
    mutate(value=nechako+north_coast)%>%
    select(-nechako, -north_coast)%>%
    pivot_wider(id_cols = c({{  var1  }},{{  var2  }}), names_from = year, values_from = value)%>%
    nest(data=everything())%>%
    rename(agg_wide = data)%>%
    mutate(bc_region = "North Coast & Nechako",
           data=NA)
}

write_sheet <- function(long_name, tbbl, title, width1, width2, date_range) {
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
  tbbl%>%
    mutate(value=scales::percent(value, accuracy=.1))%>%
    pivot_wider(id_cols=c(noc_5, class_title), names_from = syear, values_from = value)
}

ave_retire_age <- function(tbbl){
  total <- tbbl[is.na(tbbl$age),"count"][["count"]]
  tbbl%>%
    filter(!is.na(age))%>%
    mutate(age=as.numeric(age),
           weight=count/total,
           age_weight=age*weight)%>%
    summarize(retire_age=round(sum(age_weight, na.rm=TRUE),digits))%>%
    pull()
}

get_quoted_thing <- function(tbbl, quoted_thing, quoted_name, index1, index2=NULL){
  tbbl%>%
    filter({{  index1  }}==quoted_thing)%>%
    pivot_longer(cols=-c({{  index1  }},{{  index2  }}), names_to="syear", values_to=quoted_name)%>%
    filter(syear %in% min_year:max_year)%>%
    ungroup()%>%
    select(-{{  index1  }}, -{{  index2  }})
}

check_naics <- function(quoted_thing, region, tol){
  lmo_dat <- lmo_regional_emp%>%
    filter(bc_region==region)%>%
    pull(agg_wide)
  lmo_dat <- lmo_dat[[1]]
  four_dat <- four_regional_emp%>%
    filter(bc_region==region)%>%
    pull(agg_wide)
  four_dat <- four_dat[[1]]
  three_dat <- three_regional_emp%>%
    filter(bc_region==region)%>%
    pull(agg_wide)
  three_dat <- three_dat[[1]]
  two_dat <- two_regional_emp%>%
    filter(bc_region==region)%>%
    pull(agg_wide)
  two_dat <- two_dat[[1]]

  lmo <- get_quoted_thing(lmo_dat, quoted_thing, "lmo", lmo_ind_code, lmo_detailed_industry)
  four <- get_quoted_thing(four_dat, quoted_thing, "four", naics_5, class_title)
  three <- get_quoted_thing(three_dat, quoted_thing,"three", naics3, class_title)
  two <- get_quoted_thing(two_dat, quoted_thing, "two", naics2, class_title)
  lmo%>%
    full_join(four)%>%
    full_join(three)%>%
    full_join(two)%>%
    mutate(four_close=near(lmo, four, tol=tol),
           three_close=near(lmo, three, tol=tol),
           two_close=near(lmo, two, tol=tol)
    )
}


rearrange_columns <- function(tbbl){
  tbbl%>%
    select(contains("naics"), class_title, everything())
}
add_naics_5 <- function(tbbl){
  tbbl%>%
    mutate(naics_5=str_replace(naics_5, "0",""))%>%
    left_join(naics_descriptions, by=c("naics_5"="naics"))
}
add_naics_3 <- function(tbbl){
  tbbl%>%
    left_join(naics_descriptions, by=c("naics3"="naics"))
}
add_naics_2 <- function(tbbl){
  tbbl%>%
    left_join(naics_descriptions, by=c("naics2"="naics"))
}






