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
  both <- tbbl%>%
    group_by(syear, lmo_ind_code, lmo_detailed_industry)%>%
    summarise(count=sum(count, na.rm=TRUE))%>%
    filter(!is.na(syear))%>%
    pivot_wider(id_cols=c(lmo_ind_code, lmo_detailed_industry), names_from = syear, values_from = count)

  with_subtotal <- both%>%
    filter(!is.na(lmo_ind_code))%>%
    adorn_totals(name="Subtotal", fill = "Subtotal")

  total <- both%>%
    filter(is.na(lmo_ind_code))%>%
    adorn_totals(name="Grand Total", fill = "Grand Total")%>%
    filter(!is.na(lmo_ind_code))

  just_subtotal <- with_subtotal%>%
    filter(lmo_ind_code=="Subtotal")

  diff <- bind_rows(just_subtotal, total)%>%
    summarise(across(where(is.numeric), ~ diff(.x, na.rm = TRUE)))%>%
    mutate(lmo_ind_code="ind99",
           lmo_detailed_industry="Naics 1100 & 2100")

  bind_rows(with_subtotal, diff, total)
}

aggregate_pivot2 <- function(tbbl, var){
  quoted_var <- deparse(substitute(var))
  both <- tbbl%>%
    group_by(syear, {{  var  }})%>%
    summarise(count=sum(count, na.rm=TRUE))%>%
    filter(!is.na(syear))%>%
    pivot_wider(id_cols={{  var  }}, names_from = syear, values_from = count)

  with_subtotal <- both%>%
    filter(!is.na({{  var  }}))%>%
    adorn_totals(name="Subtotal")

  total <- both%>%
    filter(is.na({{  var  }}))%>%
    adorn_totals(name="Grand Total")%>%
    filter(!is.na({{  var  }}))

  just_subtotal <- with_subtotal%>%
    filter({{  var  }} =="Subtotal")

  diff <- bind_rows(just_subtotal, total)%>%
    summarise(across(where(is.numeric), ~ diff(.x, na.rm = TRUE)))%>%
    mutate(!!quoted_var := "Naics 1100 & 2100")

  bind_rows(with_subtotal, diff, total)
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
    pivot_wider(id_cols=noc_5, names_from = syear, values_from = value)
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





