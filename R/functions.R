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
           lmo_detailed_industry="Unknown")

  bind_rows(with_subtotal, diff, total)
}
agg_north_coast_nechako <- function(tbbl){
  nechako <- tbbl%>%
    filter(bc_region=="Nechako")%>%
    ungroup()%>%
    select(agg_wide)%>%
    unnest(agg_wide)%>%
    pivot_longer(cols=-c(lmo_ind_code, lmo_detailed_industry), names_to = "year", values_to = "nechako")

  north_coast <- tbbl%>%
    filter(bc_region=="North Coast")%>%
    ungroup()%>%
    select(agg_wide)%>%
    unnest(agg_wide)%>%
    pivot_longer(cols=-c(lmo_ind_code, lmo_detailed_industry), names_to = "year", values_to = "north_coast")

  full_join(nechako, north_coast)%>%
    mutate(value=nechako+north_coast)%>%
    select(-nechako, -north_coast)%>%
    pivot_wider(id_cols = c(lmo_ind_code,lmo_detailed_industry),names_from = year, values_from = value)%>%
    nest()%>%
    rename(agg_wide = data)%>%
    mutate(bc_region = "North Coast & Nechako",
           data=NA)
}

write_sheet <- function(long_name, tbbl) {
  colnames(tbbl) <- wrapR::make_title(colnames(tbbl))
  tbbl <- tbbl%>%
    mutate(across(where(is.numeric), ~round(.x, digits=digits)))
  title <- paste("Employment for 64 LMO Industries",long_name,date_range, sep=", ")
  subtitle <- "Source: LFS via RTRA"
  sheet_name <- str_trunc(long_name, width = 31) # excel cant handle sheet names longer than this
  createSheet(wb, sheet_name)
  setColumnWidth(wb, sheet = sheet_name, column = 1:2, width = c(4000,15000))

  writeWorksheet( # add the data
    wb,
    title,
    sheet_name,
    startRow = 1,
    startCol = 1,
    header = FALSE,
    rownames = FALSE
  )

  writeWorksheet( # add the data
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
