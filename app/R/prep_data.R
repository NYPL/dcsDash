#### creates "div_name"  ####
filter_by_division <- function(df, div_choice) {
  filter(df, code %in% div_choice) 
}

get_mdsqual_prop <- function(mm_prop, compare_prop, div_choice, fiscal_qtrs) {
  if (compare_prop == "compare_divs"){
    mm_prop  %>%
      filter(code %in% div_choice, fy_q %in% fiscal_qtrs) %>%
      mutate(tooltip = sprintf("%.0f%% of items in the %s division %s minimum mandatory requirements",
                               values*100, code, ifelse(prop_type=="p_above", "meet", "are below")))
  } else {
    mm_prop  %>% 
      filter(code %in% div_choice) %>%
      group_by(fy_q) %>%
      summarize(n_recs = sum(n_recs)/2,
                n_above = sum(n_above)/2,
                n_below = sum(n_below)/2,
                p_above = n_above / n_recs,
                p_below = n_below / n_recs) %>%
      gather(prop_type, values, p_above:p_below) %>%
      mutate(fy_label = paste0('FY ',str_replace(fy_q,'_q',' Q')))
  }
}

get_scores_by_element <- function(elements_by_division_all, elements_by_division, div_choice){
  if (length(div_choice) == 0 | length(div_choice) == 26) {
    elements_by_division_all
  }
  else {
    get_ebd(elements_by_division, div_choice) %>% 
      tibble::add_column(!!!elements_by_division_all[!names(elements_by_division_all) %in% names(.)])
  }
}

get_ebd <- function(elements_by_division, div_choice) {
  elements_by_division %>%
    filter(code %in% div_choice) %>% 
    select(-code) %>%
    group_by(quarter_lab, element, score) %>%
    mutate(group_n = sum(n)) %>%
    slice(1) %>%
    ungroup() %>%
    group_by(quarter_lab, element) %>%
    mutate(perc = round((group_n / sum(group_n))*100, digits = 2)) %>%
    ungroup() %>%
    mutate(element = forcats::fct_reorder(element, ele_order)) %>%
    select(element, score, perc, quarter_lab) %>%
    pivot_wider(names_from = c(score, quarter_lab), values_from = perc, values_fill = list(perc = 0), names_prefix = "score_") %>%
    rename_all(~ gsub(' \\d{2}(\\d{2})',paste0('_\\1'), .)) %>%
    arrange(element)
}

#### approvals ####
prep_approvals <- function(approvals, div_choice, ami_switch, month_start, month_end){
  approvals %>% 
    filter(code %in% div_choice,
           ami %in% ami_switch,
           between(d_month, month_start, month_end)) %>%
    group_by(fy_year, fy_qy, d_month, rights) %>%
    summarize(captures = sum(captures),
              items = n()) %>%
    ungroup() %>%
    pivot_wider(names_from = c(rights), values_from = c(items,captures), values_fill = list(items = 0, captures = 0)) %>%
    complete(d_month = seq(month_start, month_end, by = "month")) 
  # %>%
  #   filter_at(vars(starts_with("items"),starts_with("captures")),all_vars(!is.na(.)))
}

get_approvals_collections <- function(approvals, div_choice, ami_switch, month_start, month_end) {
  approvals %>%
    select(captures:collection_title, id, collection_uuid, code, ami, fy_year, d_month, rights) %>%
    filter(code %in% div_choice,
           ami %in% ami_switch,
           between(d_month, month_start, month_end)
           # ,between(fy_year, year(month_start), year(month_end))
           ) %>%
    drop_na(collection_title) %>%
    group_by(fy_year, d_month, rights, collection_id, collection_title, collection_uuid, code) %>%
    summarise(items = n(), captures = sum(captures))
}

get_approvals_top_colls <- function(app_coll_name, clicked_rights, clicked_date, not_clicked, clicked_record_type, clicked_rights_txt) {
  app_coll_name %>%
    filter(rights == clicked_rights, d_month == clicked_date) %>%
    select_at(vars(-not_clicked)) %>%
    rename(n = clicked_record_type) %>%
    top_n(10, n) %>%
    head(10) %>%
    # filter(n > 1) %>%
    mutate(hex = case_when(rights == "website" & clicked_record_type == "items" ~ "#810f7c", ## c087bd
                           rights == "onPrem" & clicked_record_type == "items" ~ "#8856a7", # b799ca
                           rights == "FALSE" & clicked_record_type == "items" ~ "#8c96c6", # b799ca
                           rights == "website" & clicked_record_type == "captures" ~ "#43a2ca", # a3abd1
                           rights == "onPrem" & clicked_record_type == "captures" ~ "#7bccc4",
                           rights == "FALSE" & clicked_record_type == "captures" ~ "#a8ddb5"),
           tooltip = sprintf("%s %s %s %s approved in %s",
                             comma_format()(n),
                             ifelse(n > 1, clicked_record_type, str_replace(clicked_record_type, "s", "")),
                             clicked_rights_txt,
                             ifelse(n > 1, "were", "was"),
                             paste(month(d_month, label = TRUE, abbr = FALSE), year(d_month))),
           url = paste0('https://digitalcollections.nypl.org/collections/',collection_uuid)
    ) %>%
    arrange(desc(n))
}

get_approvals_for_dl <- function(approvals, div_choice, ami_switch, month_start, month_end) {
  approvals %>% 
    filter(code %in% div_choice,
           ami %in% ami_switch,
           between(d_month, month_start, month_end)) %>%
    group_by(fy_year, fy_qy, d_month, rights) %>%
    summarize(captures = sum(captures), items = n()) %>%
    ungroup() %>%
    select(-fy_year) %>%
    filter(rights != "INACTIVE") %>%
    rename(fiscal_quarter = fy_qy, date = d_month) %>%
    mutate(rights = ifelse(rights == "FALSE", "other", rights))
}

#### diu & pami prep ####

prep_diu_line <- function(diu_line, div_choice) {
  diu_line %>%
    filter(code %in% div_choice) %>%
    group_by(d_month) %>%
    summarise_at(vars(created,captured,approved), sum, na.rm = TRUE)
}

get_diu_top_colls <- function(coll_name, clicked_category, clicked_date) {
  coll_name %>% 
    filter(date_label == clicked_category, d_month == as.Date(clicked_date)) %>%
    top_n(10, n) %>%
    head(10) %>%
    filter(n > 1) %>%
    mutate(hex = case_when(date_type == "date_created" ~ "#bebada",
                           date_type == "date_captured" ~ "#fb8072",
                           date_type == "date_approved" ~ "#80b1d3"),
           img = ifelse(date_label == "captured", "images", "image metadata records"),
           tooltip = sprintf("%s %s were %s in %s",
                             comma_format(accuracy = 1)(n),
                             img,
                             date_label,
                             paste(month(d_month, label = TRUE, abbr = FALSE), year(d_month))),
           url = paste0('https://digitalcollections.nypl.org/collections/',collection_uuid)
    ) %>%
    arrange(desc(n))
}

#### diu pies  ####

get_clicked_pie_df <- function(pie_df_ini, clicked_category, clicked_date, date_vars) {
  pie_df_ini %>%
    filter(between(get(c(clicked_category)), clicked_date, clicked_date)) %>%
    pivot_longer(cols = c(!!!date_vars), names_to = "date_type", values_to = "d_month")
}

get_pie <- function(df, this_date_type, clicked_category, clicked_date_str, diu_date_vars){
  # img <- ifelse(date_type == "captured", "images", "image metadata records"),
  rec_type <- case_when(this_date_type %in% diu_date_vars & this_date_type == "captured" ~ "images",
                        this_date_type %in% diu_date_vars & !this_date_type == "captured" ~ "image metadata records",
                        !this_date_type %in% diu_date_vars ~ "records")
  # print(rec_type)
  df %>% 
    filter(date_type == this_date_type) %>%
    count(d_month, date_type) %>%
    # arrange by size to apply colorscale, arrange by month later
    arrange(n) %>%
    mutate(perc = (n/sum(n))*100,
           tooltip = ifelse(!is.na(d_month),
                            sprintf("%s of these %s were %s in %s",
                             comma_format(accuracy = 1)(n),
                             rec_type,
                             date_type,
                             paste(month(d_month, label = TRUE, abbr = FALSE), year(d_month))),
                            sprintf("%s of these %s have not yet been %s",
                                    comma_format(accuracy = 1)(n),
                                    rec_type,
                                    date_type)
                            ),
           # only label slices greater than 5%
           month_label = case_when(perc > 5 & !is.na(d_month) ~ paste(month(d_month, label = TRUE, abbr = TRUE), year(d_month)),
                                   perc <= 5 & !is.na(d_month)~ "",
                                   is.na(d_month) ~ 'TBD'),
           type_click = clicked_category,
           month_click = clicked_date_str
    ) 
}

add_pie_hex <- function(df, date_vars, dark_cols, light_cols) {
  darkest_color <- case_when(df$date_type[1] == date_vars[[1]] ~ dark_cols[[1]],
                             df$date_type[1] == date_vars[[2]] ~ dark_cols[[2]],
                             df$date_type[1] == date_vars[[3]] ~ dark_cols[[3]])
  lightest_color <- case_when(df$date_type[1] == date_vars[[1]] ~ light_cols[[1]],
                              df$date_type[1] == date_vars[[2]] ~ light_cols[[2]],
                              df$date_type[1] == date_vars[[3]] ~ light_cols[[3]])
  colfunc <- colorRampPalette(c(lightest_color, darkest_color))
  return(colfunc(nrow(df)))
}

#### diu out ####
get_diu_for_dl <- function(diu_line,div_choice,date_vars=c('created','captured','approved')) {
  diu_line %>%
    filter(code %in% div_choice) %>%
    group_by(d_month) %>%
    summarise_at(vars(c(!!!date_vars)), sum, na.rm = TRUE) %>%
    rename(date = d_month)
}
