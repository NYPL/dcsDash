vb_text <- function(big_text, sm_text) {
  HTML(paste(big_text, br(), "<span style = 'font-size: 11px'>",sm_text,"</span>"))
}

num_remediated <- function(rem_df, selected_divisions){
  # print(length(selected_divisions))
  if (length(selected_divisions) == 26 | length(selected_divisions) == 0) {
    return(comma_format()(sum(rem_df$n)))
  } else {
    return(comma_format()(sum(rem_df %>% filter(code %in% selected_divisions) %$% n)))
  }
}

get_eoq_text <- function(q_string) { 
  if(startsWith(q_string, 'F')) {
    paste0("As of ",q_string)
  } else {
    q_to_date <- zoo::as.Date(zoo::as.yearqtr(q_string, format = "%Y_q%q") - 1/2, frac = 1)
    paste0("As of ", month(q_to_date,label=TRUE, abbr=FALSE)," ",day(q_to_date),", ",year(q_to_date))
  }
}

vb_ui <- function(id,width=3) {
	valueBoxOutput(id,width)
}

# vbs <- purrr::map(c("n_apps", "n_caps", "n_ami", "n_ami_caps"), vb_ui, width=4)