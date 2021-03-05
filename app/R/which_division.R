which_division <- function(sidebar_menu, chooseBy, in_centers, in_divisions, diu_divs, approvals, div_choices, div_menu){
  if (sidebar_menu != "rightsBD"){
    if (chooseBy == "Everything") {
      if (sidebar_menu == "cap") {
        diu_divs
      }
      else if (sidebar_menu == "approvals") {
        unique(approvals$code)
      } else {
        unname(div_choices) 
      }
    }
    else if (chooseBy == "Centers"){
      if (length(in_centers) == 0) {
        c()
      }
      else if (length(in_centers) == 4 & sidebar_menu == "cap") {
        diu_divs
      }
      else if (length(in_centers) == 4 & sidebar_menu == "approvals") {
        unique(approvals$code)
      }
      else {
        centers <- c()
        if ("LPA" %in% in_centers) {centers <- c(centers, unname(div_menu$LPA))}
        if ("SASB" %in% in_centers) {centers <- c(centers, unname(div_menu$SASB))}
        if ("SCH" %in% in_centers) {centers <- c(centers, unname(div_menu$SCH))}
        if ("SIBL" %in% in_centers) {centers <- c(centers, unname(div_menu$SIBL))}
        return(centers)
      }
    }
    else if (chooseBy == "Divisions"){
      if (length(in_divisions) == 0) {
        c()
      }
      else if (length(in_divisions) == 26 & sidebar_menu == "cap") {
        diu_divs
      }
      else if (length(in_divisions) == 26 & sidebar_menu == "approvals") {
        unique(approvals$code)
      }
      else {
        return(in_divisions)
      }
    }
  }
}