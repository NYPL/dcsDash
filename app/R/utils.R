library(shiny)
library(tidyr)
library(dplyr)
library(scales)
library(stringr)
library(shinydashboard)
# %$% pipe
library(magrittr)
# dates
library(lubridate)
# hide/show html elements
library(shinyjs)
# interactive plots
library(highcharter)
library(googledrive)
library(readr)

#### vars and fxns ####
# prevent csv read from printing info
options(readr.num_columns = 0)

this_qtr <- 'f21q3'

elements_by_division_file <- paste0("./data/",this_qtr,"_element_by_div.csv")
last_updated <- as.Date(file.info(elements_by_division_file)$mtime)
eoq_elements <- rollback(floor_date(last_updated, "quarter"))
end_of_quarter_elements <- paste0(month(eoq_elements,label=TRUE, abbr=TRUE)," ",str_sub(year(eoq_elements),start=-2))
elements_bar_label <- JS(paste0("function () { return 'Mar 19  |  ",end_of_quarter_elements,"' + '<br>' +  this.value + '</br>'; }"))

# load data ---------------------------------------------------------------
minmand_totals <- readRDS(file = paste0("./data/",this_qtr,"_mm.rds"))
mm_prop_path <- paste0("./data/",this_qtr,"_mm_prop.rds")
mm_prop <- readRDS(mm_prop_path)
ebd_all_path <- paste0("./data/",this_qtr,"_count_sum.csv")
mm_vb_elements <- c('genre','date','id','location')
mm_vb_files <- paste0("./data/",mm_vb_elements,'_up_',this_qtr,".rds")

genre_path <- mm_vb_files[[1]] #"./data/genre_up_f21q1.rds"
date_path <- mm_vb_files[[2]] #"./data/date_up_f21q1.rds"
id_path <- mm_vb_files[[3]] #"./data/id_up_f21q1.rds"
loc_path <- mm_vb_files[[4]] #"./data/location_up_f21q1.rds"

get_catalogued_or_all <- function(cat_check, filepath) {
  fp <- paste0('.',str_split(filepath,'[.]')[[1]][2], '_cat.', str_split(filepath,'[.]')[[1]][3])
  # print(fp)
  # print(str_split(filepath,'[.]')[[1]])
  if_else(cat_check == FALSE, filepath, fp)
}

trigger_data_load <- function(buttonId, buttonValue, local_path, gd_path, session, mainPlot=FALSE) {
  # print(buttonValue)
  if(buttonValue == 0) {
    load_or_refresh_data(local_path, gd_path, refresh = FALSE)
  } else if (mainPlot == TRUE) {
    updateActionButton(session, buttonId, label = "Data is up-to-date")
    shinyjs::disable(buttonId)
    load_or_refresh_data(local_path, gd_path, refresh = TRUE)
  } else {
    load_or_refresh_data(local_path, gd_path, refresh = TRUE)
  }
}

load_or_refresh_data <- function(local_path, gd_path, refresh = FALSE, key = FALSE) {
  if (key == TRUE) {
    drive_auth(cache = ".secrets", email = readRDS("./data/email.rds"))
    # drive_auth(path = "./data/gd_secret.json")
    drive_download(drive_get(id = readRDS("./data/gd_key.rds")),local_path, overwrite = TRUE)
    vroom::vroom(local_path)
  } else if (refresh == TRUE) {
    drive_auth(path = "./data/gd_secret.json")
    drive_download(drive_get(gd_path),local_path, overwrite = TRUE)
    vroom::vroom(local_path, delim = "\t")
  } else {
    # print(local_path)
    vroom::vroom(local_path, delim = "\t")
  }
}

# CSS/Menus ---------------------------------------------------------------

css_list <- list(".shiny-output-error { visibility: hidden; }"
                 ,".shiny-output-error:before { visibility: hidden; }"
                 ,"text {font-family: sans-serif}"
                 ,".crosshair {cursor: pointer;}"
                 ,".small-box.bg-maroon { background-color: #1b6533 !important; color: #000000 !important; }"
                 ,".small-box.bg-black { background-color: #238443 !important; color: #000000 !important; }"
                 ,".small-box.bg-fuchsia { background-color: #41ab5d !important; color: #000000 !important; }"
                 ,".small-box.bg-lime { background-color: #78c679 !important; color: #000000 !important; }"
                 ,"-webkit-font-smoothing: antialiased;"
                 ,"-webkit-filter: blur(0.000001px);")

logo_blue_gradient <- dashboardthemes::shinyDashboardLogoDIY(
  boldText = "DCS"
  ,mainText = "dashboard"
  ,textSize = 20
  ,badgeText = ""
  ,badgeTextColor = ""
  ,badgeTextSize = 0
  ,badgeBackColor = ""
  ,badgeBorderRadius = 0
  )

div_menu <- readRDS('./data/div_menu.rds') 
div_menu$SASB <- div_menu$SASB[div_menu$SASB != "HV"]

div_choices <- readRDS('./data/div_choices.rds') 
div_choices <- div_choices[div_choices != "HV"]; # without elements that are "HV"

ami_div_choices <- c("Jerome Robbins Dance Division" = "DAN",
                     "Manuscripts and Archives Division" = "MSS",
                     "Rodgers and Hammerstein Archives of Recorded Sound" = "RHA",
                     "Schomburg Moving Image and Recorded Sound Division" = "SCL")

pami_div_choices <- c("Jerome Robbins Dance Division" = "DAN",
                     "Manuscripts and Archives Division" = "MSS",
                     "Music Division" = "MUS",
                     "Rodgers and Hammerstein Archives of Recorded Sound" = "RHA",
                     "Schomburg Moving Image and Recorded Sound Division" = "SCL")
