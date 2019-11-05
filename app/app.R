library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
# tufte theme
library(ggthemes)
library(shinydashboard)
# color scheme for dashboard
library(dashboardthemes)
# %$% pipe
library(magrittr)
# dates
library(lubridate)
# color palettes for plots
library(RColorBrewer)
# library(packcircles)
# loading bars
library(shinycssloaders)
# toggle
library(shinyjs)
# dropdown select all and styled checkboxes
library(shinyWidgets)
# tooltips for approvals bar
library(ggiraph)
# diu interactive
library(highcharter)
library(plotly)

tooltip_css <- "background-color:transparent;font-family:sans-serif;color:black;"

div_menu <- list(`LPA` = c("Billy Rose Theatre Division" = "THE",
                           "Jerome Robbins Dance Division" = "DAN",
                           "Music Division" = "MUS", 
                           "Rodgers and Hammerstein Archives of Recorded Sound" = "RHA"),
                 `SASB` = c("Berg Collection" = "BRG",
                            "Dorot Jewish Division" = "JWS",
                            "General Research Division" = "GRD",
                            "George Arents Collection" = "ARN",                 
                            "Manuscripts and Archives Division" = "MSS",
                            "Map Division" = "MAP",
                            "Milstein Division" = "LHG",                 
                            "NYPL Archives" = "NYPLA",
                            "Pforzheimer Collection" = "CPS",
                            "Rare Book Division" = "RBK",                 
                            "Slavic and East European Collections" = "HV",
                            "Spencer Collection" = "SPN",
                            "Wallach Division: Art & Architecture Collection" = "ART",
                            "Wallach Division: Photography Collection"= "PHG",
                            "Wallach Division: Picture Collection" = "MMPC",
                            "Wallach Division: Print Collection" = "PRN"),
                 `SCH` = c("Schomburg Art and Artifacts Division" = "SCF",
                           "Schomburg Jean Blackwell Hutson Research and Reference Division" = "SCR",
                           "Schomburg Manuscripts, Archives and Rare Books Division" = "SCM",
                           "Schomburg Moving Image and Recorded Sound Division" = "SCL",
                           "Schomburg Photographs and Prints Division" = "SCG"),
                 `SIBL` = c("SIBL: General collection"="BG")
)

div_choices <- c("Berg Collection" = "BRG",
                 "Billy Rose Theatre Division" = "THE",
                 "Dorot Jewish Division" = "JWS",
                 "General Research Division" = "GRD",
                 "George Arents Collection" = "ARN",
                 "Jerome Robbins Dance Division" = "DAN",
                 "Manuscripts and Archives Division" = "MSS",
                 "Map Division" = "MAP",
                 "Milstein Division" = "LHG",
                 "Music Division" = "MUS",
                 "NYPL Archives" = "NYPLA",
                 "Pforzheimer Collection" = "CPS",
                 "Rare Book Division" = "RBK",
                 "Rodgers and Hammerstein Archives of Recorded Sound" = "RHA",
                 "Schomburg Art and Artifacts Division" = "SCF",
                 "Schomburg Jean Blackwell Hutson Research and Reference Division" = "SCR",
                 "Schomburg Manuscripts, Archives and Rare Books Division" = "SCM",
                 "Schomburg Moving Image and Recorded Sound Division" = "SCL",
                 "Schomburg Photographs and Prints Division" = "SCG",
                 "SIBL: General collection" = "BG",
                 "Slavic and East European Collections" = "HV",
                 "Spencer Collection" = "SPN",
                 "Wallach Division: Art & Architecture Collection" = "ART",
                 "Wallach Division: Photography Collection"= "PHG",
                 "Wallach Division: Picture Collection" = "MMPC",
                 "Wallach Division: Print Collection" = "PRN")

ami_div_choices <- c("Jerome Robbins Dance Division" = "DAN",
                     "Manuscripts and Archives Division" = "MSS",
                     "Rodgers and Hammerstein Archives of Recorded Sound" = "RHA",
                     "Schomburg Moving Image and Recorded Sound Division" = "SCL")

approvals_date_opts <- c("All data" = "all_year",
                         "Current calendar year" = "cy",
                         "FY 2020" = "fy2020",
                         "FY 2019" = "fy2019")

vb_text <- function(big_text, sm_text) {
  HTML(paste(big_text, br(), "<span style = 'font-size: 11px'>",sm_text,"</span>"))
}

calc_element_scores <- function(mm, selected_divisions, report_time) {
  mm %>%
    filter(code %in% selected_divisions) %>%
    gather(element, score, title:location) %>%
    mutate(score = ifelse(score==0.75,0.50,score)) %>%
    group_by(element, score) %>%
    summarize(n = n()) %>%
    mutate(perc = round((n / sum(n))*100, digits = 2),
           report_time = as.Date(report_time))
}

mm <- readRDS(file = "./data/q1_2020_mm.rds")

approvals <- readRDS(file = "./data/q1_fy2020_approvals_items.rds")
diu <- readRDS(file = "./data/q1_fy2020_diu_cuuid.rds")

genre_rem <- readRDS(file = "./data/genre_up_q1_2020.rds")
date_rem <- readRDS(file = "./data/date_up_q1_2020.rds")
location_rem <- readRDS(file = "./data/location_up_q1_2020.rds")

logo_blue_gradient <- shinyDashboardLogoDIY(
  boldText = "MSU"
  ,mainText = "dashboard"
  ,textSize = 16
  ,badgeText = "BETA"
  ,badgeTextColor = "white"
  ,badgeTextSize = 2
  ,badgeBackColor = "#40E0D0"
  ,badgeBorderRadius = 3)

ui <- dashboardPage(title="MSU dashboard",
                    dashboardHeader(title = logo_blue_gradient,titleWidth = 250),
                    dashboardSidebar(width = 250, useShinyjs(), 
                                     sidebarMenu(id = "sidebar_menu",
                                                 menuItem("About", tabName = "overview", icon = icon("globe")),
                                                 menuItem("Metadata Production", tabName = "prod", icon = icon("calendar-check"), startExpanded = TRUE
                                                          ,menuSubItem("Approvals", tabName = "approvals", icon = icon("archive"))
                                                          ,menuSubItem("MSU/DIU in Context", tabName = "cap", icon = icon("camera"))
                                                 ),
                                                 menuItem("Metadata Quality", tabName = "mdsqual", icon = icon("medkit"), startExpanded = TRUE
                                                          ,menuSubItem("Overview of Scores", tabName = "mdsqual_prop", icon = icon("check-square"))
                                                          ,menuSubItem("Scores by Element", tabName = "e_scores", icon = icon("clipboard-list")))
                                                 ,menuItem("Reports (pdf)", tabName = "links", icon = icon("external-link-alt"))
                                                 ,awesomeRadio(
                                                   inputId = "chooseBy",
                                                   label = "Select...", 
                                                   choices = c("Everything","Centers", "Divisions"),
                                                   selected = "Everything",
                                                   # inline = TRUE, 
                                                   status = "primary"
                                                 )
                                                 ,conditionalPanel(condition = "input.chooseBy == 'Centers'"
                                                                   ,awesomeCheckboxGroup(
                                                                     inputId = "centers",
                                                                     label = "Research Library Centers", 
                                                                     choices = names(div_menu),
                                                                     selected = names(div_menu),
                                                                     status = "info"
                                                                   )
                                                 )
                                                 ,conditionalPanel(condition = "input.chooseBy == 'Divisions'"
                                                                   ,pickerInput(
                                                                     inputId = "divisions",
                                                                     label = "Research Library Divisions",
                                                                     choices = div_menu,
                                                                     selected = unname(div_choices),
                                                                     options = list(
                                                                       `actions-box` = TRUE,
                                                                       `selected-text-format` = "count > 2"
                                                                       ,`count-selected-text` = ("{0} divisions selected")),
                                                                     multiple = TRUE
                                                                   ))
                                     )),
                    dashboardBody(# hide errors
                      tags$style(type="text/css",
                                 ".shiny-output-error { visibility: hidden; }"
                                 ,".shiny-output-error:before { visibility: hidden; }"
                                 ,"text {font-family: sans-serif}"
                                 ,".crosshair {cursor: pointer;}"
                                 ,".small-box.bg-black { background-color: #238443 !important; color: #000000 !important; }"
                                 ,".small-box.bg-fuchsia { background-color: #41ab5d !important; color: #000000 !important; }"
                                 ,".small-box.bg-lime { background-color: #78c679 !important; color: #000000 !important; }"
                                 ,"-webkit-font-smoothing: antialiased;"
                                 ,"-webkit-filter: blur(0.000001px);"
                                 ,".legbox {display: inline; width: 10px; height: 10px; margin: 5px; border: 1px solid rgba(0, 0, 0, .2); }"
                                 ,".sasb {background: #fc8d62; color: #fc8d62;}"
                                 ,".lpa {background: #66c2a5; color: #66c2a5;}"
                                 ,".sch {background: #8da0cb; color: #8da0cb;}"
                                 ,".sibl {background: #e78ac3; color: #e78ac3;}"
                                 ,".a_caps {background: #8c96c6; color: #8c96c6;}"
                                 ,".a_items {background: #b3cde3; color: #b3cde3;}"
                      ),
                      tags$body(tags$div(id="ppitest", style="width:1in;visible:hidden;padding:0px")),
                      
                      tags$script('$(document).on("shiny:connected", function(e) {
                                  var w = window.innerWidth;
                                  var h = window.innerHeight;
                                  var d =  document.getElementById("ppitest").offsetWidth;
                                  var obj = {width: w, height: h, dpi: d};
                                  Shiny.onInputChange("pltChange", obj);
                                  });
                                  $(window).resize(function(e) {
                                  var w = $(this).width();
                                  var h = $(this).height();
                                  var d =  document.getElementById("ppitest").offsetWidth;
                                  var obj = {width: w, height: h, dpi: d};
                                  Shiny.onInputChange("pltChange", obj);
                                  });
                                  '),
                      shinyDashboardThemes(theme = "grey_light"),
                      tabItems(tabItem(tabName = "overview", 
                                       fluidRow(valueBoxOutput("n_apps", width = 3),valueBoxOutput("n_caps", width = 3),valueBoxOutput("n_ami", width = 3),valueBoxOutput("n_ami_caps", width = 3))
                                       # ,fluidRow(column(width=2),column(width=8,box(width=NULL
                                       #                                              ,plotlyOutput(outputId = "top_colls", height = "350px") %>% withSpinner(color="#0dc5c1")
                                       #                                              ,htmlOutput("collection_text",class = "p", style = "color: gray; font-size: 12px; font-family: Monospace;")
                                       #                                              ))
                                       #           ,column(width=2
                                       #                   ,box(width = NULL,includeHTML("center.html"))))
                                       ,column(width = 11
                                               ,fluidRow(box(title = span(HTML("<b>Metadata Services Unit dashboard</b>")), solidHeader = TRUE, includeHTML("about.html"), status = "primary", width=12)))
                                       ,fluidRow(box(
                                         HTML(paste("* Does not include items in the following divisions: EXTERNAL, EPE/CSW, CHILDCTR, PBL, RRS, NO_DIV, UNK",
                                                    br(), 
                                                    "&dagger; May include incorrectly classified items"))
                                         ,width=11, style = "color: gray; font-size: 10px; font-family: Monospace;"))
                      ),
                      tabItem(tabName = "approvals", 
                              valueBoxOutput("progressBox")
                              ,valueBoxOutput("capProgressBox")
                              ,fluidRow(
                                column(width = 9
                                       ,box(width = NULL, girafeOutput(outputId = "monthly_approvals", width = "100%")%>% withSpinner(color="#0dc5c1"))
                                )
                                ,column(width = 3
                                        ,box(width = NULL 
                                             ,radioButtons(inputId = "app_type", label = "Approval type:"
                                                           ,choices = c("All approvals"="all_apps", "AMI approvals"="ami_apps")
                                                           # , "Other resource types"="other_apps")
                                                           ,selected = "all_apps")
                                             ,radioButtons(inputId = "date_start", label = "Date range:"
                                                           ,choices = approvals_date_opts, selected = "all_year")
                                        )
                                        ,box(width = NULL
                                             ,HTML('<p><span class = "legbox a_items">XX</span>  Items</p><p><span class = "legbox a_caps">XX</span>  Captures</p>'))
                                )
                              )
                              ,fluidRow(box(includeHTML("approvals.html"), width=11))
                      ),
                      tabItem(
                        tabName = "cap", 
                        fluidRow(
                          column(width = 8
                                 ,fluidRow(box(width = NULL, highchartOutput(outputId = "hc_alt_plot")%>% withSpinner(color="#0dc5c1")))
                                 ,fluidRow(box(width = NULL, plotlyOutput(outputId = "coll_bars", width = "100%", height = "350px")))
                          ),
                          column(width = 4
                                 ,fluidRow(width = NULL, uiOutput("selected_text_hc_alt"))
                                 ,fluidRow(box(width = NULL, 
                                               plotlyOutput(outputId = "pie_app_hc_3", width = "100%", height = "350px")
                                 ))
                                 ,fluidRow(box(width = NULL, 
                                               plotlyOutput(outputId = "pie_app_hc_4", width = "100%", height = "350px")
                                 ))
                          ) # pie col
                        ), # plot row
                        fluidRow(box(includeHTML("imaging.html"), width=11)) # text row
                      ),
                      tabItem(tabName = "mdsqual_prop"
                              ,fluidRow(width=11
                                        ,valueBoxOutput("perc_above")
                                        ,valueBoxOutput("perc_below"))
                              ,fluidRow(width=NULL
                                        ,column(width = 9
                                                ,fluidRow(box(width = 12, girafeOutput(outputId = "prop_plot") %>% withSpinner(color="#0dc5c1")))
                                                ,fluidRow(box(width=12, includeHTML("mds_prop.html"))))
                                        ,column(width = 3
                                                ,fluidRow(box(width = NULL 
                                                              ,radioButtons(inputId = "compare_prop", label = "Compare:"
                                                                            ,choices = c("Divisions"="compare_divs", "Fiscal quarters"="compare_fq")
                                                                            ,selected = "compare_divs")
                                                              ,conditionalPanel(
                                                                condition = "input.compare_prop == 'compare_divs'"
                                                                ,radioButtons(inputId = "mds_qtr", label = "Fiscal quarter:"
                                                                              ,choices = c("First quarter, FY 2020"="fy2020_q1", 
                                                                                           "Fourth quarter, FY 2019"="fy2019_q4",
                                                                                           "Third quarter, FY 2019"="fy2019_q3")
                                                                              ,selected = "fy2020_q1")
                                                              )
                                                              
                                                )
                                                )
                                                ,fluidRow(box(title="Division code key", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                                              includeHTML("help_divs.html"), width=11))
                                        )
                              )
                      ),
                      tabItem(tabName = "e_scores"
                              ,fluidRow(box(width = NULL,title=HTML(paste("Remediation project highlights",br(),
                                                                          "<span style='font-size:12px'>July-September (first quarter), fiscal year 2020</span>"))
                                            ,valueBoxOutput("rem1")
                                            ,valueBoxOutput("rem2")
                                            ,valueBoxOutput("rem3")))
                              ,fluidRow(box(width=NULL,girafeOutput(outputId = "e_plot", width = "100%")%>% withSpinner(color="#0dc5c1")))
                              ,fluidRow(box(includeHTML("e_scores.html"), width=NULL))
                      )
                      ,tabItem(tabName = "links"
                               ,fluidRow(box(title="Quarterly reports (pdf)", status = "primary", solidHeader = TRUE
                                             ,selectInput(inputId = "reports", label = "Select a quarterly report: ",
                                                          choices = c("","First quarter, FY 2020"="fy2020_q1", 
                                                                      "Fourth quarter, FY 2019"="fy2019_q4", 
                                                                      "Third quarter, FY 2019"="fy2019_q3")
                                                          ,selected = "", multiple = FALSE)
                                             ,conditionalPanel(
                                               condition = "input.reports !== ''"
                                               ,downloadButton('downloadData', 'Download')
                                             )
                               )
                               )
                      )
                      )
                    ))

server <- function(input, output, session) {
  # download quarterly pdfs
  selected_report <- reactive({
    switch(input$reports, "fy2020_q1" = "www/MSU_FY20Q1.pdf", "fy2019_q4" = "www/MSU_endofFY19_report.pdf", "fy2019_q3" = "www/MSUquarterly_2019Jan-Mar.pdf")
  })
  output$downloadData <- downloadHandler(
    filename = "report.pdf",
    content = function(file) {
      file.copy(selected_report(), file)
    },
    contentType = "application/pdf"
  )
  
  # create var with div selected
  selected <- reactive({
    switch(input$chooseBy, "Everything" = unname(div_choices), "Centers" = input$centers, "Divisions" = input$divisions)
  })
  
  month_start <- reactive({
    switch(input$date_start, all_year = as.Date("2018-07-01"), cy = as.Date("2019-01-01"), fy2020 = as.Date("2019-07-01"), fy2019 = as.Date("2018-07-01"))
  })
  
  month_end <- reactive({
    switch(input$date_start, all_year = as.Date("2019-09-30"), cy = as.Date("2019-12-31"), fy2020 = as.Date("2020-06-30"), fy2019 = as.Date("2019-06-30"))
  })
  
  # what data to use to filter loaded data below (all divs, all divs in approvals incl NO_DIV, or just selected)
  div_choice <- reactive({
    if (length(selected()) == 0) {
      c()
    }
    else if (length(selected()) == 26 & input$sidebar_menu == "cap") {
      unique(diu$code)
    }
    else if (length(selected()) == 26 & input$sidebar_menu == "approvals") {
      unique(approvals$code)
    }
    else if (input$chooseBy == "Centers" & length(selected()) > 0) {
      centers <- c()
      if ("LPA" %in% selected()) {centers <- c(centers, unname(div_menu$LPA))}
      if ("SASB" %in% selected()) {centers <- c(centers, unname(div_menu$SASB))}
      if ("SCH" %in% selected()) {centers <- c(centers, unname(div_menu$SCH))}
      if ("SIBL" %in% selected()) {centers <- c(centers, unname(div_menu$SIBL))}
      return(centers)
    }
    else { c(selected()) }
  })
  
  # conditional hide/show ami tab & radiobuttons
  observeEvent(selected(), {
    toggleState("app_type", condition = any(div_choice() %in% ami_div_choices))
  }, ignoreInit = TRUE)
  
  # hide click plots on tab init
  observe({
    if (input$sidebar_menu == "cap") {
      hide("coll_bars")
      hide("selected_text_hc_alt")
      hide("pie_app_hc_3")
      hide("pie_app_hc_4")
    }
  })
  
  # update dropdown menu based on ami availability
  observe({
    if (input$sidebar_menu == "approvals" & input$app_type == "ami_apps") {
      updateSelectInput(session, "divisions",
                        choices = ami_div_choices,
                        selected = ifelse(any(div_choice() %in% ami_div_choices), div_choice(), "no_filter")
      )}
    else {
      updateSelectInput(session, "divisions",
                        choices = div_menu,
                        selected = div_choice())
    }
  })
  
  # how to filter for ami approvals
  ami_switch <- reactive({
    switch(input$app_type, all_apps = unique(approvals$ami), ami_apps = c("AMI"), other_apps = c("Not AMI"))
  })
  
  # how to filter for fiscal quarter
  fyq_switch <- reactive({
    switch(input$mds_qtr, fy2019_q3 = c("2019_q3"), fy2019_q4 = c("2019_q4"), fy2020_q1 = c("2020_q1"))
  })
  
  # text to display in approvals valuebox
  ami_text <- renderText({ 
    switch(input$app_type, all_apps = "", ami_apps = "AMI", other_apps = "other item")
  })
  
  # prep data: filter by division code (or not), reshape for each tab
  div_name <- reactive({
    if (input$sidebar_menu == "approvals"){
      approvals %>% 
        filter(code %in% div_choice()) %>%
        filter(ami %in% ami_switch()) %>%
        filter(between(d_month, month_start(), month_end())) %>%
        group_by(fy_year, fy_qy, d_month) %>%
        summarize(n_caps = sum(captures),
                  items = n()) %>%
        gather(key = "approval_type", value = "n_apps", n_caps, items) %>%
        mutate(tooltip = sprintf("%s %s in %s", 
                                 comma_format()(n_apps), 
                                 ifelse(approval_type == 'n_caps', 'captures', 'items'), 
                                 paste(month(d_month, label = TRUE, abbr = TRUE), year(d_month))))
    }
    else if (input$sidebar_menu == "cap"){
      diu %>%
        filter(code %in% div_choice()) %>%
        count(fy_year, d_month, date_type, date_label) %>%
        mutate(tooltip = sprintf("%s records %s in %s",
                                 comma_format()(n),
                                 date_label,
                                 paste(month(d_month, label = TRUE, abbr = FALSE), year(d_month)))) %>%
        filter(fy_year %in% c('FY 2019','FY 2020'), d_month < as.Date('2019-10-01'))
    }
    else if (input$sidebar_menu == "mdsqual_prop" & input$compare_prop == "compare_divs"){
      readRDS(file = "./data/q1_2020_mm_prop.rds")  %>%
        filter(code %in% div_choice()) %>% filter(fy_q %in% fyq_switch()) %>%
        mutate(tooltip = sprintf("%.0f%% of items in the %s division %s minimum mandatory requirements",
                                 values*100, code, ifelse(prop_type=="p_above", "meet", "are below"))
        )
    }
    else if (input$sidebar_menu == "mdsqual_prop" & input$compare_prop == "compare_fq") {
      readRDS(file = "./data/q1_2020_mm_prop.rds")  %>% 
        filter(code %in% div_choice()) %>%
        group_by(fy_q) %>%
        summarize(n_recs = sum(n_recs)/2,
                  n_above = sum(n_above)/2,
                  n_below = sum(n_below)/2,
                  p_above = n_above / n_recs,
                  p_below = n_below / n_recs) %>%
        gather(prop_type, values, p_above:p_below) %>%
        mutate(fy_label = paste0('FY',str_replace(fy_q,'_q',' Q')),
               tooltip = sprintf("%.0f%% of items met minimum mandatory requirements in %s",
                                 values*100, fy_label)
        )
    }
    else if (input$sidebar_menu == "overview"){
      mm %>% filter(code %in% div_choice())
    }
    else if (input$sidebar_menu == "e_scores"){
      if (length(div_choice()) == 0 | length(div_choice()) == 26) {
        readRDS(file = "./data/q1_2020_count_sum.rds") %>%
          mutate(tooltip = sprintf("%.0f%% of %s elements had a %s score in %s",
                                   perc, element, case_when(score == 1 ~ "passing",
                                                            score == 0 ~ "failing",
                                                            score == 0.50 ~ 'partially passing',
                                                            score == 0.75 ~ 'partially passing'), quarter_lab)
          )
      }
      else {
        calc_element_scores(readRDS(file = "./data/q1_2020_minmand_select.rds"), div_choice(), "2019/09/01") %>%
          bind_rows(calc_element_scores(readRDS(file = "./data/q3_minmand_select.rds"), div_choice(), "2019/03/01")) %>%
          bind_rows(calc_element_scores(readRDS(file = "./data/q4_minmand_select.rds"), div_choice(), "2019/06/01")) %>%
          mutate(ele_order = case_when(element == 'title' ~ 1,
                                       element == 'typeOfResource' ~ 2,
                                       element == 'identifier' ~ 3,
                                       element == 'genre' ~ 4,
                                       element == 'date' ~ 5,
                                       element == 'location' ~ 6),
                 score_order = case_when(score == 0 ~ '0',
                                         score == 0.50 ~ '0.50',
                                         score == 0.75 ~ '0.75',
                                         score == 1 ~ '1.00'),
                 score_order = factor(score_order, levels=c('1.00','0.75','0.50','0')),
                 quarter_lab = case_when(month(report_time) == 3 ~ paste('Mar',as.character(year(report_time))),
                                         month(report_time) == 6 ~ paste('Jun',as.character(year(report_time))),
                                         month(report_time) == 9 ~ paste('Sep',as.character(year(report_time))),
                                         month(report_time) == 12 ~ paste('Dec',as.character(year(report_time)))),
                 tooltip = sprintf("%.0f%% of %s elements had a %s score in %s",
                                   perc, element, case_when(score == 1 ~ "passing",
                                                            score == 0 ~ "failing",
                                                            score == 0.50 ~ 'partially passing',
                                                            score == 0.75 ~ 'partially passing'), quarter_lab)) 
      }
    }
  })
  
  ### VALUE BOXES ###
  output$n_apps <- renderValueBox({
    valueBox(
      comma_format()(nrow(div_name())), vb_text("Approved items in MMS*","As of September 30, 2019"), icon = icon("book"),
      color = "aqua"
    )
  })
  
  output$n_caps <- renderValueBox({
    valueBox(
      comma_format()(sum(div_name()$captures)), vb_text("Approved captures in MMS*","As of September 30, 2019"), icon = icon("eye"),
      color = "blue"
    )
  })
  
  output$n_ami <- renderValueBox({
    valueBox(
      comma_format()(nrow(div_name() %>% filter(ami == "ami"))), vb_text("Approved AMI items in MMS*&dagger;","As of September 30, 2019"), icon = icon("film"),
      color = "teal"
    )
  })
  
  output$n_ami_caps <- renderValueBox({
    valueBox(
      comma_format()(sum(div_name() %>% filter(ami == "ami") %$% captures)), vb_text("Approved AMI captures in MMS*&dagger;","As of September 30, 2019"), icon = icon("headphones"),
      color = "light-blue"
    )
  })
  
  output$progressBox <- renderValueBox({
    valueBox(
      comma_format()(sum(div_name() %>% filter(approval_type == "items") %$% n_apps)), vb_text(paste(ami_text()," Item approvals"), ""
                                                                                               # ,"July 1, 2018 - September 30, 2019"
      ), icon = icon("thumbs-up", lib = "glyphicon"),
      # comma_format()(sum(div_name() %>% filter(approval_type == "items") %$% n_apps)), vb_text(paste("Fiscal year 2019 ",ami_text()," item approvals"),"July 1, 2018 - June 30, 2019"), icon = icon("thumbs-up", lib = "glyphicon"),
      color = "light-blue"
    )
  })
  
  output$capProgressBox <- renderValueBox({
    valueBox(
      comma_format()(sum(div_name() %>% filter(approval_type == "n_caps") %$% n_apps)), vb_text(paste(ami_text()," Capture approvals"), ""
                                                                                                # ,"July 1, 2018 - September 30, 2019"
      ), icon = icon("thumbs-up", lib = "glyphicon"),
      # comma_format()(sum(div_name() %>% filter(approval_type == "n_caps") %$% n_apps)), vb_text(paste("Fiscal year 2019 ",ami_text()," capture approvals"),"July 1, 2018 - June 30, 2019"), icon = icon("thumbs-up", lib = "glyphicon"),
      color = "purple"
    )
  })
  
  output$perc_above <- renderValueBox({
    if (input$sidebar_menu == "mdsqual_prop" & input$compare_prop == "compare_divs"){
      valueBox(
        paste0(round(sum(div_name() %>% filter(prop_type == "p_above") %$% n_above)/sum(div_name() %>% filter(prop_type == "p_above") %$% n_recs),3)*100, "%"), 
        "Meet minimum mandatory requirements", icon = icon("arrow-up", lib = "glyphicon"), color = "green"
      )
    }
    else if (input$sidebar_menu == "mdsqual_prop" & input$compare_prop == "compare_fq") {
      valueBox(
        paste0(round(sum(div_name() %>% filter(prop_type == "p_above", fy_label == "FY2020 Q1") %$% n_above)/sum(div_name() %>% filter(prop_type == "p_above", fy_label == "FY2020 Q1") %$% n_recs),3)*100, "%"), 
        "Meet minimum mandatory requirements", icon = icon("arrow-up", lib = "glyphicon"), color = "green"
      )
    }
    
  })
  
  output$perc_below <- renderValueBox({
    if (input$sidebar_menu == "mdsqual_prop" & input$compare_prop == "compare_divs"){
      valueBox(
        paste0(round(sum(div_name() %>% filter(prop_type == "p_below") %$% n_below)/sum(div_name() %>% filter(prop_type == "p_below") %$% n_recs),3)*100, "%"), 
        "Below minimum mandatory requirements", icon = icon("arrow-down", lib = "glyphicon"), color = "orange"
      )
    }
    else if (input$sidebar_menu == "mdsqual_prop" & input$compare_prop == "compare_fq") {
      valueBox(
        paste0(round(sum(div_name() %>% filter(prop_type == "p_below", fy_label == "FY2020 Q1") %$% n_below)/sum(div_name() %>% filter(prop_type == "p_below", fy_label == "FY2020 Q1") %$% n_recs),3)*100, "%"), 
        "Below minimum mandatory requirements", icon = icon("arrow-down", lib = "glyphicon"), color = "orange"
      )
    }
  })
  
  output$rem1 <- renderValueBox({
    num_rem <- reactive({
      if (length(selected()) == 26 | length(selected()) == 0) {
        comma_format()(nrow(genre_rem))
      } else {
        comma_format()(nrow(genre_rem %>% filter(code %in% div_choice())))
      }
    })
    valueBox(num_rem(), "Items had at least one genre term remediated", icon = icon("broom"), color = "lime"
    )
  })
  
  output$rem2 <- renderValueBox({
    num_rem <- reactive({
      if (length(selected()) == 26 | length(selected()) == 0) {
        comma_format()(nrow(date_rem))
      } else {
        comma_format()(nrow(date_rem %>% filter(code %in% div_choice())))
      }
    })
    valueBox(num_rem(), "Items had at least one date remediated", icon = icon("calendar-plus"), color = "fuchsia")
  })
  
  output$rem3 <- renderValueBox({
    num_rem <- reactive({
      if (length(selected()) == 26 | length(selected()) == 0) {
        comma_format()(nrow(location_rem))
      } else {
        comma_format()(nrow(location_rem %>% filter(code %in% div_choice())))
      }
    })
    valueBox(num_rem(), "Items had at least one location remediated", icon = icon("globe"), color = "black")
  })
  ### VALUE BOXES ###
  
  ### PLOTS ###
  # reactive width for prop plot using num divisions selected
  prop_width <- reactive({
    if (input$compare_prop == "compare_divs" & length(div_choice()) > 6){
      (0.8*input$pltChange$width/input$pltChange$dpi)
    }
    else if (input$compare_prop == "compare_divs" & length(div_choice()) <= 6){
      4
    }
  })
  
  prop_height <- reactive({
    if (input$compare_prop == "compare_divs" & length(div_choice()) > 6){
      5
    }
    else if (input$compare_prop == "compare_divs" & length(div_choice()) <= 6){
      3
    }
  })
  
  text_size <- reactive({
    if (input$compare_prop == "compare_divs" & length(div_choice()) > 6){
      10
    }
    else if (input$compare_prop == "compare_divs" & length(div_choice()) <= 6){
      5
    }
  })
  
  output$prop_plot <- renderGirafe({
    if (input$sidebar_menu == "mdsqual_prop" & input$compare_prop == "compare_divs"){
      girafe(ggobj = ggplot(div_name(), aes(reorder(LAB, values), tooltip = tooltip)) + 
               geom_bar_interactive(data=. %>% filter(prop_type=="p_above"), aes(y = values), fill = '#99d594', stat = "identity", width = 0.7) +
               geom_bar_interactive(data=. %>% filter(prop_type=="p_below"), aes(y = -values), fill = '#fc8d59', stat = "identity", width = 0.7) +
               geom_hline(aes(yintercept = 0), color = "grey") + 
               scale_y_continuous(labels=function(x) paste0(abs(x)*100, "%"), limits = c(-1,1)) +
               labs(x = "", y = "", title = "") +
               facet_grid(. ~ center, scales = "free_x", space = "free_x") +
               theme_tufte(base_family='sans') +
               theme(strip.background = element_rect(fill="lightgray", color = "lightgray")
                     ,strip.text.x = element_text(size = text_size())
                     ,axis.text.x = element_text(size=text_size())
                     ,panel.border = element_rect(size = 3, colour = "lightgray", fill = NA)
                     ,axis.text.y = element_text(size=text_size())),
             width_svg = prop_width()
             # width_svg = (0.8*input$pltChange$width/input$pltChange$dpi)
             ,height_svg = prop_height()
             # ,height_svg = (0.7*input$pltChange$height/input$pltChange$dpi)
      ) %>%
        girafe_options(opts_tooltip(css = tooltip_css,use_fill = TRUE))
    }
    else if (input$sidebar_menu == "mdsqual_prop" & input$compare_prop == "compare_fq") {
      girafe(ggobj = ggplot(div_name()) + 
               geom_line(data=. %>% filter(prop_type=="p_above"), aes(x = fy_label, y = values, group = 1), color = '#99d594', size=1) +
               geom_point_interactive(data=. %>% filter(prop_type=="p_above"), aes(x = fy_label, y = values, tooltip = tooltip), color = '#99d594', fill = '#99d594', size=1) +
               scale_y_continuous(labels=function(x) paste0(abs(x)*100, "%"), limits = c(0,1)) +
               labs(x = "", y = "", title = "") +
               theme_tufte(base_family='sans') +
               theme(strip.background = element_rect(fill="lightgray", color = "lightgray")
                     ,strip.text.x = element_text(size = 4)
                     ,axis.text.x = element_text(size=4)
                     ,panel.border = element_rect(size = 3, colour = "lightgray", fill = NA)
                     ,axis.text.y = element_text(size=4))
             ,width_svg = 4
             ,height_svg = 2
      ) %>%
        girafe_options(opts_tooltip(css = tooltip_css,use_fill = TRUE))
    }
    
  })
  
  sm_bu_pu = brewer.pal(n = 4, "BuPu")[2:4]
  output$monthly_approvals <- renderGirafe({
    p <- ggplot(div_name(), aes(d_month, n_apps, fill=approval_type, tooltip = tooltip)) +
      geom_bar_interactive(stat = 'identity', position = position_dodge2(padding = 0, reverse = FALSE)) +
      scale_x_date(date_labels = "%b %Y", date_breaks="month", expand = c(0.03,0.03)) +
      scale_y_continuous(labels = comma) +
      scale_fill_manual(values=sm_bu_pu) +
      facet_grid(. ~ fy_year, scales = "free_x", space = "free_x", labeller = as_labeller(c(`2019` = "FY 2019", `2020` = "FY 2020"))) +
      theme_tufte(base_family='sans') +
      theme(axis.title.x=element_blank()
            ,axis.title.y=element_blank()
            ,axis.text.y = element_text(size=10)
            ,axis.text.x = element_text(size=10)
            ,legend.position = "none"
            ,strip.background = element_rect(fill="lightgray", color = "lightgray")
            ,panel.border = element_rect(size = 3, colour = "lightgray", fill = NA)
            ,strip.text.x = element_text(size = 10))
    
    girafe(ggobj = p,
           width_svg = (0.8*input$pltChange$width/input$pltChange$dpi)
           # ,height_svg = (0.6*input$pltChange$height/input$pltChange$dpi)
           ) %>%
      girafe_options(opts_tooltip(css = tooltip_css,use_fill = TRUE))
  })
  
  coll_name <- reactive({
    diu %>%
      filter(code %in% div_choice()) %>%
      count(fy_year, d_month, date_type, date_label, collection_id, collection_title, collection_uuid, code) %>%
      filter(fy_year %in% c('FY 2019','FY 2020'), d_month < as.Date('2019-10-01')) %>%
      drop_na(collection_title)
  })
  
  output$hc_alt_plot <- renderHighchart({
    canvasClickFunction <- JS("function(event) {Shiny.onInputChange('altClicked', [this.name, event.point.category, event.point.y]);}")
    time_value <- div_name() %>%
      select(d_month, date_label, n)  %>%
      pivot_wider(names_from = date_label, values_from = n)
    highchart() %>%
      hc_xAxis(type = "datetime", dateTimeLabelFormats = list(month = "%b %Y")) %>%
      hc_plotOptions(series = list(events = list(click = canvasClickFunction))) %>%
      hc_add_series(time_value, "line", name = "created", hcaes(d_month, created), color = "#bebada", marker = list(symbol = "circle")) %>%
      hc_add_series(time_value, "line", name = "captured", hcaes(d_month, captured), color = "#fb8072", marker = list(symbol = "circle")) %>%
      hc_add_series(time_value, "line", name = "approved", hcaes(d_month, approved), color = "#80b1d3", marker = list(symbol = "circle")) %>%
      hc_add_theme(hc_theme_google())
  })
  
  observeEvent(input$altClicked, {
    # print(input$altClicked)
    clicked_y <- comma_format()(as.numeric(input$altClicked[3]))
    clicked_date_str <- strftime(as.Date(as.POSIXct(as.numeric(input$altClicked[2])/1000, origin="1970-01-01")), "%B %Y")
    clicked_date <- strftime(as.Date(as.POSIXct(as.numeric(input$altClicked[2])/1000, origin="1970-01-01")), "%Y-%m-%d") # type chr
    clicked_category <- input$altClicked[1]
    clicked_month_label <- paste0(clicked_category,'_month')
    clicked_date_label <- paste0('date_',clicked_category)
    
    withProgress(message = "Computing results", detail = "fetching data", value = 0, {
      
      incProgress(0.25, detail = "wrangling data for this point")
      
      subDN <- readRDS(file = "./data/q1_fy2020_diu_for_pie.rds") %>%
        filter(code %in% div_choice()) %>%
        filter(between(get(c(clicked_month_label)), as.Date(clicked_date), as.Date(clicked_date))) %>%
        pivot_longer(cols = c(date_created, date_captured, date_approved), names_to = "date_type", values_to = "date")  %>%
        # draft items have no approval date
        drop_na(date) %>%
        mutate(d_month = as.Date(cut(date, breaks="month")),
               month_txt = month(d_month, label = TRUE, abbr = FALSE),
               fy_quarter = if_else(between(month(date),7,9),'Q1',
                                    if_else(between(month(date),10,12),'Q2',
                                            if_else(between(month(date),1,3),'Q3','Q4'))),
               fy_year = if_else(between(month(date),7,12),paste('FY',as.character(year(date)+1)),paste('FY',as.character(year(date)))),
               fy_qy = paste0('FY',as.character(fy_year),' ',fy_quarter),
               date_label = str_remove(date_type, "date_"))
      other_dts <- setdiff(unique(subDN$date_type),c(clicked_date_label))
      
      first_half <- subDN %>%
        filter(date_type == other_dts[[1]]) %>%
        count(fy_year, d_month, date_type, date_label) %>%
        arrange(n) %>%
        mutate(perc = (n/sum(n))*100,
               img = ifelse(date_label == "captured", "images", "image metadata records"),
               tooltip = sprintf("%s of these %s were %s in %s",
                                 comma_format()(n),
                                 img,
                                 date_label,
                                 paste(month(d_month, label = TRUE, abbr = FALSE), year(d_month))),
               # only label slices greater than 5%
               month_label = ifelse(perc > 5, paste(month(d_month, label = TRUE, abbr = TRUE), year(d_month)),""),
               darkest_color = case_when(date_type == "date_created" ~ "#9894ae",date_type == "date_captured" ~ "#e17366",date_type == "date_approved" ~ "#80b1d3"),
               lightest_color = case_when(date_type == "date_created" ~ "#f4f4f6",date_type == "date_captured" ~ "#fcf1ef",date_type == "date_approved" ~ "#f2f7fa")
        ) 
      colfunc <- colorRampPalette(c(first_half$lightest_color[[1]], first_half$darkest_color[[1]]))
      
      second_half <- subDN %>%
        filter(date_type == other_dts[[2]]) %>%
        count(fy_year, d_month, date_type, date_label) %>%
        arrange(n) %>%
        mutate(perc = (n/sum(n))*100,
               img = ifelse(date_label == "captured", "images", "image metadata records"),
               tooltip = sprintf("%s of these %s were %s in %s",
                                 comma_format()(n),
                                 img,
                                 date_label,
                                 paste(month(d_month, label = TRUE, abbr = FALSE), year(d_month))),
               # only label slices greater than 5%
               month_label = ifelse(perc > 5, paste(month(d_month, label = TRUE, abbr = TRUE), year(d_month)),""),
               darkest_color = case_when(date_type == "date_created" ~ "#9894ae",date_type == "date_captured" ~ "#e17366",date_type == "date_approved" ~ "#80b1d3"),
               lightest_color = case_when(date_type == "date_created" ~ "#f4f4f6",date_type == "date_captured" ~ "#fcf1ef",date_type == "date_approved" ~ "#f2f7fa")
        ) 
      colfunc2 <- colorRampPalette(c(second_half$lightest_color[[1]], second_half$darkest_color[[1]]))
      
      incProgress(0.25, detail = "gathering collections")
      
      top_colls <- coll_name() %>% 
        filter(date_label == clicked_category, d_month == as.Date(clicked_date)) %>%
        top_n(10, n) %>%
        mutate(hex = case_when(date_type == "date_created" ~ "#bebada",
                               date_type == "date_captured" ~ "#fb8072",
                               date_type == "date_approved" ~ "#80b1d3"),
               img = ifelse(date_label == "captured", "images", "image metadata records"),
               tooltip = sprintf("%s %s were %s in %s",
                                 comma_format()(n),
                                 img,
                                 date_label,
                                 paste(month(d_month, label = TRUE, abbr = FALSE), year(d_month))),
               link = paste0('<a href="https://digitalcollections.nypl.org/collections/',collection_uuid,'">',collection_title,'</a>')
        )
      top_colls$collection_title <- factor(top_colls$collection_title, levels = unique(top_colls$collection_title)[order(top_colls$n, decreasing = FALSE)])
      # print(top_colls$link)
      
      incProgress(0.25, detail = "generating plots")
      
      output$selected_text_hc_alt <- renderUI({
        img <- ifelse(clicked_category == "captured", " images ", " image metadata records ")
        tagList(
          HTML(paste0("<b>Of the ",clicked_y,img,clicked_category," in ",clicked_date_str,"...</b>")),
          tags$style("#selected_text_hc_alt {background-color:#f8f8f8;}")
        )
      })
      
      output$pie_app_hc_3 <- renderPlotly({
        p <- plot_ly(first_half, labels = ~month_label, values = ~n
                     ,title = paste0('<b>',first_half$date_label[[1]],'</b>')
                     ,marker = list(colors = colfunc(nrow(first_half)))
                     ,textposition="outside"
                     ,hoverinfo = 'text'
                     ,hovertext =  stringr::str_wrap(string = first_half$tooltip, width = 20,
                                                     indent = 1, # let's add extra space from the margins
                                                     exdent = 1  # let's add extra space from the margins
                     )
                     ,text = first_half$month_label
                     ,textinfo = "text"
                     ,rotation = -100
        ) %>%
          add_pie(hole = 0.6) %>%
          layout(showlegend = F,
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
                 ,dragmode =  "zoom"
                 ,margin = list(b=50,l=50,r=50,t=50,pad=200))
        ggplotly(p) %>%
          config(displayModeBar = F)
      })
      
      output$pie_app_hc_4 <- renderPlotly({
        p <- plot_ly(second_half, labels = ~month_label, values = ~n
                     ,title = paste0('<b>',second_half$date_label[[1]],'</b>')
                     ,marker = list(colors = colfunc2(nrow(second_half)))
                     ,textposition="outside"
                     ,hoverinfo = 'text'
                     ,hovertext =  stringr::str_wrap(string = second_half$tooltip, width = 20,
                                                     indent = 1, # let's add extra space from the margins
                                                     exdent = 1  # let's add extra space from the margins
                     )
                     ,text = second_half$month_label
                     ,textinfo = "text"
                     ,rotation = -100
        ) %>%
          add_pie(hole = 0.6) %>%
          layout(showlegend = F,
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
                 ,dragmode =  "zoom"
                 ,margin = list(b=50,l=50,r=50,t=50,pad=200))
        ggplotly(p) %>%
          config(displayModeBar = F)
      })
      
      output$coll_bars <- renderPlotly({
        # print(top_colls$link)
        plot_ly(top_colls, x= ~n, y = ~collection_title
                ,type = "bar"
                ,orientation = 'h'
                ,marker = list(color=top_colls$hex)
                ,text = top_colls$link
                ,textposition="auto"
                ,cliponaxis = FALSE
                ,hoverinfo = 'text'
                ,hovertext = stringr::str_wrap(string = top_colls$tooltip, width = 70,
                                               indent = 1, # let's add extra space from the margins
                                               exdent = 1  # let's add extra space from the margins
                )
        ) %>%
          layout(showlegend = F
                 ,title = paste0('<b>Top collections ',top_colls$date_label[[1]],' in '
                                 ,paste(month(top_colls$d_month[[1]], label = TRUE, abbr = FALSE), year(top_colls$d_month[[1]])),'</b>')
                 ,xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
                 ,yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
                 ,margin = list(b=10,l=10,r=10,t=35,pad=0)
          ) %>%
          config(displayModeBar = F)
      })
      
      show("selected_text_hc_alt")
      show("coll_bars")
      show("pie_app_hc_3")
      show("pie_app_hc_4")
      
    })
  })

  output$e_plot <- renderGirafe({
    p <- ggplot(div_name(), aes(x = reorder(quarter_lab, month(report_time)), y = perc, fill=score_order, tooltip = tooltip)) +
      geom_bar_interactive(stat = 'identity', position = "stack", width=0.9) +
      scale_fill_manual(values = c("#91cf60", "#fee08b", "#fc8d59")) +
      scale_y_continuous(labels=function(x) paste0(x, "%")) +
      theme_minimal() +
      labs(x = "", y = "", title = "", fill="Element Score") +
      facet_wrap( ~ reorder(element,ele_order), strip.position="bottom", nrow = 1) +
      guides(fill = guide_legend(override.aes = list(size = 0.5))) +
      theme(strip.placement = "outside"
            ,strip.text.x = element_text(size=10.5)
            ,axis.text.x = element_text(size=9.5)
            ,axis.text.y = element_text(size=10)
            ,legend.title = element_text(size = 9.5)
            ,legend.text = element_text(size = 10))
    
    girafe(ggobj = p,
           width_svg = (0.9*input$pltChange$width/input$pltChange$dpi)
           # ,height_svg = (0.6*input$pltChange$height/input$pltChange$dpi)
           ) %>%
      girafe_options(opts_tooltip(css = tooltip_css,use_fill = TRUE))
  })
  
}

shinyApp(ui = ui, server = server)