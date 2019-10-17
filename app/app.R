library(shiny)
library(tidyverse)
library(plotly)
library(scales)
library(ggthemes)
library(shinydashboard)
library(dashboardthemes)
library(magrittr)
library(lubridate)
library(RColorBrewer)
library(packcircles)
library(shinycssloaders)
library(shinyjs)
library(shinyWidgets)
# library(plotly)
library(ggiraph)

dropdownButton <- function(label = "", status = c("default", "primary", "success", "info", "warning", "danger"), ..., width = NULL) {
  
  status <- match.arg(status)
  # dropdown button content
  html_ul <- list(
    class = "dropdown-menu",
    style = if (!is.null(width)) 
      paste0("width: ", validateCssUnit(width), ";"),
    lapply(X = list(...), FUN = tags$li, style = "margin-left: 10px; margin-right: 10px;")
  )
  # dropdown button apparence
  html_button <- list(
    class = paste0("btn btn-", status," dropdown-toggle"),
    type = "button", 
    `data-toggle` = "dropdown"
  )
  html_button <- c(html_button, list(label))
  html_button <- c(html_button, list(tags$span(class = "caret")))
  # final result
  tags$div(
    class = "dropdown",
    do.call(tags$button, html_button),
    do.call(tags$ul, html_ul),
    tags$script(
      "$('.dropdown-menu').click(function(e) {
      e.stopPropagation();
});")
  )
  }

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
                 `SIBL` = c(`SIBL: General collection`="BG")
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

circle_init <- readRDS(file = "./data/circl_init.rds")
mm <- readRDS(file = "./data/q1_2020_mm.rds")
mm_colls <- readRDS(file = "./data/q4_mm_colls.rds")

mm_prop <- readRDS(file = "./data/q1_2020_mm_prop.rds") 

mm_select_q3 <- readRDS(file = "./data/q3_minmand_select.rds")
mm_select_q4 <- readRDS(file = "./data/q4_minmand_select.rds")
mm_select <- readRDS(file = "./data/q1_2020_minmand_select.rds")
count_sum <- readRDS(file = "./data/q1_2020_count_sum.rds")
# print(names(count_sum))

approvals <- readRDS(file = "./data/q1_fy2020_approvals_items.rds")
# print(names(approvals))

genre_rem <- readRDS(file = "./data/genre_up_q1_2020.rds")
date_rem <- readRDS(file = "./data/date_up_q1_2020.rds")
location_rem <- readRDS(file = "./data/location_up_q1_2020.rds")
# print(names(location_rem))

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
                                                          # ,menuSubItem("AMI Details", tabName = "ami", icon = icon("film"))
                                                 ),
                                                 menuItem("Metadata Quality", tabName = "mdsqual", icon = icon("medkit"), startExpanded = TRUE
                                                          ,menuSubItem("Overview of Scores", tabName = "mdsqual_prop", icon = icon("check-square"))
                                                          ,menuSubItem("Scores by Element", tabName = "e_scores", icon = icon("clipboard-list")))
                                                 ,menuItem("Links", tabName = "links", icon = icon("external-link-alt"))
                                                 # menuItem("Help", tabName = "faq", icon = icon("question-circle")),
                                                 # ,menuItem(selectInput(inputId = "divisions", label = HTML("<p style='color:black;'>Research Library Divisions</p>"), 
                                                 #                      choices = div_menu, selected = "no_filter", selectize = TRUE)
                                                 #          ) #multiple = TRUE,
                                                 ,menuItem(pickerInput(
                                                   inputId = "divisions",
                                                   label = HTML("<p style='color:black;'>Research Library Divisions</p>"), 
                                                   choices = div_menu,
                                                   selected = unique(mm$code),
                                                   options = list(
                                                     `actions-box` = TRUE,
                                                     `selected-text-format` = "count > 2"
                                                     ,`count-selected-text` = ("{0} divisions selected")),
                                                   multiple = TRUE
                                                 ))
                                                 # ,menuItem(dropdownButton(
                                                 #   label = "Check some boxes", status = "default", width = 80,
                                                 #   checkboxGroupInput(inputId = "check1", label = "Choose", choices = div_menu)
                                                 # ))
                                                 # ,menuItem(selectizeInput("select", "Research Library Divisions", div_menu,
                                                 #                          multiple = TRUE, options = list(
                                                 #                            'plugins' = list('remove_button'),
                                                 #                            'create' = TRUE,
                                                 #                            'persist' = FALSE)
                                                 # ))
                                     )),
                    dashboardBody(# hide errors
                      tags$style(type="text/css",
                                 ".shiny-output-error { visibility: hidden; }"
                                 ,".shiny-output-error:before { visibility: hidden; }"
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
                                       # ,column(width = 11
                                       #         ,fluidRow(box(title = span(HTML("<b>Metadata Services Unit dashboard</b>")), solidHeader = TRUE, includeHTML("about.html"), status = "primary", width=12)))
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
                                       # ,box(width = NULL, plotOutput(outputId = "monthly_approvals")%>% withSpinner(color="#0dc5c1"))
                                       ,box(width = NULL, girafeOutput(outputId = "monthly_approvals", width = "100%")%>% withSpinner(color="#0dc5c1"))
                                       # ,div(id="ppitest", girafeOutput(outputId = "monthly_approvals"))
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
                      tabItem(tabName = "mdsqual_prop"
                              ,fluidRow(width=11
                                        ,valueBoxOutput("perc_above")
                                        ,valueBoxOutput("perc_below"))
                              ,fluidRow(width=NULL
                                        ,column(width = 9
                                                ,fluidRow(box(width = 12, plotOutput(outputId = "prop_plot")%>% withSpinner(color="#0dc5c1")))
                                                ,fluidRow(box(width=12, includeHTML("mds_prop.html"))))
                                        ,column(width = 3
                                                ,fluidRow(box(width = NULL 
                                                              ,radioButtons(inputId = "compare_prop", label = "Compare:"
                                                                            ,choices = c("Divisions"="compare_divs", "Fiscal quarters"="compare_fq")
                                                                            ,selected = "compare_divs")
                                                              ,conditionalPanel(
                                                                condition = "input.compare_prop == 'compare_divs'"
                                                                ,radioButtons(inputId = "mds_qtr", label = "Fiscal quarter:"
                                                                              ,choices = c("First quater, FY 2020"="fy2020_q1", "Fourth quater, FY 2019"="fy2019_q4","Third quarter, FY 2019"="fy2019_q3")
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
                              ,fluidRow(box(width = NULL,title=HTML(paste("Remediation project highlights",br(),"<span style='font-size:12px'>July-September (first quarter), fiscal year 2020</span>"))
                                            ,valueBoxOutput("rem1")
                                            ,valueBoxOutput("rem2")
                                            ,valueBoxOutput("rem3")))
                              ,fluidRow(box(width=NULL,plotOutput(outputId = "e_plot")%>% withSpinner(color="#0dc5c1")))
                              ,fluidRow(box(includeHTML("e_scores.html"), width=NULL))
                      )
                      ,tabItem(tabName = "links"
                               ,fluidRow(box(title="Quarterly reports", status = "primary", solidHeader = TRUE
                                             ,selectInput(inputId = "reports", label = "Select a quarterly report: ",
                                                         choices = c("","First quater, FY 2020"="fy2020_q1", "Fourth quater, FY 2019"="fy2019_q4", "Third quarter, FY 2019"="fy2019_q3")
                                                         ,selected = "", multiple = FALSE)
                                             )
                               )
                      )
                      )
                    ))

server <- function(input, output, session) {
  # download quarterly pdfs
  
  # create var with div selected
  selected <- reactive({input$divisions})
  
  # conditional hide/show ami tab & radiobuttons
  observeEvent(selected(), {
    # toggle(selector = "ul li ul li:eq(1)", condition = selected() %in% ami_div_choices)
    # toggle(id = "app_type", condition = selected() %in% ami_div_choices)
    toggleState("app_type", condition = selected() %in% ami_div_choices)
  })
  
  # update dropdown menu based on ami availability
  observe({
    if (input$sidebar_menu == "approvals" & input$app_type == "ami_apps") {
      updateSelectInput(session, "divisions",
                        choices = ami_div_choices,
                        selected = ifelse(selected() %in% ami_div_choices, selected(), "no_filter")
      )}
    else {
      updateSelectInput(session, "divisions",
                        choices = div_menu,
                        selected = selected())
      }
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
      div_choices
    }
    else if (length(selected()) == 26 & !(input$sidebar_menu == "approvals")) {
      div_choices
      }
    else if (length(selected()) == 26 & (input$sidebar_menu == "approvals")) {
      unique(approvals$code)
    }
    else {
      c(selected())
      }
  })
  
  # how to filter for ami approvals
  ami_switch <- reactive({
    switch(input$app_type, all_apps = unique(approvals$ami), ami_apps = c("AMI"), other_apps = c("Not AMI"))
  })
  
  # how to filter for fiscal quater
  fyq_switch <- reactive({
    switch(input$mds_qtr, fy2019_q3 = c("2019_q3"), fy2019_q4 = c("2019_q4"), fy2020_q1 = c("2020_q1"))
  })
  
  # text to display in approvals valuebox
  ami_text <- renderText({ 
    switch(input$app_type, all_apps = "", ami_apps = "AMI", other_apps = "other item")
  })
  
  baseline_n_caps <- reactive({
    if (input$divisions == "no_filter"){600} else {100}
  })
  
  output$collection_text <- renderText({ 
    paste("Collections in MMS with at least ",as.character(baseline_n_caps())," approved captures")
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
    else if (input$sidebar_menu == "mdsqual_prop" & input$compare_prop == "compare_divs"){
      mm_prop %>% filter(code %in% div_choice()) %>% filter(fy_q %in% fyq_switch())
    }
    else if (input$sidebar_menu == "mdsqual_prop" & input$compare_prop == "compare_fq") {
      mm_prop %>% 
        filter(code %in% div_choice()) %>%
        group_by(fy_q) %>%
        summarize(n_recs = sum(n_recs)/2,
                  n_above = sum(n_above)/2,
                  n_below = sum(n_below)/2,
                  p_above = n_above / n_recs,
                  p_below = n_below / n_recs) %>%
        gather(prop_type, values, p_above:p_below) %>%
        mutate(fy_label = paste0('FY',str_replace(fy_q,'_q',' Q')))
    }
    else if (input$sidebar_menu == "overview"){
      mm %>% filter(code %in% div_choice())
    }
    else if (input$sidebar_menu == "e_scores"){
      if (length(selected()) == 0 | length(selected()) == 26) {
        count_sum
      }
      else {
        baseline <- calc_element_scores(mm_select_q3, div_choice(), "2019/03/01") %>%
          bind_rows(calc_element_scores(mm_select_q4, div_choice(), "2019/06/01") )
        current <- calc_element_scores(mm_select, div_choice(), "2019/09/01") %>%
          bind_rows(baseline) %>%
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
                                         month(report_time) == 12 ~ paste('Dec',as.character(year(report_time))))) 
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
  output$top_colls <- renderPlotly({
    if (input$divisions == "no_filter") {
      return(circle_init)
    }
    else {
      coll_counts <- div_name() %>%
        filter(!is.na(coll_id)) %>%
        group_by(coll_id, coll_name, center, center_color) %>%
        summarize(n_recs = n(),
                  n_caps = sum(captures)) %>%
        filter(n_caps >= baseline_n_caps())
      
      packing <- circleProgressiveLayout(coll_counts$n_caps, sizetype='area')
      cap_bub <- bind_cols(coll_counts, packing) %>%
        rowid_to_column("id")
      
      dat.gg <- circleLayoutVertices(packing, npoints = 100)
      
      p <- ggplot(data = dat.gg) +
        # Make the bubbles
        geom_polygon(aes(x, y, group = id, fill=as.factor(id))
                     ,alpha = 1, show.legend = FALSE) +
        scale_fill_manual(values = cap_bub$center_color) +
        # geom_text(data = cap_bub, aes(x, y, size = n_caps, label = paste(coll_name,comma(n_caps),sep='\n'))) +
        theme_void() +
        theme(panel.background = element_rect(fill = "#F8F8F8")
              ,legend.position="none") +
        coord_equal()
      
      gp <- ggplotly(p) %>%
        config(displayModeBar = F) %>%
        layout(xaxis = list(showgrid = F),
               yaxis = list(showgrid = F),
               plot_bgcolor="rgb(248,248,248)",
               paper_bgcolor="rgb(248,248,248)")
      
      # needed to add hoverinfo manually
      for(i in 1:length(gp$x$data)){
        # gp$x$data[[i]]$hoverlabel = 'text'
        name <- cap_bub %>% filter(id == gp$x$data[[i]]$name) %$% coll_name
        num <- cap_bub %>% filter(id == gp$x$data[[i]]$name) %$% n_caps
        gp$x$data[[i]]$text = paste('<b>',name,'</b>','\n',comma(num),' captures')
      }
      
      return(gp)
    }

  })
  
  observe({
    output$prop_plot <- renderPlot({
      if (input$sidebar_menu == "mdsqual_prop" & input$compare_prop == "compare_divs"){
        ggplot(div_name(), aes(reorder(LAB, values))) + 
          geom_bar(data=. %>% filter(prop_type=="p_above"), aes(y = values), fill = '#99d594', stat = "identity", width = 0.7) +
          geom_bar(data=. %>% filter(prop_type=="p_below"), aes(y = -values), fill = '#fc8d59', stat = "identity", width = 0.7) +
          geom_hline(aes(yintercept = 0), color = "grey") + 
          scale_y_continuous(labels=function(x) paste0(abs(x)*100, "%"), limits = c(-1,1)) +
          labs(x = "", y = "", title = "") +
          facet_grid(. ~ center, scales = "free_x", space = "free_x") +
          theme_tufte(base_family='sans') +
          theme(strip.background = element_rect(fill="lightgray", color = "lightgray")
                ,strip.text.x = element_text(size = 10)
                ,axis.text.x = element_text(size=10)
                ,panel.border = element_rect(size = 3, colour = "lightgray", fill = NA)
                ,axis.text.y = element_text(size=10))
      }
      else if (input$sidebar_menu == "mdsqual_prop" & input$compare_prop == "compare_fq") {
        ggplot(div_name()) + 
          # geom_bar(data=. %>% filter(prop_type=="p_above"), aes(x = fy_label, y = values), fill = '#99d594', stat = "identity", width = 0.7) +
          geom_line(data=. %>% filter(prop_type=="p_above"), aes(x = fy_label, y = values, group = 1), color = '#99d594', size=1.5) +
          geom_point(data=. %>% filter(prop_type=="p_above"), aes(x = fy_label, y = values), color = '#99d594', fill = '#99d594', size=3) +
          # geom_line(data=. %>% filter(prop_type=="p_below"), aes(x = fy_label, y = -values, group = 1), color = '#fc8d59', size=1.5) +
          # geom_hline(aes(yintercept = 0), color = "grey") +
          scale_y_continuous(labels=function(x) paste0(abs(x)*100, "%"), limits = c(0,1)) +
          labs(x = "", y = "", title = "") +
          # facet_grid(. ~ fy_q, scales = "free_x", space = "free_x") +
          theme_tufte(base_family='sans') +
          theme(strip.background = element_rect(fill="lightgray", color = "lightgray")
                ,strip.text.x = element_text(size = 10)
                ,axis.text.x = element_text(size=10)
                ,panel.border = element_rect(size = 3, colour = "lightgray", fill = NA)
                ,axis.text.y = element_text(size=10))
      }

    }, width = ifelse(selected() == "no_filter" | input$compare_prop == "compare_fq" | (input$compare_prop == "compare_divs" & length(unique(div_name()$code)) > 6),"auto",300))
  })
  
  sm_bu_pu = brewer.pal(n = 4, "BuPu")[2:4]
  # output$monthly_approvals <- renderPlot({
  #   ggplot(div_name(), aes(d_month, n_apps, fill=approval_type)) +
  #     geom_bar(stat = 'identity', position = "dodge") +
  #     scale_x_date(date_labels = "%b %Y", date_breaks="month", expand = c(0.03,0.03)) +
  #     # scale_x_date(date_labels = "%b %Y", date_breaks="month", limits = as.Date(c('6/20/2018', '9/10/2019'), format="%m/%d/%Y")) +
  #     scale_y_continuous(labels = comma) +
  #     # geom_text(aes(d_month, items, label=comma(items)), vjust = -0.2, size=3) +
  #     scale_fill_manual(values=sm_bu_pu) +
  #     # scale_color_brewer(palette="Dark2", guide='none') +
  #     # facet_grid(. ~ fy_qy, scales = "free_x", space = "free_x") +
  #     facet_grid(. ~ fy_year, scales = "free_x", space = "free_x", labeller = as_labeller(c(`2019` = "FY2019", `2020` = "FY2020"))) +
  #     # facet_grid_sc(cols = vars(fy_year), scales = list(x = scales_x), space = "free_x") +
  #     # theme_calc() +
  #     theme_tufte(base_family='sans') +
  #     theme(axis.title.x=element_blank()
  #           ,axis.title.y=element_blank()
  #           ,axis.text.y = element_text(size=10)
  #           ,axis.text.x = element_text(size=10)
  #           ,legend.position = "none"
  #           ,strip.background = element_rect(fill="lightgray", color = "lightgray")
  #           ,panel.border = element_rect(size = 3, colour = "lightgray", fill = NA)
  #           ,strip.text.x = element_text(size = 10))
  # })
  
  output$monthly_approvals <- renderGirafe({
    p <- ggplot(div_name(), aes(d_month, n_apps, fill=approval_type, tooltip = tooltip)) +
      geom_bar_interactive(stat = 'identity', position = "dodge") +
      scale_x_date(date_labels = "%b %Y", date_breaks="month", expand = c(0.03,0.03)) +
      scale_y_continuous(labels = comma) +
      scale_fill_manual(values=sm_bu_pu) +
      # scale_fill_manual_interactive(values=sm_bu_pu) +
      facet_grid(. ~ fy_year, scales = "free_x", space = "free_x", labeller = as_labeller(c(`2019` = "FY2019", `2020` = "FY2020"))) +
      theme_tufte(base_family='sans') +
      theme(axis.title.x=element_blank()
            ,axis.title.y=element_blank()
            ,axis.text.y = element_text(size=10)
            ,axis.text.x = element_text(size=10)
            ,legend.position = "none"
            ,strip.background = element_rect(fill="lightgray", color = "lightgray")
            ,panel.border = element_rect(size = 3, colour = "lightgray", fill = NA)
            ,strip.text.x = element_text(size = 10))
    
    tooltip_css <- "background-color:transparent;font-family:sans-serif;color:black;"
    
    g <- girafe(ggobj = p,
                width_svg = (0.8*input$pltChange$width/input$pltChange$dpi),
                height_svg = (0.7*input$pltChange$height/input$pltChange$dpi))
    
    x <- girafe_options(x = g,
                        opts_tooltip(css = tooltip_css,
                                     use_fill = TRUE
                                     # ,opacity = .7
                        )
                        # ,opts_sizing(rescale = TRUE, width = 1)
                        )
    x
  })
  
  # output$monthly_approvals <- renderPlotly({
  #   p <- ggplot(div_name(), aes(d_month, n_apps, fill=approval_type)) +
  #     geom_bar(stat = 'identity', position = "dodge") +
  #     scale_x_date(date_labels = "%b %Y", date_breaks="month", expand = c(0.03,0.03)) +
  #     # scale_x_date(date_labels = "%b %Y", date_breaks="month", limits = as.Date(c('6/20/2018', '9/10/2019'), format="%m/%d/%Y")) +
  #     scale_y_continuous(labels = comma) +
  #     # geom_text(aes(d_month, items, label=comma(items)), vjust = -0.2, size=3) +
  #     scale_fill_manual(values=sm_bu_pu) +
  #     # scale_color_brewer(palette="Dark2", guide='none') +
  #     # facet_grid(. ~ fy_qy, scales = "free_x", space = "free_x") +
  #     facet_grid(. ~ fy_year, scales = "free_x", space = "free_x", labeller = as_labeller(c(`2019` = "FY2019", `2020` = "FY2020"))) +
  #     # facet_grid_sc(cols = vars(fy_year), scales = list(x = scales_x), space = "free_x") +
  #     # theme_calc() +
  #     theme_tufte(base_family='sans') +
  #     theme(axis.title.x=element_blank()
  #           ,axis.title.y=element_blank()
  #           ,axis.text.y = element_text(size=10)
  #           ,axis.text.x = element_text(size=10)
  #           ,legend.position = "none"
  #           ,strip.background = element_rect(fill="lightgray", color = "lightgray")
  #           ,panel.border = element_rect(size = 3, colour = "lightgray", fill = NA)
  #           ,strip.text.x = element_text(size = 10))
  #   
  #   p <- plot_ly(div_name(), x= ~d_month, y = ~n_apps, group = ~approval_type, color = ~approval_type
  #                ,type = "bar"
  #                # ,orientation = 'h'
  #   ,marker = list(color=sm_bu_pu)
  #                # ,text = comma_format()(view_sum$tot)
  #                ,textposition="outside"
  #                ,cliponaxis = FALSE
  #                ,hoverinfo = 'text'
  #                # ,hovertext = overview_tooltip(view_sum$user_lab,view_sum$tot,'page views')
  #   ) %>%
  #     layout(showlegend = F
  #            # ,bargap = 0
  #            ,xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE)
  #            ,yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE)
  #            # ,margin = list(b=10,l=10,r=70,t=10,pad=4)
  #     )
  #   ggplotly(p) %>%
  #     config(displayModeBar = F)
  # })
  
  output$e_plot <- renderPlot({
    ggplot(div_name(), aes(x = reorder(quarter_lab, month(report_time)), y = perc, fill=score_order)) +
      geom_bar(stat = 'identity', position = "stack", width=0.9) +
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
  })
  
}

shinyApp(ui = ui, server = server)