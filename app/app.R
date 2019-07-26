library(shiny)
library(tidyverse)
library(plotly)
library(scales)
library(ggthemes)
library(shinydashboard)
library(dashboardthemes)
library(Cairo)
options(shiny.usecairo=T)
library(magrittr)
library(lubridate)
library(RColorBrewer)
library(packcircles)
library(shinycssloaders)
library(shinyjs)

div_choices <- c("All Divisions" = "no_filter",
                 "Berg Collection" = "BRG",
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

ami_div_choices <- c("All Divisions" = "no_filter",
                     "Jerome Robbins Dance Division" = "DAN",
                     "Manuscripts and Archives Division" = "MSS",
                     "Rodgers and Hammerstein Archives of Recorded Sound" = "RHA",
                     "Schomburg Moving Image and Recorded Sound Division" = "SCL")

circle_init <- readRDS(file = "./data/circl_init.rds")
mm <- readRDS(file = "./data/q4_mm.rds")
mm_colls <- readRDS(file = "./data/q4_mm_colls.rds")

mm_prop <- readRDS(file = "./data/q4_mm_prop.rds")
mm_select_q3 <- readRDS(file = "./data/q3_minmand_select.rds")
mm_select <- readRDS(file = "./data/q4_minmand_select.rds")
count_sum <- readRDS(file = "./data/q3_count_sum.rds") %>%
  bind_rows(readRDS(file = "./data/q4_count_sum.rds"))

approvals <- readRDS(file = "./data/fy2019_approvals_items.rds")

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
                                         ,menuSubItem("Detailed Comparison", tabName = "e_scores", icon = icon("clipboard-list"))),
                                # menuItem("Help", tabName = "faq", icon = icon("question-circle")),
                                menuItem(selectInput(inputId = "divisions", label = HTML("<p style='color:black;'>Research Library Divisions</p>"), 
                                                     choices = div_choices, selectize = TRUE, selected = "no_filter")) #multiple = TRUE,
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
                     # ,column(width = 3
                     #         ,fluidRow(box(title = "Contact", includeHTML("contact.html"), width=NULL)))
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
                         ,box(width = NULL, plotOutput(outputId = "monthly_approvals")%>% withSpinner(color="#0dc5c1"))
                         )
                       ,column(width = 3
                               ,box(width = NULL 
                                    ,radioButtons(inputId = "app_type", label = "Approval type:"
                                                  ,choices = c("All approvals"="all_apps", "AMI approvals"="ami_apps"
                                                               # , "Other resource types"="other_apps"
                                                               )
                                                  ,selected = "all_apps")
                                    )
                               ,box(width = NULL
                                    ,HTML('<p><span class = "legbox a_items">XX</span>  Items</p><p><span class = "legbox a_caps">XX</span>  Captures</p>'))
                               )
                       )
                     ,fluidRow(box(includeHTML("approvals.html"), width=11))
                     ),
             # tabItem(tabName = "ami", 
             #         valueBoxOutput("AMIprogressBox"),valueBoxOutput("AMIcapBox"),valueBoxOutput("AMIremBox")
             #         ,fluidRow(box(width = 9, plotOutput(outputId = "ami_details")%>% withSpinner(color="#0dc5c1")))
             #         ,fluidRow(box(includeHTML("ami.html"), width=11))
             # ),
             tabItem(tabName = "mdsqual_prop"
                     ,fluidRow(width=11
                                ,valueBoxOutput("perc_above")
                                ,valueBoxOutput("perc_below"))
                     ,fluidRow(width=NULL
                       ,column(width = 9
                            ,fluidRow(box(width = 12, plotOutput(outputId = "prop_plot")%>% withSpinner(color="#0dc5c1")))
                            ,fluidRow(box(width=12, includeHTML("mds_prop.html"))))
                       ,column(width = 3,fluidRow(box(title="Division code key", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                                    includeHTML("help_divs.html"), width=11))))
                     ),
             tabItem(tabName = "e_scores"
                     ,fluidRow(box(width = NULL,title=HTML(paste("Remediation project highlights",br(),"<span style='font-size:12px'>April-June (fourth quarter), fiscal year 2019</span>"))
                                                  ,valueBoxOutput("tor_rem")
                                                  ,valueBoxOutput("genre_auth_rem")
                                                  ,valueBoxOutput("date_rem")))
                            ,fluidRow(box(width=NULL,plotOutput(outputId = "e_plot")%>% withSpinner(color="#0dc5c1")))
                            ,fluidRow(box(includeHTML("e_scores.html"), width=NULL))
                     )
             # ,tabItem(tabName = "faq",
             #         fluidRow(column(width = 8
             #                        ,fluidRow(box(title="Abbreviations", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
             #                                      includeHTML("help_abbrevs.html"), width=11))
             #                        ,fluidRow(box(title="Division code key", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
             #                                      includeHTML("help_divs.html"), width=11))
             #                        ,fluidRow(box(title="FAQ", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
             #                                      includeHTML("help_qs.html"), width=11)))
             #                 ,column(width = 3, fluidRow(box(title = "Contact", includeHTML("contact.html"), width=NULL))))
             # 
             #         )
             )
  ))

server <- function(input, output, session) {
  
  # create var with div selected
  selected <- reactive({input$divisions})
  
  # conditional hide/show ami tab & radiobuttons
  observeEvent(selected(), {
    toggle(selector = "ul li ul li:eq(1)", condition = selected() %in% ami_div_choices)
    # toggle(id = "app_type", condition = selected() %in% ami_div_choices)
    toggleState("app_type", condition = selected() %in% ami_div_choices)
  })
  
  # update dropdown menu based on ami availability
  observe({
    if (input$sidebar_menu == "ami" | (input$sidebar_menu == "approvals" & input$app_type == "ami_apps")) {
      updateSelectInput(session, "divisions",
                        choices = ami_div_choices,
                        selected = ifelse(selected() %in% ami_div_choices, selected(), "no_filter")
      )}
    else {
      updateSelectInput(session, "divisions",
                        choices = div_choices,
                        selected = selected())
      }
  })
  
  # what data to use to filter loaded data below (all divs, all divs in approvals incl NO_DIV, or just selected)
  div_choice <- reactive({
    if (selected() == "no_filter" & !(input$sidebar_menu == "approvals" | input$sidebar_menu == "ami")) {
      setdiff(div_choices, "no_filter")
      }
    else if (selected() == "no_filter" & (input$sidebar_menu == "approvals" | input$sidebar_menu == "ami")) {
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
          group_by(d_month) %>%
          summarize(n_caps = sum(captures),
                    items = n()) %>%
        gather(key = "approval_type", value = "n_apps", n_caps, items)
    }
    else if (input$sidebar_menu == "mdsqual_prop"){
        mm_prop %>% filter(code %in% div_choice())
    }
    else if (input$sidebar_menu == "overview"){
        mm_colls %>% filter(code %in% div_choice())
    }
    else if (input$sidebar_menu == "ami"){
      approvals %>% 
        filter(code %in% div_choice()) %>%
        filter(ami == 'AMI') %>%
        group_by(center) %>%
        summarize(`Approved Captures` = sum(captures),
                  `Approved Items` = n()) %>%
        gather(n_type, n_values, `Approved Captures`:`Approved Items`) %>%
        bind_rows(tibble(center = 'LPA',n_type='Remediated Items',n_values=935*4))
    }
    else if (input$sidebar_menu == "e_scores"){
      if (input$divisions != 'no_filter') {
        baseline <- mm_select_q3 %>%
          filter(code %in% div_choice()) %>%
          gather(element, score, title:location) %>%
          mutate(score = ifelse(score==0.75,0.50,score)) %>%
          group_by(element, score) %>%
          summarize(n = n()) %>%
          mutate(perc = round((n / sum(n))*100, digits = 2),
                 report_time = as.Date("2019/03/01"))
        mm_select %>%
          filter(code %in% div_choice()) %>%
          gather(element, score, title:location) %>%
          mutate(score = ifelse(score==0.75,0.50,score)) %>%
          group_by(element, score) %>%
          summarize(n = n()) %>%
          mutate(perc = round((n / sum(n))*100, digits = 2),
                 report_time = as.Date("2019/06/01")) %>%
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
      else {
        # maybe loads faster for default view?
        count_sum
      }
    }
  })
  
  ### VALUE BOXES ###
  output$n_apps <- renderValueBox({
    valueBox(
      comma_format()(nrow(div_name())), "Approved items in MMS*", icon = icon("book"),
      color = "aqua"
    )
  })
  
  output$n_caps <- renderValueBox({
    valueBox(
      comma_format()(sum(div_name()$captures)), "Approved captures in MMS*", icon = icon("eye"),
      color = "blue"
    )
  })
  
  output$n_ami <- renderValueBox({
    valueBox(
      comma_format()(nrow(div_name() %>% filter(ami == "ami"))), HTML("Approved AMI items in MMS*&dagger;"), icon = icon("film"),
      color = "teal"
    )
  })
  
  output$n_ami_caps <- renderValueBox({
    valueBox(
      comma_format()(sum(div_name() %>% filter(ami == "ami") %$% captures)), HTML("Approved AMI captures in MMS*&dagger;"), icon = icon("headphones"),
      color = "light-blue"
    )
  })
  
  
  output$progressBox <- renderValueBox({
    valueBox(
      comma_format()(sum(div_name() %>% filter(approval_type == "items") %$% n_apps)), HTML(paste("Fiscal year 2019 ",ami_text()," item approvals",br(),"<span style = 'font-size: 11px'>July 1, 2018 - June 30, 2019</span>")), icon = icon("thumbs-up", lib = "glyphicon"),
      color = "light-blue"
    )
  })
  
  output$capProgressBox <- renderValueBox({
    valueBox(
      comma_format()(sum(div_name() %>% filter(approval_type == "n_caps") %$% n_apps)), HTML(paste("Fiscal year 2019 ",ami_text()," capture approvals",br(),"<span style = 'font-size: 11px'>July 1, 2018 - June 30, 2019</span>")), icon = icon("thumbs-up", lib = "glyphicon"),
      color = "purple"
    )
  })
  
  output$AMIprogressBox <- renderValueBox({
    valueBox(
      comma_format()(sum(div_name() %>% filter(n_type == "Approved Items") %$% n_values)), 
      HTML(paste("Fiscal year 2019 AMI item approvals",br(),"<span style = 'font-size: 11px'>July 1, 2018 - June 30, 2019</span>")), icon = icon("thumbs-up", lib = "glyphicon"),
      color = "light-blue"
    )
  })
  
  output$AMIcapBox <- renderValueBox({
    valueBox(
      comma_format()(sum(div_name() %>% filter(n_type == "Approved Captures") %$% n_values)), 
      HTML(paste("Fiscal year 2019 AMI capture approvals",br(),"<span style = 'font-size: 11px'>July 1, 2018 - June 30, 2019</span>")), icon = icon("thumbs-up", lib = "glyphicon"),
      color = "light-blue"
    )
  })
  
  output$AMIremBox <- renderValueBox({
    valueBox(
      comma_format()(sum(div_name() %>% filter(n_type == "Remediated Items") %$% n_values)), 
      HTML(paste("**placeholder data**",br(),"Fiscal year 2019 AMI item remediation",br(),"<span style = 'font-size: 11px'>July 1, 2018 - June 30, 2019</span>")), icon = icon("thumbs-up", lib = "glyphicon"),
      color = "light-blue"
    )
  })
  
  output$perc_above <- renderValueBox({
    valueBox(
      paste0(round(sum(div_name() %>% filter(prop_type == "p_above") %$% n_above)/sum(div_name() %>% filter(prop_type == "p_above") %$% n_recs),3)*100, "%"), 
      "Meet minimum mandatory requirements", icon = icon("arrow-up", lib = "glyphicon"), color = "green"
    )
  })
  
  output$perc_below <- renderValueBox({
    valueBox(
      paste0(round(sum(div_name() %>% filter(prop_type == "p_below") %$% n_below)/sum(div_name() %>% filter(prop_type == "p_below") %$% n_recs),3)*100, "%"), 
      "Below minimum mandatory requirements", icon = icon("arrow-down", lib = "glyphicon"), color = "orange"
    )
  })
  
  output$genre_auth_rem <- renderValueBox({
    # SCM ART ARTIFACTS IS NEGATIVE!!
    q3 <- sum(div_name() %>% filter(quarter_lab == "Mar 2019", element == "genre", !score == 1.0) %$% n)
    q4 <- sum(div_name() %>% filter(quarter_lab == "Jun 2019", element == "genre", !score == 1.0) %$% n)
    q3_val <- ifelse(length(q3) == 0,0,q3)
    q4_val <- ifelse(length(q4) == 0,0,q4)
    val <- comma_format()(q3_val-q4_val)
    if (val < 0) val <- "--"
    valueBox(val, "Items had at least one genre term remediated", icon = icon("broom"), color = "fuchsia"
    )
  })
  
  output$tor_rem <- renderValueBox({
    q3 <- div_name() %>% filter(quarter_lab == "Mar 2019", element == "typeOfResource", score == 0.0) %$% n
    q4 <- div_name() %>% filter(quarter_lab == "Jun 2019", element == "typeOfResource", score == 0.0) %$% n
    q3_val <- ifelse(length(q3) == 0,0,q3)
    q4_val <- ifelse(length(q4) == 0,0,q4)
    val <- comma_format()(q3_val-q4_val)
    valueBox(val, "Items had a type of resource added", icon = icon("plus"), color = "black")
  })
  
  output$date_rem <- renderValueBox({
    q3 <- sum(div_name() %>% filter(quarter_lab == "Mar 2019", element == "date", !score == 1.0) %$% n)
    q4 <- sum(div_name() %>% filter(quarter_lab == "Jun 2019", element == "date", !score == 1.0) %$% n)
    q3_val <- ifelse(length(q3) == 0,0,q3)
    q4_val <- ifelse(length(q4) == 0,0,q4)
    val <- comma_format()(q3_val-q4_val)
    if (val < 0) val <- "--"
    valueBox(val, "Items had at least one date remediated", icon = icon("calendar-plus"), color = "lime")
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
    }, width = ifelse(selected() == "no_filter","auto",300))
  })
  
  sm_bu_pu = brewer.pal(n = 4, "BuPu")[2:4]
  output$monthly_approvals <- renderPlot({
    ggplot(div_name(), aes(d_month, n_apps, fill=approval_type)) + 
      geom_bar(stat = 'identity', position = "dodge") +
      scale_x_date(date_labels = "%b %Y", date_breaks="month") +
      scale_y_continuous(labels = comma) +
      # geom_text(aes(d_month, items, label=comma(items)), vjust = -0.2, size=3) +
      scale_fill_manual(values=sm_bu_pu) +
      # scale_color_brewer(palette="Dark2", guide='none') +
      theme_tufte(base_family='sans') +
      theme(axis.title.x=element_blank()
            ,axis.title.y=element_blank()
            ,axis.text.y = element_text(size=10)
            ,axis.text.x = element_text(size=10)
            ,legend.position = "none") 
  })
  
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