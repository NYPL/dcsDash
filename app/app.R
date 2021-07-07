# UI ---------------------------------------------------------------
ui <- function(req) {
  dashboardPage(title="DCS dashboard",
                    dashboardHeader(title = logo_blue_gradient,titleWidth = 250),
                    dashboardSidebar(width = 250, useShinyjs(), 
                                     sidebarMenu(id = "tabs",
                                                 menuItem("About", tabName = "overview", icon = icon("globe"))
                                                 ,menuItem("Publishing timeline", tabName = "publishing", icon = icon("archive"))
                                                 ,menuItem("Project lifecycle", tabName = "lifecycle", icon = icon("camera"))
                                                 ,menuItem("Rights", tabName = "rightsBD", icon = icon("edit"))
                                                 ,menuItem("AMI Preservation", tabName = "pami", icon = icon("microphone"))
                                                 ## Option 2
                                                 ,menuItem(HTML("Metadata Quality"), icon = icon("medkit"), startExpanded = FALSE, tabName = "hiddenMQ"
                                                           ,menuSubItem("Overview", tabName = "mdsqual_prop", icon = icon("check-square"))
                                                           ,menuSubItem("Evaluation by element", tabName = "e_scores", icon = icon("clipboard-list"))
                                                           ,menuSubItem("Recommended elements", tabName = "ifapp", icon = icon("receipt"))
                                                           ,menuSubItem("Resource type accuracy", tabName = "tor", icon = icon("forward"))
                                                           ,menuSubItem("Name element", tabName = "name", icon = icon("user-edit"))
                                                 )
                                                 ,hidden(menuItem("hiddenQual", tabName = "mdsqual"))
                                                 ,chooseBy_ui("chooseBy")
                                                 ,centers_ui("centers")
                                                 ,divisions_ui("divisions")
                                     )),
                    dashboardBody(tags$style(type="text/css",css_list),
                                  dashboardthemes::shinyDashboardThemes(theme = "grey_light"),
                                  # tabItems ---------------------------------------------------------------
                                  tabItems(tabItem(tabName = "overview", 
                                                   fluidRow(purrr::map(c("n_apps", "n_caps", "n_ami", "n_ami_caps"), vb_ui))
                                                   # ,fluidRow(column(width = 9))
                                                   ,fluidRow(column(width = 10,
                                                                    fluidRow(box(title = span(HTML("<b>Digital Collections Services dashboard</b>")), solidHeader = TRUE
                                                                                 ,includeHTML("./text/about.html")
                                                                                 ,status = "primary", width=12)))
                                                             ,column(width = 2,
                                                                     fluidRow(get_latest_data("overview_btn","overview_btn_title")))
                                                   )
                                  ),
                                  tabItem(tabName = "publishing", 
                                          fluidRow(purrr::map(c("progressBox", "capProgressBox", "app_rights_box", "cap_rights_box"), vb_ui))
                                          ,fluidRow(
                                            column(width = 9
                                                   ,plot_box("monthly_approvals",spinner=TRUE)
                                                   ,fluidRow(uiOutput("app_coll_bars_box"))
                                                   ,box(includeHTML("./text/approvals.html"), width=12)
                                            )
                                            ,column(width = 3
                                                    ,box(width = NULL 
                                                         ,radioButtons(inputId = "app_type", label = "Approval type:",choices = c("All approvals"="all_apps", "AMI approvals"="ami_apps"),selected = "all_apps")
                                                         ,radioButtons(inputId = "f_or_c", label = "Calendar or fiscal year:",choices = c("Calendar"="calendar","Fiscal"="fiscal"), selected = "calendar")
                                                         ,conditionalPanel(condition = "input.f_or_c == 'calendar'",calendar_ui("cal_slide"))
                                                         ,fiscal_ui("fis_slide")
                                                         
                                                    )
                                                    ,app_gloss_box()
                                                    ,get_latest_data("approvals_btn","approvals_btn_title")#get_latest_data("approvals_btn")
                                                    ,dl_box("downloadApp")
                                            )
                                          )
                                  ),
                                  tabItem(
                                    tabName = "lifecycle", 
                                    fluidRow(
                                      column(width = 8, align="center"
                                             ,plot_box("diu_plot",spinner=TRUE)
                                             ,div(style = "margin-top:-30px;margin-bottom:-15px;", calendar_ui("diu_slide", label = ""))
                                             ,uiOutput("diu_coll_bars_box")
                                             ,box(includeHTML("./text/imaging.html"), width=12, align = "left")
                                      ),
                                      column(width = 4
                                             ,fluidRow(width = NULL, uiOutput("diu_clicked_text"))
                                             ,plot_box("diu_pie_1", boxheight="210px", plotwidth="100%", plotheight="350px")
                                             ,plot_box("diu_pie_2", boxheight="210px", plotwidth="100%", plotheight="350px")
                                             ,fluidRow(diu_gloss_box())
                                             ,get_latest_data("diu_btn","diu_btn_title")
                                             ,fluidRow(dl_box("downloadDIU"))
                                      ) # pie col
                                    ) # plot row
                                  )
                                  ,tabItem(
                                    tabName = "rightsBD"
                                    ,fluidRow(box(uiOutput("rightsBD_title"), width=10)) # text row
                                    ,fluidRow(
                                      column(width = 9, plot_box("rightsBD_plot",spinner=TRUE))
                                      ,column(width = 3, fluidRow(dl_box("downloadRights")))
                                    )
                                  )
                                  ,tabItem(
                                    tabName = "pami", 
                                    fluidRow(
                                      column(width = 8, align="center"
                                             ,plot_box("pami_plot",spinner=TRUE)
                                             ,box(width=12, align = "left"
                                                  ,includeHTML("./text/pami.html")
                                                  )
                                      ),
                                      column(width = 4
                                             ,fluidRow(width = NULL, uiOutput("pami_clicked_text"))
                                             ,plot_box("pami_pie_1", boxheight="210px", plotwidth="100%", plotheight="350px")
                                             ,plot_box("pami_pie_2", boxheight="210px", plotwidth="100%", plotheight="350px")
                                             ,fluidRow(pami_gloss_box())
                                             ,fluidRow(dl_box("downloadPAMI"))
                                      ) # pie col
                                    ) # plot row
                                  )
                                  ,tabItem(tabName = "mdsqual"
                                           ,fluidRow(box(width = NULL, HTML("<h3><strong>Metadata Quality navigation</strong></h3><h4><strong>Minimum mandatory metadata quality criteria</strong></h4>"),
                                                         HTML("<p>The MSU conducts targeted metadata quality assessment and remediation of the metadata elements required for the <a href='https://drive.google.com/open?id=1WkzQcf5UBcGTOLJGpCDEfxfLAxhZP6-W-IrgvTIRjO0'>minimum viable description</a> of digitized items as well as metadata for recommended elements and attributes."),
                                                         p("Select the ", actionLink("link_to_overview", "overview")," page to look at the proportion of records in our Research Library Divisions that meet our minimum standards for digitized items each fiscal quarter."),
                                                         # br(),br(),
                                                         p("Select the ", actionLink("link_to_e_scores", "evaluation by element"), " page to see metadata quality remediation progress for each of our mandatory elements (Title, Type of Resource, Identifier, Genre, Date, and Location) since the start of our targeted remediation project in March 2019."),
                                                         # br(),
                                                         HTML("<h4><strong>Metadata quality criteria for additional elements</strong></h4>"),
                                                         p("Building on our foundational metadata quality work for mandatory elements, the MSU assessed seven recommended metadata elements for compliance with our metadata criteria: Language, Name, Form, Note, Table of Contents, Description, and Subject."),
                                                         p("Select the ", actionLink("link_to_rec", "recommended elements")," page to see metadata quality remediation progress for each of these elements since the start of our targeted remediation project in March 2019."),
                                                         HTML("<h4><strong>Project highlights</strong></h4>"),
                                                         p("The MSU organized the two projects currently on display for Preservation of Audio and Moving Image (PAMI) staff interested in remote work."),
                                                         p("Select the ", actionLink("link_to_tor", "resource type accuracy")," page to view progress on this remediation project, which focuses on correcting issues of accuracy with the type of resource element that arose when data was migrated to MMS for its launch in 2012. The migration generated erroneous “still image” resource types for material that is accurately described using other/additional resource types."),
                                                         p("Select the ", actionLink("link_to_name", "name element")," page to view progress on this remediation project, which focuses on ensuring that name roles and authorized headings are present and conform to our standards. The presence of this information helps patrons better understand objects in our Digital Collections.")
                                                         )
                                                     )
                                           )
                                  ,tabItem(tabName = "mdsqual_prop"
                                           ,fluidRow(width=11, purrr::map(c("perc_above", "perc_below"), vb_ui, width=4))
                                           ,fluidRow(width=NULL
                                                     ,column(width = 9
                                                             ,plot_box("prop_plot",spinner=TRUE, boxwidth=12)
                                                             ,fluidRow(box(width=12, includeHTML("./text/mds_prop.html"))))
                                                     ,column(width = 3
                                                             ,fluidRow(box(width = NULL
                                                                           ,radioButtons(inputId = "compare_prop", label = "Compare:",choices = c("Divisions"="compare_divs", "Fiscal quarters"="compare_fq"),selected = "compare_divs")
                                                                           ,fy_ui("fy_select")
                                                                           ,catalogued_check("prop_cat")
                                                             )
                                                             )
                                                             ,fluidRow(box(title="Division code key", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, includeHTML("./text/help_divs.html"), width=11))
                                                     )
                                           )
                                  )
                                  ,tabItem(tabName = "e_scores"
                                           ,fluidRow(box(width = NULL, title=uiOutput("e_plot_title"), purrr::map(c("rem1", "rem2", "rem3", "rem4", "rem5"), vb_ui) ))
                                           ,fluidRow(column(width = 10,plot_box("e_plot",spinner=TRUE))
                                                     ,column(width = 2,fluidRow(box(width=NULL, uiOutput('elements_uncat_text'), catalogued_check('elements_cat'), status = "info"
                                                                                    , radioButtons("phase_btn","Remediation project phase:",choices=c("Phase 1","Phase 2")))))
                                                                                # ,box(width=NULL, radioButtons("phase_btn","Remediation project phase:",choices=c("Phase 1","Phase 2")))))
                                           )
                                           ,fluidRow(box(uiOutput("elements_text"), width=NULL)) #includeHTML("./text/e_scores.html"), width=NULL))
                                  )
                                  # ,tabItem(tabName = "ext"
                                  #          ,fluidRow(box(width = NULL, title=uiOutput("ext_plot_title"), purrr::map(c("ext1", "ext2", "ext3", "ext4"), vb_ui) ))
                                  #          ,fluidRow(column(width = 10,plot_box("ext_plot",spinner=TRUE))
                                  #                    # ,column(width = 2,catalogued_check("ext_cat"))
                                  #                    ,elements_help_box("ext_uncat_text","ext_check")
                                  #          )
                                  #          ,fluidRow(box(includeHTML("./text/extended.html"), width=NULL))
                                  # )
                                  ,tabItem(tabName = "ifapp"
                                           ,fluidRow(box(width = NULL, title=uiOutput("ifapp_plot_title"), purrr::map(c("ifapp1", "ifapp2", "ifapp3", "ifapp4"), vb_ui) ))
                                           ,fluidRow(column(width = 10,plot_box("ifapp_plot",spinner=TRUE))
                                                     ,column(width = 2,fluidRow(box(width=NULL, uiOutput('ifapp_uncat_text'), catalogued_check('ifapp_check'), status = "info" )))
                                           )
                                           ,fluidRow(box(includeHTML("./text/recommended.html"), width=NULL))
                                  )
                                  ,tabItem(tabName = 'tor'
                                           ,fluidRow(box(width=12,title = 'Type of resource: metadata quality remediation for accuracy'))
                                           ,fluidRow(
                                             column(style='border-right: 2px solid darkblue',width = 3, plot_box("img_plot",spinner=TRUE))
                                             ,column(width = 9, plot_box("tor_plot",spinner=TRUE))
                    
                                           )
                                           ,fluidRow(box(width=12, includeHTML("./text/tor.html")))
                                  )
                                  ,tabItem(tabName = 'name'
                                           ,fluidRow(box(width=12,title = 'Name element remediation', 
                                                         purrr::map(c("namevb_1", "namevb_2", "namevb_3"), vb_ui, width=4)))
                                           ,fluidRow(
                                             column(width = 12, plot_box("name_plot",spinner=TRUE),fluidRow(box(width=12, includeHTML("./text/names.html"))))
                                             # ,column(width = 3,fluidRow(box(width=NULL
                                             #   ,radioButtons(inputId = "name_select", label = "Name attribute:",choices = c("Name roles"="name_role", "Name authority"="name_auth"),selected = "name_role")
                                             # )))
                                           )
                                  )
                                  ) # tabItems
                    ) # dashboardBody
) #dashboardpage
}



# SERVER ---------------------------------------------------------------
server <- function(input, output, session) {
  
  # initialize data, update data & buttons ---------------------------------------------------------------
  # drive_auth(cache = ".secrets", email = readRDS("./data/email.rds"))
  
  # enable url bookmarking
  ExcludedIDs <- reactiveVal(value = NULL)

  observe({
    toExclude <- setdiff(names(input), c("tabs")) #,"sidebarItemExpanded" #  (does not work for 2-layer nested menusubitem)
    setBookmarkExclude(toExclude)
    ExcludedIDs(toExclude)
    session$doBookmark()
  })

  onBookmarked(function(url) {
    updateQueryString(url)
  })
  
  observeEvent(input$sidebarItemExpanded, {
    if(input$sidebarItemExpanded == "MetadataQuality"){
      updateTabItems(session, "tabs", selected = "mdsqual")
    }
  })
  
  observeEvent(input$link_to_overview, {
    updateTabItems(session, "tabs", "mdsqual_prop")
  })
  
  observeEvent(input$link_to_e_scores, {
    updateTabItems(session, "tabs", "e_scores")
  })
  
  observeEvent(input$link_to_ext, {
    updateTabItems(session, "tabs", "ext")
  })
  
  observeEvent(input$link_to_rec, {
    updateTabItems(session, "tabs", "ifapp")
  })
  
  observeEvent(input$link_to_tor, {
    updateTabItems(session, "tabs", "tor")
  })
  
  observeEvent(input$link_to_name, {
    updateTabItems(session, "tabs", "name")
  })
  
  overview_file_path <- "./data/download/approvals_num.csv"
  
  overview_data <- eventReactive(input$overview_btn, {
    trigger_data_load("overview_btn",input$overview_btn[1], overview_file_path, "~/approvals_num.csv", session, mainPlot=TRUE)
  }, ignoreNULL = FALSE)
  
  overview_file_date <- eventReactive(input$overview_btn, {
    file.info(overview_file_path)$ctime
  }, ignoreNULL = FALSE)
  
  # dynamic set file last updated
  output$overview_btn_title <- renderUI({
    # print(overview_file_date())
    fd <- if (input$overview_btn == 0){as_date(overview_file_date())} 
    else {"today"}
    span(paste0("Data last updated: ",fd), style = "font-size: 14px ; font-weight: bold")
  })
  
  approvals_file_path <- "./data/download/approvals.csv"
  
  approvals <- eventReactive(input$approvals_btn, {
    trigger_data_load("approvals_btn",input$approvals_btn[1], approvals_file_path, "~/approvals.csv", session, mainPlot=TRUE)
  }, ignoreNULL = FALSE)
  
  # dynamic set file last updated
  output$approvals_btn_title <- renderUI({
    fd <- if (input$approvals_btn == 0){as_date(file.info(approvals_file_path)$ctime)} 
          else {"today"}
    span(paste0("Data last updated: ",fd), style = "font-size: 14px ; font-weight: bold")
  })
  
  diu_divs <- eventReactive(input$diu_btn, {
    trigger_data_load("diu_btn",input$diu_btn[1], "./data/download/diu_divs.csv", "~/diu_divs.csv", session)$code
  }, ignoreNULL = FALSE)
  
  diu_colls <- eventReactive(input$diu_btn, {
    trigger_data_load("diu_btn",input$diu_btn[1], "./data/download/diu_colls.csv", "~/diu_colls.csv", session)
  }, ignoreNULL = FALSE)
  
  diu_pie <- eventReactive(input$diu_btn, {
    trigger_data_load("diu_btn",input$diu_btn[1], "./data/download/diu_pie.csv", "~/diu_pie.csv", session)
  }, ignoreNULL = FALSE)
  
  diu_line_file_path <- "./data/download/diu_line.csv"
  
  diu_line <- eventReactive(input$diu_btn, {
    trigger_data_load("diu_btn",input$diu_btn[1], diu_line_file_path, "~/diu_line.csv", session, mainPlot=TRUE)
  }, ignoreNULL = FALSE)
  
  # dynamic set file last updated
  output$diu_btn_title <- renderUI({
    # print(diu_file_date())
    fd <- if (input$diu_btn == 0){as_date(file.info(diu_line_file_path)$ctime)} 
    else {"today"}
    span(paste0("Data last updated: ",fd), style = "font-size: 14px ; font-weight: bold")
  })
  
  # get user input & init react ---------------------------------------------------------------
  month_start <- reactive({
    fs <- if (length(input$fis_slide) < 2) { c(input$fis_slide, input$fis_slide) } else { input$fis_slide }
    switch(input$f_or_c, 
           calendar = if_else(input$cal_slide[[1]] == 2017, ymd("2017-07-01"), ymd(paste0(input$cal_slide[[1]],"-01-01"))), 
           fiscal = ymd(paste0(as.numeric(fs[[1]])-1,"-07-01")))
  })
  
  month_end <- reactive({
    # print(month_start())
    # print(input$fis_slide[[-1]])
    fs <- if (length(input$fis_slide) < 2) { c(input$fis_slide, input$fis_slide) } else { input$fis_slide }
    # print(fs[[-1]]-1)
    switch(input$f_or_c,
           calendar = if_else(input$cal_slide[[-1]] == year(today()), today()-1, ymd(paste0(input$cal_slide[[-1]],"-12-31"))),
           fiscal = if_else(fs[[-1]]-1 == year(today()), today()-1, ymd(paste0(as.numeric(fs[[-1]]),"-06-30")))) 
  })
  
  # how to filter for ami approvals (approvals tab only: see observers below)
  ami_switch <- reactive({
    switch(input$app_type, all_apps = unique(approvals()$ami), ami_apps = c("AMI"), other_apps = c("Not AMI"))
  })
  
  # what data to use to filter loaded data below (all divs, all divs in approvals incl NO_DIV, or just selected)
  div_choice <- reactive({
    req(input$tabs, input$chooseBy)
    which_division(input$tabs, input$chooseBy, input$centers, input$divisions, diu_divs(), approvals(), div_choices, div_menu)
  })
  
  # conditional hide/show ami tab & radiobuttons
  observeEvent(input$divisions, {
    toggleState("app_type", condition = any(div_choice() %in% ami_div_choices))
  }, ignoreInit = TRUE)
  
  observeEvent(input$app_type, {
    disabled_choices <- !div_choices %in% ami_div_choices
    if (input$app_type == "ami_apps") {
      shinyWidgets::updatePickerInput(session, inputId = "divisions",
                                      choices = div_menu,
                                      selected = ami_div_choices,
                                      choicesOpt = list(
                                        disabled = disabled_choices
                                        ,style = ifelse(disabled_choices, yes = "color: rgba(119, 119, 119, 0.5);", no = "")
                                      ))
    } else {
      shinyWidgets::updatePickerInput(session, inputId = "divisions",
                                      choices = div_menu,
                                      selected = div_choice())
    }
  }, ignoreInit = TRUE)
  
  # reset approvals ami radiobtn 
  observeEvent(input$tabs, {
    reset("app_type")
  })
  
  # reset approvals ami radiobtn 
  observeEvent(input$tabs, {
    reset("app_type")
  })
  
  # reset click plots when select a new division/s
  observe({
    input$tabs
    input$chooseBy
    input$centers
    input$divisions
    input$approvals_btn
    input$diu_btn
    shinyjs::hide("app_coll_bars")
    shinyjs::hide("diu_coll_bars")
    shinyjs::hide("diu_clicked_text")
    shinyjs::hide("diu_pie_1")
    shinyjs::hide("diu_pie_2")
    shinyjs::hide("pami_clicked_text")
    shinyjs::hide("pami_pie_1")
    shinyjs::hide("pami_pie_2")
  })
  
  # overview page ---------------------------------------------------------------
  overview_df <- reactive({ filter_by_division(overview_data(), div_choice()) }) #reactive({ filter_by_division(minmand_totals, div_choice()) })
  
  date_text_overview <- reactive({ paste0("As of ", month(as_date(overview_file_date()),label=TRUE, abbr=FALSE)," ",day(as_date(overview_file_date())),", ",year(as_date(overview_file_date()))) })
  output$n_apps <- renderValueBox({
    valueBox(
      comma_format()(sum(overview_df()$items)), vb_text("Approved items in MMS",date_text_overview()), icon = icon("book"),
      color = "aqua"
    )
  })
  outputOptions(output, "n_apps", suspendWhenHidden = FALSE, priority = 10)
  
  output$n_caps <- renderValueBox({
    valueBox(
      comma_format()(sum(overview_df()$captures)), vb_text("Approved captures in MMS",date_text_overview()), icon = icon("eye"),
      color = "blue"
    )
  })
  outputOptions(output, "n_caps", suspendWhenHidden = FALSE, priority = 10)
  
  output$n_ami <- renderValueBox({
    valueBox(
      comma_format()(sum(overview_df() %>% filter(ami == "AMI") %$% items)), vb_text("Approved AMI items in MMS",date_text_overview()), icon = icon("film"),
      color = "teal"
    )
  })

  output$n_ami_caps <- renderValueBox({
    valueBox(
      comma_format()(sum(overview_df() %>% filter(ami == "AMI") %$% captures)), vb_text("Approved AMI captures in MMS",date_text_overview()), icon = icon("headphones"),
      color = "light-blue"
    )
  })

  # names page ---------------------------------------------------------------
  names_df <- reactive({ 
    filter_by_division(load_or_refresh_data('./data/name_df.csv','',FALSE,FALSE),div_choice()) %>%
      group_by(report_time) %>%
      summarize(name_elements = sum(name_elements),
                role_elements = sum(role_elements),
                # `Name roles` = round(role_elements/name_elements*100, 2),
                name_authority = sum(name_authority),
                `Name authorities` = round(sum(name_authority)/name_elements*100, 2),
                role_authority = sum(role_authority),
                `Name roles` = round(sum(role_authority)/name_elements*100, 2)) %>%
                # `MARC relator code` = round(sum(role_authority)/name_elements*100, 2)) %>%
      mutate(rep_date = zoo::as.Date(zoo::as.yearqtr(report_time, format = "%Y_q%q") - 1/2, frac = 1),
             report_label = paste0(month(rep_date,label=TRUE, abbr=TRUE)," ",str_sub(year(rep_date),start=-2)))
  })
  

  plotdf <- reactive({
    # print(head(names_df()))
    names_df() %>%
      pivot_longer(name_elements:`Name roles`, names_to = 'category', values_to = 'value') %>%
      mutate(category = factor(category, levels = c("name_elements", "name_authority", "role_elements", "role_authority","Name roles","Name authorities"))) %>%
      filter(category %in% c("Name roles","Name authorities"))
  })
  
  output$name_plot <- renderHighchart({
    req(div_choice())

    q_to_label_js <- paste0("var change =  {",paste0(paste0("'",names_df()$report_time,"':'",names_df()$report_label,"'"),collapse=", "),"}; ")
    
    hchart(plotdf(), "column", hcaes(x = report_time, y = value, group = category)) %>%
      hc_xAxis(labels = list(style = list(fontSize = '15px')
                             ,formatter = JS(paste0("function() {",q_to_label_js,"var value = change[this.value]; return value !== 'undefined' ? value : this.value;}"))
                             ), 
               title = list(text = "")) %>%
      hc_yAxis(labels = list(format = '{value}%'),
               title = list(text = ""), max=100
               # , min=min_var
               ) %>%
      hc_colors(nhex) %>%
      hc_chart(backgroundColor = 'rgb(240,240,240)') %>%
      hc_tooltip(formatter = JS(name_role_tooltip(q_to_label_js)))
  })
  
  max_q <- reactive({ max(names_df()$report_time) })
  
  end_of_quarter_vb_text <- reactive({ get_eoq_text(max_q()) })
  
  this_q_in_df <- paste0(str_replace(str_split(this_qtr,'q')[[1]][[1]], 'f', '20'), '_q', str_split(this_qtr,'q')[[1]][[2]])
  
  output$namevb_1 <- renderValueBox({
    req(div_choice())
    valueBox(
      comma_format()(filter(names_df(), report_time == max_q())$name_elements), 
      vb_text('Name elements', end_of_quarter_vb_text()), color = "olive"
    )
  })
  
  output$namevb_2 <- renderValueBox({
    req(div_choice())
    valueBox(
      comma_format()(filter(names_df(), report_time == this_q_in_df)$role_authority - filter(names_df(), report_time == '2019_q3')$role_authority), 
      vb_text('Name roles added or remediated', 'Since March 2019'), color = "olive"
    )
  })
  
  output$namevb_3 <- renderValueBox({
    req(div_choice())
    valueBox(
      comma_format()(filter(names_df(), report_time == this_q_in_df)$name_authority - filter(names_df(), report_time == '2019_q3')$name_authority), 
      vb_text('Name authorities added or remediated', 'Since March 2019'), color = "olive"
    )
  })
  # tor page ---------------------------------------------------------------
  tor_df <- reactive({ 
    filter_by_division(load_or_refresh_data('./data/tor_df.csv','',FALSE,FALSE),div_choice()) %>%
      # load_or_refresh_data('./data/tor_df.csv','',FALSE,FALSE) %>%
      group_by(report_time,resource_type) %>%
      summarise(n = sum(n)) %>%
      arrange(desc(n)) %>%
      mutate(rep_date = zoo::as.Date(zoo::as.yearqtr(report_time, format = "%Y_q%q") - 1/2, frac = 1),
             report_label = paste0(month(rep_date,label=TRUE, abbr=TRUE)," ",str_sub(year(rep_date),start=-2)))
    })
  img <- reactive({ filter(tor_df(), resource_type == 'still image') })
  other <- reactive({ filter(tor_df(), resource_type != 'still image', resource_type != 'software, multimedia') })
  q_to_label_js <- reactive({paste0("var change =  {",paste0(paste0("'",tor_df()$report_time,"':'",tor_df()$report_label,"'"),collapse=", "),"};")})
  
  
  output$img_plot <- renderHighchart({
    # print(img())
    hchart(img(), "column", hcaes(x = resource_type, y = n, group = report_time)) %>%
      hc_xAxis(title = list(text = ""), labels = list(style = list(fontSize = '15px'))) %>%
      hc_yAxis(title = list(text = "")) %>%
      hc_legend(labelFormatter = JS(paste0("function() {",q_to_label_js(),"var value = change[this.name]; return value !== 'undefined' ? value : this.name;}"))) %>%
      hc_colors(qhex) %>%
      hc_chart(backgroundColor = 'rgb(240,240,240)')%>%
      hc_tooltip(formatter = JS(tor_tooltip(q_to_label_js())))
  })
  
  output$tor_plot <- renderHighchart({
    hchart(other(), "column", hcaes(x = resource_type, y = n, group = report_time)) %>%
      hc_xAxis(title = list(text = ""), labels = list(style = list(fontSize = '15px'))) %>%
      hc_yAxis(title = list(text = "")) %>%
      hc_legend(labelFormatter = JS(paste0("function() {",q_to_label_js(),"var value = change[this.name]; return value !== 'undefined' ? value : this.name;}"))) %>%
      hc_colors(qhex) %>%
      hc_chart(backgroundColor = 'rgb(240,240,240)') %>%
      hc_tooltip(formatter = JS(tor_tooltip(q_to_label_js())))
  })

  
  # Rights BD ---------------------------------------------------------------
  rights_df <- reactive({
    load_or_refresh_data('./data/rights.csv',"~/rights.csv",TRUE,FALSE)
  })
  
  output$rightsBD_title <- renderUI({
    req(rights_df())
    end_date <- max(rights_df()$date)
    HTML(paste("<b>Percent remaining unlabeled</b>",br(),"<span style='font-size:12px'>May 2014 - ",month(end_date,label=TRUE, abbr=FALSE)," ",year(end_date),"</span>"))
  })
  
  output$rightsBD_plot <- renderHighchart({
    req(rights_df())

    fit.xy <- lm(percent ~ date, rights_df())
    
    ss_fit <- rights_df() %>%
      tibble::rowid_to_column("ID") %>% 
      left_join(tibble::enframe(name = NULL, fit.xy$fitted.values) %>% tibble::rowid_to_column("ID") %>% rename(trendline = value), by = "ID") %>%
      select(-ID)
    
    hchart(ss_fit, "area", hcaes(date, percent), color = "#cb181d", fillOpacity = 0.1, name = "Percent unlabeled", showInLegend = TRUE) %>% 
      hc_add_series(ss_fit, type = "line", hcaes(date, trendline), name = "Trendline", color = "black", dashStyle = "ShortDot", showInLegend = TRUE) %>%
      hc_yAxis(labels = list(format = "{value}%"), max = 80, min = 0, tickInterval = 20, title = list(text = "")) %>%
      hc_xAxis(type = "datetime", dateTimeLabelFormats = list(month = '%b %e, %Y'), title = list(text = "")) %>%
      hc_tooltip(valueDecimals = 2, pointFormat = "<span style=\"color: {series.color} \">\u25CF</span> {series.name}: <b>{point.y}%</b>")
  })

  # approvals page ---------------------------------------------------------------
  approvals_filtered <- reactive({ 
    # print(month_end())
    prep_approvals(approvals(), div_choice(), ami_switch(), month_start(), month_end()) }) 
  app_coll_name <- reactive({ get_approvals_collections(approvals(), div_choice(), ami_switch(), month_start(), month_end()) })
  
  # create series name/visibility init table (hide captures on init load), update with clicked legend values
  # keeps clicked series visibile/hidden when plot reloads (e.g. with division/center change)
  app_vis <- reactiveValues(series_viz = tibble::tibble(series_name = c("captures on DC","captures on premises","captures other rights",
                                                                        "items on DC","items on premises","items other rights"), 
                                                        visibility = c(FALSE,FALSE,FALSE,TRUE,TRUE,TRUE)))
  
  observeEvent(input$app_legendClicked, {
    # Visibility of target. When deselecting, the target was visible at the moment of click.
    # seriesStatus <- switch(input$app_legendClicked[2], "FALSE" = "visible", "TRUE" = "invisible")
    newVis <- switch(input$app_legendClicked[2], "FALSE" = TRUE, "TRUE" = FALSE)
    # print(paste0("You clicked into the legend and selected series ", input$app_legendClicked[1], " and this Series is now ", seriesStatus, "."))
    app_vis$series_viz <- app_vis$series_viz %>% mutate(visibility = ifelse(series_name == input$app_legendClicked[1], newVis, visibility))
    # print(app_vis$series_viz)
  })
  
  output$monthly_approvals <- renderHighchart({
    # print(head(approvals_filtered()))
    app_click_fxn <- JS("function(event) {Shiny.onInputChange('approvals_click', [this.name, event.point.category, event.point.y]);}")
    app_legendClickFunction <- JS("function(event) {Shiny.onInputChange('app_legendClicked', [this.name, event.target.visible]);}")

    highchart() %>% 
      hc_chart(type = "column", backgroundColor = 'rgb(240,240,240)'
               ,marginRight = 35
               ) %>%
      hc_plotOptions(column = list(stacking = "normal"), 
                     series = list(cursor = "pointer", events = list(click = app_click_fxn, legendItemClick = app_legendClickFunction))
                     ) %>%
      hc_xAxis(categories = as.list(approvals_filtered()$d_month)
               ,labels=list(formatter=JS("function () { return Highcharts.dateFormat('%b %y', new Date(this.value)); }")) #, step = 2
               ,plotLines = get_FY_facet_lines(month_start(), month_end())
      ) %>%
      hc_add_series(name="captures on DC",data = approvals_filtered()$captures_website,stack = "captures",color=c("#43a2ca"), 
                    visible = filter(app_vis$series_viz, series_name == "captures on DC") %$% visibility) %>%
      hc_add_series(name="captures on premises",data = approvals_filtered()$captures_onPrem,stack = "captures",color=c("#7bccc4"), 
                    visible = filter(app_vis$series_viz, series_name == "captures on premises") %$% visibility) %>%
      hc_add_series(name="captures other rights",data = approvals_filtered()$captures_FALSE,stack = "captures",color=c("#a8ddb5"), 
                    visible = filter(app_vis$series_viz, series_name == "captures other rights") %$% visibility) %>%
      hc_add_series(name="items on DC",data = approvals_filtered()$items_website,stack = "items",color=c("#810f7c"), 
                    visible = filter(app_vis$series_viz, series_name == "items on DC") %$% visibility) %>%
      hc_add_series(name="items on premises",data = approvals_filtered()$items_onPrem,stack = "items",color=c("#8856a7"), 
                    visible = filter(app_vis$series_viz, series_name == "items on premises") %$% visibility) %>%
      hc_add_series(name="items other rights",data = approvals_filtered()$items_FALSE,stack = "items",color=c("#8c96c6"), 
                    visible = filter(app_vis$series_viz, series_name == "items other rights") %$% visibility) %>%
      hc_tooltip(formatter = JS(approvals_tooltip)) 
    })
  
  # store clicked values for plotting, use to create approvals collection plot
  app_clickers <- reactiveValues() 
  
  # dynamic set height of collection plot box based on whether it will be a plot or text
  output$app_coll_bars_box <- renderUI({
    dynamic_collection_bar_box(app_clickers$top_colls,"app_coll_bars")
  })
  
  observeEvent(input$approvals_click, {
    show("app_coll_bars")
    clicked_date <- ymd(input$approvals_click[2])
    clicked_record_type <- ifelse(str_detect(input$approvals_click[1], "items"), "items", "captures")
    not_clicked <- ifelse(str_detect(input$approvals_click[1], "items"), "captures", "items")
    clicked_rights <- case_when(str_detect(input$approvals_click[1], "on DC") ~ "website",
                                             str_detect(input$approvals_click[1], "premises") ~ "onPrem",
                                             str_detect(input$approvals_click[1], "other rights") ~ "FALSE")
    app_clickers$clicked_rights_txt <- case_when(str_detect(input$approvals_click[1], "on DC") ~ "on Digital Collections",
                                                 str_detect(input$approvals_click[1], "premises") ~ "that can be displayed on NYPL premises",
                                                 str_detect(input$approvals_click[1], "other rights") ~ "with other rights")
    
    # not_clicked <- app_clickers$not_clicked
    
    tc <- get_approvals_top_colls(app_coll_name(), clicked_rights, clicked_date, not_clicked, clicked_record_type, app_clickers$clicked_rights_txt)
    
    if (nrow(filter(tc, n > 1)) > 0) {
      app_clickers$top_colls <- filter(tc, n > 1)
    } else {
      app_clickers$top_colls <- tc
    }
  })
  
  output$app_coll_bars <- renderHighchart({
    req(app_clickers$top_colls)
    top_colls <- app_clickers$top_colls

    if (nrow(top_colls) == 1) {
      # print(top_colls)
      highchart() %>%
        hc_chart(height = 100) %>%
        hc_title(useHTML = TRUE,
                 text =  paste0(top_colls$tooltip[[1]], ' in <a href="',top_colls$url[[1]],'" target="_blank" style="color:',top_colls$hex[[1]],';"><u>',
                                top_colls$collection_title[[1]],'</u></a>.')
        )
    } else {
      # print(head(top_colls))
      highchart() %>%
        hc_chart(type = "bar") %>% #, events = list(load = JS(hc_load))
        hc_xAxis(categories = top_colls$collection_title, visible = TRUE
                 # , labels = list(enabled = FALSE)
                 , lineWidth=2) %>%
        hc_yAxis(visible = FALSE) %>%
        hc_add_series(data = top_colls, type = "bar", hcaes(x = collection_title, y = n, key = url, color=hex), name = "collection", showInLegend = FALSE,
                      pointWidth=25
                      # ,dataLabels = list(enabled = TRUE, format = '{point.category}')
        ) %>%
        hc_tooltip(formatter = JS(gen_hc_tooltip)) %>%
        # otherwise links open same page
        hc_plotOptions(
          series = list(
            cursor = "pointer",
            point = list(events = list(click = JS( "function () { window.open(this.options.key, '_blank'); }")))
          )
        ) %>%
        hc_title(text = paste0('<b>Top collections ',app_clickers$clicked_rights_txt,', approved in ',paste(month(top_colls$d_month[[1]], label = TRUE, abbr = FALSE), year(top_colls$d_month[[1]])),'</b>')
                 ,style = list(color = "black", useHTML = TRUE))
    }
  })
  
  # text to display in approvals valuebox
  ami_text <- renderText({ 
    switch(input$app_type, all_apps = "", ami_apps = "AMI", other_apps = "other item")
  })
  
  output$progressBox <- renderValueBox({
    num <- sum(select(approvals_filtered(),starts_with("items")), na.rm=TRUE)
    valueBox(
      comma_format()(num), vb_text(paste(ami_text()," Item approvals"), ""
      ), icon = icon("thumbs-up", lib = "glyphicon"),
      color = "purple"
    )
  })
  
  output$capProgressBox <- renderValueBox({
    num <- sum(select(approvals_filtered(),starts_with("captures")), na.rm=TRUE)
    valueBox(
      comma_format()(num), vb_text(paste(ami_text()," Capture approvals"), ""
      ), icon = icon("thumbs-up", lib = "glyphicon"),
      color = "light-blue"
    )
  })
  
  output$app_rights_box <- renderValueBox({
    with_rights <- sum(select(approvals_filtered(), "items_website", "items_onPrem"), na.rm=TRUE)
    num <- percent(with_rights / sum(select(approvals_filtered(),starts_with("items")), na.rm=TRUE),0.1)
    valueBox(
      num, vb_text(paste(ami_text()," Items on Digital Collections"), ""), icon = icon("image"),
      color = "purple"
    )
  })
  
  output$cap_rights_box <- renderValueBox({
    num <- percent(sum(select(approvals_filtered(), "captures_website", "captures_onPrem"), na.rm=TRUE) / sum(select(approvals_filtered(),starts_with("captures")), na.rm=TRUE),0.1)
    valueBox(
      num, vb_text(paste(ami_text()," Captures on Digital Collections"), ""), icon = icon("image"),
      color = "light-blue"
    )
  })
  
  # diu page ---------------------------------------------------------------
  diu_coll_name <- reactive({ filter_by_division(diu_colls(), div_choice()) })
  
  # draft items have no approval date
  diu_pie_df <- reactive({ filter_by_division(diu_pie(), div_choice()) %>% 
      drop_na(starts_with("date")) %>%
      select(capture_id, code, ends_with("_month")) %>%
      rename_at(vars(ends_with('_month')), ~ str_remove(., '_month'))
    })
  
  diu_month_start <- reactive({
    if_else(input$diu_slide[[1]] == 2017, ymd("2017-07-01"), ymd(paste0(input$diu_slide[[1]],"-01-01")))
  })
  
  diu_month_end <- reactive({
    if_else(input$diu_slide[[-1]] == year(today()), today()-1, ymd(paste0(input$diu_slide[[-1]],"-12-31")))
  })
  
  output$diu_plot <- renderHighchart({
    diu_line_filtered <- prep_diu_line(diu_line(), div_choice()) %>% filter(between(d_month, diu_month_start(), diu_month_end()))
    diu_click <- JS("function(event) {Shiny.onInputChange('diu_clicked', [this.name, event.point.category, event.point.y]);}")

    highchart() %>%
      hc_xAxis(type = "datetime", dateTimeLabelFormats = list(month = "%b %Y"), align = "center") %>%
      hc_plotOptions(series = list(cursor = "pointer", events = list(click = diu_click))) %>%
      hc_add_series(diu_line_filtered, "line", name = "created", hcaes(d_month, created), color = "#bebada", marker = list(symbol = "circle")) %>%
      hc_add_series(diu_line_filtered, "line", name = "captured", hcaes(d_month, captured), color = "#fb8072", marker = list(symbol = "circle")) %>%
      hc_add_series(diu_line_filtered, "line", name = "approved", hcaes(d_month, approved), color = "#80b1d3", marker = list(symbol = "circle")) 
  })
  
  diu_date_vars <- c('created','captured','approved')
  diu_dark_cols <- c('#726f82','#af594f','#597b93')
  diu_light_cols <- c('#d1cee5','#fca69c','#b2d0e4')
  
  # store clicked values for plotting
  clickers <- reactiveValues() 
  observeEvent(input$diu_clicked, {
    shinyjs::show("diu_clicked_text")
    shinyjs::show("diu_coll_bars")
    shinyjs::show("diu_pie_1")
    shinyjs::show("diu_pie_2")
    # print(input$diu_clicked)
    clickers$clicked_y <- clicked_y <- comma_format()(as.numeric(input$diu_clicked[3]))
    clicked_posix <- as.Date(as.POSIXct(as.numeric(input$diu_clicked[2])/1000, origin="1970-01-01"))
    clickers$clicked_date_str <- clicked_date_str <- strftime(clicked_posix, "%B %Y")
    clickers$clicked_date <- clicked_date <- ymd(strftime(clicked_posix, "%Y-%m-%d")) # type chr
    clickers$clicked_category <- clicked_category <- input$diu_clicked[1]
    # clicked_month_label <- paste0(clicked_category,'_month')
    # clicked_date_label <- paste0('date_',clicked_category)
    
    withProgress(message = "Computing results", detail = "fetching data", value = 0, {
      
      incProgress(0.25, detail = "wrangling data for this point")
      
      # get all captures in this division(s) and the clicked month
      subDN <- get_clicked_pie_df(diu_pie_df(), clicked_category, clicked_date, diu_date_vars)
      
      other_dts <- setdiff(diu_date_vars,c(clicked_category))
      
      first_half <- get_pie(subDN, other_dts[[1]],clicked_category,clicked_date_str,diu_date_vars) %>% 
        tibble::add_column(hex = add_pie_hex(.,diu_date_vars,diu_dark_cols,diu_light_cols)) %>% arrange(d_month)
      clickers$first_half <- first_half

      second_half <- get_pie(subDN, other_dts[[2]],clicked_category,clicked_date_str,diu_date_vars) %>%
        tibble::add_column(hex = add_pie_hex(.,diu_date_vars,diu_dark_cols,diu_light_cols)) %>% arrange(d_month)
      clickers$second_half <- second_half
      
      tc <- get_diu_top_colls(diu_coll_name(), clicked_category, clicked_date)
      if (nrow(filter(tc, n > 1)) > 0) { clickers$top_colls <- filter(tc, n > 1) } else { clickers$top_colls <- tc  }
      
      incProgress(0.25, detail = "generating plots")
      
    })
  })
  
  output$diu_clicked_text <- renderUI({
    req(clickers$clicked_y)

    img <- ifelse(clickers$clicked_category == "captured", " images ", " image metadata records ")
    tagList(
      HTML(paste0("<b>Of the ",clickers$clicked_y,img,clickers$clicked_category," in ",clickers$clicked_date_str,"...</b>")),
      tags$style("#diu_clicked_text {background-color:#f8f8f8;}")
    )
  })
  
  output$diu_pie_1 <- renderHighchart({
    req(clickers$first_half)

    highchart() %>%
      hc_add_series(clickers$first_half, hcaes(x=d_month, y=n, color = hex), startAngle = -90, endAngle = 90, type="pie",
                    dataLabels=list(enabled=TRUE, distance = -30, formatter = JS("function() { if (this.point.month_label === '') { return null } else { return this.point.month_label} }"))
                    ,innerSize= '50%', size ='100%'
      ) %>%
      hc_tooltip(formatter = JS(pie_tooltip)) %>%
      hc_title(text = paste0('<b>',clickers$first_half$date_type[[1]],'</b>')
               ,style = list(useHTML = TRUE)
               ,verticalAlign = 'middle'
               ,floating = TRUE
      )
  })
  
  output$diu_pie_2 <- renderHighchart({
    req(clickers$second_half)
    
    highchart() %>%
      hc_add_series(clickers$second_half, hcaes(x=d_month, y=n, color = hex), startAngle = -90, endAngle = 90, type="pie",
                    dataLabels=list(enabled=TRUE, distance = -30, formatter = JS("function() { if (this.point.month_label === '') { return null } else { return this.point.month_label} }"))
                    ,innerSize= '50%', size ='100%'
      ) %>%
      hc_tooltip(formatter = JS(pie_tooltip)) %>%
      hc_title(text = paste0('<b>',clickers$second_half$date_type[[1]],'</b>')
               ,style = list(useHTML = TRUE)
               ,verticalAlign = 'middle'
               ,floating = TRUE
      )
  })
  
  # dynamic set height of collection plot box based on whether it will be a plot or text
  output$diu_coll_bars_box <- renderUI({
    dynamic_collection_bar_box(clickers$top_colls,"diu_coll_bars")
  })
  
  output$diu_coll_bars <- renderHighchart({
    req(clickers$top_colls)
    top_colls <- clickers$top_colls
    if (nrow(top_colls) == 1) {
      # top_colls <- filter(tc, n > 1)
      highchart() %>%
        hc_chart(height = 100) %>%
        hc_title(useHTML = TRUE,
                 text =  paste0(top_colls$tooltip[[1]], ' in <a href="',top_colls$url[[1]],'" target="_blank" style="color:',top_colls$hex[[1]],';"><u>',
                                top_colls$collection_title[[1]],'</u></a>.')
        )
    } else {
      # top_colls <- tc
      highchart() %>%
        hc_chart(type = "bar"
                 # very long collection titles being drawn outside plot area on axis side (cut off), need to calc manually in plot load
                 # ,events = list(load = JS(hc_load))
        ) %>%
        hc_xAxis(categories = top_colls$collection_title, visible = TRUE
                 # , labels = list(enabled = FALSE)
                 , lineWidth=2) %>%
        hc_yAxis(visible = FALSE) %>%
        hc_add_series(data = top_colls, type = "bar", hcaes(x = collection_title, y = n, key = url, color=hex), name = "collection", showInLegend = FALSE,
                      pointWidth=25
                      # , dataLabels = list(enabled = TRUE
                      #                                  # ,formatter = JS(diu_colls_labeller)
                      #                                  ,format = '{point.category}'
                      # )
        ) %>%
        hc_tooltip(formatter = JS(gen_hc_tooltip)) %>%
        # otherwise links open same page
        hc_plotOptions(
          series = list(
            cursor = "pointer",
            point = list(events = list(click = JS( "function () { window.open(this.options.key, '_blank'); }")))
          )
        ) %>%
        hc_title(text = paste0('<b>Top collections ',top_colls$date_label[[1]],' in ',
                               paste(month(top_colls$d_month[[1]], label = TRUE, abbr = FALSE), 
                                     year(top_colls$d_month[[1]])),'</b>')
                 ,style = list(color = "black", useHTML = TRUE))
    }
  
  })
  

  # pami page ---------------------------------------------------------------
  piedf <- vroom::vroom(paste0('./data/pami_pie_',this_qtr,'.csv')) #('./data/pami_pie_f21q2.csv')
  pami_line <- vroom::vroom(paste0('./data/pami_line_',this_qtr,'.csv')) %>% #vroom::vroom(paste0('./data/pami_line_2',this_qtr,'.csv')) %>%
    group_by(d_month, date_type) %>%
    summarise(n = sum(n)) %>%
    pivot_wider(names_from = date_type, values_from = n) %>%
    mutate(reported = ifelse(d_month > ymd('2019-01-01') & is.na(reported), 0, reported),
           ingested = replace_na(ingested,0),
           digitized= ifelse(d_month > ymd('2017-07-01') & is.na(digitized), 0, digitized)) 
  # print(head(pami_line))
  
  output$pami_plot <- renderHighchart({

    pami_click <- JS("function(event) {Shiny.onInputChange('pami_clicked', [this.name, event.point.category, event.point.y]);}")
    
    highchart() %>%
      hc_xAxis(type = "datetime", dateTimeLabelFormats = list(month = "%b %Y"), align = "center") %>%
      hc_plotOptions(series = list(cursor = "pointer", events = list(click = pami_click))) %>%
      hc_add_series(pami_line, "line", name = "digitized", hcaes(d_month, digitized), color = "#a61d4d", marker = list(symbol = "circle")) %>%
      hc_add_series(pami_line, "line", name = "reported", hcaes(d_month, reported), color = "#1b9e77", marker = list(symbol = "circle")) %>%
      hc_add_series(pami_line, "line", name = "ingested", hcaes(d_month, ingested), color = "#7570b3", marker = list(symbol = "circle")) 
  })
  

  pami_date_vars <- c('digitized','reported','ingested')
  pami_dark_cols <- c('#530e26','#093125','#33305a')
  pami_light_cols <- c('#d28ea6','#2bdba6','#a5a1cd')
  
  # store clicked values for plotting
  pami_clickers <- reactiveValues() 
  observeEvent(input$pami_clicked, {
    shinyjs::show("pami_clicked_text")
    shinyjs::show("pami_pie_1")
    shinyjs::show("pami_pie_2")

    pami_clickers$clicked_y <- clicked_y <- comma_format()(as.numeric(input$pami_clicked[3]))
    clicked_posix <- as.Date(as.POSIXct(as.numeric(input$pami_clicked[2])/1000, origin="1970-01-01"))
    pami_clickers$clicked_date_str <- clicked_date_str <- strftime(clicked_posix, "%B %Y")
    pami_clickers$clicked_date <- clicked_date <- ymd(strftime(clicked_posix, "%Y-%m-%d"))
    pami_clickers$clicked_category <- clicked_category <- input$pami_clicked[1]

    # print(clicked_date)
    # print(clicked_category)
    
    withProgress(message = "Computing results", detail = "fetching data", value = 0, {

      incProgress(0.25, detail = "wrangling data for this point")

      # get all captures in this division(s) and the clicked month
      p_subDN <- get_clicked_pie_df(piedf, clicked_category, clicked_date, pami_date_vars)
      
      p_other_dts <- setdiff(pami_date_vars,c(clicked_category))
      # print(p_other_dts[[1]])
      
      fh <- get_pie(p_subDN, p_other_dts[[1]], clicked_category, clicked_date_str, diu_date_vars) %>% 
        tibble::add_column(hex = add_pie_hex(.,pami_date_vars,pami_dark_cols,pami_light_cols)) %>% arrange(d_month)
      pami_clickers$first_half <- fh
      # print(head(fh))
      
      sh <- get_pie(p_subDN, p_other_dts[[2]], clicked_category, clicked_date_str, diu_date_vars) %>% 
        tibble::add_column(hex = add_pie_hex(.,pami_date_vars,pami_dark_cols,pami_light_cols)) %>% arrange(d_month)
      pami_clickers$second_half <-  sh
      # print(head(sh))

      incProgress(0.25, detail = "generating plots")

    })
  })

  output$pami_clicked_text <- renderUI({
    # print(pami_clickers)
    req(pami_clickers$clicked_y)
    tagList(
      HTML(paste0("<b>Of the ",pami_clickers$clicked_y," records ",pami_clickers$clicked_category," in ",pami_clickers$clicked_date_str,"...</b>")),
      tags$style("#pami_clicked_text {background-color:#f8f8f8;}")
    )
  })
  
  output$pami_pie_1 <- renderHighchart({
    req(pami_clickers$first_half)
    
    highchart() %>%
      hc_add_series(pami_clickers$first_half, hcaes(x=d_month, y=n, color=hex), startAngle = -90, endAngle = 90, type="pie",
                    dataLabels=list(enabled=TRUE, distance = -30, formatter = JS("function() { if (this.point.month_label === '') { return null } else { return this.point.month_label} }"))
                    ,innerSize= '50%', size ='100%'
      ) %>%
      hc_tooltip(formatter = JS(pie_tooltip)) %>%
      hc_title(text = paste0('<b>',pami_clickers$first_half$date_type[[1]],'</b>')
               ,style = list(useHTML = TRUE)
               ,verticalAlign = 'middle'
               ,floating = TRUE
      )
  })
  
  output$pami_pie_2 <- renderHighchart({
    req(pami_clickers$second_half)
    
    highchart() %>%
      hc_add_series(pami_clickers$second_half, hcaes(x=d_month, y=n, color=hex), startAngle = -90, endAngle = 90, type="pie",
                    dataLabels=list(enabled=TRUE, distance = -30, formatter = JS("function() { if (this.point.month_label === '') { return null } else { return this.point.month_label} }"))
                    ,innerSize= '50%', size ='100%'
      ) %>%
      hc_tooltip(formatter = JS(pie_tooltip)) %>%
      hc_title(text = paste0('<b>',pami_clickers$second_half$date_type[[1]],'</b>')
               ,style = list(useHTML = TRUE)
               ,verticalAlign = 'middle'
               ,floating = TRUE
      )
  })
  
  # proportion page ---------------------------------------------------------------
  fiscal_qtrs <- reactive({ selected_fiscal_qtr(input$fy_select) })
  
  mds_prop <- reactive({ 
    df <- readRDS(get_catalogued_or_all(input$prop_cat, mm_prop_path))
    get_mdsqual_prop(df, input$compare_prop, div_choice(), fiscal_qtrs()) 
  })
  
  output$prop_plot <- renderHighchart({
    if (input$compare_prop == "compare_divs"){
      plines <- get_center_facet_lines(mds_prop())
      
      highchart() %>%
        hc_chart(type = "column") %>%
        hc_plotOptions(series=list(stacking='normal')) %>% 
        hc_yAxis(lineColor = "lightgrey", lineWidth = 5,
                 labels  = list(formatter = JS("function () { return Math.abs(this.value) + '%'; }")), 
                 max = 125, min = -100, showLastLabel = FALSE, tickInterval = 25, minorTickInterval = 25,
                 plotBands = list(
                   list(from = 105, to = 125, color = "rgba(211, 211, 211, 0.3)",
                        label = list(text = "", verticalAlign = "top")))
        ) %>%
        hc_xAxis(categories = mds_prop()$code, labels=list(rotation = 0, staggerLines = 2),
                 plotLines = plines) %>% 
        hc_legend(enabled = FALSE) %>%
        hc_tooltip(formatter = JS(prop_tooltip)) %>%
        hc_add_series(mds_prop() %>% filter(prop_type=="p_above") %$% values*100, name = "meet", showInLegend = FALSE,color=c("#99d594")) %>%
        hc_add_series(mds_prop() %>% filter(prop_type=="p_below") %$% -values*100, name = "are below", showInLegend = FALSE,color=c("#fc8d59"))
    }
    else {
      highchart() %>% 
        hc_xAxis(categories = mds_prop()$fy_label) %>% 
        hc_yAxis(labels  = list(formatter = JS("function () { return Math.abs(this.value) + '%'; }")), maxPadding = 5, minPadding = 5, ceiling = 100) %>% #, max = 100
        hc_legend(enabled = FALSE) %>%
        hc_tooltip(formatter = JS(line_tooltip)) %>%
        hc_add_series(name = "meet requirements", data = mds_prop() %>% filter(prop_type=="p_above") %$% values*100, color = '#99d594')
    }
    
  })
  
  end_of_quarter_prop <- reactive({ 
    if(input$compare_prop == "compare_divs") { max_q <- fiscal_qtrs() } else { max_q <- max(sort(unique(fiscal_slider_text(unique(mm_prop$fy_q))), decreasing = TRUE)) }  
    get_eoq_text(max_q)
  })
  
  output$perc_above <- renderValueBox({
    if (input$compare_prop == "compare_divs"){
      valueBox(
        paste0(round(sum(mds_prop() %>% filter(prop_type == "p_above") %$% n_above)/sum(mds_prop() %>% filter(prop_type == "p_above") %$% n_recs),3)*100, "%"), 
        vb_text("Meet minimum mandatory requirements",end_of_quarter_prop()), icon = icon("arrow-up", lib = "glyphicon"), color = "green"
      )
    }
    else {
      end_of_quarter_prop <- get_eoq_text(max(sort(unique(fiscal_slider_text(unique(mm_prop$fy_q))), decreasing = TRUE)))
      # print(vb_text("Meet minimum mandatory requirements",end_of_quarter_prop()))
      valueBox(
        paste0(round(sum(mds_prop() %>% filter(prop_type == "p_above", fy_q == max(fy_q)) %$% n_above)/sum(mds_prop() %>% filter(prop_type == "p_above", fy_q == max(fy_q)) %$% n_recs),3)*100, "%"), 
        vb_text("Meet minimum mandatory requirements",end_of_quarter_prop()), icon = icon("arrow-up", lib = "glyphicon"), color = "green"
      )
    }
    
  })
  
  output$perc_below <- renderValueBox({
    if (input$compare_prop == "compare_divs"){
      valueBox(
        paste0(round(sum(mds_prop() %>% filter(prop_type == "p_below") %$% n_below)/sum(mds_prop() %>% filter(prop_type == "p_below") %$% n_recs),3)*100, "%"), 
        vb_text("Below minimum mandatory requirements",end_of_quarter_prop()), icon = icon("arrow-down", lib = "glyphicon"), color = "orange"
      )
    }
    else {
      valueBox(
        paste0(round(sum(mds_prop() %>% filter(prop_type == "p_below", fy_q == max(fy_q)) %$% n_below)/sum(mds_prop() %>% filter(prop_type == "p_below", fy_q == max(fy_q)) %$% n_recs),3)*100, "%"), 
        vb_text("Below minimum mandatory requirements",end_of_quarter_prop()), icon = icon("arrow-down", lib = "glyphicon"), color = "orange"
      )
    }
  })
  
  
  # elements phase 1 data ---------------------------------------------------------------
  elements_by_division_all <- reactive({ readr::read_csv(get_catalogued_or_all(input$elements_cat, ebd_all_path)) })
  elements_by_division <- reactive({ readr::read_csv(get_catalogued_or_all(input$elements_cat, elements_by_division_file)) })
  genre_rem <- reactive({ readRDS(get_catalogued_or_all(input$elements_cat, genre_path)) }) 
  date_rem <- reactive({ readRDS(get_catalogued_or_all(input$elements_cat, date_path)) }) 
  location_rem <- reactive({ readRDS(get_catalogued_or_all(input$elements_cat, loc_path)) }) 
  id_rem <- reactive({ readRDS(get_catalogued_or_all(input$elements_cat, id_path)) }) 
  
  output$e_plot_title <- renderUI({
    HTML(paste("Remediation project highlights",br(), "<span style='font-size:12px'>March 2019 - ",month(eoq_elements,label=TRUE, abbr=FALSE)," ",year(eoq_elements),"</span>"))
  })
  
  prop_uncatalogued <- reactive({
    if (input$elements_cat == FALSE) {
      df <- if (length(div_choice()) == 0 | length(div_choice()) == 26) {
        readr::read_csv('./data/prop_no_id.csv')
      } else {
        readr::read_csv('./data/prop_no_id.csv') %>%
          filter(code %in% div_choice()) 
      }
      round(sum(df$count)/sum(df$total) * 100, digits = 0)
    } else {0}
    
  })
  #  
  output$elements_uncat_text <- renderUI({
    these_this <- if_else(length(div_choice()) == 1, 'this division', 'these divisions')
    if(!is.na(prop_uncatalogued()) & prop_uncatalogued() != 0) {
      tags$p(HTML(paste0("<i class='fa fa-info-circle'></i>  MSU staff estimate that approximately ",prop_uncatalogued(),"% of MMS items in ",these_this," are not associated with public-facing catalog, finding aid, or TMS records.")), style="white-space: pre-wrap")
    } 
  })
  
  elements_df <- reactive({ get_scores_by_element(elements_by_division_all(),elements_by_division(),div_choice()) })
  
  # elements extended/phase 2 data ---------------------------------------------------------------
  ext_file <- paste0('.',str_split(elements_by_division_file,'[.]')[[1]][2], '_ext.', str_split(elements_by_division_file,'[.]')[[1]][3])
  ext_elements <- c('typeOfResource', 'archives_identifiers', 'genre', 'date')
  ext_vb_files <- paste0("./data/",ext_elements,'_up_',this_qtr,"_ext.rds")
  
  output$ext_plot_title <- renderUI({
    HTML(paste("Remediation project highlights",br(), "<span style='font-size:12px'>March 2019 - ",month(eoq_elements,label=TRUE, abbr=FALSE)," ",year(eoq_elements),"</span>"))
  })
  
  ext_init <- reactive({ readr::read_csv(get_catalogued_or_all(input$elements_cat, ext_file)) })
  ext_df <- reactive({ 
    get_ebd(ext_init(), div_choice()) %>% 
      filter(!element %in% c('title','location')) %>%
      mutate(element=recode(element, "tms_identifiers"="TMS identifiers","archives_identifiers"="Archives identifiers"))
  })
  ext_min <- reactive({ ifelse(floor(min(select(ext_df(), starts_with("score_1"))))-10 < 0, 0, floor(min(select(ext_df(), starts_with("score_1"))))-10)  })
  
  # elements phase1/phase2 plot ---------------------------------------------------------------
  
  output$elements_text <- renderUI({
    if(input$phase_btn == "Phase 1") {
      includeHTML("./text/e_scores.html")
    } else {
      includeHTML("./text/extended.html")
    }
  })
  
  output$e_plot <- renderHighchart({
    if(input$phase_btn == "Phase 1") {
      current_data <- sort(names(select(elements_df(), ends_with(str_replace(end_of_quarter_elements,' ','_')))))
      highchart() %>% 
        hc_chart(type = "column") %>%
        hc_plotOptions(column = list(stacking = "normal")) %>%
        hc_xAxis(categories = elements_df()$element,#  y[sort(order(y)[elements_df()$element])]                   
                 labels=list(formatter=elements_bar_label)) %>%
        hc_yAxis(labels = list(format = "{value}%"), max = 100, min = floor(min(select(elements_df(), starts_with("score_1"))))-10,
                 plotBands = list(color="rgba(169,169,169,0.3)",from=100-prop_uncatalogued(),to=100,zIndex=3)
        ) %>%
        hc_legend(enabled = FALSE) %>%
        hc_add_series(name="Mar 19",data = elements_df()$score_0_Mar_19,stack = "baseline",color=c("#fc8d59")) %>%
        hc_add_series(name="Mar 19",data = elements_df()$score_0.5_Mar_19,stack = "baseline",color=c("#fee08b")) %>%
        hc_add_series(name="Mar 19",data = elements_df()$score_1_Mar_19,stack = "baseline",color=c("#91cf60")) %>%
        hc_add_series(name=end_of_quarter_elements,data = elements_df()[[current_data[[1]]]],stack = "current",color=c("#fc8d59")) %>%
        hc_add_series(name=end_of_quarter_elements,data = elements_df()[[current_data[[2]]]],stack = "current",color=c("#fee08b")) %>%
        hc_add_series(name=end_of_quarter_elements,data = elements_df()[[current_data[[3]]]],stack = "current",color=c("#91cf60")) %>%
        hc_tooltip(formatter = JS(element_tooltip))
    } else {
      current_data <- sort(names(select(ext_df(), ends_with(str_replace(end_of_quarter_elements,' ','_')))))
      highchart() %>%
        hc_chart(type = "column") %>%
        hc_plotOptions(column = list(stacking = "normal")) %>%
        hc_xAxis(categories = ext_df()$element,#  y[sort(order(y)[elements_df()$element])]
                 labels=list(formatter=elements_bar_label)) %>%
        hc_yAxis(labels = list(format = "{value}%"), max = 100, min = ext_min()) %>%
        hc_legend(enabled = FALSE) %>%
        hc_add_series(name="Mar 19",data = ext_df()$score_0_Mar_19,stack = "baseline",color=c("#fc8d59")) %>%
        hc_add_series(name="Mar 19",data = ext_df()$score_0.5_Mar_19,stack = "baseline",color=c("#fee08b")) %>%
        hc_add_series(name="Mar 19",data = ext_df()$score_1_Mar_19,stack = "baseline",color=c("#91cf60")) %>%
        hc_add_series(name=end_of_quarter_elements,data = ext_df()[[current_data[[1]]]],stack = "current",color=c("#fc8d59")) %>%
        hc_add_series(name=end_of_quarter_elements,data = ext_df()[[current_data[[2]]]],stack = "current",color=c("#fee08b")) %>%
        hc_add_series(name=end_of_quarter_elements,data = ext_df()[[current_data[[3]]]],stack = "current",color=c("#91cf60")) %>%
        hc_tooltip(formatter = JS(element_tooltip))
    }
    })
  
  # elements phase1/phase2 vbs ---------------------------------------------------------------
  
  output$rem1 <- renderValueBox({
    if(input$phase_btn == "Phase 1") {
      valueBox(num_remediated(id_rem(), div_choice()), "Items had at least one identifier added", icon = icon("id-card"), color = "lime")
    } else {
      valueBox(num_remediated(readRDS(get_catalogued_or_all(input$elements_cat, ext_vb_files[[1]])), div_choice()), "Items had at least one resource type remediated", icon = icon("photo-video"), color = "lime")
    }
  })

  output$rem2 <- renderValueBox({
    if(input$phase_btn == "Phase 1"){
      valueBox(num_remediated(genre_rem(), div_choice()), "Items had at least one genre term remediated", icon = icon("broom"), color = "fuchsia")
    } else {
      valueBox(num_remediated(readRDS(get_catalogued_or_all(input$elements_cat, ext_vb_files[[2]])), div_choice()), "Items had at least one archives identifier added", icon = icon("id-card"), color = "fuchsia")
      }
  })

  output$rem3 <- renderValueBox({
    if(input$phase_btn == "Phase 1"){
      valueBox(num_remediated(date_rem(), div_choice()), "Items had at least one date remediated", icon = icon("calendar-plus"), color = "black")
    } else {
      valueBox(num_remediated(readRDS(get_catalogued_or_all(input$elements_cat, ext_vb_files[[3]])), div_choice()), "Items had at least one genre term remediated", icon = icon("broom"), color = "black")
    }
  })

  output$rem4 <- renderValueBox({
    if(input$phase_btn == "Phase 1"){
      valueBox(num_remediated(location_rem(), div_choice()), "Items had at least one location remediated", icon = icon("globe"), color = "maroon")
    } else {
      valueBox(num_remediated(readRDS(get_catalogued_or_all(input$elements_cat, ext_vb_files[[4]])), div_choice()), "Items had at least one date remediated", icon = icon("calendar-plus"), color = "maroon")
    }
  })
  
  # elements if applicable page ---------------------------------------------------------------
  output$ifapp_plot_title <- renderUI({
    HTML(paste("Remediation project highlights",br(), "<span style='font-size:12px'>March 2019 - ",month(eoq_elements,label=TRUE, abbr=FALSE)," ",year(eoq_elements),"</span>"))
  })
  
  ifapp_file <- paste0('.',str_split(elements_by_division_file,'[.]')[[1]][2], '_ifapp.', str_split(elements_by_division_file,'[.]')[[1]][3])
  ifapp_init <- reactive({ readr::read_csv(get_catalogued_or_all(input$ifapp_check, ifapp_file)) })
  ifapp_df <- reactive({ get_ebd(ifapp_init(), div_choice()) %>% filter(!element %in% c('title','location')) })
  ifapp_min <- reactive({ ifelse(floor(min(select(ifapp_df(), starts_with("score_1"))))-10 < 0, 0, floor(min(select(ifapp_df(), starts_with("score_1"))))-10)  })
  
  ifapp_elements <- c('name', 'form', 'note', 'subject')
  ifapp_vb_files <- paste0("./data/",ifapp_elements,'_up_',this_qtr,"_ifapp.rds")
  
  output$ifapp_plot <- renderHighchart({
    # print(head(ifapp_df()))
    current_data <- sort(names(select(ifapp_df(), ends_with(str_replace(end_of_quarter_elements,' ','_')))))
    # print(current_data)
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_plotOptions(column = list(stacking = "normal")) %>%
      hc_xAxis(categories = ifapp_df()$element,#  y[sort(order(y)[elements_df()$element])]
               labels=list(formatter=elements_bar_label)) %>%
      hc_yAxis(labels = list(format = "{value}%"), max = 100, min = ifapp_min()
               # ,plotBands = list(color="rgba(169,169,169,0.3)",from=100-prop_uncatalogued(),to=100,zIndex=3)
      ) %>%
      hc_legend(enabled = FALSE) %>%
      hc_add_series(name="Mar 19",data = ifapp_df()$score_0_Mar_19,stack = "baseline",color=c("#fc8d59")) %>%
      hc_add_series(name="Mar 19",data = ifapp_df()$score_0.5_Mar_19,stack = "baseline",color=c("#fee08b")) %>%
      hc_add_series(name="Mar 19",data = ifapp_df()$score_1_Mar_19,stack = "baseline",color=c("#91cf60")) %>%
      hc_add_series(name=end_of_quarter_elements,data = ifapp_df()[[current_data[[1]]]],stack = "current",color=c("#fc8d59")) %>%
      hc_add_series(name=end_of_quarter_elements,data = ifapp_df()[[current_data[[2]]]],stack = "current",color=c("#fee08b")) %>%
      hc_add_series(name=end_of_quarter_elements,data = ifapp_df()[[current_data[[3]]]],stack = "current",color=c("#91cf60")) %>%
      hc_tooltip(formatter = JS(element_tooltip))
  })
  
  output$ifapp1 <- renderValueBox({
    valueBox(num_remediated(readRDS(get_catalogued_or_all(input$ifapp_check, ifapp_vb_files[[1]])), div_choice()), "Items had at least one name remediated", icon = icon("user"), color = "lime")
  })
  
  output$ifapp2 <- renderValueBox({
    valueBox(num_remediated(readRDS(get_catalogued_or_all(input$ifapp_check, ifapp_vb_files[[2]])), div_choice()), "Items had at least one form term remediated", icon = icon("broom"), color = "fuchsia")
  })
  
  output$ifapp3 <- renderValueBox({
    valueBox(num_remediated(readRDS(get_catalogued_or_all(input$ifapp_check, ifapp_vb_files[[3]])), div_choice()), "Items had at least one note remediated", icon = icon("copy"), color = "black")
  })
  
  output$ifapp4 <- renderValueBox({
    valueBox(num_remediated(readRDS(get_catalogued_or_all(input$ifapp_check, ifapp_vb_files[[4]])), div_choice()), "Items had at least one subject remediated", icon = icon("shapes"), color = "maroon")
  })
  
  # reports and data download  ---------------------------------------------------------------
  # download rights
  output$downloadRights <- downloadHandler(
    filename = function() {
      paste('rights_data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write_csv(rights_df(), con)
    }
  )
  
  # download approvals
  app_data_out <- reactive({
    get_approvals_for_dl(approvals(), div_choice(), ami_switch(), month_start(), month_end())
  })
  
  output$downloadApp <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write_csv(app_data_out(), con)
    }
  )
  
  # download diu
  diu_data_out <- reactive({
    get_diu_for_dl(diu_line(),div_choice())
  })
  
  output$downloadDIU <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write_csv(diu_data_out(), con)
    }
  )
  
  # download pami
  output$downloadPAMI <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write_csv(pami_line %>% rename(date = d_month), con)
    }
  )
  
}

shinyApp(ui = ui, server = server, enableBookmarking = "url")