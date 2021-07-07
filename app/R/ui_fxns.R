
get_latest_data <- function(buttonid, titleid) {
  box(width = NULL, title = uiOutput(titleid), 
      status = "info", solidHeader = TRUE,
      actionButton(inputId=buttonid, label='Get latest data?'))
}

selected_fiscal_qtr <- function(input_slider) {
  # print(input_slider)
  paste0( str_remove(str_split(input_slider,',')[[1]][[1]], 'FY '), '_', tolower(trimws(str_split(input_slider,',')[[1]][[2]])) )
}

fiscal_slider_text <- function(fiscal_quarters) {
  quarters <- rep(NA, length(fiscal_quarters))
  for (f in seq_along(fiscal_quarters))
  {
    quarters[[f]] <- paste0('FY ',str_split(fiscal_quarters,'_')[[f]][1],', ',str_to_upper(str_split(fiscal_quarters,'_')[[f]][2]))
  }
  return(quarters)
}

fy_ui <- function(id) {
  # mm_prop <- readRDS("./data/q3f20_mm_prop.rds")
  quarters <- sort(unique(fiscal_slider_text(unique(mm_prop$fy_q))), decreasing = TRUE)
	conditionalPanel(condition = "input.compare_prop == 'compare_divs'"
	                 ,selectInput(inputId = id,
	                              label = "Select fiscal quarter:",
	                              choices = quarters,
	                              selected = max(quarters),
	                              multiple = FALSE)
	                 )
}

chooseBy_ui <- function(id) {
	conditionalPanel(condition = "input.tabs != 'rightsBD' & input.tabs != 'pami'"
		,shinyWidgets::awesomeRadio(inputId = id,
			label = "Select...", 
			choices = c("Everything","Centers", "Divisions"),
			selected = "Everything",
			# inline = TRUE, 
			status = "primary")
		)
}

centers_ui <- function(id) {
	conditionalPanel(condition = "input.chooseBy == 'Centers' & input.tabs != 'rightsBD' & input.tabs != 'pami'"
		,shinyWidgets::awesomeCheckboxGroup(inputId = id,
			label = "Research Library Centers", 
			choices = names(div_menu),
			selected = names(div_menu),
			status = "info")
		)
}

divisions_ui <- function(id) {
	conditionalPanel(condition = "input.chooseBy == 'Divisions' & input.tabs != 'rightsBD' & input.tabs != 'pami'"
		,shinyWidgets::pickerInput(inputId = id,
			label = "Research Library Divisions",
			choices = div_menu,
			selected = unname(div_choices),
			options = list(`actions-box` = TRUE,
				`selected-text-format` = "count > 2"
				,`count-selected-text` = ("{0} divisions selected")
				,`dropup-auto`=FALSE),
			multiple = TRUE)
		)
}

calendar_ui <- function(id, label="Select date range:") {
  sliderInput(id,label, 
              min=2017, 
              max=year(today()), 
              value=c(2018,year(today())), 
              sep = "")
}

fiscal_ui <- function(id) {
  max_year <- if_else(month(today()) >= 7, year(today())+1, year(today()))
  if (max_year < 2021) {
    conditionalPanel(condition = "input.f_or_c == 'fiscal'"
                     ,checkboxGroupInput(id,"Select fiscal years:", 
                                  choices = c("FY 2019" = 2019, "FY 2020" = 2020), 
                                  selected = c(2019, 2020),
                                  inline = TRUE) 
    )
  } else {
    conditionalPanel(condition = "input.f_or_c == 'fiscal'"
                     ,sliderInput(id,"Select date range:", 
                                  min=2018, 
                                  max=max_year,
                                  value=c(2019,max_year),
                                  round=TRUE,
                                  pre = 'FY ',
                                  sep = "") 
                     )
  }
}

plot_box <- function(id, spinner=FALSE, boxwidth=NULL, boxheight=NULL, plotwidth="100%", plotheight="400px") {
	if(spinner==TRUE) {
		fluidRow(box(width = boxwidth, height=boxheight, highchartOutput(outputId = id, width = plotwidth, height = plotheight) %>% shinycssloaders::withSpinner(color="#0dc5c1")))
	} else {
		fluidRow(box(width = boxwidth, height=boxheight, highchartOutput(outputId = id, width = plotwidth, height = plotheight)))
	}
}

app_gloss_box <- function() {
	box(width = NULL, title = span("Glossary", style = "font-size: 14px ; font-weight: bold"), status = "info", solidHeader = TRUE,
		tags$p(HTML("<i class='fa fa-book'></i>  Items"), style="white-space: pre-wrap; color: #88419d") %>%
		bsplus::bs_embed_tooltip("A single item record represents one or more digitized assets, such as a fully-digitized book or a photo album.") %>%
		bsplus::bs_set_data(container = "body"),
		tags$p(HTML("<i class='fa fa-image'></i>  Captures"), style="white-space: pre-wrap; color: #0868ac") %>%
		bsplus::bs_embed_tooltip("A capture is a digitized asset, such as a single page from a book, a sound recording, or the recto of a photograph.") %>%
		bsplus::bs_set_data(container = "body")
		)
}

diu_gloss_box <- function() {
	box(width = 9, title = span("Glossary", style = "font-size: 14px ; font-weight: bold"), status = "info", solidHeader = TRUE, 
       tags$p(HTML("<i class='fa fa-plus-circle'></i>  Created"), style="white-space: pre-wrap; color: #9894ae") %>%
         bsplus::bs_embed_tooltip("Metadata records are created in our repository before digitized images are attached.") %>%
         bsplus::bs_set_data(container = "body"),
       tags$p(HTML("<i class='fa fa-camera'></i>  Captured"), style="white-space: pre-wrap; color: #fb8072") %>%
         bsplus::bs_embed_tooltip("An image is captured when it has been photographed and attached to a metadata record in our repository.") %>%
         bsplus::bs_set_data(container = "body"),
       tags$p(HTML("<i class='fa fa-thumbs-up'></i>  Approved"), style="white-space: pre-wrap; color: #668da8") %>%
         bsplus::bs_embed_tooltip("An image metadata record is cleaned up to meet our minimum standards and approved to be made available for use.") %>%
         bsplus::bs_set_data(container = "body")
       )
}

pami_gloss_box <- function() {
  box(width = 9, title = span("Glossary", style = "font-size: 14px ; font-weight: bold"), status = "info", solidHeader = TRUE,
      tags$p(HTML("<i class='fa fa-record-vinyl'></i>  Digitized"), style="white-space: pre-wrap; color: #a61d4d") %>%
        bsplus::bs_embed_tooltip("Physical audio or video objects have been recorded and converted to digital assets.") %>%
        bsplus::bs_set_data(container = "body"),
      tags$p(HTML("<i class='fa fa-flag'></i>  Reported"), style="white-space: pre-wrap; color: #1b9e77") %>%
        bsplus::bs_embed_tooltip("Assets related to an object have passed quality control and are ready for ingest into the repository.") %>%
        bsplus::bs_set_data(container = "body"),
      tags$p(HTML("<i class='fa fa-file-import'></i>  Ingested"), style="white-space: pre-wrap; color: #7570b3") %>%
        bsplus::bs_embed_tooltip("Assets have been ingested into the repository and are available for post-digitization description by staff.") %>%
        bsplus::bs_set_data(container = "body")
  )
}

catalogued_check <- function(id) {
  checkboxInput(id, tags$p(HTML("Catalogued records only?"))%>%
                  bsplus::bs_embed_tooltip("Only view MMS item records associated with public-facing catalog, finding aid, or TMS records.", placement = "bottom") %>%
                  bsplus::bs_set_data(container = "body"), value = FALSE)
}

dl_box <- function(id){
	box(width = NULL, downloadButton(outputId=id, 'Download data (csv)'))
}

dynamic_collection_bar_box <- function(df, id){
  req(df)
  pheight <- ifelse(nrow(df) > 1,"350px","100px")
  plot_box(id, plotwidth="100%", plotheight=pheight)
}
