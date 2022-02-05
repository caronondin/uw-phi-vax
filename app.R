#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load required packages -----
library(shiny)
library(shinythemes)
library(DT)
library(dplyr)
library(formattable)
library(tidyr)
library(leaflet)
library(plotly)
library(readxl)
library(shinycssloaders)
#library(shinyWidgets)

#setwd("~/Desktop/PHI - Research Assitant/Winter/vax/aim1")
vaccine_trends <- readRDS("aim_1/01_vaccine_trends.RDS")
sdi <- readRDS("aim_1/02_sdi.RDS")
raw_extracted_dhs <- readRDS("aim_1/03_raw_extracted_dhs.RDS")
prepped_dhs_for_mov <- readRDS("aim_1/04_prepped_dhs_for_mov.RDS")
disease_trends <- readRDS("aim_1/05_disease_trends.RDS")
merged_data_for_visuals <- readRDS("aim_1/06_merged_data_for_visuals.RDS")
vaccine_preventable_diseases <- read_excel("aim_1/vaccine_preventable_diseases.xlsx")
merged_data_for_visuals <- readRDS("aim_1/06_merged_data_for_visuals.RDS")
merged_data_for_vac_dis <- dplyr::left_join(vaccine_preventable_diseases,disease_trends, "cause_name", "cause_name")


#aim_2
index_results <- readRDS("aim_2/10_index_results.RDS")
sdi_dup <- sdi
colnames(sdi_dup)[2] <- "location"
colnames(sdi_dup)[4] <- "year"
merged_data_for_vacii_sdi <- dplyr::left_join(index_results,sdi_dup[,-c("sdi")], by=c("location","year"))

print(merged_data_for_vacii_sdi)


#library(sf)
#shape <- read_sf(dsn = "./aim_2", layer = "gadm36_0")

############################################### ui.R ##################################################
body <-navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
                  tags$head(includeCSS("aim_2/navbarpage_style.css")),
                  title = "Global Vaccination Improvement Dashboard",
                  sidebarPanel(
                      h3(strong("Improvement Index Ranking Table")),
                      tags$head(tags$style(HTML('.js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
                                                  background: #20c997;
                                                  border-top: 1px solid #18bc9c ;
                                                  border-bottom: 1px solid #18bc9c ;}
                            /* changes the colour of the number tags */
                           .irs-from, .irs-to, .irs-single { background: #20c997}'
                      ))),
                      sliderInput("year", "Year", value =2019, min = 1990, max=2019,step=1,sep = "",animate=TRUE),
                      radioButtons("sdi_group_present","SDI Group Present", choices = c("All"="all","Low" ="low","Medium" = "medium","High" = "high"),inline = TRUE),
                      tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: #92c9e8 !important;}')),
                      DT::dataTableOutput("table")),
                  mainPanel(
                    div(class="outer",absolutePanel(fixed=FALSE, width = "100%", 
                                                    draggable = FALSE, height = "100%",
                    tags$style(type="text/css",
                               ".shiny-output-error { visibility: hidden; }",
                               ".shiny-output-error:before { visibility: hidden; }"
                    ),
                      tabsetPanel(id = "t1",
                                  tabPanel("Vaccination Improvement Index Mapper",value="t_sdi",
                                           #leaflet::leafletOutput("mymap", height = "80vh")),
                                           fluidRow(column(6, " ", style='padding:5px;')),
                                           fluidRow(column(width = 12, "Select location by clicking location_name in left Global SDI Ranking table.",
                                                           style='font-family:Avenir, Helvetica;font-size:30;text-align:center')),
                                           fluidRow(column(6, " ", style='padding:5px;')),
                                           fluidRow(column(12,plotlyOutput("index_map",height = "45vh"))),
                                          # tags$head(tags$style(HTML('.info-box {min-height: 40px;} .info-box-icon {height: 40px; line-height: 40px;} .info-box-content {padding-top: 0px; padding-bottom: 0px;}'))),
                                           #fluidRow(infoBoxOutput("dah_per_the_mean_cat"),tags$style("#dah_per_the_mean_cat {width:630px;"),infoBoxOutput("the_per_cap_mean"),tags$style("#the_per_cap_mean {width:400px")),
                                           #fluidRow(infoBoxOutput("ghes_per_the_mean"),tags$style("#ghes_per_the_mean {width:570px;"),
                                            #        infoBoxOutput("haqi"),tags$style("#haqi {width:180px"),
                                           #         infoBoxOutput("cpi"),tags$style("#cpi {width:370px")),
                                          fluidRow(column(6, " ", style='padding:2px;')),
                                          fluidRow(column(12, plotlyOutput("index_trend_plot",height = "30vh")))),
                                           #div(class="outer",
                                               #tags$head(includeCSS("aim_2/style.css")),
                                               #absolutePanel(id = "controls", class = "panel panel-default",
                                                            # top = 420, left = 700, width = 250, fixed=TRUE,
                                                            # draggable = TRUE, height = "auto"))),
                                  tabPanel("Vaccination Improvement Index Ranking Table",
                                           DT::dataTableOutput("indextable")
                                           ),
                                  tabPanel("Vaccination Trends", value = "t_vac",
                                           fluidRow(column(width = 11,h3(strong(htmlOutput("content_vac"))))),
                                           fluidRow(column(width = 11, "Select location by clicking location_name in left Global SDI Ranking table.",
                                                           style='font-family:Avenir, Helvetica;font-size:30;text-align:left')),
                                           fluidRow(column(11,h2(("   ")))),
                                           radioButtons("vaccine_plot","Plot type:", choices = c("Time Series of Vaccine Coverage" ="line_trend","Single Year Vaccine Coverage"="bar_plot"),inline = TRUE),
                                           fluidRow(column(12,plotlyOutput("all_vaccine_plot",height = "50vh")))),
                                  tabPanel("Mortality and Disability Trends",value = "d_vac",
                                           fluidRow(column(width = 11,h3(strong(htmlOutput("content_dis"))))),
                                           fluidRow(column(width = 11, "Select location by clicking location_name in left Global SDI Ranking table.",
                                                           style='font-family:Avenir, Helvetica;font-size:30;text-align:left')),
                                           #fluidRow(column(6, radioButtons("disease_estimate","Choose y-axis:", choices = c("Number Value"="number_val","Percent Value" ="percent_val","Rate Value" = "rate_val"),inline = TRUE)),
                                                    #column(6,radioButtons("disease_plot","Choose plot type:", choices = c("Time Series of Disease Trend" ="line_trend","Single Year Disease trend"="bar_plot"),inline = TRUE))),
                                           radioButtons("disease_estimate","Choose y-axis:", choices = c("Number Value"="number_val","Percent Value" ="percent_val","Rate Value" = "rate_val"),inline = TRUE),
                                           fluidRow(column(12,plotlyOutput("all_disease_plot", height = "38vh"))),
                                           fluidRow(column(12,plotlyOutput("all_disability_plot", height = "38vh")))),
                                  tabPanel("Vaccination and Disease Trends",value="vac_dis_tab",
                                           fluidRow(column(11,h3(strong(htmlOutput("content_vac_dis"))))),
                                           selectInput("vaccinations", "Vaccination:",choices=NULL),
                                           fluidRow(column(12,plotlyOutput("selected_vac_dis_plot",height = "45vh"))),
                                           fluidRow(
                                             column(3,
                                                    h4("BCG"),
                                                    helpText("Bacillus Calmette–Guérin"),
                                                    DT::dataTableOutput("BCGtable")),
                                             column(3,
                                                    h4("DTP1 & DTP3"),
                                                    helpText("Diphtheria, tetanus, pertussis"),
                                                    DT::dataTableOutput("DTPtable")),
                                             column(2,
                                                    h4("HepB3"),
                                                    helpText("Hepatitis B"),
                                                    DT::dataTableOutput("HepB3table")), 
                                             column(2,
                                                    h4("MCV1 & MCV2"),
                                                    helpText("Measles"),
                                                    DT::dataTableOutput("MCVtable")), 
                                             column(1,
                                                    h4("RotaC"),
                                                    helpText("Rotavirus"),
                                                    DT::dataTableOutput("RotaCtable"))
                                           )),
                                  tabPanel("Data Explorer",
                                           fluidRow(column(6, " ", style='padding:5px;')),
                                           fluidRow(column(6, radioButtons("dataset","Choose Dataset", choices = c("All"="all","SDI" ="sdi","Vaccine Trends" = "vaccine trends","Disease Trends" = "disease trends","Improvement Index"= "improvement index"),inline = TRUE)),
                                                           column(5, style = "margin-top: 10px;",div(downloadButton("download","Download the data"), style = "float: right"))),
                                           fluidRow(column(11,DT::dataTableOutput("alldatatable") %>% withSpinner(color="#0dc5c1"))))
                      )
                  )))
)

server <- function(input, output,session) {
    #year <- reactive({
     #   req(input$year)
      #  merged_data_for_visuals[merged_data_for_visuals$year == input$year,]
    #})
  
  year <- reactive({
    merged_data_for_vacii_sdi[merged_data_for_vacii_sdi$year == input$year,]
  })
  
  observeEvent(year(),{
    choices = sort(unique(merged_data_for_vacii_sdi$year))
    updateSliderInput(session,'year', value=unique(year()$year),
                      min = min(as.numeric(choices)), max = max(as.numeric(choices)), step = 1)
  })
  
  #observeEvent(index_results,{
    #choices = sort(unique(index_results$year))
    #updateSliderInput(session,'index_year', value=range(choices),
         #             min = min(as.numeric(choices)), max = max(as.numeric(choices)), step = 1)
  #})
  
  
  index_year_input <- reactive({
    req(input$index_year_input)
    index_results[index_results$year == input$index_year_input,]
  })
  

  observeEvent(merged_data_for_vac_dis,{
    choices = sort(unique(merged_data_for_vac_dis$vaccine_name))
    updateSelectInput(session,'vaccinations', choices = choices)
  })
  
  dis_data_for_selected_vac <- reactive({
    filter(merged_data_for_vac_dis, merged_data_for_vac_dis$vaccine_name==input$vaccinations)
  })
  
  selected_dis_vac_data <- reactive({
    return(
      list(
        selected_vac_data = filter(vaccine_trends, vaccine_trends$vaccine_name==input$vaccinations),
        dis_data_for_selected_vac = filter(merged_data_for_vac_dis, merged_data_for_vac_dis$vaccine_name==input$vaccinations)
      ))
  })
  
  sdi_group_present <- reactive({
    print("sdi_group_present")
      req(input$sdi_group_present)
      if (input$sdi_group_present == "all"){
          all_sdi_group <- year() 
      }
      else{
          filter(year(), sdi_group_present == input$sdi_group_present)
      }
  })
    
  output$table = DT::renderDataTable({
      sdi_rank_table<-sdi_group_present()[,c("location","result","sdi","sdi_group_present")]
      print("sorting")
      print(sdi_rank_table)
      sdi_rank_table$rank <- NA
      sdi_rank_table$rank = dense_rank(desc(sdi_rank_table$result))
      sdi_rank_table <- sdi_rank_table[,c("rank","location","result","sdi","sdi_group_present")]
      sdi_rank_table<-sdi_rank_table[order(sdi_rank_table$rank),]
      true_false_formatter <-
          formatter("span",
                    style = x ~ formattable::style(
                        font.weight = "bold",
                        color = ifelse(x == "high", "forestgreen", ifelse(x == "low", "red", "black"))
                    ))
      
      formattable(
          sdi_rank_table,
          list(
              ## a coloured bar with length proportional to value
              'result' = color_tile("white", "pink"),
              ## use custom formatter for TRUE/FALSE values
              sdi_group_present = true_false_formatter
          )
      ) %>%
          as.datatable(rownames = FALSE, 
                       selection = list(mode = 'single',target="cell", selected = matrix(c(0, 1), nrow = 1,ncol = 2)), 
                       options = list(paging = FALSE,
                                      scrollY = '500px', 
                                      scrollY=TRUE, 
                                      autoWidth = FALSE,
                                      ordering = FALSE,
                                      #dom = 'Bfrtip',
                                      pageLength=1000)
          )
  })
  
  
  observeEvent(sdi_group_present(),{
    print("map data")
    map_data <- sdi_group_present()
    map_data$hover <- with(map_data, paste(location, '<br>','<br>',
                                           "Development Assistance Per Total Health Spending Categorical: ",round(dah_per_the_mean_cat,3),'<br>',
                                           "Total Health Spending per Personn: ",round(the_per_cap_mean,3),'<br>',
                                           "Government Health Spending per Total Health Spending: ",round(ghes_per_the_mean,3),'<br>',
                                           "HAQI: ",round(haqi,3),'<br>',
                                           "Corruption Perception Index:", round(cpi,3),'<br>',
                                           "Skilled Attendants at Birth: ",round(perc_skil_attend,3),'<br>',
                                           "Immigrant Population (%): ",round(imm_pop_perc,3),'<br>',
                                           "Urbanicity (%)",round(perc_urban,3),'<br>',
                                           "Agreement Vaccines are Safe",round(mean_agree_vac_safe,3),'<br>',
                                           "Agreement Vaccines are Important",round(mean_agree_vac_important,3),'<br>',
                                           "Agreement Vaccines are Effective: ",round(mean_agree_vac_effective,3)
                                           ))
    output$index_map <- renderPlotly({
      height  = 2000 
      units="px"
      
      # light grey boundaries
      l <- list(color = toRGB("white"), width = 0.5)
      
      # specify map projection/options
      g <- list(
        showframe = FALSE,
        showcoastlines = FALSE,showland = TRUE,showcountries = TRUE,
        resolution = 150,
        countrycolor = toRGB("white"),
        landcolor = toRGB("grey85"),
        projection = list(scale=1.2))
      

      fig <- plot_ly(map_data)
      fig <- fig %>% 
        add_trace(
          z = ~result, color = ~result, type = 'choropleth',
          text = ~hover, locations = ~iso_code, colors="Blues", 
          marker = list(line = l))%>%
        colorbar(title = 'Improvement Index')%>% 
        layout(
          autosize = T,
          title = paste0(input$year," Global Vaccine Improvement Index Mapper"),
          mapbox=list(
            style="open-street-map",
            center = list(lon = -90, lat = 34)),
          geo = g)
      })
    
    output$index_trend_plot <- renderPlotly({
      index_trend_data <- filter(index_results,location == "United States of America")
      vii_left <- list(
        xref = 'paper',
        yref = 'y',
        x = 0.01,
        y = index_trend_data$result[1]+0.01,
        xanchor = 'middle',
        yanchor = 'center',
        text = ~round(index_trend_data$result[1],3),
        font = list(family = 'Arial',
                    size = 16,
                    color = 'rgba(67,67,67,1)'),
        showarrow = FALSE)
      
      vii_right <- list(
        xref = 'paper',
        yref = 'y',
        x = 0.97,
        y = index_trend_data$result[26]+0.02,
        xanchor = 'middle',
        yanchor = 'center',
        text = ~round(index_trend_data$result[26],3),
        font = list(family = 'Arial',
                    size = 16,
                    color = 'rgba(67,67,67,1)'),
        showarrow = FALSE)
      
      fig_a <- plot_ly(index_trend_data, x = ~year)
      fig_a <- fig_a  %>% add_trace(y=~result,type='scatter', name = "vaccine improvement index", mode = 'lines', line = list(color = 'rgba(49,130,189, 1)',width=2))
      fig_a <- fig_a %>% add_trace(x = ~c(year[1], year[26]), y = ~c(result[1], result[26]), type = 'scatter', mode = 'markers', marker = list(color = 'rgba(49,130,189, 1)', size = 10))
      fig_a <- fig_a %>% 
        layout( autosize = T,
                title = paste0("Time Series of Vaccine Improvement Index in United States of America"), 
                showlegend = FALSE,
                xaxis = list(title = "Year",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                yaxis = list(title = "Vaccine Improvement Index",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE))
      fig_a <- fig_a %>% layout(annotations = vii_left) 
      fig_a <- fig_a %>% layout(annotations = vii_right) 
      fig_a
    })
    
    output$indextable = DT::renderDataTable({
      index_rank_table<-sdi_group_present()[,-c("year","gbd_location_id","iso_code","iso_num_code")]
      index_rank_table$rank <- NA
      index_rank_table$rank = dense_rank(desc(index_rank_table$result))
      #index_rank_table <- index_rank_table[,c("rank","location_name","sdi","sdi_group_present")]
      index_rank_table<-index_rank_table[order(index_rank_table$rank),]
      
      print(index_rank_table)
      
      colnames(index_rank_table) = c("Location", "SDI","Development Assistance Per Total Health Spending Categorical","Total Health Spending per Person",
                                     "Government Health Spending per Total Health Spending","HAQI","Corruption Perception Index","Skilled Attendants at Birth","Immigrant Population (%)",
                                     "Urbanicity (%)","Agreement Vaccines are Safe","Agreement Vaccines are Important","Agreement Vaccines are Effective","Improvement Index","location_id","level",
                                     "SDI Group Present","Rank")
      print(index_rank_table)
      index_rank_table <- index_rank_table[,c(18,1,14,2,3,4,5,6,7,8,9,10,11,12,13)]
      
      customGreen0 = "#DeF7E9"
      customGreen = "#71CA97"
      customPurple = "#8787e0"
      customPurple0 = #dcdcf2
        
        print(index_rank_table)
      formattable(
        index_rank_table,
        list(
          ## a coloured bar with length proportional to value
          'Improvement Index' = color_tile("white", customPurple),
          "SDI" = color_tile("white", "pink"),
          "Development Assistance Per Total Health Spending Categorical" = color_tile(customGreen, customGreen0),
          "Total Health Spending per Person"= color_tile(customGreen, customGreen0),
          "Government Health Spending per Total Health Spending"= color_tile(customGreen, customGreen0),
          "HAQI"= color_tile(customGreen, customGreen0),
          "Corruption Perception Index"= color_tile(customGreen, customGreen0),
          "Skilled Attendants at Birth"= color_tile(customGreen, customGreen0),
          "Immigrant Population (%)"= color_tile(customGreen, customGreen0),
          "Urbanicity (%)"= color_tile(customGreen, customGreen0),
          "Agreement Vaccines are Safe"= color_tile(customGreen, customGreen0),
          "Agreement Vaccines are Important"= color_tile(customGreen, customGreen0),
          "Agreement Vaccines are Effective"= color_tile(customGreen, customGreen0)
        )
      ) %>%
        as.datatable(rownames = FALSE, 
                     options = list(paging = FALSE,
                                    scrollY = '600px', 
                                    scrollY=TRUE, 
                                    scrollX=TRUE, 
                                    #searching = FALSE,
                                    autoWidth = FALSE,
                                    ordering = FALSE,
                                    pageLength=1000)
        )
    })
  })
  
  
  output$indextable = DT::renderDataTable({
    index_rank_table<-index_year_input()[,-c("year","gbd_location_id","iso_code","iso_num_code")]
    index_rank_table$rank <- NA
    index_rank_table$rank = dense_rank(desc(index_rank_table$result))
    #index_rank_table <- index_rank_table[,c("rank","location_name","sdi","sdi_group_present")]
    index_rank_table<-index_rank_table[order(index_rank_table$rank),]
    
    colnames(index_rank_table) = c("Location", "SDI","Development Assistance Per Total Health Spending Categorical","Total Health Spending per Person",
                                   "Government Health Spending per Total Health Spending","HAQI","Corruption Perception Index","Skilled Attendants at Birth","Immigrant Population (%)",
                                   "Urbanicity (%)","Agreement Vaccines are Safe","Agreement Vaccines are Important","Agreement Vaccines are Effective","Improvement Index","Rank")
    print(index_rank_table)
    index_rank_table <- index_rank_table[,c(15,1,14,2,3,4,5,6,7,8,9,10,11,12,13)]
    
    customGreen0 = "#DeF7E9"
    customGreen = "#71CA97"
    customPurple = "#8787e0"
    customPurple0 = #dcdcf2
      
      print(index_rank_table)
    formattable(
      index_rank_table,
      list(
        ## a coloured bar with length proportional to value
        'Improvement Index' = color_tile("white", customPurple),
        "SDI" = color_tile("white", "pink"),
        "Development Assistance Per Total Health Spending Categorical" = color_tile(customGreen, customGreen0),
        "Total Health Spending per Person"= color_tile(customGreen, customGreen0),
        "Government Health Spending per Total Health Spending"= color_tile(customGreen, customGreen0),
        "HAQI"= color_tile(customGreen, customGreen0),
        "Corruption Perception Index"= color_tile(customGreen, customGreen0),
        "Skilled Attendants at Birth"= color_tile(customGreen, customGreen0),
        "Immigrant Population (%)"= color_tile(customGreen, customGreen0),
        "Urbanicity (%)"= color_tile(customGreen, customGreen0),
        "Agreement Vaccines are Safe"= color_tile(customGreen, customGreen0),
        "Agreement Vaccines are Important"= color_tile(customGreen, customGreen0),
        "Agreement Vaccines are Effective"= color_tile(customGreen, customGreen0)
      )
    ) %>%
      as.datatable(rownames = FALSE, 
                   options = list(paging = FALSE,
                                  scrollY = '500px', 
                                  scrollY=TRUE, 
                                  scrollX=TRUE, 
                                  #searching = FALSE,
                                  autoWidth = FALSE,
                                  ordering = FALSE,
                                  pageLength=1000)
      )
  })

  
  
  #output$dah_per_the_mean_cat <- renderInfoBox({
    #infoBox(
    #  "Development Assistance Per Total Health Spending Categorical", " ",
    #  color = "purple",
    #  fill = TRUE,
    #)
 # })
  
 # output$the_per_cap_mean <- renderInfoBox({
   # infoBox(
    #  "Total Health Spending per Person", " ",
    #  color = "yellow",
    #  fill = TRUE,
    #)
  #})
  
  #output$ghes_per_the_mean <- renderInfoBox({
  #  infoBox(
  #    "Government Health Spending per Total Health Spending", " ",
   #   color = "aqua",
   #   fill = TRUE,
  #  )
 # })
  
  #output$haqi <- renderInfoBox({
   # infoBox(
   #   "HAQI", " ",
   #   color = "blue",
   #   fill = TRUE,
   # )
  #})
  
  #output$cpi <- renderInfoBox({
  #  infoBox(
  #    "Corruption Perception Index", " ",
  #    color = "orange",
  #    fill = TRUE,
  #  )
  #})
  
  output$content_vac <- renderText("United States of America")
  output$content_dis <- renderText("United States of America")
  output$content_vac_dis <- renderText("United States of America")
  
  output$all_vaccine_plot <- renderPlotly({
      vac_plotdata <- filter(vaccine_trends,gsub(" ", "", location_name) == gsub(" ", "", "United States of America"))
      if (input$vaccine_plot == "line_trend"){
          fig_a <- plot_ly(vac_plotdata, x = ~year_id,y=~prop_val, color = ~vaccine_name)%>%
              add_lines()
          fig_a <- fig_a %>% 
              layout(autosize = T,
                     title ="Time Series of Vaccination Coverage",  showlegend = T,
                     xaxis = list(title = "Year",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                     yaxis = list(title = "Vaccination coverage (%)",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE))
          fig_a
      }
      else{
          single_year_vac_plotdata <- filter(vac_plotdata,year_id == input$year)
          fig1 <- plot_ly(x = ~single_year_vac_plotdata$prop_val, y = ~reorder(single_year_vac_plotdata$vaccine_name, single_year_vac_plotdata$prop_val), name = single_year_vac_plotdata$vaccine_name,
                          type = 'bar', orientation = 'h',
                          color = single_year_vac_plotdata$vaccine_name)
                          #marker = list(color = 'rgba(50, 171, 96, 0.6)',
                                        #line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1))) 
          fig1 <- fig1 %>% layout( autosize = T,
                                   title = paste0("Vaccination Coverage in ", input$year),
                                  yaxis = list(title = "Vaccine",showgrid = FALSE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85)),
                                  xaxis = list(title = "Vaccination coverage (%)", zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE)) 
          fig1 <- fig1 %>% add_annotations(xref = 'x1', yref = 'y',
                                           x = single_year_vac_plotdata$prop_val * 1 + 0.1,  y = single_year_vac_plotdata$vaccine_name,
                                           text = paste(round(single_year_vac_plotdata$prop_val*100, 2), '%'),
                                           font = list(family = 'Arial', size = 12, color = 'rgb(50, 171, 96)'),
                                           showarrow = FALSE)
      }
  })
  
  output$all_disease_plot <- renderPlotly({
      print("input")
      print(input$disease_estimate)
      disease_plotdata <- filter(disease_trends,gsub(" ", "", location_name) == gsub(" ", "", "United States of America"))
      if (input$disease_estimate == "number_val"){
          fig_dis <- plot_ly(disease_plotdata, x = ~year_id,y= ~deaths_number_val, color = ~cause_name)%>%
              add_lines()
          title = "Time Series of Deaths, Disease or Disability Number"
          y_title = "Number of Deaths in Population"
      }
      else if (input$disease_estimate == "percent_val"){
          fig_dis <- plot_ly(disease_plotdata, x = ~year_id,y= ~deaths_percent_val, color = ~cause_name)%>%
              add_lines()
          title = "Time Series of Deaths, Disease or Disability Percent"
          y_title="Deaths for a particular cuase/Deaths from all causes"
      }
      else{
          fig_dis <- plot_ly(disease_plotdata, x = ~year_id,y= ~deaths_rate_val, color = ~cause_name)%>%
              add_lines()
          title = "Time Series of Deaths, Disease or Disability Rate"
          y_title="Deaths per 100,000 population"
      }
      fig_dis <- fig_dis %>% 
          layout( autosize = T,
                  title =title,  showlegend = T,
                 xaxis = list(title = "Year",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                 yaxis = list(title =y_title,showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE))
      fig_dis
  })
  
  output$all_disability_plot <- renderPlotly({
      disability_plotdata <- filter(disease_trends,gsub(" ", "", location_name) == gsub(" ", "", "United States of America"))
      if (input$disease_estimate == "number_val"){
          fig_dis <- plot_ly(disability_plotdata, x = ~year_id,y= ~ylds_number_val, color = ~cause_name)%>%
              add_lines()
          title = "Time Series of Years Lived in Less Than Ideal health"
          y_title = "Years lived with disability in population "
      }
      else if (input$disease_estimate == "percent_val"){
          fig_dis <- plot_ly(disability_plotdata, x = ~year_id,y= ~ylds_percent_val, color = ~cause_name)%>%
              add_lines()
          title = "Time Series of Proportion of Years Lived in Less Than Ideal health"
          y_title="YLDs for particular cause/YLDs for all causes"
      }
      else{
          fig_dis <- plot_ly(disability_plotdata, x = ~year_id,y= ~ylds_rate_val, color = ~cause_name)%>%
              add_lines()
          title = "Time Series of Years Lived in Less Than Ideal health per 100,000 population"
          y_title="YLDs per 100,000 population"
      }
      fig_disa <- plot_ly(disability_plotdata, x = ~year_id,y=~ylds_number_val, color = ~cause_name)%>%
          add_lines()
      fig_disa <- fig_disa %>% 
          layout( autosize = T,
                  title =title,  showlegend = T,
                 xaxis = list(title = "Year",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                 yaxis = list(title = y_title,showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE))
      fig_disa
  })
  
  
  observeEvent(selected_dis_vac_data(),{
    output$selected_vac_dis_plot <- renderPlotly({
      selected_vac_plotdata <- filter(selected_dis_vac_data()$selected_vac_data,gsub(" ", "", location_name) == gsub(" ", "", "United States of America"))
      selected_dis_plotdata <- filter(selected_dis_vac_data()$dis_data_for_selected_vac,gsub(" ", "", location_name) == gsub(" ", "", "United States of America"))
      merged_selected_plotdata <- dplyr::left_join(selected_vac_plotdata,selected_dis_plotdata, "year_id", "year_id")
      print(selected_dis_plotdata)
      fig <- plot_ly()
      # Add traces
      fig <- plot_ly(merged_selected_plotdata)
      fig <- fig %>% add_trace(x= ~year_id, y = ~deaths_rate_val, type = 'scatter', mode = 'lines+makers', color = ~cause_name) 
      ay <- list(
        overlaying = "y",
        side = "right",
        title = "<b> Vaccine</b> coverage (%)")
      
      fig <- fig %>% add_trace(x =  ~year_id, y = ~prop_val, type = 'scatter',name = ~vaccine_name.x,yaxis = "y2", mode = 'lines',line = list(color = 'rgba(49,130,189, 1)', width = 4)) 
      # Set figure title, x and y-axes titles
      fig <- fig %>% layout(
        autosize = T,
        title = list(text="Vaccine & Corresponding Disease Trend", x=0.25),
        xaxis = list(title="Year"),
        yaxis = list(title= "<b> Deaths</b> per 100,000 population"),
        yaxis2 = ay,
        legend = list(x = 3000, y = 1.2)
      )%>%
        layout(xaxis = list(
                 zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
               yaxis = list(
                 zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff')
        )
      
      fig
    })
  })
  
  observeEvent(input$table_cell_clicked,{
      info = input$table_cell_clicked
      if (is.null(info$value) || info$col != 1) return()
      updateTabsetPanel(session, 't1', selected = 'Vaccination Trends')
      if(input$t1 == "t_sdi"){
          updateTabsetPanel(session, 't1', selected = 'Vaccination Trends') 
      }
      else if(input$t1 == "t_vac"){
          updateTabsetPanel(session, 't1', selected = 'Disease Trends') 
      }
      
      print("info value")
      print(info$value)
      output$content_vac <- renderText(info$value)
      output$content_dis <- renderText(info$value)
      output$content_vac_dis <- renderText(info$value)
      
      output$index_trend_plot <- renderPlotly({
        index_trend_data <- filter(index_results,gsub(" ", "",location) == gsub(" ", "", info$value))
        
        vii_left <- list(
          xref = 'paper',
          yref = 'y',
          x = 0.01,
          y = index_trend_data$result[1]+0.01,
          xanchor = 'middle',
          yanchor = 'center',
          text = ~round(index_trend_data$result[1],3),
          font = list(family = 'Arial',
                      size = 16,
                      color = 'rgba(67,67,67,1)'),
          showarrow = FALSE)
        
        vii_right <- list(
          xref = 'paper',
          yref = 'y',
          x = 0.97,
          y = index_trend_data$result[26]+0.02,
          xanchor = 'middle',
          yanchor = 'center',
          text = ~round(index_trend_data$result[26],3),
          font = list(family = 'Arial',
                      size = 16,
                      color = 'rgba(67,67,67,1)'),
          showarrow = FALSE)
        
        fig_a <- plot_ly(index_trend_data, x = ~year)
        fig_a <- fig_a  %>% add_trace(y=~result,type='scatter', name = "vaccine improvement index", mode = 'lines', line = list(color = 'rgba(49,130,189, 1)',width=2))
        fig_a <- fig_a %>% add_trace(x = ~c(year[1], year[26]), y = ~c(result[1], result[26]), type = 'scatter', mode = 'markers', marker = list(color = 'rgba(49,130,189, 1)', size = 10))
        fig_a <- fig_a %>% 
          layout( autosize = T,
                  title = paste0("Time Series of Vaccine Improvement Index in ",info$value), 
                 showlegend = FALSE,
                 xaxis = list(title = "Year",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                 yaxis = list(title = "Vaccine Improvement Index",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE))
        fig_a <- fig_a %>% layout(annotations = vii_left) 
        fig_a <- fig_a %>% layout(annotations = vii_right) 
        fig_a
      })
      
      output$all_vaccine_plot <- renderPlotly({
          vac_plotdata <- filter(vaccine_trends,gsub(" ", "", location_name) == gsub(" ", "", info$value))
          if (input$vaccine_plot == "line_trend"){
              fig_a <- plot_ly(vac_plotdata, x = ~year_id,y=~prop_val, color = ~vaccine_name)%>%
                  add_lines()
              fig_a <- fig_a %>% 
                  layout( autosize = T,
                          title ="Time Series of Vaccination Coverage",  showlegend = T,
                         xaxis = list(title = "Year",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                         yaxis = list(title = "Vaccination coverage (%)",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE))
              fig_a
          }
          else{
              single_year_vac_plotdata <- filter(vac_plotdata,year_id == input$year)
              fig1 <- plot_ly(x = ~single_year_vac_plotdata$prop_val, y = ~reorder(single_year_vac_plotdata$vaccine_name, single_year_vac_plotdata$prop_val), name = paste0('Vaccniation Coverage in',input$year),
                              type = 'bar', orientation = 'h',
                              marker = list(color = 'rgba(50, 171, 96, 0.6)',
                                            line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1))) 
              fig1 <- fig1 %>% layout( autosize = T,
                                       title = paste0("Vaccination Coverage in ", input$year),
                                      yaxis = list(title = "Vaccine",showgrid = FALSE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85)),
                                      xaxis = list(title = "Vaccination coverage (%)", zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE)) 
              fig1 <- fig1 %>% add_annotations(xref = 'x1', yref = 'y',
                                               x = single_year_vac_plotdata$prop_val * 1 + 0.1,  y = single_year_vac_plotdata$vaccine_name,
                                               text = paste(round(single_year_vac_plotdata$prop_val*100, 2), '%'),
                                               font = list(family = 'Arial', size = 12, color = 'rgb(50, 171, 96)'),
                                               showarrow = FALSE)
          }
      })
      output$all_disease_plot <- renderPlotly({
          disease_plotdata <- filter(disease_trends,gsub(" ", "", location_name) == gsub(" ", "", info$value))
          if (input$disease_estimate == "number_val"){
              fig_dis <- plot_ly(disease_plotdata, x = ~year_id,y= ~deaths_number_val, color = ~cause_name)%>%
                  add_lines()
              title = "Time Series of Deaths, Disease or Disability Number"
              y_title = "Number of Deaths in Population"
          }
          else if (input$disease_estimate == "percent_val"){
              fig_dis <- plot_ly(disease_plotdata, x = ~year_id,y= ~deaths_percent_val, color = ~cause_name)%>%
                  add_lines()
              title = "Time Series of Deaths, Disease or Disability Percent"
              y_title="Deaths for a particular cuase/Deaths from all causes"
          }
          else{
              fig_dis <- plot_ly(disease_plotdata, x = ~year_id,y= ~deaths_rate_val, color = ~cause_name)%>%
                  add_lines()
              title = "Time Series of Deaths, Disease or Disability Rate"
              y_title="Deaths per 100,000 population"
          }
          fig_dis <- fig_dis %>% 
              layout( autosize = T,
                      title =title,  showlegend = T,
                     xaxis = list(title = "Year",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                     yaxis = list(title = y_title,showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE))
          fig_dis
      })
      
      output$all_disability_plot <- renderPlotly({
          disability_plotdata <- filter(disease_trends,gsub(" ", "", location_name) == gsub(" ", "", info$value))
          if (input$disease_estimate == "number_val"){
              fig_dis <- plot_ly(disability_plotdata, x = ~year_id,y= ~ylds_number_val, color = ~cause_name)%>%
                  add_lines()
              title = "Time Series of Years Lived in Less Than Ideal health"
              y_title = "Years lived with disability in population "
          }
          else if (input$disease_estimate == "percent_val"){
              fig_dis <- plot_ly(disability_plotdata, x = ~year_id,y= ~ylds_percent_val, color = ~cause_name)%>%
                  add_lines()
              title = "Time Series of Proportion of Years Lived in Less Than Ideal health"
              y_title="YLDs for particular cause/YLDs for all causes"
          }
          else{
              fig_dis <- plot_ly(disability_plotdata, x = ~year_id,y= ~ylds_rate_val, color = ~cause_name)%>%
                  add_lines()
              title = "Time Series of Years Lived in Less Than Ideal health per 100,000 population"
              y_title="YLDs per 100,000 population"
          }
          fig_disa <- plot_ly(disability_plotdata, x = ~year_id,y=~ylds_number_val, color = ~cause_name)%>%
              add_lines()
          fig_disa <- fig_disa %>% 
              layout( autosize = T,
                      title =title,  showlegend = T,
                     xaxis = list(title = "Year",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                     yaxis = list(title = y_title,showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE))
          fig_disa
      })
      
      observeEvent(selected_dis_vac_data(),{
        output$selected_vac_dis_plot <- renderPlotly({
          selected_vac_plotdata <- filter(selected_dis_vac_data()$selected_vac_data,gsub(" ", "", location_name) == gsub(" ", "", info$value))
          selected_dis_plotdata <- filter(selected_dis_vac_data()$dis_data_for_selected_vac,gsub(" ", "", location_name) == gsub(" ", "", info$value))
          merged_selected_plotdata <- dplyr::left_join(selected_vac_plotdata,selected_dis_plotdata, "year_id", "year_id")
          print(selected_dis_plotdata)
          fig <- plot_ly()
          # Add traces
          fig <- plot_ly(merged_selected_plotdata)
          fig <- fig %>% add_trace(x= ~year_id, y = ~deaths_rate_val, type = 'scatter', mode = 'lines+makers', color = ~cause_name) 
          ay <- list(
            overlaying = "y",
            side = "right",
            title = "<b> Deaths</b> per 100,000 population")
          
          fig <- fig %>% add_trace(x =  ~year_id, y = ~prop_val, type = 'scatter',name = ~vaccine_name.x,yaxis = "y2", mode = 'lines',line = list(color = 'rgba(49,130,189, 1)', width = 4)) 
          # Set figure title, x and y-axes titles
          fig <- fig %>% layout(
            autosize = T,
            title = list(text="Vaccine & Corresponding Disease Trend", x=0.25), yaxis2 = ay,
            xaxis = list(title="Year"),
            yaxis = list(title="<b> Vaccine</b> coverage (%)"),
            legend = list(x = 3000, y = 1.2)
          )%>%
            layout(xaxis = list(
              zerolinecolor = '#ffff',
              zerolinewidth = 2,
              gridcolor = 'ffff'),
              yaxis = list(
                zerolinecolor = '#ffff',
                zerolinewidth = 2,
                gridcolor = 'ffff')
            )
          
          fig
        })
      })
  })
  
  #output$vac_name <- renderText({ 
   # print("here")
    #input$vaccinations
    #print(input$vaccinations)
  #})
  
  #output$vac_description <- renderText({ 
  #  unique(filter(vaccine_preventable_diseases,vaccine_name == input$vaccinations)$vaccine_description)
  #})
  
  output$vac_dis <- renderText({ 
    unique(filter(vaccine_preventable_diseases,vaccine_name == input$vaccinations)$cause_name)
  })
  
  output$BCGtable = DT::renderDataTable({
    bcg_table <- vaccine_preventable_diseases[vaccine_preventable_diseases$vaccine_name == "BCG",][,c("cause_name")]
    formattable(
      bcg_table
    ) %>%
      as.datatable(rownames = FALSE,  colnames = NULL,
                   options = list(paging = FALSE,
                                  dom = 't',
                                  ordering = FALSE)
      )
  })
  
  
  output$DTPtable = DT::renderDataTable({
    table <- vaccine_preventable_diseases[vaccine_preventable_diseases$vaccine_name == "DTP1",][,c("cause_name")]
    formattable(
      table
    ) %>%
      as.datatable(rownames = FALSE, colnames = NULL,
                   options = list(paging = FALSE,
                                  dom = 't',
                                  ordering = FALSE))
  })
  output$HepB3table = DT::renderDataTable({
    table <- vaccine_preventable_diseases[vaccine_preventable_diseases$vaccine_name == "HepB3",][,c("cause_name")]
    formattable(
      table
    ) %>%
      as.datatable(rownames = FALSE,  colnames = NULL,
                   options = list(paging = FALSE,
                                  dom = 't',
                                  ordering = FALSE))
  })
  output$MCVtable = DT::renderDataTable({
    table <- unique(vaccine_preventable_diseases[vaccine_preventable_diseases$vaccine_name == "MCV1" | vaccine_preventable_diseases$vaccine_name == "MCV2",][,c("cause_name")])
    formattable(
      table
    ) %>%
      as.datatable(rownames = FALSE,  colnames = NULL,
                   options = list(paging = FALSE,
                                  dom = 't',
                                  ordering = FALSE))
  })
  output$RotaCtable = DT::renderDataTable({
    table <- vaccine_preventable_diseases[vaccine_preventable_diseases$vaccine_name == "RotaC",][,c("cause_name")]
    formattable(
      table
    ) %>%
      as.datatable(rownames = FALSE,  colnames = NULL,
                   options = list(paging = FALSE,
                                  dom = 't',
                                  ordering = FALSE))
  })
  
  dataexplorer <- reactive({
      req(input$dataset)
      if (input$dataset == "all"){
          dataexplorer <- merged_data_for_visuals
      }
      else if(input$dataset == "sdi"){
          dataexplorer <- sdi
      }
      else if(input$dataset == "vaccine trends"){
          dataexplorer <- vaccine_trends
      }
      else if (input$dataset == "disease trends"){
        dataexplorer<-disease_trends
      }
      else{
          dataexplorer <- index_results
      }
  })
  
  output$alldatatable = DT::renderDataTable({
          data<-dataexplorer()
          pl=10
          if (input$dataset == "all"){
              x<-data %>%
                  dplyr::select(-c("location_id","level"))
          }
          else if(input$dataset == "sdi"){
              x<- data %>%
                  dplyr::select(-c("location_id","level"))
              pl=14
          }
          else if(input$dataset == "vaccine trends"){
              x<-data %>%
                  dplyr::select(-c("location_id"))
              pl=14
          }
          else if (input$dataset == "disease trends"){
            x<-data %>%
              dplyr::select(-c("location_id","cause_id"))
            pl=9
          }
          else{
              x<-data 
              pl=14
          }
          formattable(
             x
          ) %>%
              as.datatable(rownames = FALSE,
                           filter = 'top',
                           options = list(paging = TRUE,
                                          searching = FALSE,
                                          scrollX=TRUE, 
                                          ordering = TRUE,
                                          dom = '<lf<t>p>',
                                          pageLength=pl,
                                          lengthChange = FALSE))
      })
  output$download <- downloadHandler(
      filename =  paste0(input$dataset,".csv",sep=""),
      content = function(fname){
          write.csv(dataexplorer(), fname)
      }
  )
}

shinyApp(body, server)




