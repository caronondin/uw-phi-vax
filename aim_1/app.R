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
library(rintrojs)
library(shinycssloaders)


#setwd("~/Desktop/PHI - Research Assitant/Winter/vax/aim1")
vaccine_trends <- readRDS("01_vaccine_trends.RDS")
sdi <- readRDS("02_sdi.RDS")
raw_extracted_dhs <- readRDS("03_raw_extracted_dhs.RDS")
prepped_dhs_for_mov <- readRDS("04_prepped_dhs_for_mov.RDS")
disease_trends <- readRDS("05_disease_trends.RDS")
merged_data_for_visuals <- readRDS("06_merged_data_for_visuals.RDS")
vaccine_preventable_diseases <- read_excel("vaccine_preventable_diseases.xlsx")

merged_data_for_vac_dis <- dplyr::left_join(vaccine_preventable_diseases,disease_trends, "cause_name", "cause_name")

merged_data_for_visuals <- readRDS("06_merged_data_for_visuals.RDS")

############################################### ui.R ##################################################
body <-navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
                  title = "Global Vaccination Improvement Dashboard",
                  sidebarPanel(
                      h3(strong("Global SDI Ranking Table")),
                      #selectInput("year", "Year:",choices=sort(unique(merged_data_for_visuals$year))),
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
                      tabsetPanel(id = "t1",
                                  tabPanel("Vaccination Improvement Index Mapper",value="t_sdi",
                                           h3(strong("A Global Vaccination Improvement Index Mapper will be updated here later"),style='font-family:Avenir, Helvetica;font-size:40;text-align:center'),
                                           leaflet::leafletOutput("mymap", height = "80vh")),
                                  tabPanel("Vaccination Trends", value = "t_vac",
                                           h3(strong(htmlOutput("content_vac"))),
                                           fluidRow(column(width = 12, "Select location by clicking location_name in left Global SDI Ranking table or click location on map.",
                                                           style='font-family:Avenir, Helvetica;font-size:30;text-align:left')),
                                           radioButtons("vaccine_plot",NULL, choices = c("Time Series of Vaccine Coverage" ="line_trend","Single Year Vaccine Coverage"="bar_plot"),inline = TRUE),
                                           plotlyOutput("all_vaccine_plot"),
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
                                               column(2,
                                                      h4("RotaC"),
                                                      helpText("Rotavirus"),
                                                      DT::dataTableOutput("RotaCtable")), 
                                           )),
                                  tabPanel("Mortality and Disability Trends",value = "d_vac",
                                           h3(strong(htmlOutput("content_dis"))),
                                           fluidRow(column(width = 12, "Select location by clicking location_name in left Global SDI Ranking table or click location on map.",
                                                           style='font-family:Avenir, Helvetica;font-size:30;text-align:left')),
                                           #fluidRow(column(6, radioButtons("disease_estimate","Choose y-axis:", choices = c("Number Value"="number_val","Percent Value" ="percent_val","Rate Value" = "rate_val"),inline = TRUE)),
                                                    #column(6,radioButtons("disease_plot","Choose plot type:", choices = c("Time Series of Disease Trend" ="line_trend","Single Year Disease trend"="bar_plot"),inline = TRUE))),
                                           radioButtons("disease_estimate","Choose y-axis:", choices = c("Number Value"="number_val","Percent Value" ="percent_val","Rate Value" = "rate_val"),inline = TRUE),
                                           plotlyOutput("all_disease_plot"),
                                           plotlyOutput("all_disability_plot")),
                                  tabPanel("Vaccination and Disease Trends",value="vac_dis_tab",
                                           h3(strong(htmlOutput("content_vac_dis"))),
                                           selectInput("vaccinations", "Vaccination:",choices=NULL),
                                           plotlyOutput("selected_vac_dis_plot"),),
                                  tabPanel("Data Explorer",fluidRow(column(6, radioButtons("dataset","Choose Dataset", choices = c("All"="all","SDI" ="sdi","Vaccine Trends" = "vaccine trends","Disease Trends" = "disease trends"),inline = TRUE)),
                                                           column(6, style = "margin-top: 10px;",div(downloadButton("download","Download the data"), style = "float: right"))),
                                           DT::dataTableOutput("alldatatable") %>% withSpinner(color="#0dc5c1"))
                      )
                  )
)

server <- function(input, output,session) {
    #year <- reactive({
     #   req(input$year)
      #  merged_data_for_visuals[merged_data_for_visuals$year == input$year,]
    #})
  
  observeEvent(merged_data_for_visuals,{
    choices = sort(unique(merged_data_for_visuals$year))
    updateSliderInput(session,'year', value=range(choices),
                      min = min(as.numeric(choices)), max = max(as.numeric(choices)), step = 1)
  })
  year <- reactive({
      merged_data_for_visuals[merged_data_for_visuals$year == input$year,]
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
      req(input$sdi_group_present)
      if (input$sdi_group_present == "all"){
          all_sdi_group <- year() 
      }
      else{
          filter(year(), sdi_group_present == input$sdi_group_present)
      }
  })
    
  output$table = DT::renderDataTable({
      sdi_rank_table<-sdi_group_present()[,c("location_name","sdi","sdi_group_present")]
      sdi_rank_table$rank <- NA
      sdi_rank_table$rank = dense_rank(desc(sdi_rank_table$sdi))
      sdi_rank_table <- sdi_rank_table[,c("rank","location_name","sdi","sdi_group_present")]
      print("sorting")
      sdi_rank_table<-sdi_rank_table[order(sdi_rank_table$rank),]
      print(sdi_rank_table)
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
              'sdi' = color_tile("white", "pink"),
              ## use custom formatter for TRUE/FALSE values
              sdi_group_present = true_false_formatter
          )
      ) %>%
          as.datatable(rownames = FALSE, 
                       selection = list(mode = 'single',target="cell", selected = matrix(c(0, 1), nrow = 1,ncol = 2)), 
                       options = list(paging = TRUE,
                                      #scrollX=TRUE, 
                                      ordering = FALSE,
                                      #dom = 'Bfrtip',
                                      pageLength=12)
          )
  })
  
  output$content_vac <- renderText("Switzerland")
  output$content_dis <- renderText("Switzerland")
  output$content_vac_dis <- renderText("Switzerland")
  
  output$all_vaccine_plot <- renderPlotly({
      vac_plotdata <- filter(vaccine_trends,gsub(" ", "", location_name) == gsub(" ", "", "Switzerland"))
      if (input$vaccine_plot == "line_trend"){
          fig_a <- plot_ly(vac_plotdata, x = ~year_id,y=~prop_val, color = ~vaccine_name)%>%
              add_lines()
          fig_a <- fig_a %>% 
              layout(title ="Time Series of Vaccination Coverage",  showlegend = T,
                     xaxis = list(title = "Year",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                     yaxis = list(title = "Modeled estimate of vaccination coverage (%)",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE))
          fig_a
      }
      else{
          single_year_vac_plotdata <- filter(vac_plotdata,year_id == input$year)
          fig1 <- plot_ly(x = ~single_year_vac_plotdata$prop_val, y = ~reorder(single_year_vac_plotdata$vaccine_name, single_year_vac_plotdata$prop_val), name = paste0('Vaccniation Coverage in',input$year),
                          type = 'bar', orientation = 'h',
                          marker = list(color = 'rgba(50, 171, 96, 0.6)',
                                        line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1))) 
          fig1 <- fig1 %>% layout(title = paste0("Vaccination Coverage in ", input$year),
                                  yaxis = list(title = paste0("Vaccination coverage (%)"),showgrid = FALSE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85)),
                                  xaxis = list(title = "Vaccine", zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE)) 
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
      disease_plotdata <- filter(disease_trends,gsub(" ", "", location_name) == gsub(" ", "", "Switzerland"))
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
          y_title="Proportion of deaths for a particular cuase relative to deaths from all causes"
      }
      else{
          fig_dis <- plot_ly(disease_plotdata, x = ~year_id,y= ~deaths_rate_val, color = ~cause_name)%>%
              add_lines()
          title = "Time Series of Deaths, Disease or Disability Rate"
          y_title="Deaths per 100,000 population"
      }
      fig_dis <- fig_dis %>% 
          layout(title =title,  showlegend = T,
                 xaxis = list(title = "Year",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                 yaxis = list(title =y_title,showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE))
      fig_dis
  })
  
  output$all_disability_plot <- renderPlotly({
      disability_plotdata <- filter(disease_trends,gsub(" ", "", location_name) == gsub(" ", "", "Switzerland"))
      if (input$disease_estimate == "number_val"){
          fig_dis <- plot_ly(disability_plotdata, x = ~year_id,y= ~ylds_number_val, color = ~cause_name)%>%
              add_lines()
          title = "Time Series of Years Lived in Less Than Ideal health"
          y_title = "Number of years lived with disability in the population "
      }
      else if (input$disease_estimate == "percent_val"){
          fig_dis <- plot_ly(disability_plotdata, x = ~year_id,y= ~ylds_percent_val, color = ~cause_name)%>%
              add_lines()
          title = "Time Series of Proportion of Years Lived in Less Than Ideal health"
          y_title="Proportion of YLDs for a particular cause relative to YLDs for all causes"
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
          layout(title =title,  showlegend = T,
                 xaxis = list(title = "Year",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                 yaxis = list(title = y_title,showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE))
      fig_disa
  })
  
  observeEvent(selected_dis_vac_data(),{
    output$selected_vac_dis_plot <- renderPlotly({
      selected_vac_plotdata <- filter(selected_dis_vac_data()$selected_vac_data,gsub(" ", "", location_name) == gsub(" ", "", "Switzerland"))
      selected_dis_plotdata <- filter(selected_dis_vac_data()$dis_data_for_selected_vac,gsub(" ", "", location_name) == gsub(" ", "", "Switzerland"))
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
        title = list(text="Vaccine & Corresponding Disease Trend", x=0.25), yaxis2 = ay,
        xaxis = list(title="xaxis title "),
        yaxis = list(title="<b> Vaccine</b> coverage (%)"),
        legend = list(x = 3000, y = 1.4)
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
      output$content_vac <- renderText(info$value)
      output$content_dis <- renderText(info$value)
      output$content_vac_dis <- renderText(info$value)
      output$all_vaccine_plot <- renderPlotly({
          vac_plotdata <- filter(vaccine_trends,gsub(" ", "", location_name) == gsub(" ", "", info$value))
          if (input$vaccine_plot == "line_trend"){
              fig_a <- plot_ly(vac_plotdata, x = ~year_id,y=~prop_val, color = ~vaccine_name)%>%
                  add_lines()
              fig_a <- fig_a %>% 
                  layout(title ="Time Series of Vaccination Coverage",  showlegend = T,
                         xaxis = list(title = "Year",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                         yaxis = list(title = "Modeled estimate of vaccination coverage (%)",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE))
              fig_a
          }
          else{
              single_year_vac_plotdata <- filter(vac_plotdata,year_id == input$year)
              fig1 <- plot_ly(x = ~single_year_vac_plotdata$prop_val, y = ~reorder(single_year_vac_plotdata$vaccine_name, single_year_vac_plotdata$prop_val), name = paste0('Vaccniation Coverage in',input$year),
                              type = 'bar', orientation = 'h',
                              marker = list(color = 'rgba(50, 171, 96, 0.6)',
                                            line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1))) 
              fig1 <- fig1 %>% layout(title = paste0("Vaccination Coverage in ", input$year),
                                      yaxis = list(title = paste0("Vaccination coverage (%)"),showgrid = FALSE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85)),
                                      xaxis = list(title = "Vaccine", zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE)) 
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
              y_title="Proportion of deaths for a particular cuase relative to deaths from all causes"
          }
          else{
              fig_dis <- plot_ly(disease_plotdata, x = ~year_id,y= ~deaths_rate_val, color = ~cause_name)%>%
                  add_lines()
              title = "Time Series of Deaths, Disease or Disability Rate"
              y_title="Deaths per 100,000 population"
          }
          fig_dis <- fig_dis %>% 
              layout(title =title,  showlegend = T,
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
              y_title = "Number of years lived with disability in the population "
          }
          else if (input$disease_estimate == "percent_val"){
              fig_dis <- plot_ly(disability_plotdata, x = ~year_id,y= ~ylds_percent_val, color = ~cause_name)%>%
                  add_lines()
              title = "Time Series of Proportion of Years Lived in Less Than Ideal health"
              y_title="Proportion of YLDs for a particular cause relative to YLDs for all causes"
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
              layout(title =title,  showlegend = T,
                     xaxis = list(title = "Year",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                     yaxis = list(title = y_title,showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE))
          fig_disa
      })
  })
  
  output$mymap <- renderLeaflet({
      leaflet() %>%
          addProviderTiles(provider = "CartoDB.Positron")%>%
          setView(lng = -10.61, lat = 40, zoom = 2)
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
      else{
          dataexplorer <- disease_trends
      }
  })
  
  output$alldatatable = DT::renderDataTable({
          data<-dataexplorer()
          if (input$dataset == "all"){
              x<-data %>%
                  dplyr::select(-c("location_id","level"))
          }
          else if(input$dataset == "sdi"){
              x<- data %>%
                  dplyr::select(-c("location_id","level"))
          }
          else if(input$dataset == "vaccine trends"){
              x<-data %>%
                  dplyr::select(-c("location_id"))
          }
          else{
              x<-data %>%
                  dplyr::select(-c("location_id","cause_id"))
          }
          formattable(
             x
          ) %>%
              as.datatable(rownames = FALSE,
                           filter = 'top',
                           options = list(paging = TRUE,
                                          seaching = FALSE,
                                          scrollX=TRUE, 
                                          ordering = TRUE,
                                          dom = '<lf<t>p>',
                                          pageLength=12))
      })
  output$download <- downloadHandler(
      filename =  paste0(input$dataset,".csv",sep=""),
      content = function(fname){
          write.csv(dataexplorer(), fname)
      }
  )
}

shinyApp(body, server)