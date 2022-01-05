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


#setwd("~/Desktop/PHI - Research Assitant/Winter/vax/aim1")
vaccine_trends <- readRDS("01_vaccine_trends.RDS")
sdi <- readRDS("02_sdi.RDS")
raw_extracted_dhs <- readRDS("03_raw_extracted_dhs.RDS")
prepped_dhs_for_mov <- readRDS("04_prepped_dhs_for_mov.RDS")
disease_trends <- readRDS("05_disease_trends.RDS")
merged_data_for_visuals <- readRDS("06_merged_data_for_visuals.RDS")
vaccine_preventable_diseases <- read_excel("vaccine_preventable_diseases.xlsx")

#low_sdi_lctns <- merged_data_for_visuals[sdi_group_present=="low"][,c("location_name","sdi")]
#medium_sdi_lctns <- merged_data_for_visuals[sdi_group_present=="medium"][,c("location_name","sdi")]
#high_sdi_lctns <- merged_data_for_visuals[sdi_group_present=="high"][,c("location_name","sdi")]

############################################### ui.R ##################################################
body <-navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
                  title = "Global Vaccination Improvement Dashboard",
                  sidebarPanel(
                      h3(strong("Global SDI Ranking Table")),
                      selectInput("year", "Year:",choices=sort(unique(merged_data_for_visuals$year))),
                      radioButtons("sdi_group_present","SDI Group Present", choices = c("All"="all","Low" ="low","Medium" = "medium","High" = "high"),inline = TRUE),
                      tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: #92c9e8 !important;}')),
                      DT::dataTableOutput("table")),
                  mainPanel(
                      tabsetPanel(id = "t1",
                                  tabPanel("Global SDI Mapper",value="t_sdi",
                                           h3(strong("A World SDI Ranking Map will be updated here later"),style='font-family:Avenir, Helvetica;font-size:40;text-align:center'),
                                           leaflet::leafletOutput("mymap", height = "80vh")),
                                  tabPanel("Vaccination Trends", value = "t_vac",
                                           h3(strong(htmlOutput("content_vac"))),
                                           fluidRow(column(width = 12, "Select location by clicking location_name in left Global SDI Ranking table or click location on map.",
                                                           style='font-family:Avenir, Helvetica;font-size:30;text-align:left')),
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
                                  tabPanel("Disease Trends",value = "d_vac",
                                           h3(strong(htmlOutput("content_dis"))),
                                           fluidRow(column(width = 12, "Select location by clicking location_name in left Global SDI Ranking table or click location on map.",
                                                           style='font-family:Avenir, Helvetica;font-size:30;text-align:left')),
                                           radioButtons("disease_estimate",NULL, choices = c("Number Value"="number_val","Percent Value" ="percent_val","Rate Value" = "rate_val"),inline = TRUE),
                                           plotlyOutput("all_disease_plot"),
                                           plotlyOutput("all_disability_plot"))
                      )
                  )
)

server <- function(input, output,session) {
    year <- reactive({
        req(input$year)
        merged_data_for_visuals[merged_data_for_visuals$year == input$year,]
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
    
    output$all_vaccine_plot <- renderPlotly({
        vac_plotdata <- filter(vaccine_trends,gsub(" ", "", location_name) == gsub(" ", "", "Switzerland"))
        fig_a <- plot_ly(vac_plotdata, x = ~year_id,y=~prop_val, color = ~vaccine_name)%>%
            add_lines()
        fig_a <- fig_a %>% 
            layout(title ="Time Series of Vaccination Coverage",  showlegend = T,
                   xaxis = list(title = "Year",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                   yaxis = list(title = "Modeled estimate of vaccination coverage (%)",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE))
        fig_a
    })
    
    output$all_disease_plot <- renderPlotly({
        print("input")
        print(input$disease_estimate)
        disease_plotdata <- filter(disease_trends,gsub(" ", "", location_name) == gsub(" ", "", "Switzerland"))
        if (input$disease_estimate == "number_val"){
            fig_dis <- plot_ly(disease_plotdata, x = ~year_id,y= ~deaths_number_val, color = ~cause_name)%>%
                add_lines()
            title = "Time Series of Deaths, Disease or Disability Cause Number"
            y_title = "Number of Deaths in Population"
        }
        else if (input$disease_estimate == "percent_val"){
            fig_dis <- plot_ly(disease_plotdata, x = ~year_id,y= ~deaths_percent_val, color = ~cause_name)%>%
                add_lines()
            title = "Time Series of Deaths, Disease or Disability Cause Percent"
            y_title="Proportion of deaths for a particular cuase relative to deaths from all causes"
        }
        else{
            fig_dis <- plot_ly(disease_plotdata, x = ~year_id,y= ~deaths_rate_val, color = ~cause_name)%>%
                add_lines()
            title = "Time Series of Deaths, Disease or Disability Cause Rate"
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
        output$all_vaccine_plot <- renderPlotly({
            vac_plotdata <- filter(vaccine_trends,gsub(" ", "", location_name) == gsub(" ", "", info$value))
            fig_vac <- plot_ly(vac_plotdata, x = ~year_id,y=~prop_val, color = ~vaccine_name)%>%
                add_lines()
            fig_vac <- fig_vac %>% 
                layout(title ="Time Series of Vaccination Coverage",  showlegend = T,
                       xaxis = list(title = "Year",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                       yaxis = list(title = "Modeled estimate of vaccination coverage (%)",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE))
            fig_vac
        })
        output$all_disease_plot <- renderPlotly({
            disease_plotdata <- filter(disease_trends,gsub(" ", "", location_name) == gsub(" ", "", info$value))
            if (input$disease_estimate == "number_val"){
                fig_dis <- plot_ly(disease_plotdata, x = ~year_id,y= ~deaths_number_val, color = ~cause_name)%>%
                    add_lines()
                title = "Time Series of Deaths, Disease or Disability Cause Number"
                y_title = "Number of Deaths in Population"
            }
            else if (input$disease_estimate == "percent_val"){
                fig_dis <- plot_ly(disease_plotdata, x = ~year_id,y= ~deaths_percent_val, color = ~cause_name)%>%
                    add_lines()
                title = "Time Series of Deaths, Disease or Disability Cause Percent"
                y_title="Proportion of deaths for a particular cuase relative to deaths from all causes"
            }
            else{
                fig_dis <- plot_ly(disease_plotdata, x = ~year_id,y= ~deaths_rate_val, color = ~cause_name)%>%
                    add_lines()
                title = "Time Series of Deaths, Disease or Disability Cause Rate"
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
        #output$lineplot <- renderPlot({
        #vac_data <- filter(year(),location_name == info$value)
        #vac_data %>% 
        #ggplot(aes(x=reorder(Education, CompTotal), y=CompTotal)) +
        #geom_boxplot()+
        #coord_flip()+
        #scale_y_log10()+
        #labs(x="Education", y="Salary in US Dollars")
    })
    
    output$mymap <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(provider = "CartoDB.Positron")
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
}

shinyApp(body, server)


