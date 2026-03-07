library(shiny)
library(dplyr)
library(DT)

source("Calculator_Logic.R")

ui <- fluidPage(
  
  titlePanel("Shipping Greenhouse Gas Calculator"),
  
  fluidRow(
    
    column(
      width = 4,
      
      actionButton("add_leg", "Add Leg"),
      actionButton("remove_leg", "Remove Leg"),
      
      br(), br(),
      
      uiOutput("legs_ui"),
      
      br(),
      
      downloadButton("download_data", "Download Results")
      
    ),
    
    column(
      width = 8,
      
      h3("Emissions Results"),
      
      DTOutput("results_table")
      
    )
    
  )
  
)

server <- function(input, output, session) {
  
  ### TRACK NUMBER OF LEGS
  legs <- reactiveVal(1)
  
  observeEvent(input$add_leg,{
    legs(legs() + 1)
  })
  
  observeEvent(input$remove_leg,{
    if(legs() > 1){
      legs(legs() - 1)
    }
  })
  
  
  ### BUILD LEG INPUT PANELS
  output$legs_ui <- renderUI({
    
    n <- legs()
    
    tagList(
      
      lapply(1:n, function(i){
        
        wellPanel(
          
          h4(paste("Transit Leg", i)),
          
          selectInput(
            paste0("transportation_type_",i),
            "Transportation Type",
            choices = transportation_type
          ),
          
          uiOutput(paste0("vehicle_ui_",i)),
          
          numericInput(
            paste0("distance_value_",i),
            "Distance",
            value = 1,
            min = 0
          ),
          
          selectInput(
            paste0("distance_unit_",i),
            "Distance Unit",
            choices = distance_unit
          ),
          
          conditionalPanel(
            condition = sprintf(
              "input['transportation_type_%s'] == 'LTL'",i
            ),
            
            numericInput(
              paste0("weight_value_",i),
              "Weight",
              value = 1,
              min = 0
            ),
            
            selectInput(
              paste0("weight_unit_",i),
              "Weight Unit",
              choices = weight_unit
            )
            
          )
          
        )
        
      })
      
    )
    
  })
  
  
  ### VEHICLE TYPE DEPENDS ON TRANSPORTATION TYPE
  observe({
    
    lapply(1:legs(), function(i){
      
      output[[paste0("vehicle_ui_",i)]] <- renderUI({
        
        req(input[[paste0("transportation_type_",i)]])
        
        selectInput(
          paste0("vehicle_type_",i),
          "Vehicle Type",
          choices = get_vehicle_types(
            input[[paste0("transportation_type_",i)]]
          )
        )
        
      })
      
    })
    
  })
  
  
  ### BUILD EMISSIONS DATA
  emissions_data <- reactive({
    
    n <- legs()
    
    df <- bind_rows(
      
      lapply(1:n, function(i){
        
        transportation_type <- input[[paste0("transportation_type_",i)]]
        vehicle_type <- input[[paste0("vehicle_type_",i)]]
        distance_value <- input[[paste0("distance_value_",i)]]
        distance_unit <- input[[paste0("distance_unit_",i)]]
        weight_value <- input[[paste0("weight_value_",i)]]
        weight_unit <- input[[paste0("weight_unit_",i)]]
        
        if(is.null(transportation_type) |
           is.null(vehicle_type) |
           is.null(distance_value) |
           is.null(distance_unit)) return(NULL)
        
        if(transportation_type == "FTL"){
          weight_value <- 0
          weight_unit <- "short tons"
        }
        
        data.frame(
          
          Leg = as.character(i),
          
          Transportation_Type = transportation_type,
          Vehicle_Type = vehicle_type,
          
          Distance = distance_value,
          Distance_Unit = distance_unit,
          
          Weight = weight_value,
          Weight_Unit = weight_unit,
          
          CO2 = round(calculate_CO2(
            transportation_type,
            vehicle_type,
            distance_value,
            distance_unit,
            weight_value,
            weight_unit
          ), digits = 3),
          
          CH4 = round(calculate_CH4(
            transportation_type,
            vehicle_type,
            distance_value,
            distance_unit,
            weight_value,
            weight_unit
          ), digits = 3),
          
          N2O = round(calculate_N2O(
            transportation_type,
            vehicle_type,
            distance_value,
            distance_unit,
            weight_value,
            weight_unit
          ), digits = 3),
          
          CO2e = round(calculate_CO2e(
            transportation_type,
            vehicle_type,
            distance_value,
            distance_unit,
            weight_value,
            weight_unit
          ), digits = 6)
          
        )
        
      })
      
    )
    
    if(nrow(df)==0) return(NULL)
    
    totals <- df %>%
      summarise(
        Leg = "Total",
        Transportation_Type = "",
        Vehicle_Type = "",
        Distance = NA_real_,
        Distance_Unit = "",
        Weight = NA_real_,
        Weight_Unit = "",
        CO2 = sum(CO2, na.rm = TRUE),
        CH4 = sum(CH4, na.rm = TRUE),
        N2O = sum(N2O, na.rm = TRUE),
        CO2e = sum(CO2e, na.rm = TRUE)
      )
    
    bind_rows(df, totals)
    
  })
  
  
  ### RENDER RESULTS TABLE
  output$results_table <- renderDT({
    
    req(emissions_data())
    
    datatable(
      emissions_data(),
      rownames = FALSE,
      options = list(
        pageLength = 10,
        dom = 't'
      )
    )
    
  })
  
  
  ### DOWNLOAD HANDLER
  output$download_data <- downloadHandler(
    
    filename = function(){
      paste0("shipping_emissions_",Sys.Date(),".csv")
    },
    
    content = function(file){
      
      write.csv(
        emissions_data(),
        file,
        row.names = FALSE
      )
      
    }
    
  )
  
}

shinyApp(ui,server)