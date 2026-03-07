library(shiny)
library(dplyr)
library(reactable)
library(shinyWidgets)

source("Calculator_Logic.R")

`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

ui <- fluidPage(
  titlePanel("Freight GHG Calculator"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("legs_ui"),
      br(),
      actionButton("add_leg", "Add Freight Leg"),
      actionButton("remove_leg", "Remove Last Leg"),
      br(), br(),
      downloadButton("download_data","Download Table")
    ),
    mainPanel(
      reactableOutput("results_table", height = "500px")
    )
  )
)

server <- function(input, output, session){
  
  # Initialize 1 leg
  rv <- reactiveValues(
    legs = tibble(
      Leg = 1,
      freight_type = "FTL",
      vehicle_type = "Medium- and Heavy-Duty Truck",
      distance_value = 1,
      distance_unit = "mile",
      weight_value = 0,
      weight_unit = "short ton"
    ),
    counter = 1
  )
  
  # Add leg
  observeEvent(input$add_leg,{
    rv$counter <- rv$counter + 1
    rv$legs <- bind_rows(rv$legs,
                         tibble(
                           Leg = rv$counter,
                           freight_type = "FTL",
                           vehicle_type = "Medium- and Heavy-Duty Truck",
                           distance_value = 1,
                           distance_unit = "mile",
                           weight_value = 1,
                           weight_unit = "short ton"
                         )
    )
  })
  
  # Remove last leg
  observeEvent(input$remove_leg,{
    if(nrow(rv$legs) > 1){
      rv$legs <- rv$legs[-nrow(rv$legs), ]
      rv$counter <- rv$counter - 1
    }
  })
  
  # Render dynamic UI for legs in sidebar
  output$legs_ui <- renderUI({
    req(rv$legs)
    lapply(1:nrow(rv$legs), function(i){
      leg <- rv$legs[i,]
      wellPanel(
        h4(paste("Leg", leg$Leg)),
        selectInput(paste0("t_type_",i),"Freight Type",
                    choices = freight_type,
                    selected = leg$freight_type),
        selectInput(paste0("v_type_",i),"Vehicle Type",
                    choices = get_vehicle_types(leg$freight_type),
                    selected = leg$vehicle_type),
        numericInput(paste0("d_val_",i),"Distance",value=leg$distance_value),
        selectInput(paste0("d_unit_",i),"Distance Unit",choices=distance_unit,selected=leg$distance_unit),
        # Only show weight input for LTL
        if(leg$freight_type=="LTL"){
          tagList(
            # In the dynamic UI:
            numericInput(
              paste0("w_val_",i),
              "Weight",
              value = ifelse(is.na(leg$weight_value) || leg$weight_value==0, 1, leg$weight_value)
            ),
            selectInput(
              paste0("w_unit_",i),
              "Weight Unit",
              choices = weight_unit,
              selected = ifelse(is.na(leg$weight_unit), "short ton", leg$weight_unit)
            )
          )
        }
      )
    })
  })
  
  # Update reactive legs safely
  observe({
    req(rv$legs)
    for(i in 1:nrow(rv$legs)){
      t_in <- input[[paste0("t_type_",i)]] %||% rv$legs$freight_type[i]
      rv$legs$freight_type[i] <- t_in
      vt_choices <- get_vehicle_types(t_in)
      v_in <- input[[paste0("v_type_",i)]] %||% vt_choices[1]
      if(!(v_in %in% vt_choices)) v_in <- vt_choices[1]
      rv$legs$vehicle_type[i] <- v_in
      rv$legs$distance_value[i] <- input[[paste0("d_val_",i)]] %||% rv$legs$distance_value[i]
      rv$legs$distance_unit[i] <- input[[paste0("d_unit_",i)]] %||% rv$legs$distance_unit[i]
      
      # Weight only updated for LTL
      if(t_in=="LTL"){
        rv$legs$weight_value[i] <- input[[paste0("w_val_",i)]] %||% rv$legs$weight_value[i]
        rv$legs$weight_unit[i] <- input[[paste0("w_unit_",i)]] %||% rv$legs$weight_unit[i]
      } else {
        rv$legs$weight_value[i] <- 1
        rv$legs$weight_unit[i] <- "short ton"
      }
    }
  })
  
  # Reactive emissions calculation
  emissions_data <- reactive({
    req(rv$legs)
    df <- rv$legs %>%
      rowwise() %>%
      mutate(
        CO2_kg = round(calculate_CO2(freight_type,vehicle_type,distance_value,distance_unit,weight_value,weight_unit), 3),
        CH4_g = round(calculate_CH4(freight_type,vehicle_type,distance_value,distance_unit,weight_value,weight_unit), 3),
        N2O_g = round(calculate_N2O(freight_type,vehicle_type,distance_value,distance_unit,weight_value,weight_unit), 3),
        CO2e_tonnes = round(calculate_CO2e(freight_type,vehicle_type,distance_value,distance_unit,weight_value,weight_unit), 6)
      ) %>%
      ungroup()
    
    df$weight_value[df$freight_type == "FTL"] <- NA
    df$weight_unit[df$freight_type == "FTL"] <- NA
    
    # Totals row
    totals <- df %>%
      summarise(
        Leg=NA,
        freight_type="Total",
        vehicle_type="",
        distance_value=sum(distance_value),
        distance_unit="",
        weight_value=NA,
        weight_unit="",
        CO2_kg=sum(CO2_kg),
        CH4_g=sum(CH4_g),
        N2O_g=sum(N2O_g),
        CO2e_tonnes=sum(CO2e_tonnes)
      )
    
    bind_rows(df,totals)
  })
  
  # Render table
output$results_table <- renderReactable({
  df <- emissions_data()
  
  # Create a table version where FTL weights appear blank
  df_table <- df %>%
    mutate(
      weight_value = ifelse(freight_type == "FTL" & !is.na(weight_value), NA, weight_value),
      weight_unit  = ifelse(freight_type == "FTL" & !is.na(weight_unit), "", weight_unit)
    )
  
  reactable(
    df_table,
    columns = list(
      Leg = colDef(name="Leg", width=50),
      freight_type = colDef(name="Freight Type"),
      vehicle_type = colDef(name="Vehicle Type"),
      distance_value = colDef(name="Distance"),
      distance_unit = colDef(name="Distance Unit"),
      weight_value = colDef(name="Weight"),
      weight_unit = colDef(name="Weight Unit"),
      CO2_kg = colDef(name="CO₂ Emissions (kg)"),
      CH4_g = colDef(name="CH₄ Emissions (g)"),
      N2O_g = colDef(name="N₂O Emissions (g)"),
      CO2e_tonnes = colDef(name="Total Emission\n(metric tonnes CO₂e)")
    ),
    bordered = FALSE,
    highlight = TRUE,
    striped = FALSE,
    defaultColDef = colDef(align="center", minWidth=80),
    fullWidth = TRUE,
    defaultPageSize = 10,
    rowStyle = function(index){
      if(df_table$freight_type[index]=="Total"){
        list(fontWeight="bold")
      } else list()
    }
  )
})
  
  # Download CSV
  output$download_data <- downloadHandler(
    filename = function(){ "shipping_emissions.csv" },
    content = function(file){ write.csv(emissions_data(), file, row.names = FALSE) }
  )
  
}

shinyApp(ui, server)