library(shiny)
library(dplyr)
library(reactable)
library(shinyWidgets)

source("Calculator_Logic.R")

ui <- fluidPage(
  titlePanel("Shipping GHG Calculator"),
  sidebarLayout(
    sidebarPanel(
      # Action buttons
      actionButton("add_leg", "Add Leg"),
      br(), br(),
      
      # Dynamic UI for each leg goes here
      uiOutput("legs_ui"),
      br(),
      downloadButton("download_data","Download Table")
    ),
    mainPanel(
      # Table only
      reactableOutput("results_table", height = "500px")
    )
  )
)

server <- function(input, output, session){
  
  # Initialize 1 leg
  rv <- reactiveValues(
    legs = tibble(
      Leg = 1,
      transportation_type = "FTL",
      vehicle_type = "Medium- and Heavy-Duty Truck",
      distance_value = 1,
      distance_unit = "mile",
      weight_value = 1,
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
                           transportation_type = "FTL",
                           vehicle_type = "Medium- and Heavy-Duty Truck",
                           distance_value = 1,
                           distance_unit = "mile",
                           weight_value = 1,
                           weight_unit = "short ton"
                         )
    )
  })
  
  # Render dynamic UI for legs in sidebar
  output$legs_ui <- renderUI({
    req(rv$legs)
    lapply(1:nrow(rv$legs), function(i){
      leg <- rv$legs[i,]
      wellPanel(
        h4(paste("Leg", leg$Leg)),
        selectInput(paste0("t_type_",i),"Transportation Type",
                    choices = transportation_type,
                    selected = leg$transportation_type),
        selectInput(paste0("v_type_",i),"Vehicle Type",
                    choices = get_vehicle_types(leg$transportation_type),
                    selected = leg$vehicle_type),
        numericInput(paste0("d_val_",i),"Distance",value=leg$distance_value),
        selectInput(paste0("d_unit_",i),"Distance Unit",choices=distance_unit,selected=leg$distance_unit),
        numericInput(paste0("w_val_",i),"Weight",value=leg$weight_value),
        selectInput(paste0("w_unit_",i),"Weight Unit",choices=weight_unit,selected=leg$weight_unit)
      )
    })
  })
  
  # Update reactive legs safely
  observe({
    req(rv$legs)
    for(i in 1:nrow(rv$legs)){
      t_in <- input[[paste0("t_type_",i)]] %||% rv$legs$transportation_type[i]
      rv$legs$transportation_type[i] <- t_in
      vt_choices <- get_vehicle_types(t_in)
      v_in <- input[[paste0("v_type_",i)]] %||% vt_choices[1]
      if(!(v_in %in% vt_choices)) v_in <- vt_choices[1]
      rv$legs$vehicle_type[i] <- v_in
      rv$legs$distance_value[i] <- input[[paste0("d_val_",i)]] %||% rv$legs$distance_value[i]
      rv$legs$distance_unit[i] <- input[[paste0("d_unit_",i)]] %||% rv$legs$distance_unit[i]
      rv$legs$weight_value[i] <- input[[paste0("w_val_",i)]] %||% rv$legs$weight_value[i]
      rv$legs$weight_unit[i] <- input[[paste0("w_unit_",i)]] %||% rv$legs$weight_unit[i]
    }
  })
  
  # Reactive emissions calculation
  emissions_data <- reactive({
    req(rv$legs)
    df <- rv$legs %>%
      rowwise() %>%
      mutate(
        CO2_kg = round(calculate_CO2(transportation_type,vehicle_type,distance_value,distance_unit,weight_value,weight_unit), 3),
        CH4_g = round(calculate_CH4(transportation_type,vehicle_type,distance_value,distance_unit,weight_value,weight_unit), 3),
        N2O_g = round(calculate_N2O(transportation_type,vehicle_type,distance_value,distance_unit,weight_value,weight_unit), 3),
        CO2e_tonnes = round(calculate_CO2e(transportation_type,vehicle_type,distance_value,distance_unit,weight_value,weight_unit), 6)
      ) %>%
      ungroup()
    
    # Totals row
    totals <- df %>%
      summarise(
        Leg=NA,
        transportation_type="Total",
        vehicle_type="",
        distance_value=sum(distance_value),
        distance_unit="",
        weight_value=sum(weight_value),
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
    reactable(
      df,
      columns=list(
        Leg = colDef(name="Leg", width=50),
        transportation_type = colDef(name="Transportation\nType"),
        vehicle_type = colDef(name="Vehicle\nType"),
        distance_value = colDef(name="Distance"),
        distance_unit = colDef(name="Distance\nUnit"),
        weight_value = colDef(name="Weight"),
        weight_unit = colDef(name="Weight\nUnit"),
        CO2_kg = colDef(name="CO₂ Emissions (kg)"),
        CH4_g = colDef(name="CH₄ Emissions (g)"),
        N2O_g = colDef(name="N₂O Emissions (g)"),
        CO2e_tonnes = colDef(name="Total Emission\n(metric tonnes CO₂e)")
      ),
      bordered=TRUE,
      highlight=TRUE,
      resizable=TRUE,
      defaultColDef=colDef(align="center", minWidth=80),
      fullWidth=TRUE,
      defaultPageSize=10
    )
  })
  
  # Download CSV
  output$download_data <- downloadHandler(
    filename = function(){ "shipping_emissions.csv" },
    content = function(file){ write.csv(emissions_data(), file, row.names = FALSE) }
  )
  
}

shinyApp(ui, server)