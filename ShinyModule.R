library(shiny)
library(move)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggiraph)
library(hrbrthemes)
library(ctmm)
library(purrr)

# set some default styling for ggirafe
css_default_hover <- girafe_css_bicolor(primary = "#8f7d75", secondary = "#fadccf")
hrbrthemes::import_roboto_condensed()

set_girafe_defaults(
  # opts_hover = opts_hover(css = css_default_hover),
  opts_zoom = opts_zoom(min = 1, max = 4),
  opts_tooltip = opts_tooltip(css = "padding:3px;background-color:#333333;color:white;"),
  opts_sizing = opts_sizing(rescale = TRUE),
  opts_toolbar = opts_toolbar(saveaspng = FALSE, position = "bottom", delay_mouseout = 5000)
)

# to display messages to the user in the log file of the App in MoveApps
# one can use the function from the src/common/logger.R file:
# logger.fatal() -> logger.trace()

shinyModuleUserInterface <- function(id, label) {
  ns <- NS(id) ## all IDs of UI functions need to be wrapped in ns()
  tagList(
    titlePanel("Outlier detection"),
    selectInput(
      ns("select_var"),
      label = "Select Variable",
      choices =
        c("Speed" = "speed",
          "Distance" = "distance")
    ),
    uiOutput(ns("moreControls")),
    uiOutput(ns("moreControls2")),
    girafeOutput(ns("plot"))
  )
}

shinyModule <- function(input, output, session, data){ ## The parameter "data" is reserved for the data object passed on from the previous app
  ns <- session$ns ## all IDs of UI functions need to be wrapped in ns()
  
  outl <- lapply(data, \(x) outlie(x, plot = FALSE))
  
  reactive_data <- reactive({
    outl_tibbl <- unlist(sapply(outl, \(x) x[, input$select_var])) 
    outl_tibbl <- outl_tibbl|> 
      as_tibble() |> 
      pivot_longer(everything(), names_to = "Individium", values_to = "speed")
  })
  
  y <- reactive({
    req(input$y)
    input$y
  })
  
  observeEvent(reactive_data(), {
    
    quantiles = quantile(reactive_data()$speed, probs = seq(0, round(max(reactive_data()$speed)), by = 0.05))
    
    max_range <- round(max(reactive_data()$speed))
    max_range
    names(quantiles)
    c(unique(as.numeric(quantiles)), round(max(reactive_data()$speed)))
    
    output$moreControls <- renderUI({
      tagList(
        shinyWidgets::sliderTextInput(inputId = "decade", 
                                      label = "Range:", 
                                      choices = c(unique(as.numeric(quantiles)), round(max(reactive_data()$speed))))
      )
    })
    
    output$moreControls2 <- renderUI({
      tagList(
        shinyWidgets::sliderTextInput(ns("slider_filterperc"), 
                                      label = "Percentile:",
                                      choices = names(quantiles))
      )
    })
  })

  observeEvent(input$slider_filter,  {
    updateSliderInput(session = session, inputId = "Empty", value = 1 - input$Full)
  })
  
  # when air change, update water
  observeEvent(input$Empty,  {
    updateSliderInput(session = session, inputId = "Full", value = 1 - input$Empty)
  })
  
  output$plot <- renderGirafe({
    
    req(input$slider_filter)
    
    plot_data <- reactive_data() |> 
      filter(speed >= input$slider_filter[1] & speed <= input$slider_filter[2])
    
    gg <- ggplot(plot_data, aes(x=speed)) + 
      geom_histogram_interactive(hover_nearest = TRUE) +
      theme_ipsum_rc()
    girafe(ggobj = gg)

  })
  
  
  return(reactive({ 
    req(input$slider_filter)
    req(input$select_var)

    if(input$select_var == "speed"){
      map2(data, outl, function(a,b){
        
        ind <- which(b$speed >= input$slider_filter[1] & 
                       b$speed <= input$slider_filter[2])
        if (length(ind) > 0) {
          a[ind, ]
        } else {
          NULL
        }
        
      })
    } else if(input$select_var == "distance"){
      map2(data, outl, function(a,b){
        
        ind <- which(b$speed >= input$slider_filter[1] & 
                       b$speed <= input$slider_filter[2])
        if (length(ind) > 0) {
          a[ind, ]
        } else {
          NULL
        }
        
      })
    }
    })) ## if data are not modified, the unmodified input data must be returned
}





