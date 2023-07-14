library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggiraph)
library(hrbrthemes)
library(ctmm)
library(purrr)
library(shinyWidgets)
library(shinyjs)

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
    useShinyjs(),
    titlePanel("Outlier detection"),
    plotOutput(ns("outl_plot")),
    p("On the x-axis distances from the median longitude and latitude are ploted and on the y-axis the minimum speed required to explain the location estimate's displacement as straight-line motion."), 
    hr(),
    fluidRow(
      column(6, 
             selectInput(
               ns("select_var"),
               label = "Select Variable",
               choices =
                 c("Speed (m/s)" = "speed",
                   "Distance (m)" = "distance")
             )
             
      ),
      column(6, 
             uiOutput(ns("recursiveUI"))
      )), 
    uiOutput(ns("moreControls")),
    uiOutput(ns("moreControls2")),
    girafeOutput(ns("plot"))
  )
}

shinyModule <- function(input, output, session, data){ ## The parameter "data" is reserved for the data object passed on from the previous app
  ns <- session$ns ## all IDs of UI functions need to be wrapped in ns()
  
  outl <- reactive({
  
#  req(input$filtertest)
#    lapply(data, \(x) {
#      xx <- outlie(x, plot = FALSE)
#      xx})
    #lapply(data, \(x) {
    #  xx <- outlie(x, plot = FALSE)
    #  if (input$recursive) {
    #    while(max(xx[[input$select_var]]) > input$filtertest[2]) {
    #      srk_tl <- srk_tl[!xx[[input$select_var]] < input$filtertest[2], ]
    #      xx <- outlie(srk_tl, plot = FALSE)
    #    }
    #  }
    #  xx
    #})
    
    lapply(data, \(x) {
      xx <- outlie(x, plot = FALSE)
      if(!is.null(input$recursive) & !is.null(input$filtertest)) {
        if (input$select_var == "speed" & input$recursive) {
          while(max(xx[["speed"]]) > input$filtertest[2]) {
            srk_tl <- srk_tl[!xx[["speed"]] < input$filtertest[2], ]
            xx <- outlie(srk_tl, plot = FALSE)
          }
        }
      }
      xx
    })
  })
  
  output$recursiveUI <- renderUI({
      checkboxInput(ns("recursive"), "Recursive", value = FALSE)
  })
  
  observeEvent(input$select_var, {
    if (input$select_var == "speed") {
      shinyjs::show(ns("recursive"))
    } else {
      shinyjs::hideElement(ns("recursive"))
    }
  })
  
  output$outl_plot <- renderPlot({
    plot(outl())
  })
  
  reactive_data <- reactive({
    outl_tibbl <- unlist(sapply(outl(), \(x) x[, input$select_var])) 
    outl_tibbl <- outl_tibbl|> 
      as_tibble() |> 
      pivot_longer(everything(), names_to = "Individium", values_to = "speed")
  })
  
  observeEvent(reactive_data(), {
    
    req(reactive_data())
    
    quantiles = quantile(reactive_data()$speed, probs = seq(0, 1, by = 0.05))
    quantiles = quantiles[!duplicated(quantiles)]
    
    # if(input$select_var == "speed") browser()
    
    output$moreControls <- renderUI({
      
      tagList(
        sliderTextInput(ns("slider_filtertest"), 
                        label = "Range:", 
                        choices = round(as.numeric(quantiles), 6), 
                        selected = round(c(unique(as.numeric(quantiles))[1], as.numeric(quantiles)[length(as.numeric(quantiles))]), 6)
        )
      )
    })
    
    output$moreControls2 <- renderUI({
      tagList(
        sliderTextInput(ns("slider_filterperc"), 
                        label = "Percentile:",
                        choices = names(quantiles),
                        selected = c(names(quantiles)[1],
                                     names(quantiles)[length(names(quantiles))]))
      )
    })
  })

  observeEvent(input$slider_filtertest,  {
    req(input$slider_filterperc)
    quantiles = quantile(reactive_data()$speed, probs = seq(0, 1, by = 0.05))
    quantiles = quantiles[!duplicated(quantiles)]
    ind <- which(round(as.numeric(quantiles), 6) %in% round(as.numeric(input$slider_filtertest), 6))
    updateSliderTextInput(session = session, inputId = "slider_filterperc", selected = names(quantiles)[ind])
  }, ignoreInit = TRUE)

  observeEvent(input$slider_filterperc,  {
    req(input$slider_filtertest)

    quantiles = quantile(reactive_data()$speed, probs = seq(0, 1, by = 0.05))
    quantiles = quantiles[!duplicated(quantiles)]
    ind <- which(names(quantiles) %in% input$slider_filterperc)
#    print(paste0("Percentile: ", as.numeric(quantiles)[ind]))
    updateSliderTextInput(session = session, inputId = "slider_filtertest", selected = round(as.numeric(quantiles)[ind], 6))
    }, ignoreInit = TRUE)
  
  output$plot <- renderGirafe({
    
    req(input$slider_filtertest)
    
    plot_data <- reactive_data() |> 
      filter(speed >= input$slider_filtertest[1] & speed <= input$slider_filtertest[2])
    
    gg <- ggplot(plot_data, aes(x=speed)) + 
      geom_histogram_interactive(hover_nearest = TRUE) +
      labs(x = if (input$select_var == "speed") "Speed (m/s)" else "Distance (m)") +
      theme_ipsum_rc()
    girafe(ggobj = gg)

  })
  
  dat.f <- reactive({ 
    
    req(input$slider_filtertest)
    req(input$select_var)
    
    xx <- if(input$select_var == "speed") {
      map2(data, outl(), function(a, b) {
        ind <- which(b$speed >= input$slider_filtertest[1] & 
                       b$speed <= input$slider_filtertest[2])
        if (length(ind) > 0) {
          a[ind, ]
        } else {
          NULL
        }
      })
    } else if(input$select_var == "distance") {
      map2(data, outl(), function(a, b) {
        ind <- which(b$speed >= input$slider_filtertest[1] & 
                       b$speed <= input$slider_filtertest[2])
        if (length(ind) > 0) {
          a[ind, ]
        } else {
          NULL
        }
      })
    }
    xx
  })
  
  #return(reactive(filtered_data())) ## if data are not modified, the unmodified input data must be returned
  return(
    reactive({
      dat.f()
    })
  )
}





