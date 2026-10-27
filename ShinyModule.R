library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggiraph)
library(hrbrthemes)
library(ctmm)
library(purrr)
library(shinyWidgets)
library(shinycssloaders)

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
    p("On the x-axis distances from the median longitude and latitude are ploted and on the y-axis the minimum speed required to explain the location estimate's displacement as straight-line motion."),
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
             # Recursion only applies to speed, so only show it then
             conditionalPanel(
               condition = "input.select_var == 'speed'",
               ns = ns,
               checkboxInput(ns("recursive"), "Recursive", value = FALSE)
             )
      )),
    uiOutput(ns("moreControls")),
    uiOutput(ns("moreControls2")),
    uiOutput(ns("dropped_note")),
    hr(),
    fluidRow(
      column(6, withSpinner(plotOutput(ns("outl_plot"), height = "400px"), hide.ui = FALSE)),
      column(6, withSpinner(girafeOutput(ns("plot"), height = "400px"), hide.ui = FALSE))
    )
  )
}

shinyModule <- function(input, output, session, data){ ## The parameter "data" is reserved for the data object passed on from the previous app
  ns <- session$ns ## all IDs of UI functions need to be wrapped in ns()

  # Raw outliers; not affected by recursion. Drives the slider's quantile space
  # so its range stays anchored to the actual data max.
  outl_raw <- reactive({
    lapply(data, \(x) outlie(x, plot = FALSE))
  })

  outl_rows <- reactive({
    if (input$select_var == "speed" &&
        isTRUE(input$recursive) &&
        !is.null(input$slider_filtertest)) {
      threshold <- bounds()[2]
      map2(data, outl_raw(), function(x, xx) {
        rows <- seq_len(nrow(x))
        while (nrow(x) > 0 && max(xx[["speed"]]) > threshold) {
          keep <- xx[["speed"]] <= threshold
          if (!any(keep)) break
          x <- x[keep, ]
          rows <- rows[keep]
          xx <- outlie(x, plot = FALSE)
        }
        
        # Return row indices along with outlier output. When `recursive = TRUE`,
        # we remove some rows from the data at each pass, meaning that the
        # indices can drift from the original input to the data that we're
        # actually returning. Passing `rows` along means we can index relative
        # to the original data.
        list(outl = xx, rows = rows)
      })
    } else {
      map2(data, outl_raw(), \(x, xx) list(outl = xx, rows = seq_len(nrow(x))))
    }
  })

  outl <- reactive({
    lapply(outl_rows(), \(r) r$outl)
  })

  # Quantile choices for the sliders, computed from raw outliers only.
  slider_quantiles <- reactive({
    req(input$select_var)
    outl_tibbl <- unlist(sapply(outl_raw(), \(x) x[, input$select_var]))
    req(length(outl_tibbl) > 0)
    q <- quantile(outl_tibbl, probs = seq(0, 1, by = 0.01))
    # Drop quantiles that would show as the same value on the slider
    q[!duplicated(round(as.numeric(q), 6))]
  })

  # Filter bounds for the current slider selection. The slider shows quantiles
  # rounded to 6 decimals; filtering on those rounded values can drop a point
  # sitting exactly at a quantile (e.g. the minimum at 0%). So look up the
  # unrounded quantile behind each selected value and filter on that instead.
  bounds <- reactive({
    req(input$slider_filtertest)
    sel <- as.numeric(input$slider_filtertest)
    q <- as.numeric(slider_quantiles())
    idx <- match(round(sel, 6), round(q, 6))
    # No match while the slider is still being rebuilt for a new variable
    ifelse(is.na(idx), sel, q[idx])
  })

  filtered_outl <- reactive({
    if (is.null(input$slider_filtertest)) return(outl())
    var <- input$select_var
    lo <- bounds()[1]
    hi <- bounds()[2]
    filtered <- lapply(outl(), \(x) {
      keep <- x[[var]] >= lo & x[[var]] <= hi
      out <- x[keep, , drop = FALSE]
      # `outlie` is an S4 object; data.frame subsetting drops that bit and breaks plot dispatch.
      class(out) <- class(x)
      asS4(out)
    })
    # Leave out individuals with no points in range, matching the returned
    # data. ctmm's plot method can't draw an individual with zero rows.
    purrr::keep(filtered, \(x) nrow(x) > 0)
  })

  output$outl_plot <- renderPlot({
    validate(need(length(filtered_outl()) > 0, "No points in the selected range."))
    plot(filtered_outl())
  })

  reactive_data <- reactive({
    req(input$select_var)

    outl_tibbl <- unlist(sapply(outl(), \(x) x[, input$select_var]))
    req(length(outl_tibbl) > 0)

    tbl <- as_tibble(outl_tibbl)
    req(ncol(tbl) > 0)

    tbl |>
      pivot_longer(everything(), names_to = "Individium", values_to = "speed")
  })

  observeEvent(slider_quantiles(), {

    quantiles <- slider_quantiles()

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
    quantiles <- slider_quantiles()
    ind <- match(round(as.numeric(input$slider_filtertest), 6), round(as.numeric(quantiles), 6))
    req(!anyNA(ind))
    new_perc <- names(quantiles)[ind]
    if (!identical(new_perc, input$slider_filterperc)) {
      updateSliderTextInput(session = session, inputId = "slider_filterperc", selected = new_perc)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$slider_filterperc,  {
    req(input$slider_filtertest)

    quantiles <- slider_quantiles()
    ind <- match(input$slider_filterperc, names(quantiles))
    req(!anyNA(ind))
    new_vals <- round(as.numeric(quantiles)[ind], 6)
    if (!isTRUE(all.equal(new_vals, as.numeric(input$slider_filtertest)))) {
      updateSliderTextInput(session = session, inputId = "slider_filtertest", selected = new_vals)
    }
    }, ignoreInit = TRUE)

  output$plot <- renderGirafe({

    req(input$slider_filtertest)
    lo <- bounds()[1]
    hi <- bounds()[2]

    plot_data <- reactive_data() |>
      filter(speed >= lo & speed <= hi)

    validate(need(nrow(plot_data) > 0, "No points in the selected range."))

    gg <- ggplot(plot_data, aes(x=speed)) +
      geom_histogram_interactive(hover_nearest = TRUE) +
      labs(x = if (input$select_var == "speed") "Speed (m/s)" else "Distance (m)") +
      theme_ipsum_rc()
    girafe(ggobj = gg)

  })

  # Each individual's data within the selected range; NULL if it has no points in range
  filtered_data <- reactive({

    req(input$slider_filtertest)
    req(input$select_var)
    # Wait for the slider to be rebuilt after the variable changes
    req(all(round(as.numeric(input$slider_filtertest), 6) %in% round(as.numeric(slider_quantiles()), 6)))

    var <- input$select_var
    lo <- bounds()[1]
    hi <- bounds()[2]
    map2(data, outl_rows(), function(a, r) {
      b <- r$outl
      ind <- which(b[[var]] >= lo & b[[var]] <= hi)
      if (length(ind) > 0) {
        a[r$rows[ind], ]
      } else {
        NULL
      }
    })
  })

  # Individuals with no points in range, which are left out of the output
  dropped_ids <- reactive({
    names(purrr::keep(filtered_data(), is.null))
  })

  output$dropped_note <- renderUI({
    ids <- dropped_ids()
    if (length(ids) == 0) return(NULL)
    p(
      class = "text-warning",
      paste0(
        "No points in the selected range for: ", paste(ids, collapse = ", "),
        ". These animals will not be passed to the next app."
      )
    )
  })

  dat.f <- reactive({
    ids <- dropped_ids()
    if (length(ids) > 0) {
      logger.warn(
        "No points in the selected range for: %s. Removed from the output.",
        paste(ids, collapse = ", ")
      )
    }

    # Remove NULL entries, which result from individuals with no points that
    # meet the input thresholds. Downstream apps may fail on NULL input.
    filtered <- purrr::compact(filtered_data())

    if (length(filtered) == 0) {
      NULL
    } else {
      filtered
    }
  })

  #return(reactive(filtered_data())) ## if data are not modified, the unmodified input data must be returned
  return(
    reactive({
      dat.f()
    })
  )
}
