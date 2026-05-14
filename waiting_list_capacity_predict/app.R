library(shiny)
library(ggplot2)
library(plotly)
library(NHSRwaitinglist)
library(BSOLutils)
library(scales)


# Function to take compliance input and draw plot
plot_calc <- function(current_wl_size, weekly_demand, target_wait
                        , weeks_to_target, compliance) {
  #
  # q <- 1300 #current_wl_size
  # d <- 130 #weekly_demand
  # t <- 18 #target_wait
  # weeks <- 52 #weeks_to_target

  current_compliance <- 1 - exp(-(weekly_demand * target_wait) / current_wl_size)


  target_q <- calc_target_queue_size(weekly_demand,
                                     target_wait,
                                     qexp(compliance))

  rel <- calc_relief_capacity(weekly_demand,
                              current_wl_size,
                              target_q,
                              weeks_to_target - 1)

  dt <-
    data.frame(
      weeks = seq(weeks_to_target),
      demand = weekly_demand,
      capacity = rel,
      queue = current_wl_size
    )


  for (i in 2:nrow(dt)) {
    dt[i,]$queue <-  dt[i - 1,]$queue + dt[i,]$demand - dt[i,]$capacity
  }

  #weeks <- 52 / 12

  ggplot(dt, aes(x = weeks, y = queue)) +
    geom_line(colour = icb_theme_cols("cluster_lightblue"), size = 1) +
    geom_hline(yintercept = dt[1,]$queue, colour = icb_theme_cols("cluster_orange"),
               linetype = "dashed", linewidth = 1) +
    geom_hline(yintercept = dt[1,]$queue, colour = icb_theme_cols("cluster_orange"),
               linetype = "dashed", linewidth = 1) +
    geom_hline(yintercept = target_q, colour = icb_theme_cols("cluster_green1"),
               linetype = "dashed", linewidth = 1) +
    scale_y_continuous(labels = comma) +
    labs(
      title = paste0(
        "Trajectory to reach ", percent(compliance), "compliance to waiting list target in ", weeks_to_target, " weeks."
      ),
      subtitle = "Exponential distribution use for queue, assuming 'perfect world'",
      x = "Time in Weeks",
      y = "Queue Size"
    ) +
    theme_icb()
}

ui <- fluidPage(

    # --- Page margins, input width, AND wider default page size ---
    tags$head(
        tags$style(HTML("
      /* widen the default Shiny container */
      .container, .container-fluid {
        max-width: 2000px !important;   /* adjust as needed */
        width: 100% !important;
      }

      body {
        margin: 10px 40px 10px 10px !important;   /* left small, right larger */
      }
      #compliance_pct {
        width: 90px !important;                   /* smaller numericInput box */
      }
      .plot-container .plotly {
        height: 650px !important;                 /* make the graph bigger */
      }
    "))
    ),

    titlePanel("Simple waiting list prediction, based on Queuing Theory"),
    br(), br(),

    sidebarLayout(
        sidebarPanel(
            width = 3,
            numericInput(
                "compliance_pct",
                "Compliance with target (%)",
                value = 92,
                min = 1,
                max = 99,
                step = 1
            ),
            numericInput(
                "target_wait",
                "Target Waiting Time",
                value = 18,
                min = 1,
                max = 120,
                step = 1
            ),
            numericInput(
                "current_wl_size",
                "Current waiting list size",
                value = 1300,
                min = 1,
                max = 50000,
                step = 1
            ),
            numericInput(
                "weekly_demand",
                "Weekly demand",
                value = 130,
                min = 1,
                max = 10000,
                step = 1
            ),
            numericInput(
                "weeks_to_target",
                "Number of weeks to meet target",
                value = 52,
                min = 1,
                max = 10000,
                step = 1
            ),
            actionButton("refresh_plot", "Refresh plot")
        ),

        mainPanel(
            plotlyOutput("interactive_plot", height = "650px"),
            br(),
            htmlOutput("explanation_text")
        )
    )
)

server <- function(input, output, session) {


  eventReactive(input$refresh_plot, {
    ...
  }, ignoreNULL = TRUE)


    compliance_decimal <- reactive({
        input$compliance_pct / 100
    })

    current_compliance_pct <- eventReactive(input$refresh_plot, {
       100 * (1 - exp(-(input$weekly_demand * input$target_wait) /
                 input$current_wl_size))})

    base_plot <- eventReactive(input$refresh_plot, {
      plot_calc(
        input$current_wl_size,
        input$weekly_demand,
        input$target_wait,
        input$weeks_to_target,
        compliance_decimal()
      )
    })


    factor_value <- eventReactive(input$refresh_plot, {
      qexp(0.92)
    })

    current_queue <- eventReactive(input$refresh_plot, {input$current_wl_size})

    target_queue <- eventReactive(input$refresh_plot, {
      NHSRwaitinglist::calc_target_queue_size(
        demand = input$weekly_demand,
        target_wait = input$target_wait,
        factor = factor_value()
      )
    })

    current_compliance <- eventReactive(input$refresh_plot, {
      1 - exp(-(input$weekly_demand * input$target_wait) / input$current_wl_size)
    })

    relief_capacity <- eventReactive(input$refresh_plot, {
      NHSRwaitinglist::calc_relief_capacity(
        demand = input$weekly_demand,
        target_queue_size = target_queue(),
        time_to_target = input$weeks_to_target,
        queue_size = input$current_wl_size
      )
    })

    output$interactive_plot <- renderPlotly({
      ggplotly(base_plot()) %>%
        layout(
          annotations = list(
            list(
              x = 1,
              y = target_queue(),
              text = paste0("Target queue size = ", round(target_queue())),
              showarrow = FALSE,
              xanchor = "left",
              yanchor = "bottom",
              font = list(color = BSOLutils::icb_theme_cols("cluster_green1"))
            ),
            list(
              x = input$weeks_to_target,
              y = input$current_wl_size,
              text = paste0("Current queue size = ", round(input$current_wl_size)),
              showarrow = FALSE,
              xanchor = "right",
              yanchor = "top",
              font = list(color = BSOLutils::icb_theme_cols("cluster_orange"))
            )
          )
        )
    })

    output$explanation_text <- renderUI({
      HTML(paste0(
        "<p style='font-size:16px;'>",
        "<b>Current waiting list performance:</b> ", sprintf("%.0f%%", current_compliance_pct()), "<br>",
        "<b>Target Queue size:</b> ", sprintf("%.0f", target_queue()), "<br>",
        "<b>Weekly \"relief\" capacity to hit waiting list target:</b> ", sprintf("%.0f", relief_capacity()), "<br>",
        "</p>"
      ))
    })

}

shinyApp(ui, server)
