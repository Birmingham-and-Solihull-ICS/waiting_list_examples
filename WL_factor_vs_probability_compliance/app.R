library(shiny)
library(ggplot2)
library(plotly)
library(NHSRwaitinglist)
library(BSOLutils)
library(scales)

# Function to take compliance input and draw plot
factor_calc <- function(compliance_dec = 0.92) {

    dt2 <- data.frame(
        factor = seq(0.01, 6, 0.01),
        pexp_val = pexp(1, seq(0.01, 6, 0.01)),
        pexp_pct = 100 * pexp(1, seq(0.01, 6, 0.01))
    )

    ggplot(dt2, aes(x = factor, customdata = pexp_val * 100)) +
        geom_line(aes(y = pexp_val),
                  col = icb_theme_cols("cluster_green1"), size = 1) +
        geom_vline(xintercept = qexp(compliance_dec),
                   col = icb_theme_cols("cluster_orange"),
                   linetype = "dashed", size = 1) +
        geom_hline(yintercept = compliance_dec,
                   col = icb_theme_cols("cluster_orange"),
                   linetype = "dashed", size = 1) +
        scale_y_continuous(labels = percent, breaks = seq(0,1,0.1), expand = expansion(0,0.01)) +
        labs(
            title = paste0(
                "Factor for ", round(100 * compliance_dec), "% compliance to waiting list target"
            ),
            subtitle = "Exponential distribution",
            x = "Factor",
            y = "Probability of meeting target (%)"
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

  titlePanel("What `factor` do I need to use to calculate waiting list targets?"),
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
        "weekly_demand",
        "Weekly demand",
        value = 500,
        min = 1,
        max = 10000,
        step = 1
      )
    ),

    mainPanel(
      plotlyOutput("interactive_plot", height = "650px"),
      br(),
      htmlOutput("explanation_text")
    )
  )
)

server <- function(input, output, session) {

    compliance_decimal <- reactive({
        input$compliance_pct / 100
    })

    base_plot <- reactive({
        factor_calc(compliance_decimal())
    })

    factor_value <- reactive({
        qexp(compliance_decimal())
    })


    target_queue <- reactive({
      NHSRwaitinglist::calc_target_queue_size(demand = input$weekly_demand
                                              , target_wait = input$target_wait
                                              , factor = factor_value())
    })

    output$interactive_plot <- renderPlotly({
      ggplotly(base_plot(), tooltip = c("x", "customdata")) %>%
        style(
          hovertemplate = paste(
            "<b>Factor:</b> %{x:.3f}<br>",
            "<b>Probability of meeting target:</b> %{customdata:.1f}%<br>",
            "<extra></extra>"
          )
        ) %>%
        layout(
          hovermode = "closest",
          xaxis = list(
            showspikes = TRUE,
            spikemode = "across",
            spikesnap = "cursor",
            spikethickness = 1,
            spikecolor = BSOLutils::icb_theme_cols("cluster_lightblue"),
            spikedash = "dashed"
          ),
          yaxis = list(
            showspikes = TRUE,
            spikemode = "across",
            spikesnap = "cursor",
            spikethickness = 1,
            spikecolor = BSOLutils::icb_theme_cols("cluster_lightblue"),
            spikedash = "dashed"
          )
        )
    })


    output$explanation_text <- renderUI({

        HTML(paste0(
            "<p style='font-size:16px;'>",
            "<b>Selected compliance:</b> ", sprintf("%.0f%%", input$compliance_pct), "<br>",
            "<b>Corresponding factor:</b> ", sprintf("%.3f", factor_value()), "<br>",
            "<b>Target Queue size:</b> ", sprintf("%.3f", target_queue()),
            "</p>"
        ))
    })
}

shinyApp(ui, server)
