library(shiny)
library(shinyjs)
library(bslib)
library(DT)

ui_interface <- fluidPage(
    title = "Fit Results",
    theme = bs_theme(preset = "bootstrap"),
    helpText(textOutput("dynamicTitle")),
    tabsetPanel(
        tabPanel(
            "Import data",
            fluidRow(
                column(
                    width = 4,
                    selectInput("file", "Choose a file:", choices = NULL),
                    actionButton("refresh", "Refresh", class = "btn-success")
                ),
                column(
                    width = 4,
                    checkboxGroupInput("check_regions",
                        label = "Regions",
                        choices = NULL,
                        selected = NULL
                    )
                ),
                column(
                    width = 4,
                    checkboxGroupInput("check_age_groups",
                        label = "Age groups",
                        choices = NULL,
                        selected = NULL
                    )
                )
            ), fluidRow(
                column(
                    width = 12,
                    tableOutput("output_debug")
                )
            ), fluidRow(
                column(
                    width = 12,
                    verbatimTextOutput("output_debug_text")
                )
            )
        ), tabPanel(
            "Incidence graphs",
            layout_sidebar(
                sidebar = sidebar(
                    checkboxInput("only_converged", "Only converged", value = FALSE),
                    sliderInput("threshold_converged", "Threshold for convergence", min = 0, max = 1000, value = 1000, step = 0.01),
                    downloadButton("downloadPlot_inc_reg", "Download Inc Reg"),
                    downloadButton("downloadPlot_inc_reg_age", "Download Inc Reg Age"),
                    downloadButton("downloadPlot_abs_inc_reg", "Download Abs Inc Reg"),
                    downloadButton("downloadPlot_abs_inc_reg_age", "Download Abs Inc Reg")
                ),
                navset_card_underline(
                    nav_panel("Incidence regions", plotOutput("inc_reg")),
                    nav_panel("Incidence national", plotOutput("inc_nat")),
                    nav_panel("Incidence regions age", plotOutput("inc_reg_age")),
                    nav_panel("Abs incidence regions", plotOutput("abs_inc_reg")),
                    nav_panel("Abs incidence regions age", plotOutput("abs_inc_reg_age")),
                    nav_panel("Data", layout_columns(
                        card(card_header("All parameters"), DT::dataTableOutput("table_all_par", height = "100%")),
                        card(card_header("Simulation hyperparam"), DT::dataTableOutput("table_sim_data")),
                        card(card_header("Simulation performance"), tableOutput("table_converged_res")),
                        card(card_header("Exitvalue distribution"), plotOutput("exitvalue_hist")),
                        col_widths = c(5, 4, 3, 12)
                    )),
                    full_screen = TRUE, height = "auto"
                ),
                width = 300, height = "1000px"
            )
        ), tabPanel(
            "Histograms", layout_sidebar(
                sidebar = sidebar(
                    checkboxGroupInput("check_params",
                        label = "Params",
                        choices = NULL,
                        selected = NULL,
                        inline = TRUE
                    )
                ),
                nav_panel("Parameters histogram", plotOutput("hist_plot", height = "400px"))
            )
        )
    )
)
