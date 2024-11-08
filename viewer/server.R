library(shiny)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(stringr)
source("viewer/pso_visualizer.R")

make_reactive_plot <- function(sim, orig, what, pso = NULL, thresh = NULL, conv = NULL, check_reg = NULL, check_age = NULL) {
    reactive({
        req(sim)
        req(orig)
        if (is.null(check_reg)) {
            plot_res <- plot_error_ts_fn_national(
                sim()[[what]],
                orig()
            )
        } else {
            req(pso)
            req(thresh)
            req(conv)
            req(check_reg)
            if (is.null(check_age)) {
                plot_res <- plot_error_ts_fn_regions(sim()[[what]],
                    orig(),
                    pso(),
                    selected_regions = check_reg()
                )
            } else {
                req(check_age)

                plot_res <- plot_error_ts_fn(sim()[[what]],
                    orig(),
                    pso(),
                    selected_regions = check_reg(),
                    selected_age_groups = check_age()
                )
            }
        }
        plot_res
    })
}

make_download_handler <- function(plot_reactive, title = "test") {
    return(downloadHandler(
        filename = function() {
            paste0("plot_", format(Sys.time(), "%Y%m%d_%H%M%S_"), title, ".pdf")
        },
        content = function(file) {
            ggsave(file, plot = plot_reactive(), device = "pdf", width = 20, height = 10, units = "cm", scale = 2.5)
        }
    ))
}

server_fnc <- function(input, output, session) {
    # Observe events
    observe({
        updateSelectInput(session, "file", choices = list_files())
    })

    observe({
        req(results_react())
        values <- exitvalue_results_fn(results_react())
        updateSliderInput(session, "threshold_converged", value = ceiling(values$max), min = floor(values$min), max = ceiling(values$max))
    })

    observeEvent(input$file, {
        req(pso_data_reactive())
        updateCheckboxGroupInput(session, "check_regions",
            choices = get_region_names_react()$listed,
            selected = get_region_names_react()$ids
        )
        updateCheckboxGroupInput(session, "check_age_groups",
            choices = get_age_group_names_react()$listed,
            selected = get_age_group_names_react()$ids
        )
        updateCheckboxGroupInput(session, "check_params",
            choices = get_params_names_react()$listed,
            selected = get_params_names_react()$ids
        )
        output$dynamicTitle <- renderText({
            paste(
                "File sim: ", input$file,
                "Epidemic model: ", epidemic_model_react(),
                "Fitness model: ", fitness_method_react(),
                "Season: ", season_data_react(),
                "Syntactic: ", syntehtic_data_react()
            )
        })
    })

    pso_data_reactive <- reactive({
        req(input$file)
        fromJSON(paste0("sim_results/", input$file))
    })

    get_region_names_react <- reactive({
        req(pso_data_reactive())
        get_region_names(pso_data_reactive())
    })

    get_age_group_names_react <- reactive({
        req(pso_data_reactive())
        get_age_group_names(pso_data_reactive())
    })

    get_params_names_react <- reactive({
        req(pso_data_reactive())
        get_params_names(pso_data_reactive())
    })

    epidemic_model_react <- reactive({
        req(pso_data_reactive())
        get_epidemic_model(pso_data_reactive())
    })

    fitness_method_react <- reactive({
        req(pso_data_reactive())
        get_fitness_method(pso_data_reactive())
    })

    season_data_react <- reactive({
        req(pso_data_reactive())
        get_season_data(pso_data_reactive())
    })

    syntehtic_data_react <- reactive({
        req(pso_data_reactive())
        get_syntehtic_data(pso_data_reactive())
    })

    sim_data_react <- reactive({
        req(pso_data_reactive())
        sim_data_fn(pso_data_reactive())
    })

    params_transformation_react <- reactive({
        req(pso_data_reactive())
        get_params_transformations_fn(pso_data_reactive())
    })

    abs_incidence_reg_age_react <- reactive({
        req(pso_data_reactive())
        abs_incidence_reg_age_fn(pso_data_reactive())
    })

    abs_incidence_reg_react <- reactive({
        req(pso_data_reactive())
        abs_incidence_reg_fn(pso_data_reactive())
    })

    incidence_reg_age_react <- reactive({
        req(pso_data_reactive())
        inc_reg_age_fn(pso_data_reactive())
    })

    incidence_reg_react <- reactive({
        req(pso_data_reactive())
        inc_reg_fn(pso_data_reactive())
    })

    incidence_nat_react <- reactive({
        req(pso_data_reactive())
        inc_nat_fn(pso_data_reactive())
    })

    results_react <- reactive({
        req(pso_data_reactive())
        results_fn(pso_data_reactive())
    })

    pop_reg_age_react <- reactive({
        req(pso_data_reactive())
        pop_reg_age_fn(pso_data_reactive())
    })

    pop_reg_react <- reactive({
        req(pso_data_reactive())
        pop_reg_fn(pso_data_reactive())
    })

    exitstatus_results_react <- reactive({
        req(results_react())
        exitstatus_results_fn(results_react())
    })

    converged_parameters_react <- reactive({
        req(results_react())
        converged_parameters_fn(results_react(),
            threshold = input$threshold_converged,
            only_converged = input$only_converged
        )
    })

    converged_parameters_melted_react <- reactive({
        req(converged_parameters_react())
        converged_parameters_melted_fn(converged_parameters_react())
    })

    summary_values_react <- reactive({
        req(converged_parameters_melted_react())
        summary_values_fn(converged_parameters_melted_react())
    })

    params_selected_react <- reactive({
        req(input$check_params)
        req(get_params_names_react())
        params_selected_fn(input$check_params, get_params_names_react()$antilisted)
    })

    df_long_react <- reactive({
        req(converged_parameters_react())
        req(pso_data_reactive())
        req(pop_reg_react())
        req(pop_reg_age_react())

        progress <- shiny::Progress$new()
        progress$set(message = "Computing timeseries", value = 0)
        on.exit(progress$close())
        update_progress <- function(value = NULL, detail = NULL, n = 10) {
            progress$inc(amount = 1 / n, detail = detail)
        }

        df_long_fn(
            converged_parameters_react(),
            pso_data_reactive(),
            pop_reg_react(),
            pop_reg_age_react(),
            update_progress
        )
    })

    # Output section
    output$output_debug_text <- renderPrint({
        print(params_transformation_react())
    })

    output$exitvalue_hist <- renderPlot({
        req(results_react())
        exitvalue_hist_fn(results_react())
    })

    output$hist_plot <- renderPlot({
        req(converged_parameters_melted_react())
        req(summary_values_react())
        graph <- hist_plot_fn(converged_parameters_melted_react(), summary_values_react(), params_selected_react())
        graph
    })

    output$table_all_par <- renderDataTable({
        req(summary_values_react())
        summary_values_react()
    })

    output$table_sim_data <- renderDataTable({
        req(sim_data_react())
        sim_data_react()
    })

    output$table_converged_res <- renderTable({
        req(exitstatus_results_react())
        exitstatus_results_react()
    })

    plot_react_inc_nat <- make_reactive_plot(
        reactive(df_long_react()),
        reactive(incidence_nat_react()),
        "inc_nat"
    )

    plot_react_inc_reg <- make_reactive_plot(
        reactive(df_long_react()),
        reactive(incidence_reg_react()),
        "inc_reg",
        reactive(pso_data_reactive()),
        reactive(input$threshold_converged),
        reactive(input$only_converged),
        reactive(input$check_regions)
    )
    plot_react_inc_reg_age <- make_reactive_plot(
        reactive(df_long_react()),
        reactive(incidence_reg_age_react()),
        "inc_reg_age",
        reactive(pso_data_reactive()),
        reactive(input$threshold_converged),
        reactive(input$only_converged),
        reactive(input$check_regions),
        reactive(input$check_age_groups)
    )
    plot_react_abs_inc_reg <- make_reactive_plot(
        reactive(df_long_react()),
        reactive(abs_incidence_reg_react()),
        "abs_inc_reg",
        reactive(pso_data_reactive()),
        reactive(input$threshold_converged),
        reactive(input$only_converged),
        reactive(input$check_regions)
    )
    plot_react_abs_inc_reg_age <- make_reactive_plot(
        reactive(df_long_react()),
        reactive(abs_incidence_reg_age_react()),
        "abs_inc_reg_age",
        reactive(pso_data_reactive()),
        reactive(input$threshold_converged),
        reactive(input$only_converged),
        reactive(input$check_regions),
        reactive(input$check_age_groups)
    )

    output$inc_nat <- renderPlot({
        plot_react_inc_nat()
    })
    output$inc_reg <- renderPlot({
        plot_react_inc_reg()
    })
    output$inc_reg_age <- renderPlot({
        plot_react_inc_reg_age()
    })

    output$abs_inc_reg <- renderPlot({
        plot_react_abs_inc_reg()
    })

    output$abs_inc_reg_age <- renderPlot({
        plot_react_abs_inc_reg_age()
    })

    output$downloadPlot_inc_reg <- make_download_handler(reactive(plot_react_inc_reg()), title = "inc_reg")
    output$downloadPlot_inc_reg_age <- make_download_handler(reactive(plot_react_inc_reg_age()), title = "inc_reg_age")
    output$downloadPlot_abs_inc_reg <- make_download_handler(reactive(plot_react_abs_inc_reg()), title = "abs_inc_reg")
    output$downloadPlot_abs_inc_reg_age <- make_download_handler(reactive(plot_react_abs_inc_reg_age()), title = "abs_inc_reg_age")
}
