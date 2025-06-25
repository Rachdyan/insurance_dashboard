
###### COMPARE FIRM ---------

# UI: COMPARE GROUP ------
selected_group_firm_data <- reactive({
    group_firm_db %>% filter(groupname %in% input$comparison_select_firm_group)
})


observeEvent(selected_group_firm_data(), {
    selectedCols <- input$comparison_select_firm_group
    updateVirtualSelect("select_firm_compare", choices = prepare_choices(selected_group_firm_data(), shortname, shortname, group_by = groupname), selected = selectedCols)
})

selected_firm_data <- reactive({
    selected_group_firm_data() %>% filter(shortname %in% input$select_firm_compare)
})

observeEvent(selected_firm_data(), {
    selectedCols <- input$select_firm_compare
    updateVirtualSelect("select_firm_compare", choices = prepare_choices(selected_group_firm_data(), shortname, shortname, group_by = groupname), selected = selectedCols)
})

output$selected_firm_box_output <- renderUI({
    req(input$select_firm_compare)
    lapply(input$select_firm_compare, function(x){
        box(x, width = "12")

    })
})

observeEvent(input$comparison_generate_firm_stat_btn, {
    runjs("$('#firm_stat_modal').modal('hide')")

})


comparison_firm_plots_status <- reactiveValues(
    premium_written_status = NULL,
    yoy_change_premium_written_status = NULL,
    premium_earned_status = NULL,
    total_losses_status = NULL,
    loss_ratio_status = NULL,
    num_of_policies_status = NULL,
    avg_premium_status = NULL,
    direct_defense_status = NULL,
    case_reserves_status = NULL,
    total_claims_closed_with_payment_status = NULL,
    total_claims_closed_without_payment_status = NULL,
    first_party_claims_closed_without_payment_status = NULL,
    third_party_claims_closed_without_payment_status = NULL,
    first_party_open_claims_status = NULL,
    third_party_open_claims_status = NULL,
)

observeEvent(input$comparison_generate_firm_stat_btn, {
    if("Written Premium" %in% input$comparison_select_firm_statistic){
        comparison_firm_plots_status$premium_written_status <- TRUE
    } else {
        comparison_firm_plots_status$premium_written_status <- NULL
    }

    if("% Change in Written Premiums" %in% input$comparison_select_firm_statistic){
        comparison_firm_plots_status$yoy_change_premium_written_status <- TRUE
    } else {
        comparison_firm_plots_status$yoy_change_premium_written_status <- NULL
    }

    if("Earned Premium" %in% input$comparison_select_firm_statistic){
        comparison_firm_plots_status$premium_earned_status <- TRUE
    } else {
        comparison_firm_plots_status$premium_earned_status <- NULL
    }

    if("Total Losses" %in% input$comparison_select_firm_statistic){
        comparison_firm_plots_status$total_losses_status <- TRUE
    } else {
        comparison_firm_plots_status$total_losses_status <- NULL
    }

    if("Loss Ratio" %in% input$comparison_select_firm_statistic){
        comparison_firm_plots_status$loss_ratio_status <- TRUE
    } else {
        comparison_firm_plots_status$loss_ratio_status <- NULL
    }

    if("Number of Policies" %in% input$comparison_select_firm_statistic){
        comparison_firm_plots_status$num_of_policies_status <- TRUE
    } else {
        comparison_firm_plots_status$num_of_policies_status <- NULL
    }

    if("Average Premium" %in% input$comparison_select_firm_statistic){
        comparison_firm_plots_status$avg_premium_status <- TRUE
    } else {
        comparison_firm_plots_status$avg_premium_status <- NULL
    }

    if("Direct Defense Cost Containment Paid" %in% input$comparison_select_firm_statistic){
        comparison_firm_plots_status$direct_defense_status <- TRUE
    } else {
        comparison_firm_plots_status$direct_defense_status <- NULL
    }

    if("Case Reserves" %in% input$comparison_select_firm_statistic){
        comparison_firm_plots_status$case_reserves_status <- TRUE
    } else {
        comparison_firm_plots_status$case_reserves_status <- NULL
    }

    if("Total Claims Closed with Payment" %in% input$comparison_select_firm_statistic){
        comparison_firm_plots_status$total_claims_closed_with_payment_status <- TRUE
    } else {
        comparison_firm_plots_status$total_claims_closed_with_payment_status <- NULL
    }

    if("Total Claims Closed without Payment" %in% input$comparison_select_firm_statistic){
        comparison_firm_plots_status$total_claims_closed_without_payment_status <- TRUE
    } else {
        comparison_firm_plots_status$total_claims_closed_without_payment_status <- NULL
    }

    if("1st Party Claims Closed Without Payment" %in% input$comparison_select_firm_statistic){
        comparison_firm_plots_status$first_party_claims_closed_without_payment_status <- TRUE
    } else {
        comparison_firm_plots_status$first_party_claims_closed_without_payment_status <- NULL
    }

    if("3rd Party Claims Closed Without Payment" %in% input$comparison_select_firm_statistic){
        comparison_firm_plots_status$third_party_claims_closed_without_payment_status <- TRUE
    } else {
        comparison_firm_plots_status$third_party_claims_closed_without_payment_status <- NULL
    }

    if("1st Party Open Claims" %in% input$comparison_select_firm_statistic){
        comparison_firm_plots_status$first_party_open_claims_status <- TRUE
    } else {
        comparison_firm_plots_status$first_party_open_claims_status <- NULL
    }

    if("3rd Party Open Claims" %in% input$comparison_select_firm_statistic){
        comparison_firm_plots_status$third_party_open_claims_status <- TRUE
    } else {
        comparison_firm_plots_status$third_party_open_claims_status <- NULL
    }
})

# EXPORT
output$comparison_export_firm_ui <- renderUI({
    hidden(span(id = "comparison_firm_graph_head",
        downloadBttn(
            outputId = "export_comparison_firm_excel",
            "Export to Excel",
            style = 'simple',
            size = 'sm',
            icon = icon("file-excel")),
        downloadBttn(
            outputId = "export_comparison_firm_pdf",
            "Export to PDF",
            style = 'simple',
            size = 'sm',
            icon = icon("file-pdf"))
    )
    )
}
)

## YEAR SLIDER
output$comparison_year_range_firm_ui <- renderUI({
    hidden(
        span(id = 'firmyearrange',
            sliderInput(
                inputId = "firm_year_range",
                label = "",
                min = 2018,
                max = 2022,
                value = c(2018, 2022),
                step = 1,
                width = '30%',
                sep = ""
            )
        )
    )
}
)

observeEvent(input$comparison_generate_firm_stat_btn, {
    show('comparison_firm_graph_head')
    show('firmyearrange')
})

## DATA ------
comparison_raw_firm_data <- eventReactive(c(input$comparison_generate_firm_stat_btn, input$firm_year_range), {
    req(input$select_firm_compare, input$comparison_select_firm_statistic)
    data <- all_data %>%
        filter(shortname %in% input$select_firm_compare) %>%
        group_by(shortname, identifier) %>%
        complete(year = full_seq(2018:2022, 1)) %>%
        mutate(value = replace_na(value, 0)) %>%
        ungroup() %>%
        mutate(shortname = as.factor(shortname))
})

#
# firm_data_year_range <- reactive({
#     req(input$select_firm_compare)
#     comparison_raw_firm_data()$year
# })


comparison_firm_data_filtered <- eventReactive(c(input$comparison_generate_firm_stat_btn, input$firm_year_range), {
    req(input$select_firm_compare, input$comparison_select_firm_statistic)
    data <- comparison_raw_firm_data() %>%
        filter(year >= input$firm_year_range[1] & year <= input$firm_year_range[2])

})

comparison_firm_data_filtered_summarized <- eventReactive(c(input$comparison_generate_firm_stat_btn, input$firm_year_range), {
    req(c(input$select_firm_compare, input$comparison_select_firm_statistic))
    comparison_firm_data_filtered() %>%
        group_by(shortname, identifier, year) %>%
        summarize(value = sum(value)) %>%
        ungroup()

})


comparison_firm_color_palette <- eventReactive(c(input$comparison_generate_firm_stat_btn, input$firm_year_range), {
    req(input$select_firm_compare)
    palette <- setNames(
        cividis(length(levels(comparison_firm_data_filtered()$shortname)), alpha = 1, begin = 0, end = 1, direction = 1),
        levels(comparison_firm_data_filtered()$shortname))
})



##  WRITTEN PREMIUM PLOT ----------

output$comparison_premium_written_firm_ui <- renderUI({
    if (is.null(comparison_firm_plots_status$premium_written_status)) return()
    fluidRow(
        br(),
        div(class = "plot-title", h4(style = 'display: inline-block;', "Premiums Written")),
        br(),
        plotlyOutput("comparison_written_premium_firm_plot", width = '85%', height = "475px") %>% withSpinner(),
        align = "center"
    )
})

comparison_firm_written_premium_data <-  eventReactive(c(input$comparison_generate_firm_stat_btn, input$firm_year_range), {
    req(c(input$select_firm_compare, input$comparison_select_firm_statistic))
    data <- comparison_firm_data_filtered_summarized() %>%
        filter(identifier == "01")
})

comparison_firm_written_premium_export_table <- reactive({
    if(nrow(comparison_firm_written_premium_data()) == 0){
        zeroGrob()
    } else {
        comparison_firm_written_premium_data() %>%
            group_by(shortname) %>%
            mutate(col_names = paste0("Written Premiums ", seq(min(year), max(year), 1))) %>%
            ungroup() %>%
            select(shortname, value, col_names) %>%
            mutate(value = ifelse(!is.na(value), glue("${comma(value)}"), "")) %>%
            rename(`Firm Name` = shortname) %>%
            pivot_wider(names_from = col_names, values_from = value) %>%
            table_to_grob()
    }
})

comparison_firm_written_premium_export_table_excel <- reactive({
    if(nrow(comparison_firm_written_premium_data()) == 0){
        tibble()
    } else {
        comparison_firm_written_premium_data() %>%
            select(shortname, value, year) %>%
            rename(`Firm Name` = shortname) %>%
            pivot_wider(names_from = year, values_from = value)
    }
})

comparison_firm_written_premium_ggplot <- eventReactive(c(input$comparison_generate_firm_stat_btn, input$firm_year_range), {
    if(nrow(comparison_firm_written_premium_data()) == 0){
        return(ggplot() + theme_void())
    } else{
    ggplot(comparison_firm_written_premium_data(), aes(x = as.factor(year), y = value, fill = shortname, text = glue("<span><b>Value:</b> ${comma(value)}"))) +
        geom_bar(stat="identity", position="dodge") +
        scale_fill_manual(values = comparison_firm_color_palette()) +
        scale_y_continuous(limits = c(0, max(comparison_firm_written_premium_data()$value) * 1.3), labels = label_number(prefix = "$", scale_cut = cut_short_scale()), expand = c(0.001,0)) +
        ylab("Written Premium (LI 01)") +
        universal_graph_theme_with_legend(0.125, 0.9)
    }
})

comparison_firm_written_premium_ggplot_export <- reactive({
    comparison_firm_written_premium_ggplot() +
        ggtitle("Written Premiums (Quantified)") +
        single_graph_export_theme
})


output$comparison_written_premium_firm_plot <- renderPlotly({
    plotly <- ggplotly(comparison_firm_written_premium_ggplot(), tooltip = "text", dynamicTicks = TRUE) %>%
        layout(legend = list(x = 0.01, y = 0.99, title = list(text=''), bgcolor = 'rgba(255,255,255,0.2)', font = list(size = 9)))  %>%
        layout(showlegend = TRUE,
            yaxis = list(showline= T, linewidth=1.5, linecolor='black',
                showticklabels = T,  tickprefix = "   $"),
            xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2))

    plotly$x$layout$yaxis$autorange <- FALSE
    plotly$x$layout$yaxis$range <- c(0, max(comparison_firm_written_premium_data()$value) * 1.3)
    plotly

})


##  YOY CHANGE WRITTEN PREMIUM PLOT ----------
output$comparison_yoy_change_premium_written_firm_ui <- renderUI({
    if (is.null(comparison_firm_plots_status$yoy_change_premium_written_status)) return()
    fluidRow(
        br(),
        div(class = "plot-title", h4(style = 'display: inline-block;', "% Change in Written Premiums")),
        br(),
        plotlyOutput("comparison_firm_yoy_change_written_premiums_plot", width = '85%', height = "475px") %>% withSpinner(),
        align = "center"
    )
})

comparison_firm_yoy_change_written_premiums_data <- eventReactive(c(input$comparison_generate_firm_stat_btn, input$firm_year_range), {
    req(c(input$comparison_select_firm, input$comparison_select_firm_statistic))
    comparison_firm_data_filtered_summarized() %>%
        filter(identifier == "01") %>%
        group_by(shortname) %>%
        mutate(value = na_if(value, 0),
            yoychange = (value/lag(value) - 1) * 100) %>%
        ungroup()
})

comparison_firm_yoy_change_written_premium_export_table <- reactive({
    if(nrow(comparison_firm_yoy_change_written_premiums_data()) == 0){
        zeroGrob()
    } else {
        comparison_firm_yoy_change_written_premiums_data() %>%
            group_by(shortname) %>%
            mutate(col_names = paste0("YOY % Change ", seq(min(year), max(year), 1))) %>%
            ungroup() %>%
            select(shortname, yoychange, col_names) %>%
            mutate(yoychange = ifelse(!is.na(yoychange), glue("{round(yoychange, 2)}%"), "")) %>%
            rename(`Firm Name` = shortname) %>%
            pivot_wider(names_from = col_names, values_from = yoychange) %>%
            table_to_grob()
    }
})

comparison_firm_yoy_change_written_premium_export_table_excel <- reactive({
    if(nrow(comparison_firm_yoy_change_written_premiums_data()) == 0){
        tibble()
    } else {
        comparison_firm_yoy_change_written_premiums_data() %>%
            select(shortname, yoychange, year) %>%
            rename(`Firm Name` = shortname) %>%
            pivot_wider(names_from = year, values_from = yoychange)
    }
})

comparison_firm_yoy_change_written_premiums_ggplot <- eventReactive(c(input$comparison_generate_firm_stat_btn, input$firm_year_range), {
    if(nrow(comparison_firm_yoy_change_written_premiums_data()) == 0){
        return(ggplot() + theme_void())
    } else{
        ggplot(comparison_firm_yoy_change_written_premiums_data(), aes(x = as.factor(year), y = yoychange, group = shortname, color = shortname, text = glue("<span><b>Value:</b> {round(yoychange, 2)}%"))) +
            geom_line(linewidth = 1.5) +
            scale_color_manual(values = comparison_firm_color_palette()) +
            scale_fill_manual(values = comparison_firm_color_palette()) +
            geom_point(aes(fill = shortname), shape=21, size=4, color = 'black') +
            scale_y_continuous(labels = label_number(suffix = "%", prefix = "")) +
            ylab("Percent") +
            universal_graph_theme_with_legend(0.125, 0.9)
    }
})

comparison_firm_yoy_change_written_premiums_ggplot_export <- reactive({
    comparison_firm_yoy_change_written_premiums_ggplot() +
        ggtitle("% Change in Written Premiums") +
        single_graph_export_theme
})



output$comparison_firm_yoy_change_written_premiums_plot <- renderPlotly({
    plotly <- ggplotly(comparison_firm_yoy_change_written_premiums_ggplot(), tooltip = "text", dynamicTicks = TRUE) %>%
        layout(legend = list(x = 0.01, y = 0.99, title = list(text=''), bgcolor = 'rgba(255,255,255,0.2)', font = list(size = 9), dynamicTicks = TRUE))  %>%
        layout(showlegend = TRUE,
            yaxis = list(showline= T, linewidth=1.5, linecolor='black',
                showticklabels = T,  tickprefix = "  ", ticksuffix = '%'),
            xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2)) %>%
        fix_legend_plotly()
})


## EARNED PREMIUM PLOT --------
output$comparison_premium_earned_firm_ui <- renderUI({
    if (is.null(comparison_firm_plots_status$premium_earned_status)) return()
    fluidRow(
        br(),
        div(class = "plot-title", h4(style = 'display: inline-block;', "Premiums Earned")),
        br(),
        plotlyOutput("comparison_earned_premium_firm_plot", width = '85%', height = "475px") %>% withSpinner(),
        align = "center"
    )
})

comparison_firm_earned_premium_data <- eventReactive(c(input$comparison_generate_firm_stat_btn, input$firm_year_range), {
    req(c(input$select_firm_compare, input$comparison_select_firm_statistic))
    comparison_firm_data_filtered_summarized() %>%
        filter(identifier == "02")
})

comparison_firm_earned_premium_export_table <- reactive({
    if(nrow(comparison_firm_earned_premium_data()) == 0){
        zeroGrob()
    } else {
        comparison_firm_earned_premium_data() %>%
            group_by(shortname) %>%
            mutate(col_names = paste0("Earned Premiums ", seq(min(year), max(year), 1))) %>%
            ungroup() %>%
            select(shortname, value, col_names) %>%
            mutate(value = ifelse(!is.na(value), glue("${comma(value)}"), "")) %>%
            rename(`Firm Name` = shortname) %>%
            pivot_wider(names_from = col_names, values_from = value) %>%
            table_to_grob()
    }
})

comparison_firm_earned_premium_export_table_excel <- reactive({
    if(nrow(comparison_firm_earned_premium_data()) == 0){
        tibble()
    } else {
        comparison_firm_earned_premium_data() %>%
            select(shortname, value, year) %>%
            rename(`Firm Name` = shortname) %>%
            pivot_wider(names_from = year, values_from = value)
    }
})


comparison_firm_earned_premium_ggplot <- eventReactive(c(input$comparison_generate_firm_stat_btn, input$firm_year_range), {
    if(nrow(comparison_firm_earned_premium_data()) == 0){
        return(ggplot() + theme_void())
    } else{
        ggplot(comparison_firm_earned_premium_data(), aes(x = as.factor(year), y = value, fill = shortname, text = glue("<span><b>Value:</b> ${comma(value)}"))) +
            geom_bar(stat="identity", position="dodge") +
            scale_fill_manual(values = comparison_firm_color_palette()) +
            scale_y_continuous(limits = c(0, max(comparison_firm_earned_premium_data()$value)* 1.3), labels = label_number(prefix = "$", scale_cut = cut_short_scale()), expand = c(0.001,0)) +
            ylab("Earned Premium (LI 02)") +
            universal_graph_theme_with_legend(0.125, 0.9)
    }
})

comparison_firm_earned_premium_ggplot_export <- reactive({
    comparison_firm_earned_premium_ggplot() +
        ggtitle("Earned Premiums (Quantified)") +
        single_graph_export_theme
})

output$comparison_earned_premium_firm_plot <- renderPlotly({
    plotly <- ggplotly(comparison_firm_earned_premium_ggplot(), tooltip = "text", dynamicTicks = TRUE) %>%
        layout(legend = list(x = 0.01, y = 0.99, title = list(text=''), bgcolor = 'rgba(255,255,255,0.2)', font = list(size = 9), dynamicTicks = TRUE))  %>%
        layout(showlegend = TRUE,
            yaxis = list(showline= T, linewidth=1.5, linecolor='black',
                showticklabels = T,  tickprefix = "  $"),
            xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2))

    plotly$x$layout$yaxis$autorange <- FALSE
    plotly$x$layout$yaxis$range <- c(0, max(comparison_firm_earned_premium_data()$value) * 1.3)
    plotly
})





## TOTAL LOSSES PLOT -------------
output$comparison_total_losses_firm_ui <- renderUI({
    if (is.null(comparison_firm_plots_status$total_losses_status)) return()
    fluidRow(
        br(),
        div(class = "plot-title", h4(style = 'display: inline-block;', "Total Losses")),
        br(),
        plotlyOutput("comparison_total_losses_firm_plot", width = '85%', height = "475px") %>% withSpinner(),
        align = "center"
    )
})

comparison_firm_total_losses_data <- eventReactive(c(input$comparison_generate_firm_stat_btn, input$firm_year_range), {
    req(c(input$select_firm_compare, input$comparison_select_firm_statistic))
    comparison_firm_data_filtered_summarized() %>%
        filter(identifier == "05")
})

comparison_firm_total_losses_export_table <- reactive({
    if(nrow(comparison_firm_total_losses_data()) == 0){
        zeroGrob()
    } else{
        comparison_firm_total_losses_data() %>%
            group_by(shortname) %>%
            mutate(col_names = paste0("Losses Paid ", seq(min(year), max(year), 1))) %>%
            ungroup() %>%
            select(shortname, value, col_names) %>%
            mutate(value = ifelse(!is.na(value), glue("${comma(value)}"), "")) %>%
            rename(`Firm Name` = shortname) %>%
            pivot_wider(names_from = col_names, values_from = value) %>%
            table_to_grob()
    }
})


comparison_firm_total_losses_export_table_excel <- reactive({
    if(nrow(comparison_firm_total_losses_data()) == 0){
        tibble()
    } else {
        comparison_firm_total_losses_data() %>%
            select(shortname, value, year) %>%
            rename(`Firm Name` = shortname) %>%
            pivot_wider(names_from = year, values_from = value)
    }
})

comparison_firm_total_losses_ggplot <- eventReactive(c(input$comparison_generate_firm_stat_btn, input$firm_year_range), {
    if(nrow(comparison_firm_total_losses_data()) == 0){
        return(ggplot() + theme_void())
    } else{
    ggplot(comparison_firm_total_losses_data(), aes(x = as.factor(year), y = value, fill = shortname, text = glue("<span><b>Value:</b> ${comma(value)}"))) +
        geom_bar(stat="identity", position="dodge") +
        scale_fill_manual(values = comparison_firm_color_palette()) +
        scale_y_continuous(limits = c(0, max(comparison_firm_total_losses_data()$value) * 1.3), labels = label_number(prefix = "$", scale_cut = cut_short_scale()), expand = c(0.001,0)) +
        ylab("Losses Paid (LI 05)") +
        universal_graph_theme_with_legend(0.125, 0.9)
    }
})

comparison_firm_total_losses_ggplot_export <- reactive({
    comparison_firm_total_losses_ggplot() +
        ggtitle("Direct Losses Paid") +
        single_graph_export_theme
})


output$comparison_total_losses_firm_plot <- renderPlotly({
    if(length(comparison_firm_total_losses_ggplot()$data) == 0){
        ggplotly(comparison_group_total_losses_ggplot())
    } else{
        plotly <- ggplotly(comparison_firm_total_losses_ggplot(), tooltip = "text", dynamicTicks = TRUE) %>%
            layout(legend = list(x = 0.01, y = 0.99, title = list(text=''), bgcolor = 'rgba(255,255,255,0.2)', font = list(size = 9)),
                yaxis = list(showline= T, linewidth=1.5, linecolor='black', showticklabels = T,  tickprefix = "  $"),
                xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2))

        plotly$x$layout$yaxis$autorange <- FALSE
        plotly$x$layout$yaxis$range <- c(0, max(comparison_firm_total_losses_data()$value) * 1.3)
        plotly
    }
})

## LOSS RATIO PLOT -------------

output$comparison_loss_ratio_firm_ui <- renderUI({
    if (is.null(comparison_firm_plots_status$loss_ratio_status)) return()
    fluidRow(
        br(),
        div(class = "plot-title", h4(style = 'display: inline-block;', "Loss Ratio")),
        br(),
        plotlyOutput("comparison_loss_ratio_firm_plot", width = '85%', height = "475px") %>% withSpinner(),
        align = "center"
    )
})

comparison_firm_loss_ratio_data <- eventReactive(c(input$comparison_generate_firm_stat_btn, input$firm_year_range), {
    summarized_data <-  comparison_firm_data_filtered_summarized()

    premium_affiliatedfminsco = summarized_data %>% filter(identifier == "01")
    losses_paid_affiliatedfminsco = summarized_data %>% filter(identifier == "05")

    left_join(losses_paid_affiliatedfminsco, premium_affiliatedfminsco,
        by = c("shortname", "year")) %>%
        select(shortname, year, value.x, value.y) %>%
        group_by(shortname, year) %>%
        summarize(value.x = sum(value.x), value.y = sum(value.y)) %>%
        mutate(value.x = na_if(value.x, 0), value.y = na_if(value.y, 0)) %>%
        mutate(lossratio = value.x / value.y)
})


comparison_firm_loss_ratio_export_table <- reactive({
    if(nrow(comparison_firm_loss_ratio_data()) == 0){
        zeroGrob()
    } else{
        comparison_firm_loss_ratio_data() %>%
            group_by(shortname) %>%
            mutate(col_names = paste0("Loss Ratio ", seq(min(year), max(year), 1))) %>%
            ungroup() %>%
            select(shortname, lossratio, col_names) %>%
            mutate(lossratio = ifelse(!is.na(lossratio), glue("{round(lossratio, 4)}"), "")) %>%
            rename(`Firm Name` = shortname) %>%
            pivot_wider(names_from = col_names, values_from = lossratio) %>%
            table_to_grob()
    }
})


comparison_firm_loss_ratio_export_table_excel <- reactive({
    if(nrow(comparison_firm_loss_ratio_data()) == 0){
        tibble()
    } else {
        comparison_firm_loss_ratio_data() %>%
            select(shortname, lossratio, year) %>%
            rename(`Firm Name` = shortname) %>%
            pivot_wider(names_from = year, values_from = lossratio)
    }
})


comparison_firm_loss_ratio_ggplot <- eventReactive(c(input$comparison_generate_firm_stat_btn, input$firm_year_range), {
    if(identical(comparison_firm_loss_ratio_data(), numeric(0)) || nrow(comparison_firm_loss_ratio_data()) == 0 || all(is.na(comparison_firm_loss_ratio_data()$lossratio))){
        return(ggplot() + theme_void())
    } else {
        ggplot(comparison_firm_loss_ratio_data(), aes(x = as.factor(year), y = lossratio, group = shortname, color = shortname, text = glue("<span><b>Loss Ratio:</b> {round(lossratio, 4)}"))) +
            geom_line(linewidth = 1.5) +
            scale_color_manual(values = comparison_firm_color_palette()) +
            scale_fill_manual(values = comparison_firm_color_palette()) +
            geom_point(aes(fill = shortname), shape=21, size=4, color = 'black') +
            scale_y_continuous(limits = c(0, max(comparison_firm_loss_ratio_data()$lossratio, na.rm = T) * 1.2), labels = comma) +
            ylab("Loss Ratio") +
            universal_graph_theme_with_legend(0.125, 0.9)
    }
})

comparison_firm_loss_ratio_ggplot_export <- reactive({
    comparison_firm_loss_ratio_ggplot() +
        ggtitle("Loss Ratio") +
        single_graph_export_theme
})

output$comparison_loss_ratio_firm_plot <- renderPlotly({
    if(length(comparison_firm_loss_ratio_ggplot()$data) == 0){
        ggplotly(comparison_firm_loss_ratio_ggplot())
    } else{
        ggplotly(comparison_firm_loss_ratio_ggplot(), tooltip = "text", dynamicTicks = TRUE) %>%
            layout(legend = list(x = 0.01, y = 0.99, title = list(text=''), bgcolor = 'rgba(255,255,255,0.2)', font = list(size = 9)),
                yaxis = list(showline= T, linewidth=1.5, linecolor='black', showticklabels = T, tickprefix = "  "),
                xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2)) %>%
            fix_legend_plotly()
    }
})

## NUM OF POLICIES PLOT -------------
output$comparison_num_of_policies_firm_ui <- renderUI({
    if (is.null(comparison_firm_plots_status$num_of_policies_status)) return()
    fluidRow(
        br(),
        div(class = "plot-title", h4(style = 'display: inline-block;', "Number of Policies")),
        br(),
        plotlyOutput("comparison_num_of_policies_firm_plot", width = '85%', height = "475px") %>% withSpinner(),
        align = "center"
    )
})

comparison_firm_num_of_policies_data <- eventReactive(c(input$comparison_generate_firm_stat_btn, input$firm_year_range), {
    comparison_firm_data_filtered_summarized() %>%
        filter(identifier == "11")
})

comparison_firm_num_of_policies_export_table <- reactive({
    if(nrow(comparison_firm_num_of_policies_data()) == 0){
        zeroGrob()
    } else {
        comparison_firm_num_of_policies_data() %>%
            group_by(shortname) %>%
            mutate(col_names = paste0("(n) Policies ", seq(min(year), max(year), 1))) %>%
            ungroup() %>%
            select(shortname, value, col_names) %>%
            mutate(value = ifelse(!is.na(value), glue("{value}"), "")) %>%
            rename(`Firm Name` = shortname) %>%
            pivot_wider(names_from = col_names, values_from = value) %>%
            table_to_grob()
    }
})


comparison_firm_num_of_policies_export_table_excel <- reactive({
    if(nrow(comparison_firm_num_of_policies_data()) == 0){
        tibble()
    } else {
        comparison_firm_num_of_policies_data() %>%
            select(shortname, value, year) %>%
            rename(`Firm Name` = shortname) %>%
            pivot_wider(names_from = year, values_from = value)
    }
})

comparison_firm_num_of_policies_ggplot <- eventReactive(c(input$comparison_generate_firm_stat_btn, input$firm_year_range), {
    if(nrow(comparison_firm_num_of_policies_data()) == 0){
        return(ggplot() + theme_void())
    } else{
        ggplot(comparison_firm_num_of_policies_data(), aes(x = as.factor(year), y = value, fill = shortname, text = glue("<span><b>Value:</b> {comma(value)}"))) +
            geom_bar(stat="identity", position="dodge") +
            scale_fill_manual(values = comparison_firm_color_palette()) +
            scale_y_continuous(limits = c(0, max(comparison_firm_num_of_policies_data()$value) *1.3 ), labels = label_number(prefix = " ", scale_cut = cut_short_scale()), expand = c(0.001,0)) +
            ylab("Total Policies in Force") +
            universal_graph_theme_with_legend(0.125, 0.9)
    }
})

comparison_firm_num_of_policies_ggplot_export <- reactive({
    comparison_firm_num_of_policies_ggplot() +
        ggtitle("Number of Policies") +
        single_graph_export_theme
})


output$comparison_num_of_policies_firm_plot <- renderPlotly({
    if(length(comparison_firm_num_of_policies_ggplot()$data) == 0){
        ggplotly(comparison_firm_num_of_policies_ggplot())
    } else{
        plotly <- ggplotly(comparison_firm_num_of_policies_ggplot(), tooltip = "text", dynamicTicks = TRUE) %>%
            layout(legend = list(x = 0.01, y = 0.99, title = list(text=''), bgcolor = 'rgba(255,255,255,0.2)', font = list(size = 9), dynamicTicks = TRUE))  %>%
            layout(showlegend = TRUE,
                yaxis = list(showline= T, linewidth=1.5, linecolor='black',
                    showticklabels = T,  tickprefix = "  "),
                xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2))

        plotly$x$layout$yaxis$autorange <- FALSE
        plotly$x$layout$yaxis$range <- c(0, max(comparison_firm_num_of_policies_data()$value) * 1.3)
        plotly
    }
})

## AVG PREMIUM --------

output$comparison_avg_premium_firm_ui <- renderUI({
    if (is.null(comparison_firm_plots_status$avg_premium_status)) return()
    fluidRow(
        br(),
        div(class = "plot-title", h4(style = 'display: inline-block;', "Average Premium")),
        br(),
        plotlyOutput("comparison_avg_premium_firm_plot", width = '85%', height = "475px") %>% withSpinner(),
        align = "center"
    )
})

comparison_firm_avg_premium_data <- eventReactive(c(input$comparison_generate_firm_stat_btn, input$firm_year_range), {
    summarized_data <- comparison_firm_data_filtered_summarized()

    written_premium = summarized_data %>% filter(identifier == "01")
    num_of_policies = summarized_data %>% filter(identifier == "11")

    left_join(written_premium, num_of_policies,
        by = c("shortname", "year")) %>%
        select(shortname, year, value.x, value.y) %>%
        # firm_by(shortname, year) %>%
        mutate(value.x = na_if(value.x, 0), value.y = na_if(value.y, 0)) %>%
        mutate(avg_premium = value.x / value.y)
})


comparison_firm_avg_premium_export_table <- reactive({
    if(nrow(comparison_firm_avg_premium_data()) == 0){
        zeroGrob()
    } else {
        comparison_firm_avg_premium_data() %>%
            group_by(shortname) %>%
            mutate(col_names = paste0("Avg Premiums ", seq(min(year), max(year), 1))) %>%
            ungroup() %>%
            select(shortname, avg_premium, col_names) %>%
            mutate(avg_premium = ifelse(!is.na(avg_premium), glue("${comma(avg_premium)}"), "")) %>%
            rename(`Firm Name` = shortname) %>%
            pivot_wider(names_from = col_names, values_from = avg_premium) %>%
            table_to_grob()
    }
})

comparison_firm_avg_premium_export_table_excel <- reactive({
    if(nrow(comparison_firm_avg_premium_data()) == 0){
        tibble()
    } else {
        comparison_firm_avg_premium_data() %>%
            select(shortname, avg_premium, year) %>%
            rename(`Firm Name` = shortname) %>%
            pivot_wider(names_from = year, values_from = avg_premium)
    }
})

comparison_firm_avg_premium_ggplot <- eventReactive(c(input$comparison_generate_firm_stat_btn, input$firm_year_range), {
    if(nrow(comparison_firm_avg_premium_data()) == 0){
        return(ggplot() + theme_void())
    } else{
        ggplot(comparison_firm_avg_premium_data(), aes(x = as.factor(year), y = avg_premium, group = shortname, color = shortname, text = glue("<span><b>Value:</b> ${comma(avg_premium)}"))) +
            geom_line(linewidth = 1.5) +
            scale_color_manual(values = comparison_firm_color_palette()) +
            scale_fill_manual(values = comparison_firm_color_palette()) +
            geom_point(aes(fill = shortname), shape=21, size=4, color = 'black') +
            scale_y_continuous(limits = c(0, max(comparison_firm_avg_premium_data()$avg_premium, na.rm = T) *1.3 ), labels = label_number(prefix = "  $", scale_cut = cut_short_scale()), expand = c(0.001,0)) +
            ylab("Average Premium") +
            universal_graph_theme_with_legend(0.125, 0.9)
    }
})

comparison_firm_avg_premium_ggplot_export <- reactive({
    comparison_firm_avg_premium_ggplot() +
        ggtitle("Average Premium") +
        single_graph_export_theme
})

output$comparison_avg_premium_firm_plot <- renderPlotly({
    if(length(comparison_firm_avg_premium_ggplot()$data) == 0){
        ggplotly(comparison_firm_avg_premium_ggplot())
    } else{
        plotly <- ggplotly(comparison_firm_avg_premium_ggplot(), tooltip = "text", dynamicTicks = TRUE) %>%
            layout(legend = list(x = 0.01, y = 0.99, title = list(text=''), bgcolor = 'rgba(255,255,255,0.2)', font = list(size = 9), dynamicTicks = TRUE))  %>%
            layout(showlegend = TRUE,
                yaxis = list(showline= T, linewidth=1.5, linecolor='black',
                    showticklabels = T,  tickprefix = "  $"),
                xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2)) %>%
            fix_legend_plotly()
    }
})


## DIRECT DEFENSE PAID ------------

output$comparison_direct_defense_firm_ui <- renderUI({
    if (is.null(comparison_firm_plots_status$direct_defense_status)) return()
    fluidRow(
        br(),
        div(class = "plot-title", h4(style = 'display: inline-block;', "Direct Defense Cost Containment Paid")),
        br(),
        plotlyOutput("comparison_direct_defense_firm_plot", width = '85%', height = "475px") %>% withSpinner(),
        align = "center"
    )
})

comparison_firm_direct_defense_data <- eventReactive(c(input$comparison_generate_firm_stat_btn, input$firm_year_range), {
    comparison_firm_data_filtered_summarized() %>%
        filter(identifier == "07")
})


comparison_firm_direct_defense_export_table <- reactive({
    if(nrow(comparison_firm_direct_defense_data()) == 0){
        zeroGrob()
    } else {
        comparison_firm_direct_defense_data() %>%
            group_by(shortname) %>%
            mutate(col_names = paste0("Defense Cost Paid ", seq(min(year), max(year), 1))) %>%
            ungroup() %>%
            select(shortname, value, col_names) %>%
            mutate(value = ifelse(!is.na(value), glue("${comma(value)}"), "")) %>%
            rename(`Firm Name` = shortname) %>%
            pivot_wider(names_from = col_names, values_from = value) %>%
            table_to_grob()
    }
})


comparison_firm_direct_defense_export_table_excel <- reactive({
    if(nrow(comparison_firm_direct_defense_data()) == 0){
        tibble()
    } else {
        comparison_firm_direct_defense_data() %>%
            select(shortname, value, year) %>%
            rename(`Firm Name` = shortname) %>%
            pivot_wider(names_from = year, values_from = value)
    }
})


comparison_firm_direct_defense_ggplot <- eventReactive(c(input$comparison_generate_firm_stat_btn, input$firm_year_range), {
    if(nrow(comparison_firm_direct_defense_data()) == 0){
        return(ggplot() + theme_void())
    } else{
        ggplot(comparison_firm_direct_defense_data(), aes(x = as.factor(year), y = value, fill = shortname, text = glue("<span><b>Value:</b> {comma(value)}"))) +
            geom_bar(stat="identity", position="dodge") +
            scale_fill_manual(values = comparison_firm_color_palette()) +
            scale_y_continuous(limits = c(0, max(comparison_firm_direct_defense_data()$value) *1.3 ), labels = label_number(prefix = " ", scale_cut = cut_short_scale()), expand = c(0.001,0)) +
            ylab("Direct Defense Cost Containment Paid (LI 07)") +
            universal_graph_theme_with_legend(0.125, 0.9)
    }
})

comparison_firm_direct_defense_ggplot_export <- reactive({
    comparison_firm_direct_defense_ggplot() +
        ggtitle("Defense Cost Containment Paid") +
        single_graph_export_theme
})

output$comparison_direct_defense_firm_plot <- renderPlotly({
    if(length(comparison_firm_direct_defense_ggplot()$data) == 0){
        ggplotly(comparison_firm_direct_defense_ggplot())
    } else{
        plotly <- ggplotly(comparison_firm_direct_defense_ggplot(), tooltip = "text", dynamicTicks = TRUE) %>%
            layout(legend = list(x = 0.01, y = 0.99, title = list(text=''), bgcolor = 'rgba(255,255,255,0.2)', font = list(size = 9), dynamicTicks = TRUE))  %>%
            layout(showlegend = TRUE,
                yaxis = list(showline= T, linewidth=1.5, linecolor='black',
                    showticklabels = T,  tickprefix = "  "),
                xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2))

        plotly$x$layout$yaxis$autorange <- FALSE
        plotly$x$layout$yaxis$range <- c(0, max(comparison_firm_direct_defense_data()$value) * 1.3)
        plotly
    }
})


## CASE RESERVES ------------

output$comparison_case_reserves_firm_ui <- renderUI({
    if (is.null(comparison_firm_plots_status$case_reserves_status)) return()
    fluidRow(
        br(),
        div(class = "plot-title", h4(style = 'display: inline-block;', "Case Reserves")),
        br(),
        plotlyOutput("comparison_case_reserves_firm_plot", width = '85%', height = "475px") %>% withSpinner(),
        align = "center"
    )
})

comparison_firm_case_reserves_data <- eventReactive(c(input$comparison_generate_firm_stat_btn, input$firm_year_range), {
    comparison_firm_data_filtered_summarized() %>%
        filter(identifier == "08")
})

comparison_firm_case_reserves_export_table <- reactive({
    if(nrow(comparison_firm_case_reserves_data()) == 0){
        zeroGrob()
    } else {
        comparison_firm_case_reserves_data() %>%
            group_by(shortname) %>%
            mutate(col_names = paste0("Case Reserves ", seq(min(year), max(year), 1))) %>%
            ungroup() %>%
            select(shortname, value, col_names) %>%
            mutate(value = ifelse(!is.na(value), glue("${comma(value)}"), "")) %>%
            rename(`Firm Name` = shortname) %>%
            pivot_wider(names_from = col_names, values_from = value) %>%
            table_to_grob()
    }
})

comparison_firm_case_reserves_export_table_excel <- reactive({
    if(nrow(comparison_firm_case_reserves_data()) == 0){
        tibble()
    } else {
        comparison_firm_case_reserves_data() %>%
            select(shortname, value, year) %>%
            rename(`Firm Name` = shortname) %>%
            pivot_wider(names_from = year, values_from = value)
    }
})

comparison_firm_case_reserves_ggplot <- eventReactive(c(input$comparison_generate_firm_stat_btn, input$firm_year_range), {
    if(nrow(comparison_firm_case_reserves_data()) == 0){
        return(ggplot() + theme_void())
    } else{
        ggplot(comparison_firm_case_reserves_data(), aes(x = as.factor(year), y = value, fill = shortname, text = glue("<span><b>Value:</b> {comma(value)}"))) +
            geom_bar(stat="identity", position="dodge") +
            scale_fill_manual(values = comparison_firm_color_palette()) +
            scale_y_continuous(limits = c(0, max(comparison_firm_case_reserves_data()$value) *1.3 ), labels = label_number(prefix = " ", scale_cut = cut_short_scale()), expand = c(0.001,0)) +
            ylab("Case Reserves (LI 08)") +
            universal_graph_theme_with_legend(0.125, 0.9)
    }
})

comparison_firm_case_reserves_ggplot_export <- reactive({
    comparison_firm_case_reserves_ggplot() +
        ggtitle("Case Reserves") +
        single_graph_export_theme
})

output$comparison_case_reserves_firm_plot <- renderPlotly({
    if(length(comparison_firm_case_reserves_ggplot()$data) == 0){
        ggplotly(comparison_firm_case_reserves_ggplot())
    } else{
        plotly <- ggplotly(comparison_firm_case_reserves_ggplot(), tooltip = "text", dynamicTicks = TRUE) %>%
            layout(legend = list(x = 0.01, y = 0.99, title = list(text=''), bgcolor = 'rgba(255,255,255,0.2)', font = list(size = 9), dynamicTicks = TRUE))  %>%
            layout(showlegend = TRUE,
                yaxis = list(showline= T, linewidth=1.5, linecolor='black',
                    showticklabels = T,  tickprefix = "  "),
                xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2))

        plotly$x$layout$yaxis$autorange <- FALSE
        plotly$x$layout$yaxis$range <- c(0, max(comparison_firm_case_reserves_data()$value) * 1.3)
        plotly
    }
})

## CLAIMS CLOSED W PAYMENT ------------
output$comparison_total_claims_closed_with_payment_firm_ui <- renderUI({
    if (is.null(comparison_firm_plots_status$total_claims_closed_with_payment_status)) return()
    fluidRow(
        br(),
        div(class = "plot-title", h4(style = 'display: inline-block;', "Total Claims Closed with Payment")),
        br(),
        plotlyOutput("comparison_total_claims_closed_with_payment_firm_plot", width = '85%', height = "475px") %>% withSpinner(),
        align = "center"
    )
})

comparison_firm_total_claims_closed_with_payment_data <- eventReactive(c(input$comparison_generate_firm_stat_btn, input$firm_year_range), {
    comparison_firm_data_filtered_summarized() %>%
        filter(identifier == "20")
})

comparison_firm_total_claims_closed_with_payment_export_table <- reactive({
    if(nrow(comparison_firm_total_claims_closed_with_payment_data()) == 0){
        zeroGrob()
    } else {
        comparison_firm_total_claims_closed_with_payment_data() %>%
            group_by(shortname) %>%
            mutate(col_names = paste0("Closed w/ Payment ", seq(min(year), max(year), 1))) %>%
            ungroup() %>%
            select(shortname, value, col_names) %>%
            mutate(value = ifelse(!is.na(value), glue("{value}"), "")) %>%
            rename(`Firm Name` = shortname) %>%
            pivot_wider(names_from = col_names, values_from = value) %>%
            table_to_grob()
    }
})

comparison_firm_total_claims_closed_with_payment_export_table_excel <- reactive({
    if(nrow(comparison_firm_total_claims_closed_with_payment_data()) == 0){
        tibble()
    } else {
        comparison_firm_total_claims_closed_with_payment_data() %>%
            select(shortname, value, year) %>%
            rename(`Firm Name` = shortname) %>%
            pivot_wider(names_from = year, values_from = value)
    }
})

comparison_firm_total_claims_closed_with_payment_ggplot <- eventReactive(c(input$comparison_generate_firm_stat_btn, input$firm_year_range), {
    if(nrow(comparison_firm_total_claims_closed_with_payment_data()) == 0){
        return(ggplot() + theme_void())
    } else{
        ggplot(comparison_firm_total_claims_closed_with_payment_data(), aes(x = as.factor(year), y = value, fill = shortname, text = glue("<span><b>Value:</b> {comma(value)}"))) +
            geom_bar(stat="identity", position="dodge") +
            scale_fill_manual(values = comparison_firm_color_palette()) +
            scale_y_continuous(limits = c(0, max(comparison_firm_total_claims_closed_with_payment_data()$value) *1.3 ), labels = label_number(prefix = " ", scale_cut = cut_short_scale()), expand = c(0.001,0)) +
            ylab("Total Claims Closed with Payment (LI 20)") +
            universal_graph_theme_with_legend(0.125, 0.9)
    }
})

comparison_firm_total_claims_closed_with_payment_ggplot_export <- reactive({
    comparison_firm_total_claims_closed_with_payment_ggplot() +
        ggtitle("Total Claims Closed with Payment") +
        single_graph_export_theme
})

output$comparison_total_claims_closed_with_payment_firm_plot <- renderPlotly({
    if(length(comparison_firm_total_claims_closed_with_payment_ggplot()$data) == 0){
        ggplotly(comparison_firm_total_claims_closed_with_payment_ggplot())
    } else{
        plotly <- ggplotly(comparison_firm_total_claims_closed_with_payment_ggplot(), tooltip = "text", dynamicTicks = TRUE) %>%
            layout(legend = list(x = 0.01, y = 0.99, title = list(text=''), bgcolor = 'rgba(255,255,255,0.2)', font = list(size = 9), dynamicTicks = TRUE))  %>%
            layout(showlegend = TRUE,
                yaxis = list(showline= T, linewidth=1.5, linecolor='black',
                    showticklabels = T,  tickprefix = "  "),
                xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2))

        plotly$x$layout$yaxis$autorange <- FALSE
        plotly$x$layout$yaxis$range <- c(0, max(comparison_firm_total_claims_closed_with_payment_data()$value) * 1.3)
        plotly
    }
})


## CLAIMS CLOSED without PAYMENT ------------
output$comparison_total_claims_closed_without_payment_firm_ui <- renderUI({
    if (is.null(comparison_firm_plots_status$total_claims_closed_without_payment_status)) return()
    fluidRow(
        br(),
        div(class = "plot-title", h4(style = 'display: inline-block;', "Total Claims Closed without Payment")),
        br(),
        plotlyOutput("comparison_total_claims_closed_without_payment_firm_plot", width = '85%', height = "475px") %>% withSpinner(),
        align = "center"
    )
})

comparison_firm_total_claims_closed_without_payment_data <- eventReactive(c(input$comparison_generate_firm_stat_btn, input$firm_year_range), {
    comparison_firm_data_filtered_summarized() %>%
        filter(identifier == "23")
})

comparison_firm_total_claims_closed_without_payment_export_table <- reactive({
    if(nrow(comparison_firm_total_claims_closed_without_payment_data()) == 0){
        zeroGrob()
    } else {
        comparison_firm_total_claims_closed_without_payment_data() %>%
            group_by(shortname) %>%
            mutate(col_names = paste0("Closed w/o Payment ", seq(min(year), max(year), 1))) %>%
            ungroup() %>%
            select(shortname, value, col_names) %>%
            mutate(value = ifelse(!is.na(value), glue("{value}"), "")) %>%
            rename(`Firm Name` = shortname) %>%
            pivot_wider(names_from = col_names, values_from = value) %>%
            table_to_grob(colhead_cex = 0.6)
    }
})

comparison_firm_total_claims_closed_without_payment_export_table_excel <- reactive({
    if(nrow(comparison_firm_total_claims_closed_without_payment_data()) == 0){
        tibble()
    } else {
        comparison_firm_total_claims_closed_without_payment_data() %>%
            select(shortname, value, year) %>%
            rename(`Firm Name` = shortname) %>%
            pivot_wider(names_from = year, values_from = value)
    }
})

comparison_firm_total_claims_closed_without_payment_ggplot <- eventReactive(c(input$comparison_generate_firm_stat_btn, input$firm_year_range), {
    if(nrow(comparison_firm_total_claims_closed_without_payment_data()) == 0){
        return(ggplot() + theme_void())
    } else{
        ggplot(comparison_firm_total_claims_closed_without_payment_data(), aes(x = as.factor(year), y = value, fill = shortname, text = glue("<span><b>Value:</b> {comma(value)}"))) +
            geom_bar(stat="identity", position="dodge") +
            scale_fill_manual(values = comparison_firm_color_palette()) +
            scale_y_continuous(limits = c(0, max(comparison_firm_total_claims_closed_without_payment_data()$value) *1.3 ), labels = label_number(prefix = " ", scale_cut = cut_short_scale()), expand = c(0.001,0)) +
            ylab("Total Claims Closed without Payment (LI 23)") +
            universal_graph_theme_with_legend(0.125, 0.9)
    }
})

comparison_firm_total_claims_closed_without_payment_ggplot_export <- reactive({
    comparison_firm_total_claims_closed_without_payment_ggplot() +
        ggtitle("Total Claims Closed without Payment") +
        single_graph_export_theme
})


output$comparison_total_claims_closed_without_payment_firm_plot <- renderPlotly({
    if(length(comparison_firm_total_claims_closed_without_payment_ggplot()$data) == 0){
        ggplotly(comparison_firm_total_claims_closed_without_payment_ggplot())
    } else{
        plotly <- ggplotly(comparison_firm_total_claims_closed_without_payment_ggplot(), tooltip = "text", dynamicTicks = TRUE) %>%
            layout(legend = list(x = 0.01, y = 0.99, title = list(text=''), bgcolor = 'rgba(255,255,255,0.2)', font = list(size = 9), dynamicTicks = TRUE))  %>%
            layout(showlegend = TRUE,
                yaxis = list(showline= T, linewidth=1.5, linecolor='black',
                    showticklabels = T,  tickprefix = "  "),
                xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2))

        plotly$x$layout$yaxis$autorange <- FALSE
        plotly$x$layout$yaxis$range <- c(0, max(comparison_firm_total_claims_closed_without_payment_data()$value) * 1.3)
        plotly
    }
})

## 1st CLAIMS CLOSED WITHOUT PAYMENT ---------
output$comparison_first_party_claims_closed_without_payment_firm_ui <- renderUI({
    if (is.null(comparison_firm_plots_status$first_party_claims_closed_without_payment_status)) return()
    fluidRow(
        br(),
        div(class = "plot-title", h4(style = 'display: inline-block;', "First Party Claims Closed without Payment")),
        br(),
        plotlyOutput("comparison_first_party_claims_closed_without_payment_firm_plot", width = '85%', height = "475px") %>% withSpinner(),
        align = "center"
    )
})

comparison_firm_first_party_claims_closed_without_payment_data <- eventReactive(c(input$comparison_generate_firm_stat_btn, input$firm_year_range), {
    comparison_firm_data_filtered_summarized() %>%
        filter(identifier == "21")
})

comparison_firm_first_party_claims_closed_without_payment_export_table <- reactive({
    if(nrow(comparison_firm_first_party_claims_closed_without_payment_data()) == 0){
        zeroGrob()
    } else {
        comparison_firm_first_party_claims_closed_without_payment_data() %>%
            group_by(shortname) %>%
            mutate(col_names = paste0("1st Party w/o Payment ", seq(min(year), max(year), 1))) %>%
            ungroup() %>%
            select(shortname, value, col_names) %>%
            mutate(value = ifelse(!is.na(value), glue("{value}"), "")) %>%
            rename(`Firm Name` = shortname) %>%
            pivot_wider(names_from = col_names, values_from = value) %>%
            table_to_grob(colhead_cex = 0.6)
    }
})


comparison_firm_first_party_claims_closed_without_payment_export_table_excel <- reactive({
    if(nrow(comparison_firm_first_party_claims_closed_without_payment_data()) == 0){
        tibble()
    } else {
        comparison_firm_first_party_claims_closed_without_payment_data() %>%
            select(shortname, value, year) %>%
            rename(`Firm Name` = shortname) %>%
            pivot_wider(names_from = year, values_from = value)
    }
})

comparison_firm_first_party_claims_closed_without_payment_ggplot <- eventReactive(c(input$comparison_generate_firm_stat_btn, input$firm_year_range), {
    if(nrow(comparison_firm_first_party_claims_closed_without_payment_data()) == 0){
        return(ggplot() + theme_void())
    } else{
        ggplot(comparison_firm_first_party_claims_closed_without_payment_data(), aes(x = as.factor(year), y = value, fill = shortname, text = glue("<span><b>Value:</b> {comma(value)}"))) +
            geom_bar(stat="identity", position="dodge") +
            scale_fill_manual(values = comparison_firm_color_palette()) +
            scale_y_continuous(limits = c(0, max(comparison_firm_first_party_claims_closed_without_payment_data()$value) *1.3 ), labels = label_number(prefix = " ", scale_cut = cut_short_scale()), expand = c(0.001,0)) +
            ylab("First Party Claims Closed\n without payment (LI 21)") +
            universal_graph_theme_with_legend(0.125, 0.9)
    }
})

comparison_firm_first_party_claims_closed_without_payment_ggplot_export <- reactive({
    comparison_firm_first_party_claims_closed_without_payment_ggplot() +
        ggtitle("1st Party Claims Closed w/o Payment") +
        single_graph_export_theme
})

output$comparison_first_party_claims_closed_without_payment_firm_plot <- renderPlotly({
    if(length(comparison_firm_first_party_claims_closed_without_payment_ggplot()$data) == 0){
        ggplotly(comparison_firm_first_party_claims_closed_without_payment_ggplot())
    } else{
        plotly <- ggplotly(comparison_firm_first_party_claims_closed_without_payment_ggplot(), tooltip = "text", dynamicTicks = TRUE) %>%
            layout(legend = list(x = 0.01, y = 0.99, title = list(text=''), bgcolor = 'rgba(255,255,255,0.2)', font = list(size = 9), dynamicTicks = TRUE))  %>%
            layout(showlegend = TRUE,
                yaxis = list(showline= T, linewidth=1.5, linecolor='black',
                    showticklabels = T,  tickprefix = "  "),
                xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2))

        plotly$x$layout$yaxis$autorange <- FALSE
        plotly$x$layout$yaxis$range <- c(0, max(comparison_firm_first_party_claims_closed_without_payment_data()$value) * 1.3)
        plotly
    }
})


## 3rd CLAIMS CLOSED WITHOUT PAYMENT ---------
output$comparison_third_party_claims_closed_without_payment_firm_ui <- renderUI({
    if (is.null(comparison_firm_plots_status$third_party_claims_closed_without_payment_status)) return()
    fluidRow(
        br(),
        div(class = "plot-title", h4(style = 'display: inline-block;', "Third Party Claims Closed without Payment")),
        br(),
        plotlyOutput("comparison_third_party_claims_closed_without_payment_firm_plot", width = '85%', height = "475px") %>% withSpinner(),
        align = "center"
    )
})

comparison_firm_third_party_claims_closed_without_payment_data <- eventReactive(c(input$comparison_generate_firm_stat_btn, input$firm_year_range), {
    comparison_firm_data_filtered_summarized() %>%
        filter(identifier == "22")
})

comparison_firm_third_party_claims_closed_without_payment_export_table <- reactive({
    if(nrow(comparison_firm_third_party_claims_closed_without_payment_data()) == 0){
        zeroGrob()
    } else {
        comparison_firm_third_party_claims_closed_without_payment_data() %>%
            group_by(shortname) %>%
            mutate(col_names = paste0("3rd Party w/o Payment ", seq(min(year), max(year), 1))) %>%
            ungroup() %>%
            select(shortname, value, col_names) %>%
            mutate(value = ifelse(!is.na(value), glue("{value}"), "")) %>%
            rename(`Firm Name` = shortname) %>%
            pivot_wider(names_from = col_names, values_from = value) %>%
            table_to_grob(colhead_cex = 0.6)
    }
})

comparison_firm_third_party_claims_closed_without_payment_export_table_excel <- reactive({
    if(nrow(comparison_firm_third_party_claims_closed_without_payment_data()) == 0){
        tibble()
    } else {
        comparison_firm_third_party_claims_closed_without_payment_data() %>%
            select(shortname, value, year) %>%
            rename(`Firm Name` = shortname) %>%
            pivot_wider(names_from = year, values_from = value)
    }
})


comparison_firm_third_party_claims_closed_without_payment_ggplot <- eventReactive(c(input$comparison_generate_firm_stat_btn, input$firm_year_range), {
    if(nrow(comparison_firm_third_party_claims_closed_without_payment_data()) == 0){
        return(ggplot() + theme_void())
    } else{
        ggplot(comparison_firm_third_party_claims_closed_without_payment_data(), aes(x = as.factor(year), y = value, fill = shortname, text = glue("<span><b>Value:</b> {comma(value)}"))) +
            geom_bar(stat="identity", position="dodge") +
            scale_fill_manual(values = comparison_firm_color_palette()) +
            scale_y_continuous(limits = c(0, max(comparison_firm_third_party_claims_closed_without_payment_data()$value) *1.3 ), labels = label_number(prefix = " ", scale_cut = cut_short_scale()), expand = c(0.001,0)) +
            ylab("Third Party Claims Closed without payment (LI 22)") +
            universal_graph_theme_with_legend(0.125, 0.9)
    }
})

comparison_firm_third_party_claims_closed_without_payment_ggplot_export <- reactive({
    comparison_firm_third_party_claims_closed_without_payment_ggplot() +
        ggtitle("3rd Party Claims Closed w/o Payment") +
        single_graph_export_theme
})

output$comparison_third_party_claims_closed_without_payment_firm_plot <- renderPlotly({
    if(length(comparison_firm_third_party_claims_closed_without_payment_ggplot()$data) == 0){
        ggplotly(comparison_firm_third_party_claims_closed_without_payment_ggplot())
    } else{
        plotly <- ggplotly(comparison_firm_third_party_claims_closed_without_payment_ggplot(), tooltip = "text", dynamicTicks = TRUE) %>%
            layout(legend = list(x = 0.01, y = 0.99, title = list(text=''), bgcolor = 'rgba(255,255,255,0.2)', font = list(size = 9), dynamicTicks = TRUE))  %>%
            layout(showlegend = TRUE,
                yaxis = list(showline= T, linewidth=1.5, linecolor='black',
                    showticklabels = T,  tickprefix = "  "),
                xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2))

        plotly$x$layout$yaxis$autorange <- FALSE
        plotly$x$layout$yaxis$range <- c(0, max(comparison_firm_third_party_claims_closed_without_payment_data()$value) * 1.3)
        plotly
    }
})

## * 1st OPEN CLAIMS ---------
output$comparison_first_party_open_claims_firm_ui <- renderUI({
    if (is.null(comparison_firm_plots_status$first_party_open_claims_status)) return()
    fluidRow(
        br(),
        div(class = "plot-title", h4(style = 'display: inline-block;', "First Party Open Claims")),
        br(),
        plotlyOutput("comparison_first_party_open_claims_firm_plot", width = '85%', height = "475px") %>% withSpinner(),
        align = "center"
    )
})

comparison_firm_first_party_open_claims_data <- eventReactive(c(input$comparison_generate_firm_stat_btn, input$firm_year_range), {
    comparison_firm_data_filtered_summarized() %>%
        filter(identifier == "15")
})


comparison_firm_first_party_open_claims_export_table <- reactive({
    if(nrow(comparison_firm_first_party_open_claims_data()) == 0){
        zeroGrob()
    } else {
        comparison_firm_first_party_open_claims_data() %>%
            group_by(shortname) %>%
            mutate(col_names = paste0("1st Party Open Claims ", seq(min(year), max(year), 1))) %>%
            ungroup() %>%
            select(shortname, value, col_names) %>%
            mutate(value = ifelse(!is.na(value), glue("{value}"), "")) %>%
            rename(`Firm Name` = shortname) %>%
            pivot_wider(names_from = col_names, values_from = value) %>%
            table_to_grob(colhead_cex = 0.6)
    }
})


comparison_firm_first_party_open_claims_export_table_excel <- reactive({
    if(nrow(comparison_firm_first_party_open_claims_data()) == 0){
        tibble()
    } else {
        comparison_firm_first_party_open_claims_data() %>%
            select(shortname, value, year) %>%
            rename(`Firm Name` = shortname) %>%
            pivot_wider(names_from = year, values_from = value)
    }
})

comparison_firm_first_party_open_claims_ggplot <- eventReactive(c(input$comparison_generate_firm_stat_btn, input$firm_year_range), {
    if(nrow(comparison_firm_first_party_open_claims_data()) == 0){
        return(ggplot() + theme_void())
    } else{
        ggplot(comparison_firm_first_party_open_claims_data(), aes(x = as.factor(year), y = value, fill = shortname, text = glue("<span><b>Value:</b> {comma(value)}"))) +
            geom_bar(stat="identity", position="dodge") +
            scale_fill_manual(values = comparison_firm_color_palette()) +
            scale_y_continuous(limits = c(0, max(comparison_firm_first_party_open_claims_data()$value) *1.3 ), labels = label_number(prefix = " ", scale_cut = cut_short_scale()), expand = c(0.001,0)) +
            ylab("First Party Open Claims (LI 15)") +
            universal_graph_theme_with_legend(0.125, 0.9)
    }
})

comparison_firm_first_party_open_claims_ggplot_export <- reactive({
    comparison_firm_first_party_open_claims_ggplot() +
        ggtitle("1st Party Open Claims") +
        single_graph_export_theme
})

output$comparison_first_party_open_claims_firm_plot <- renderPlotly({
    if(length(comparison_firm_first_party_open_claims_ggplot()$data) == 0){
        ggplotly(comparison_firm_first_party_open_claims_ggplot())
    } else{
        plotly <- ggplotly(comparison_firm_first_party_open_claims_ggplot(), tooltip = "text", dynamicTicks = TRUE) %>%
            layout(legend = list(x = 0.01, y = 0.99, title = list(text=''), bgcolor = 'rgba(255,255,255,0.2)', font = list(size = 9), dynamicTicks = TRUE))  %>%
            layout(showlegend = TRUE,
                yaxis = list(showline= T, linewidth=1.5, linecolor='black',
                    showticklabels = T,  tickprefix = "  "),
                xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2))

        plotly$x$layout$yaxis$autorange <- FALSE
        plotly$x$layout$yaxis$range <- c(0, max(comparison_firm_first_party_open_claims_data()$value) * 1.3)
        plotly
    }
})

## * 3rd OPEN CLAIMS ---------
output$comparison_third_party_open_claims_firm_ui <- renderUI({
    if (is.null(comparison_firm_plots_status$third_party_open_claims_status)) return()
    fluidRow(
        br(),
        div(class = "plot-title", h4(style = 'display: inline-block;', "Third Party Open Claims")),
        br(),
        plotlyOutput("comparison_third_party_open_claims_firm_plot", width = '85%', height = "475px") %>% withSpinner(),
        align = "center"
    )
})

comparison_firm_third_party_open_claims_data <- eventReactive(c(input$comparison_generate_firm_stat_btn, input$firm_year_range), {
    comparison_firm_data_filtered_summarized() %>%
        filter(identifier == "16")
})

comparison_firm_third_party_open_claims_export_table <- reactive({
    if(nrow(comparison_firm_third_party_open_claims_data()) == 0){
        zeroGrob()
    } else {
        comparison_firm_third_party_open_claims_data() %>%
            group_by(shortname) %>%
            mutate(col_names = paste0("3rd Party Open Claims ", seq(min(year), max(year), 1))) %>%
            ungroup() %>%
            select(shortname, value, col_names) %>%
            mutate(value = ifelse(!is.na(value), glue("{value}"), "")) %>%
            rename(`Firm Name` = shortname) %>%
            pivot_wider(names_from = col_names, values_from = value) %>%
            table_to_grob(colhead_cex = 0.6)
    }
})

comparison_firm_third_party_open_claims_export_table_excel <- reactive({
    if(nrow(comparison_firm_third_party_open_claims_data()) == 0){
        tibble()
    } else {
        comparison_firm_third_party_open_claims_data() %>%
            select(shortname, value, year) %>%
            rename(`Firm Name` = shortname) %>%
            pivot_wider(names_from = year, values_from = value)
    }
})


comparison_firm_third_party_open_claims_ggplot <- eventReactive(c(input$comparison_generate_firm_stat_btn, input$firm_year_range), {
    if(nrow(comparison_firm_third_party_open_claims_data()) == 0){
        return(ggplot() + theme_void())
    } else{
        ggplot(comparison_firm_third_party_open_claims_data(), aes(x = as.factor(year), y = value, fill = shortname, text = glue("<span><b>Value:</b> {comma(value)}"))) +
            geom_bar(stat="identity", position="dodge") +
            scale_fill_manual(values = comparison_firm_color_palette()) +
            scale_y_continuous(limits = c(0, max(comparison_firm_third_party_open_claims_data()$value) *1.3 ), labels = label_number(prefix = " ", scale_cut = cut_short_scale()), expand = c(0.001,0)) +
            ylab("Third Party Open Claims (LI 16)") +
            universal_graph_theme_with_legend(0.125, 0.9)
    }
})

comparison_firm_third_party_open_claims_ggplot_export <- reactive({
    comparison_firm_third_party_open_claims_ggplot() +
        ggtitle("3rd Party Open Claims") +
        single_graph_export_theme
})


output$comparison_third_party_open_claims_firm_plot <- renderPlotly({
    if(length(comparison_firm_third_party_open_claims_ggplot()$data) == 0){
        ggplotly(comparison_firm_third_party_open_claims_ggplot())
    } else{
        plotly <- ggplotly(comparison_firm_third_party_open_claims_ggplot(), tooltip = "text", dynamicTicks = TRUE) %>%
            layout(legend = list(x = 0.01, y = 0.99, title = list(text=''), bgcolor = 'rgba(255,255,255,0.2)', font = list(size = 9), dynamicTicks = TRUE))  %>%
            layout(showlegend = TRUE,
                yaxis = list(showline= T, linewidth=1.5, linecolor='black',
                    showticklabels = T,  tickprefix = "  "),
                xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2))

        plotly$x$layout$yaxis$autorange <- FALSE
        plotly$x$layout$yaxis$range <- c(0, max(comparison_firm_third_party_open_claims_data()$value) * 1.3)
        plotly
    }
})

## PDF REPORT -----------
output$export_comparison_firm_pdf = downloadHandler(
    filename = function() {glue("comparison_firm.pdf")},
    content = function(file) {
        withProgress(message = glue("Exporting comparison_firm.pdf"), {

            selected_firm <- input$select_firm_compare %>% str_flatten_comma()
            n_selected_firm <- length(input$select_firm_compare)

            ## COVER PAGE
            cover_title <- textGrob(glue("Cyber Insurance Center"), gp=gpar(fontsize=22, fontface = 'bold'),
                x = unit(0.345, "npc"), y = unit(0.150, "npc"))

            cover_title2 <- textGrob(glue("Cyber Dashboard"), gp=gpar(fontsize=18),
                x = unit(0.055, "npc"), y = unit(0.150, "npc"))

            cover_title3 <- textGrob(glue("Firm Comparison"), gp=gpar(fontsize=12),
                x = unit(-0.15, "npc"), y = unit(0.150, "npc"), just = "left")

            cover_title4 <- textGrob(glue("{selected_firm}"), gp=gpar(fontsize=10),
                x = unit(-0.15, "npc"), y = unit(0.150, "npc"), just = "left")

            cover_margin = unit(0.9, "line")

            cover_content <- gtable_col('cover', grobs = list(cover_title,  cover_title2, cover_title3, cover_title4),
                heights = unit.c(grobHeight(cover_title) + 1.2*cover_margin,
                    grobHeight(cover_title2) + cover_margin,
                    grobHeight(cover_title3) + cover_margin,
                    grobHeight(cover_title4) + cover_margin))

            cover_content_grob = arrangeGrob(zeroGrob(),
                cover_content,
                heights = unit(c(6, 1), c('cm', 'npc')),
                as.table = FALSE)

            logo_png <- readPNG("./www/img/qcclogo_only.png")

            cover_logo <- rasterGrob(logo_png,
                height = unit(1.7, "cm"), width = unit(1.7, "cm"),
                x = unit(1.2, "npc"), y = unit(0.795, "npc"))

            cover_logo_with_line <- arrangeGrob(
                cover_logo,
                left = textGrob("                "),
                right = segmentsGrob(
                    x0 = unit(4, "npc"), y0 = unit(0.84, "npc"),
                    x1 = unit(4, "npc"), y1 = unit(0.73, "npc"))
            )

            ###### CONTENT PAGE


            logo <- rasterGrob(logo_png,
                height = unit(0.9, "cm"), width = unit(0.9, "cm"),
                x = unit(0.025, "npc"), y = unit(0.25, "npc"))

            title <- textGrob(glue("\t{selected_firm}\n\n"), gp=gpar(fontsize=11, fontface = 'bold'), x = unit(0.525, "npc"))

            bottom_text <- c(1:5) %>%
                map(~ textGrob(
                    glue("Cyber Insurance Center | {selected_firm} | {.x}"),
                    gp=gpar(fontsize=9), y = 1))

            n_plot <- length(input$comparison_select_firm_statistic)
            n_pages <- ceiling(n_plot/3)

            total_plot <- n_pages * 3

            plot_content <- list()
            table_content <- list()

            if (!is.null(comparison_firm_plots_status$premium_written_status)){
                plot_content <- append(plot_content, list(comparison_firm_written_premium_ggplot_export()))
                table_content <- append(table_content, list(comparison_firm_written_premium_export_table()))
            }

            if (!is.null(comparison_firm_plots_status$yoy_change_premium_written_status)){
                plot_content <- append(plot_content, list(comparison_firm_yoy_change_written_premiums_ggplot_export()))
                table_content <- append(table_content, list(comparison_firm_yoy_change_written_premium_export_table()))
            }

            if (!is.null(comparison_firm_plots_status$premium_earned_status)){
                plot_content <- append(plot_content, list(comparison_firm_earned_premium_ggplot_export()))
                table_content <- append(table_content, list(comparison_firm_earned_premium_export_table()))
            }

            if (!is.null(comparison_firm_plots_status$total_losses_status)){
                plot_content <- append(plot_content, list(comparison_firm_total_losses_ggplot_export()))
                table_content <- append(table_content, list(comparison_firm_total_losses_export_table()))
            }

            if (!is.null(comparison_firm_plots_status$loss_ratio_status)){
                plot_content <- append(plot_content, list(comparison_firm_loss_ratio_ggplot_export()))
                table_content <- append(table_content, list(comparison_firm_loss_ratio_export_table()))
            }

            if (!is.null(comparison_firm_plots_status$num_of_policies_status)){
                plot_content <- append(plot_content, list(comparison_firm_num_of_policies_ggplot_export()))
                table_content <- append(table_content, list(comparison_firm_num_of_policies_export_table()))
            }

            if (!is.null(comparison_firm_plots_status$avg_premium_status)){
                plot_content <- append(plot_content, list(comparison_firm_avg_premium_ggplot_export()))
                table_content <- append(table_content, list(comparison_firm_avg_premium_export_table()))
            }

            if (!is.null(comparison_firm_plots_status$direct_defense_status)){
                plot_content <- append(plot_content, list(comparison_firm_direct_defense_ggplot_export()))
                table_content <- append(table_content, list(comparison_firm_direct_defense_export_table()))
            }

            if (!is.null(comparison_firm_plots_status$case_reserves_status)){
                plot_content <- append(plot_content, list(comparison_firm_case_reserves_ggplot_export()))
                table_content <- append(table_content, list(comparison_firm_case_reserves_export_table()))
            }

            if (!is.null(comparison_firm_plots_status$total_claims_closed_with_payment_status)){
                plot_content <- append(plot_content, list(comparison_firm_total_claims_closed_with_payment_ggplot_export()))
                table_content <- append(table_content, list(comparison_firm_total_claims_closed_with_payment_export_table()))
            }

            if (!is.null(comparison_firm_plots_status$total_claims_closed_without_payment_status)){
                plot_content <- append(plot_content, list(comparison_firm_total_claims_closed_without_payment_ggplot_export()))
                table_content <- append(table_content, list(comparison_firm_total_claims_closed_without_payment_export_table()))
            }

            if (!is.null(comparison_firm_plots_status$first_party_claims_closed_without_payment_status)){
                plot_content <- append(plot_content, list(comparison_firm_first_party_claims_closed_without_payment_ggplot_export()))
                table_content <- append(table_content, list(comparison_firm_first_party_claims_closed_without_payment_export_table()))
            }

            if (!is.null(comparison_firm_plots_status$third_party_claims_closed_without_payment_status)){
                plot_content <- append(plot_content, list(comparison_firm_third_party_claims_closed_without_payment_ggplot_export()))
                table_content <- append(table_content, list(comparison_firm_third_party_claims_closed_without_payment_export_table()))
            }

            if (!is.null(comparison_firm_plots_status$first_party_open_claims_status)){
                plot_content <- append(plot_content, list(comparison_firm_first_party_open_claims_ggplot_export()))
                table_content <- append(table_content, list(comparison_firm_first_party_open_claims_export_table()))
            }

            if (!is.null(comparison_firm_plots_status$third_party_open_claims_status)){
                plot_content <- append(plot_content, list(comparison_firm_first_party_open_claims_ggplot_export()))
                table_content <- append(table_content, list(comparison_firm_third_party_open_claims_export_table()))
            }

            print(length(plot_content))
            print(length(table_content))

            while (length(plot_content) < total_plot) {
                plot_content <- append(plot_content, list(zeroGrob()))
                table_content <- append(table_content, list(zeroGrob()))
            }

            print(length(plot_content))
            print(length(table_content))

            first_page_heights = switch(as.character(n_selected_firm),
                "1" = c(0.3, 2.4, 0.1, 0.25, 0.1, 2.4, 0.1, 0.25, 0.1, 2.4, 0.5, 0.1),
                "2" = c(0.3, 2.2, 0.2, 0.25, 0.2, 2.2, 0.2, 0.25, 0.2, 2.2, 0.7, 0.1),
                "3" = c(0.3, 2.1, 0.25, 0.25, 0.25, 2.1, 0.25, 0.25, 0.25, 2.1, 0.75, 0.1))

            content_page_heights = switch(as.character(n_selected_firm),
                "1" = c(0.2, 2.4, 0.1, 0.25, 0.1, 2.4, 0.1, 0.25, 0.1, 2.4, 0.5, 0.1),
                "2" = c(0.2, 2.2, 0.2, 0.25, 0.2, 2.2, 0.2, 0.25, 0.2, 2.2, 0.7, 0.1),
                "3" = c(0.2, 2.1, 0.25, 0.25, 0.25, 2.1, 0.25, 0.25, 0.25, 2.1, 0.75, 0.1))

            incProgress(1/2)

            cover_page <- arrangeGrob(zeroGrob(),
                top = cover_content_grob,
                left = cover_logo_with_line,
                nrow = 1)

            content_page <- list()

            for(i in 1:n_pages){
                plot_num <- switch (as.character(i),
                    "1" = c(1,2,3),
                    "2" = c(4,5,6),
                    "3" = c(7,8,9),
                    "4" = c(10,11,12),
                    "5" = c(13, 14, 15),
                )

                if(i == 1){
                    current_page <- arrangeGrob(
                        grobs=list(
                            title,
                            plot_content[[plot_num[1]]], zeroGrob(),
                            table_content[[plot_num[1]]], zeroGrob(),
                            plot_content[[plot_num[2]]], zeroGrob(),
                            table_content[[plot_num[2]]], zeroGrob(),
                            plot_content[[plot_num[3]]],
                            table_content[[plot_num[3]]], zeroGrob()),
                        padding = unit(0.1, "line"),
                        top = logo, bottom = bottom_text[[1]],
                        left = textGrob("    "), right = textGrob("    "),
                        heights = first_page_heights,
                        ncol = 1, nrow = 12)
                } else {
                    current_page <- arrangeGrob(
                        grobs=list(
                            zeroGrob(),
                            plot_content[[plot_num[1]]], zeroGrob(),
                            table_content[[plot_num[1]]], zeroGrob(),
                            plot_content[[plot_num[2]]], zeroGrob(),
                            table_content[[plot_num[2]]], zeroGrob(),
                            plot_content[[plot_num[3]]],
                            table_content[[plot_num[3]]], zeroGrob()),
                        padding = unit(0.1, "line"),
                        top = logo, bottom = bottom_text[[i]],
                        left = textGrob("    "), right = textGrob("    "),
                        heights = content_page_heights,
                        ncol = 1, nrow = 12)
                }
                content_page <- c(content_page, list(current_page))
            }

            ppl <- c(list(cover_page), content_page)

            class(ppl) <- c("arrangelist", "list")
            incProgress(1/2)
            ggsave(file, ppl, device = "pdf", width = 8, height = 12)
        })
    }
)



### EXCEL EXPORT ------------

output$export_comparison_firm_excel = downloadHandler(
    filename = function() {glue("comparison_firm.xlsx")},
    content = function(file) {
        withProgress(message = glue("Exporting comparison_firm.xlsx"), {
            my_workbook <- createWorkbook()

            worksheet_name <- c()
            worksheet_content <- list()

            if (!is.null(comparison_firm_plots_status$premium_written_status)){
                worksheet_name <- append(worksheet_name, "Written Premiums")
                worksheet_content <- append(worksheet_content, list(comparison_firm_written_premium_export_table_excel()))
            }

            if (!is.null(comparison_firm_plots_status$yoy_change_premium_written_status)){
                worksheet_name <- append(worksheet_name, "YOY % Change")
                worksheet_content <- append(worksheet_content, list(comparison_firm_yoy_change_written_premium_export_table_excel()))
            }

            if (!is.null(comparison_firm_plots_status$premium_earned_status)){
                worksheet_name <- append(worksheet_name, "Earned Premiums")
                worksheet_content <- append(worksheet_content, list(comparison_firm_earned_premium_export_table_excel()))
            }

            if (!is.null(comparison_firm_plots_status$total_losses_status)){
                worksheet_name <- append(worksheet_name, "Direct Losses Paid")
                worksheet_content <- append(worksheet_content, list(comparison_firm_total_losses_export_table_excel()))
            }

            if (!is.null(comparison_firm_plots_status$loss_ratio_status)){
                worksheet_name <- append(worksheet_name, "Loss Ratio")
                worksheet_content <- append(worksheet_content, list(comparison_firm_loss_ratio_export_table_excel()))
            }

            if (!is.null(comparison_firm_plots_status$num_of_policies_status)){
                worksheet_name <- append(worksheet_name, "Num of Policies")
                worksheet_content <- append(worksheet_content, list(comparison_firm_num_of_policies_export_table_excel()))
            }

            if (!is.null(comparison_firm_plots_status$avg_premium_status)){
                worksheet_name <- append(worksheet_name, "Avg Premiums")
                worksheet_content <- append(worksheet_content, list(comparison_firm_avg_premium_export_table_excel()))
            }

            if (!is.null(comparison_firm_plots_status$direct_defense_status)){
                worksheet_name <- append(worksheet_name, "Direct Defense Paid")
                worksheet_content <- append(worksheet_content, list(comparison_firm_direct_defense_export_table_excel()))
            }

            if (!is.null(comparison_firm_plots_status$case_reserves_status)){
                worksheet_name <- append(worksheet_name, "Case Reserves")
                worksheet_content <- append(worksheet_content, list(comparison_firm_case_reserves_export_table_excel()))
            }

            if (!is.null(comparison_firm_plots_status$total_claims_closed_with_payment_status)){
                worksheet_name <- append(worksheet_name, "With Payment")
                worksheet_content <- append(worksheet_content, list(comparison_firm_total_claims_closed_with_payment_export_table_excel()))
            }

            if (!is.null(comparison_firm_plots_status$total_claims_closed_without_payment_status)){
                worksheet_name <- append(worksheet_name, "Without Payment")
                worksheet_content <- append(worksheet_content, list(comparison_firm_total_claims_closed_without_payment_export_table_excel()))
            }

            if (!is.null(comparison_firm_plots_status$first_party_claims_closed_without_payment_status)){
                worksheet_name <- append(worksheet_name, "1st Without Payment")
                worksheet_content <- append(worksheet_content, list(comparison_firm_first_party_claims_closed_without_payment_export_table_excel()))
            }

            if (!is.null(comparison_firm_plots_status$third_party_claims_closed_without_payment_status)){
                worksheet_name <- append(worksheet_name, "3rd Without Payment")
                worksheet_content <- append(worksheet_content, list(comparison_firm_third_party_claims_closed_without_payment_export_table_excel()))
            }

            if (!is.null(comparison_firm_plots_status$first_party_open_claims_status)){
                worksheet_name <- append(worksheet_name, "1st Open Claims")
                worksheet_content <- append(worksheet_content, list(comparison_firm_first_party_open_claims_export_table_excel()))
            }

            if (!is.null(comparison_firm_plots_status$third_party_open_claims_status)){
                worksheet_name <- append(worksheet_name, "3rd Open Claims")
                worksheet_content <- append(worksheet_content, list(comparison_firm_third_party_open_claims_export_table_excel()))
            }

            incProgress(1/2)

            for(i in 1:length(worksheet_name)){
                addWorksheet(
                    wb = my_workbook,
                    sheetName = worksheet_name[i])
                writeData(
                    my_workbook,
                    sheet = i,
                    worksheet_content[[i]])
            }

            incProgress(1/2)
            saveWorkbook(my_workbook, file, overwrite = TRUE)
        })
    }
)
