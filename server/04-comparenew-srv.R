
# COMPARE GROUP --------

# * UI: COMPARE GROUP ------
output$selected_group_box <- renderUI({
    req(input$comparison_select_group)
    lapply(input$comparison_select_group, function(x){
        box(x, width = "12")

    })
})

comparison_group_plots_status <- reactiveValues(
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
    first_party_open_claims_status = NULL
)

observeEvent(input$comparison_generate_group_stat_btn, {
    if("Written Premium" %in% input$comparison_select_group_statistic){
        comparison_group_plots_status$premium_written_status <- TRUE
    } else {
        comparison_group_plots_status$premium_written_status <- NULL
    }

    if("% Change in Written Premiums" %in% input$comparison_select_group_statistic){
        comparison_group_plots_status$yoy_change_premium_written_status <- TRUE
    } else {
        comparison_group_plots_status$yoy_change_premium_written_status <- NULL
    }

    if("Earned Premium" %in% input$comparison_select_group_statistic){
        comparison_group_plots_status$premium_earned_status <- TRUE
    } else {
        comparison_group_plots_status$premium_earned_status <- NULL
    }

    if("Total Losses" %in% input$comparison_select_group_statistic){
        comparison_group_plots_status$total_losses_status <- TRUE
    } else {
        comparison_group_plots_status$total_losses_status <- NULL
    }

    if("Loss Ratio" %in% input$comparison_select_group_statistic){
        comparison_group_plots_status$loss_ratio_status <- TRUE
    } else {
        comparison_group_plots_status$loss_ratio_status <- NULL
    }

    if("Number of Policies" %in% input$comparison_select_group_statistic){
        comparison_group_plots_status$num_of_policies_status <- TRUE
    } else {
        comparison_group_plots_status$num_of_policies_status <- NULL
    }

    if("Average Premium" %in% input$comparison_select_group_statistic){
        comparison_group_plots_status$avg_premium_status <- TRUE
    } else {
        comparison_group_plots_status$avg_premium_status <- NULL
    }

    if("Direct Defense Cost Containment Paid" %in% input$comparison_select_group_statistic){
        comparison_group_plots_status$direct_defense_status <- TRUE
    } else {
        comparison_group_plots_status$direct_defense_status <- NULL
    }

    if("Case Reserves" %in% input$comparison_select_group_statistic){
        comparison_group_plots_status$case_reserves_status <- TRUE
    } else {
        comparison_group_plots_status$case_reserves_status <- NULL
    }

    if("Total Claims Closed with Payment" %in% input$comparison_select_group_statistic){
        comparison_group_plots_status$total_claims_closed_with_payment_status <- TRUE
    } else {
        comparison_group_plots_status$total_claims_closed_with_payment_status <- NULL
    }

    if("Total Claims Closed without Payment" %in% input$comparison_select_group_statistic){
        comparison_group_plots_status$total_claims_closed_without_payment_status <- TRUE
    } else {
        comparison_group_plots_status$total_claims_closed_without_payment_status <- NULL
    }

    if("1st Party Claims Closed Without Payment" %in% input$comparison_select_group_statistic){
        comparison_group_plots_status$first_party_claims_closed_without_payment_status <- TRUE
    } else {
        comparison_group_plots_status$first_party_claims_closed_without_payment_status <- NULL
    }

    if("3rd Party Claims Closed Without Payment" %in% input$comparison_select_group_statistic){
        comparison_group_plots_status$third_party_claims_closed_without_payment_status <- TRUE
    } else {
        comparison_group_plots_status$third_party_claims_closed_without_payment_status <- NULL
    }

    if("1st Party Open Claims" %in% input$comparison_select_group_statistic){
        comparison_group_plots_status$first_party_open_claims_status <- TRUE
    } else {
        comparison_group_plots_status$first_party_open_claims_status <- NULL
    }

    if("3rd Party Open Claims" %in% input$comparison_select_group_statistic){
        comparison_group_plots_status$third_party_open_claims_status <- TRUE
    } else {
        comparison_group_plots_status$third_party_open_claims_status <- NULL
    }
})


### * DATA: COMPARE GROUP DATA--------

comparison_raw_group_data <- eventReactive(c(input$comparison_generate_group_stat_btn, input$group_year_range), {
    req(c(input$comparison_select_group, input$comparison_select_group_statistic))
    all_data %>%
        filter(groupname %in% input$comparison_select_group) %>%
        group_by(groupname, identifier) %>%
        complete(year = full_seq(2018:2022, 1)) %>%
        mutate(value = replace_na(value, 0)) %>%
        ungroup() %>%
        mutate(groupname = as.factor(groupname))
})

comparison_group_data_filtered <- eventReactive(c(input$comparison_generate_group_stat_btn, input$group_year_range), {
    comparison_raw_group_data() %>%
        filter(year >= input$group_year_range[1] & year <= input$group_year_range[2])
})

comparison_group_data_filtered_summarized <- eventReactive(c(input$comparison_generate_group_stat_btn, input$group_year_range), {
    req(c(input$comparison_select_group, input$comparison_select_group_statistic))
    comparison_group_data_filtered() %>%
        group_by(groupname, identifier, year) %>%
        summarize(value = sum(value)) %>%
        ungroup()

})

comparison_group_color_palette <- eventReactive(c(input$comparison_generate_group_stat_btn, input$group_year_range), {
    req(c(input$comparison_select_group, input$comparison_select_group_statistic))
    setNames(
        cividis(length(levels(comparison_group_data_filtered()$groupname)), alpha = 1, begin = 0, end = 1, direction = 1),
        # stata_pal("s2color")(length(levels(comparison_group_data_filtered()$groupname))),
        # pal_jama("default")(length(levels(comparison_group_data_filtered()$groupname))),
        levels(comparison_group_data_filtered()$groupname))
})

## YEAR SLIDER
output$comparison_year_range_group_ui <- renderUI({
    hidden(div(id = 'groupyearrange',
        sliderInput(
            inputId = "group_year_range",
            label = "",
            min = 2018,
            max = 2022,
            value = c(2018, 2022),
            step = 1,
            width = '20%',
            sep = ""
        )
        # br()
    )
    )
}
)

observeEvent(input$comparison_generate_group_stat_btn, {
    show('groupyearrange')
})


## * WRITTEN PREMIUM PLOT----

output$comparison_premium_written_group_ui <- renderUI({
    if (is.null(comparison_group_plots_status$premium_written_status)) return()
    fluidRow(
        br(),
        div(class = "plot-title", h4(style = 'display: inline-block;', "Premiums Written")),
        br(),
        plotlyOutput("comparison_written_premium_group_plot", width = '85%', height = "475px"),
        align = "center"
    )
})

comparison_group_written_premium_data <- eventReactive(c(input$comparison_generate_group_stat_btn, input$group_year_range), {
    req(c(input$comparison_select_group, input$comparison_select_group_statistic))
    comparison_group_data_filtered_summarized() %>%
        filter(identifier == "01")
})

comparison_group_written_premium_ggplot <- eventReactive(c(input$comparison_generate_group_stat_btn, input$group_year_range), {

    if(nrow(comparison_group_written_premium_data()) == 0){
        return(ggplot() + theme_void())
    } else{
    ggplot(comparison_group_written_premium_data(), aes(x = as.factor(year), y = value, fill = groupname, text = glue("<span><b>Value:</b> ${comma(value)}"))) +
        geom_bar(stat="identity", position="dodge") +
        scale_fill_manual(values = comparison_group_color_palette()) +
        scale_y_continuous(limits = c(0, max(comparison_group_written_premium_data()$value) * 1.3), labels = label_number(prefix = "  $", scale_cut = cut_short_scale()), expand = c(0.001,0)) +
        ylab("Written Premium (LI 01)") +
        universal_graph_theme_with_legend(0.15, 0.9)
    }
})

output$comparison_written_premium_group_plot <- renderPlotly({
    plotly <- ggplotly(comparison_group_written_premium_ggplot(), tooltip = "text", dynamicTicks = TRUE) %>%
        layout(legend = list(x = 0.01, y = 0.99, title = list(text=''), bgcolor = 'rgba(255,255,255,0.2)', font = list(size = 9), dynamicTicks = TRUE))  %>%
                layout(showlegend = TRUE,
                    yaxis = list(showline= T, linewidth=1.5, linecolor='black',
                        showticklabels = T,  tickprefix = "   $"),
                    xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2))

            plotly$x$layout$yaxis$autorange <- FALSE
            plotly$x$layout$yaxis$range <- c(0, max(comparison_group_written_premium_data()$value) * 1.3)
            plotly
})


## * YOY CHANGE WRITTEN PREMIUM ---------
#
output$comparison_yoy_change_premium_written_group_ui <- renderUI({
    if (is.null(comparison_group_plots_status$yoy_change_premium_written_status)) return()
    fluidRow(
        br(),
        div(class = "plot-title", h4(style = 'display: inline-block;', "% Change in Written Premiums")),
        br(),
        plotlyOutput("comparison_group_yoy_change_written_premiums_plot", width = '85%', height = "475px"),
        align = "center"
    )
})

comparison_group_yoy_change_written_premiums_data <- eventReactive(c(input$comparison_generate_group_stat_btn, input$group_year_range), {
    req(c(input$comparison_select_group, input$comparison_select_group_statistic))
    comparison_group_data_filtered_summarized() %>%
        filter(identifier == "01") %>%
        group_by(groupname) %>%
        mutate(value = na_if(value, 0),
            yoychange = (value/lag(value) - 1) * 100) %>%
        ungroup()
})

comparison_group_yoy_change_written_premiums_ggplot <- eventReactive(c(input$comparison_generate_group_stat_btn, input$group_year_range), {
    if(nrow(comparison_group_yoy_change_written_premiums_data()) == 0){
        return(ggplot() + theme_void())
    } else{
        ggplot(comparison_group_yoy_change_written_premiums_data(), aes(x = as.factor(year), y = yoychange, group = groupname, color = groupname, text = glue("<span><b>Value:</b> {round(yoychange, 2)}%"))) +
            geom_line(linewidth = 1.5) +
            scale_color_manual(values = comparison_group_color_palette()) +
            scale_fill_manual(values = comparison_group_color_palette()) +
            geom_point(aes(fill = groupname), shape=21, size=4, color = 'black') +
            scale_y_continuous(labels = label_number(suffix = "%", prefix = "")) +
            ylab("Percent") +
            universal_graph_theme_with_legend(0.15, 0.9)
    }
})


output$comparison_group_yoy_change_written_premiums_plot <- renderPlotly({
    plotly <- ggplotly(comparison_group_yoy_change_written_premiums_ggplot(), tooltip = "text", dynamicTicks = TRUE) %>%
        layout(legend = list(x = 0.01, y = 0.99, title = list(text=''), bgcolor = 'rgba(255,255,255,0.2)', font = list(size = 9), dynamicTicks = TRUE))  %>%
        layout(showlegend = TRUE,
            yaxis = list(showline= T, linewidth=1.5, linecolor='black',
                showticklabels = T,  tickprefix = "  ", ticksuffix = '%'),
            xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2)) %>%
        fix_legend_plotly()
})


## * EARNED PREMIUM PLOT----

output$comparison_premium_earned_group_ui <- renderUI({
    if (is.null(comparison_group_plots_status$premium_earned_status)) return()
    fluidRow(
        br(),
        div(class = "plot-title", h4(style = 'display: inline-block;', "Premiums Earned")),
        br(),
        plotlyOutput("comparison_earned_premium_group_plot", width = '85%', height = "475px"),
        align = "center"
    )
})


comparison_group_earned_premium_data <- eventReactive(c(input$comparison_generate_group_stat_btn, input$group_year_range), {
    req(c(input$comparison_select_group, input$comparison_select_group_statistic))
    comparison_group_data_filtered_summarized() %>%
        filter(identifier == "02")
})

comparison_group_earned_premium_ggplot <- eventReactive(c(input$comparison_generate_group_stat_btn, input$group_year_range), {
    if(nrow(comparison_group_earned_premium_data()) == 0){
        return(ggplot() + theme_void())
    } else{
    ggplot(comparison_group_earned_premium_data(), aes(x = as.factor(year), y = value, fill = groupname, text = glue("<span><b>Value:</b> ${comma(value)}"))) +
        geom_bar(stat="identity", position="dodge") +
        scale_fill_manual(values = comparison_group_color_palette()) +
        # scale_fill_viridis(discrete = T, option = "E", drop = F) +
        scale_y_continuous(limits = c(0, max(comparison_group_earned_premium_data()$value) *1.3 ), labels = label_number(prefix = " $", scale_cut = cut_short_scale()), expand = c(0.001,0)) +
        ylab("Earned Premium (LI 02)") +
        universal_graph_theme_with_legend(0.15, 0.9)
    }
})

output$comparison_earned_premium_group_plot <- renderPlotly({
    plotly <- ggplotly(comparison_group_earned_premium_ggplot(), tooltip = "text", dynamicTicks = TRUE) %>%
        layout(legend = list(x = 0.01, y = 0.99, title = list(text=''), bgcolor = 'rgba(255,255,255,0.2)', font = list(size = 9), dynamicTicks = TRUE))  %>%
        layout(showlegend = TRUE,
            yaxis = list(showline= T, linewidth=1.5, linecolor='black',
                showticklabels = T,  tickprefix = "  $"),
            xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2))

    plotly$x$layout$yaxis$autorange <- FALSE
    plotly$x$layout$yaxis$range <- c(0, max(comparison_group_earned_premium_data()$value) * 1.3)
    plotly
})

## * TOTAL LOSSES PLOT --------

output$comparison_total_losses_group_ui <- renderUI({
    if (is.null(comparison_group_plots_status$total_losses_status)) return()
    fluidRow(
        br(),
        div(class = "plot-title", h4(style = 'display: inline-block;', "Direct Losses Paid")),
        br(),
        plotlyOutput("comparison_total_losses_group_plot", width = '85%', height = "475px"),
        align = "center"
    )
})

comparison_group_total_losses_data <- eventReactive(c(input$comparison_generate_group_stat_btn, input$group_year_range), {
    req(c(input$comparison_select_group, input$comparison_select_group_statistic))
    comparison_group_data_filtered_summarized() %>%
        filter(identifier == "05")
})

comparison_group_total_losses_ggplot <- eventReactive(c(input$comparison_generate_group_stat_btn, input$group_year_range), {
    if(nrow(comparison_group_total_losses_data()) == 0){
        return(ggplot() + theme_void())
    } else{
    ggplot(comparison_group_total_losses_data(), aes(x = as.factor(year), y = value, fill = groupname, text = glue("<span><b>Value:</b> ${comma(value)}"))) +
        geom_bar(stat="identity", position="dodge") +
        scale_fill_manual(values = comparison_group_color_palette()) +
        # scale_fill_viridis(discrete = T, option = "E", drop = F) +
        scale_y_continuous(limits = c(0, max(comparison_group_total_losses_data()$value) * 1.3), labels = label_number(prefix = "  $", scale_cut = cut_short_scale()), expand = c(0.001,0)) +
        ylab("Losses Paid (LI 05)") +
        universal_graph_theme_with_legend(0.15, 0.9)
    }
})

output$comparison_total_losses_group_plot <- renderPlotly({
    if(length(comparison_group_total_losses_ggplot()$data) == 0){
        ggplotly(comparison_group_total_losses_ggplot())
    } else{
    plotly <- ggplotly(comparison_group_total_losses_ggplot(), tooltip = "text", dynamicTicks = TRUE) %>%
        layout(legend = list(x = 0.01, y = 0.99, title = list(text=''), bgcolor = 'rgba(255,255,255,0.2)', font = list(size = 9)),
            yaxis = list(showline= T, linewidth=1.5, linecolor='black', showticklabels = T,  tickprefix = "  $"),
            xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2))

    plotly$x$layout$yaxis$autorange <- FALSE
    plotly$x$layout$yaxis$range <- c(0, max(comparison_group_total_losses_data()$value) * 1.3)
    plotly
    }
})

## * LOSS RATIO PLOT-------

output$comparison_loss_ratio_group_ui <- renderUI({
    if (is.null(comparison_group_plots_status$loss_ratio_status)) return()
    fluidRow(
        br(),
        div(class = "plot-title", h4(style = 'display: inline-block;', "Loss Ratio")),
        br(),
        plotlyOutput("comparison_loss_ratio_group_plot", width = '85%', height = "475px"),
        align = "center"
    )
})

comparison_group_loss_ratio_data <- eventReactive(c(input$comparison_generate_group_stat_btn, input$group_year_range), {
    summarized_data <-  comparison_group_data_filtered() %>%
        group_by(groupname, year, identifier) %>%
        summarise(value = sum(value)) %>%
        ungroup()

    premium_affiliatedfminsco = summarized_data %>% filter(identifier == "01")
    losses_paid_affiliatedfminsco = summarized_data %>% filter(identifier == "05")

    left_join(losses_paid_affiliatedfminsco, premium_affiliatedfminsco,
        by = c("groupname", "year")) %>%
        select(groupname, year, value.x, value.y) %>%
        group_by(groupname, year) %>%
        summarize(value.x = sum(value.x), value.y = sum(value.y)) %>%
        mutate(value.x = na_if(value.x, 0), value.y = na_if(value.y, 0)) %>%
        mutate(lossratio = value.x / value.y)
})

comparison_group_loss_ratio_ggplot <- eventReactive(c(input$comparison_generate_group_stat_btn, input$group_year_range), {
    if(identical(comparison_group_loss_ratio_data(), numeric(0)) || nrow(comparison_group_loss_ratio_data()) == 0 || all(is.na(comparison_group_loss_ratio_data()$lossratio))){
        return(ggplot() + theme_void())
    } else {
    ggplot(comparison_group_loss_ratio_data(), aes(x = as.factor(year), y = lossratio, group = groupname, color = groupname, text = glue("<span><b>Loss Ratio:</b> {round(lossratio, 4)}"))) +
        geom_line(linewidth = 1.5) +
        scale_color_manual(values = comparison_group_color_palette()) +
        scale_fill_manual(values = comparison_group_color_palette()) +
        geom_point(aes(fill = groupname), shape=21, size=4, color = 'black') +
        scale_y_continuous(limits = c(0, max(comparison_group_loss_ratio_data()$lossratio, na.rm = T) * 1.2), labels = comma) +
        ylab("Loss Ratio") +
        universal_graph_theme_with_legend(0.15, 0.9)
    }
})

output$comparison_loss_ratio_group_plot <- renderPlotly({
    if(length(comparison_group_loss_ratio_ggplot()$data) == 0){
        ggplotly(comparison_group_loss_ratio_ggplot())
    } else{
    ggplotly(comparison_group_loss_ratio_ggplot(), tooltip = "text", dynamicTicks = TRUE) %>%
        layout(legend = list(x = 0.01, y = 0.99, title = list(text=''), bgcolor = 'rgba(255,255,255,0.2)', font = list(size = 9)),
            yaxis = list(showline= T, linewidth=1.5, linecolor='black', showticklabels = T, tickprefix = "  "),
            xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2)) %>%
        fix_legend_plotly()
    }

})


## * NUM OF POLICIES-------
output$comparison_num_of_policies_group_ui <- renderUI({
    if (is.null(comparison_group_plots_status$num_of_policies_status)) return()
    fluidRow(
        br(),
        div(class = "plot-title", h4(style = 'display: inline-block;', "Number of Policies")),
        br(),
        plotlyOutput("comparison_num_of_policies_group_plot", width = '85%', height = "475px"),
        align = "center"
    )
})

comparison_group_num_of_policies_data <- eventReactive(c(input$comparison_generate_group_stat_btn, input$group_year_range), {
    comparison_group_data_filtered_summarized() %>%
        filter(identifier == "11")
})


comparison_group_num_of_policies_ggplot <- eventReactive(c(input$comparison_generate_group_stat_btn, input$group_year_range), {
    if(nrow(comparison_group_num_of_policies_data()) == 0){
        return(ggplot() + theme_void())
    } else{
        ggplot(comparison_group_num_of_policies_data(), aes(x = as.factor(year), y = value, fill = groupname, text = glue("<span><b>Value:</b> {comma(value)}"))) +
            geom_bar(stat="identity", position="dodge") +
            scale_fill_manual(values = comparison_group_color_palette()) +
            scale_y_continuous(limits = c(0, max(comparison_group_num_of_policies_data()$value) *1.3 ), labels = label_number(prefix = " ", scale_cut = cut_short_scale()), expand = c(0.001,0)) +
            ylab("Total Policies in Force") +
            universal_graph_theme_with_legend(0.15, 0.9)
    }
})

output$comparison_num_of_policies_group_plot <- renderPlotly({
    if(length(comparison_group_num_of_policies_ggplot()$data) == 0){
        ggplotly(comparison_group_num_of_policies_ggplot())
    } else{
    plotly <- ggplotly(comparison_group_num_of_policies_ggplot(), tooltip = "text", dynamicTicks = TRUE) %>%
        layout(legend = list(x = 0.01, y = 0.99, title = list(text=''), bgcolor = 'rgba(255,255,255,0.2)', font = list(size = 9), dynamicTicks = TRUE))  %>%
        layout(showlegend = TRUE,
            yaxis = list(showline= T, linewidth=1.5, linecolor='black',
                showticklabels = T,  tickprefix = "  "),
            xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2))

    plotly$x$layout$yaxis$autorange <- FALSE
    plotly$x$layout$yaxis$range <- c(0, max(comparison_group_num_of_policies_data()$value) * 1.3)
    plotly
    }
})


## * AVG PREMIUM --------

output$comparison_avg_premium_group_ui <- renderUI({
    if (is.null(comparison_group_plots_status$avg_premium_status)) return()
    fluidRow(
        br(),
        div(class = "plot-title", h4(style = 'display: inline-block;', "Average Premium")),
        br(),
        plotlyOutput("comparison_avg_premium_group_plot", width = '85%', height = "475px"),
        align = "center"
    )
})

comparison_group_avg_premium_data <- eventReactive(c(input$comparison_generate_group_stat_btn, input$group_year_range), {
    summarized_data <- comparison_group_data_filtered_summarized()

    written_premium = summarized_data %>% filter(identifier == "01")
    num_of_policies = summarized_data %>% filter(identifier == "11")

    left_join(written_premium, num_of_policies,
        by = c("groupname", "year")) %>%
        select(groupname, year, value.x, value.y) %>%
        # group_by(groupname, year) %>%
        mutate(value.x = na_if(value.x, 0), value.y = na_if(value.y, 0)) %>%
        mutate(avg_premium = value.x / value.y)
})


comparison_group_avg_premium_ggplot <- eventReactive(c(input$comparison_generate_group_stat_btn, input$group_year_range), {
    if(nrow(comparison_group_avg_premium_data()) == 0){
        return(ggplot() + theme_void())
    } else{
        ggplot(comparison_group_avg_premium_data(), aes(x = as.factor(year), y = avg_premium, group = groupname, color = groupname, text = glue("<span><b>Value:</b> ${comma(avg_premium)}"))) +
            geom_line(linewidth = 1.5) +
            scale_color_manual(values = comparison_group_color_palette()) +
            scale_fill_manual(values = comparison_group_color_palette()) +
            geom_point(aes(fill = groupname), shape=21, size=4, color = 'black') +
            scale_y_continuous(limits = c(0, max(comparison_group_avg_premium_data()$avg_premium, na.rm = T) *1.3 ), labels = label_number(prefix = "  $", scale_cut = cut_short_scale()), expand = c(0.001,0)) +
            ylab("Average Premium") +
            universal_graph_theme_with_legend(0.15, 0.9)
    }
})

output$comparison_avg_premium_group_plot <- renderPlotly({
    if(length(comparison_group_avg_premium_ggplot()$data) == 0){
        ggplotly(comparison_group_avg_premium_ggplot())
    } else{
        plotly <- ggplotly(comparison_group_avg_premium_ggplot(), tooltip = "text", dynamicTicks = TRUE) %>%
            layout(legend = list(x = 0.01, y = 0.99, title = list(text=''), bgcolor = 'rgba(255,255,255,0.2)', font = list(size = 9), dynamicTicks = TRUE))  %>%
            layout(showlegend = TRUE,
                yaxis = list(showline= T, linewidth=1.5, linecolor='black',
                    showticklabels = T,  tickprefix = "  $"),
                xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2)) %>%
            fix_legend_plotly()
    }
})

## * DIRECT DEFENSE PAID ------------

output$comparison_direct_defense_group_ui <- renderUI({
    if (is.null(comparison_group_plots_status$direct_defense_status)) return()
    fluidRow(
        br(),
        div(class = "plot-title", h4(style = 'display: inline-block;', "Direct Defense Cost Containment Paid")),
        br(),
        plotlyOutput("comparison_direct_defense_group_plot", width = '85%', height = "475px"),
        align = "center"
    )
})

comparison_group_direct_defense_data <- eventReactive(c(input$comparison_generate_group_stat_btn, input$group_year_range), {
    comparison_group_data_filtered_summarized() %>%
        filter(identifier == "07")
})


comparison_group_direct_defense_ggplot <- eventReactive(c(input$comparison_generate_group_stat_btn, input$group_year_range), {
    if(nrow(comparison_group_direct_defense_data()) == 0){
        return(ggplot() + theme_void())
    } else{
        ggplot(comparison_group_direct_defense_data(), aes(x = as.factor(year), y = value, fill = groupname, text = glue("<span><b>Value:</b> {comma(value)}"))) +
            geom_bar(stat="identity", position="dodge") +
            scale_fill_manual(values = comparison_group_color_palette()) +
            scale_y_continuous(limits = c(0, max(comparison_group_direct_defense_data()$value) *1.3 ), labels = label_number(prefix = " ", scale_cut = cut_short_scale()), expand = c(0.001,0)) +
            ylab("Direct Defense Cost Containment Paid (LI 07)") +
            universal_graph_theme_with_legend(0.15, 0.9)
    }
})

output$comparison_direct_defense_group_plot <- renderPlotly({
    if(length(comparison_group_direct_defense_ggplot()$data) == 0){
        ggplotly(comparison_group_direct_defense_ggplot())
    } else{
        plotly <- ggplotly(comparison_group_direct_defense_ggplot(), tooltip = "text", dynamicTicks = TRUE) %>%
            layout(legend = list(x = 0.01, y = 0.99, title = list(text=''), bgcolor = 'rgba(255,255,255,0.2)', font = list(size = 9), dynamicTicks = TRUE))  %>%
            layout(showlegend = TRUE,
                yaxis = list(showline= T, linewidth=1.5, linecolor='black',
                    showticklabels = T,  tickprefix = "  "),
                xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2))

        plotly$x$layout$yaxis$autorange <- FALSE
        plotly$x$layout$yaxis$range <- c(0, max(comparison_group_direct_defense_data()$value) * 1.3)
        plotly
    }
})


## * CASE RESERVES GROUP ------------

output$comparison_case_reserves_group_ui <- renderUI({
    if (is.null(comparison_group_plots_status$case_reserves_status)) return()
    fluidRow(
        br(),
        div(class = "plot-title", h4(style = 'display: inline-block;', "Case Reserves")),
        br(),
        plotlyOutput("comparison_case_reserves_group_plot", width = '85%', height = "475px"),
        align = "center"
    )
})

comparison_group_case_reserves_data <- eventReactive(c(input$comparison_generate_group_stat_btn, input$group_year_range), {
    comparison_group_data_filtered_summarized() %>%
        filter(identifier == "08")
})

comparison_group_case_reserves_ggplot <- eventReactive(c(input$comparison_generate_group_stat_btn, input$group_year_range), {
    if(nrow(comparison_group_case_reserves_data()) == 0){
        return(ggplot() + theme_void())
    } else{
        ggplot(comparison_group_case_reserves_data(), aes(x = as.factor(year), y = value, fill = groupname, text = glue("<span><b>Value:</b> {comma(value)}"))) +
            geom_bar(stat="identity", position="dodge") +
            scale_fill_manual(values = comparison_group_color_palette()) +
            scale_y_continuous(limits = c(0, max(comparison_group_case_reserves_data()$value) *1.3 ), labels = label_number(prefix = " ", scale_cut = cut_short_scale()), expand = c(0.001,0)) +
            ylab("Case Reserves (LI 08)") +
            universal_graph_theme_with_legend(0.15, 0.9)
    }
})

output$comparison_case_reserves_group_plot <- renderPlotly({
    if(length(comparison_group_case_reserves_ggplot()$data) == 0){
        ggplotly(comparison_group_case_reserves_ggplot())
    } else{
        plotly <- ggplotly(comparison_group_case_reserves_ggplot(), tooltip = "text", dynamicTicks = TRUE) %>%
            layout(legend = list(x = 0.01, y = 0.99, title = list(text=''), bgcolor = 'rgba(255,255,255,0.2)', font = list(size = 9), dynamicTicks = TRUE))  %>%
            layout(showlegend = TRUE,
                yaxis = list(showline= T, linewidth=1.5, linecolor='black',
                    showticklabels = T,  tickprefix = "  "),
                xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2))

        plotly$x$layout$yaxis$autorange <- FALSE
        plotly$x$layout$yaxis$range <- c(0, max(comparison_group_case_reserves_data()$value) * 1.3)
        plotly
    }
})


## * CLAIMS CLOSED W PAYMENT ------------
output$comparison_total_claims_closed_with_payment_group_ui <- renderUI({
    if (is.null(comparison_group_plots_status$total_claims_closed_with_payment_status)) return()
    fluidRow(
        br(),
        div(class = "plot-title", h4(style = 'display: inline-block;', "Total Claims Closed with Payment")),
        br(),
        plotlyOutput("comparison_total_claims_closed_with_payment_group_plot", width = '85%', height = "475px"),
        align = "center"
    )
})

comparison_group_total_claims_closed_with_payment_data <- eventReactive(c(input$comparison_generate_group_stat_btn, input$group_year_range), {
    comparison_group_data_filtered_summarized() %>%
        filter(identifier == "20")
})

comparison_group_total_claims_closed_with_payment_ggplot <- eventReactive(c(input$comparison_generate_group_stat_btn, input$group_year_range), {
    if(nrow(comparison_group_total_claims_closed_with_payment_data()) == 0){
        return(ggplot() + theme_void())
    } else{
        ggplot(comparison_group_total_claims_closed_with_payment_data(), aes(x = as.factor(year), y = value, fill = groupname, text = glue("<span><b>Value:</b> {comma(value)}"))) +
            geom_bar(stat="identity", position="dodge") +
            scale_fill_manual(values = comparison_group_color_palette()) +
            scale_y_continuous(limits = c(0, max(comparison_group_total_claims_closed_with_payment_data()$value) *1.3 ), labels = label_number(prefix = " ", scale_cut = cut_short_scale()), expand = c(0.001,0)) +
            ylab("Total Claims Closed with Payment (LI 20)") +
            universal_graph_theme_with_legend(0.15, 0.9)
    }
})

output$comparison_total_claims_closed_with_payment_group_plot <- renderPlotly({
    if(length(comparison_group_total_claims_closed_with_payment_ggplot()$data) == 0){
        ggplotly(comparison_group_total_claims_closed_with_payment_ggplot())
    } else{
        plotly <- ggplotly(comparison_group_total_claims_closed_with_payment_ggplot(), tooltip = "text", dynamicTicks = TRUE) %>%
            layout(legend = list(x = 0.01, y = 0.99, title = list(text=''), bgcolor = 'rgba(255,255,255,0.2)', font = list(size = 9), dynamicTicks = TRUE))  %>%
            layout(showlegend = TRUE,
                yaxis = list(showline= T, linewidth=1.5, linecolor='black',
                    showticklabels = T,  tickprefix = "  "),
                xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2))

        plotly$x$layout$yaxis$autorange <- FALSE
        plotly$x$layout$yaxis$range <- c(0, max(comparison_group_total_claims_closed_with_payment_data()$value) * 1.3)
        plotly
    }
})


## * CLAIMS CLOSED without PAYMENT ------------
output$comparison_total_claims_closed_without_payment_group_ui <- renderUI({
    if (is.null(comparison_group_plots_status$total_claims_closed_without_payment_status)) return()
    fluidRow(
        br(),
        div(class = "plot-title", h4(style = 'display: inline-block;', "Total Claims Closed without Payment")),
        br(),
        plotlyOutput("comparison_total_claims_closed_without_payment_group_plot", width = '85%', height = "475px"),
        align = "center"
    )
})

comparison_group_total_claims_closed_without_payment_data <- eventReactive(c(input$comparison_generate_group_stat_btn, input$group_year_range), {
    comparison_group_data_filtered_summarized() %>%
        filter(identifier == "23")
})

comparison_group_total_claims_closed_without_payment_ggplot <- eventReactive(c(input$comparison_generate_group_stat_btn, input$group_year_range), {
    if(nrow(comparison_group_total_claims_closed_without_payment_data()) == 0){
        return(ggplot() + theme_void())
    } else{
        ggplot(comparison_group_total_claims_closed_without_payment_data(), aes(x = as.factor(year), y = value, fill = groupname, text = glue("<span><b>Value:</b> {comma(value)}"))) +
            geom_bar(stat="identity", position="dodge") +
            scale_fill_manual(values = comparison_group_color_palette()) +
            scale_y_continuous(limits = c(0, max(comparison_group_total_claims_closed_without_payment_data()$value) *1.3 ), labels = label_number(prefix = " ", scale_cut = cut_short_scale()), expand = c(0.001,0)) +
            ylab("Total Claims Closed without Payment (LI 23)") +
            universal_graph_theme_with_legend(0.15, 0.9)
    }
})

output$comparison_total_claims_closed_without_payment_group_plot <- renderPlotly({
    if(length(comparison_group_total_claims_closed_without_payment_ggplot()$data) == 0){
        ggplotly(comparison_group_total_claims_closed_without_payment_ggplot())
    } else{
        plotly <- ggplotly(comparison_group_total_claims_closed_without_payment_ggplot(), tooltip = "text", dynamicTicks = TRUE) %>%
            layout(legend = list(x = 0.01, y = 0.99, title = list(text=''), bgcolor = 'rgba(255,255,255,0.2)', font = list(size = 9), dynamicTicks = TRUE))  %>%
            layout(showlegend = TRUE,
                yaxis = list(showline= T, linewidth=1.5, linecolor='black',
                    showticklabels = T,  tickprefix = "  "),
                xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2))

        plotly$x$layout$yaxis$autorange <- FALSE
        plotly$x$layout$yaxis$range <- c(0, max(comparison_group_total_claims_closed_without_payment_data()$value) * 1.3)
        plotly
    }
})

## * 1st CLAIMS CLOSED WITHOUT PAYMENT ---------
output$comparison_first_party_claims_closed_without_payment_group_ui <- renderUI({
    if (is.null(comparison_group_plots_status$first_party_claims_closed_without_payment_status)) return()
    fluidRow(
        br(),
        div(class = "plot-title", h4(style = 'display: inline-block;', "First Party Claims Closed without Payment")),
        br(),
        plotlyOutput("comparison_first_party_claims_closed_without_payment_group_plot", width = '85%', height = "475px"),
        align = "center"
    )
})

comparison_group_first_party_claims_closed_without_payment_data <- eventReactive(c(input$comparison_generate_group_stat_btn, input$group_year_range), {
    comparison_group_data_filtered_summarized() %>%
        filter(identifier == "21")
})

comparison_group_first_party_claims_closed_without_payment_ggplot <- eventReactive(c(input$comparison_generate_group_stat_btn, input$group_year_range), {
    if(nrow(comparison_group_first_party_claims_closed_without_payment_data()) == 0){
        return(ggplot() + theme_void())
    } else{
        ggplot(comparison_group_first_party_claims_closed_without_payment_data(), aes(x = as.factor(year), y = value, fill = groupname, text = glue("<span><b>Value:</b> {comma(value)}"))) +
            geom_bar(stat="identity", position="dodge") +
            scale_fill_manual(values = comparison_group_color_palette()) +
            scale_y_continuous(limits = c(0, max(comparison_group_first_party_claims_closed_without_payment_data()$value) *1.3 ), labels = label_number(prefix = " ", scale_cut = cut_short_scale()), expand = c(0.001,0)) +
            ylab("First Party Claims Closed without payment (LI 21)") +
            universal_graph_theme_with_legend(0.15, 0.9)
    }
})

output$comparison_first_party_claims_closed_without_payment_group_plot <- renderPlotly({
    if(length(comparison_group_first_party_claims_closed_without_payment_ggplot()$data) == 0){
        ggplotly(comparison_group_first_party_claims_closed_without_payment_ggplot())
    } else{
        plotly <- ggplotly(comparison_group_first_party_claims_closed_without_payment_ggplot(), tooltip = "text", dynamicTicks = TRUE) %>%
            layout(legend = list(x = 0.01, y = 0.99, title = list(text=''), bgcolor = 'rgba(255,255,255,0.2)', font = list(size = 9), dynamicTicks = TRUE))  %>%
            layout(showlegend = TRUE,
                yaxis = list(showline= T, linewidth=1.5, linecolor='black',
                    showticklabels = T,  tickprefix = "  "),
                xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2))

        plotly$x$layout$yaxis$autorange <- FALSE
        plotly$x$layout$yaxis$range <- c(0, max(comparison_group_first_party_claims_closed_without_payment_data()$value) * 1.3)
        plotly
    }
})


## * 3rd CLAIMS CLOSED WITHOUT PAYMENT ---------
output$comparison_third_party_claims_closed_without_payment_group_ui <- renderUI({
    if (is.null(comparison_group_plots_status$third_party_claims_closed_without_payment_status)) return()
    fluidRow(
        br(),
        div(class = "plot-title", h4(style = 'display: inline-block;', "Third Party Claims Closed without Payment")),
        br(),
        plotlyOutput("comparison_third_party_claims_closed_without_payment_group_plot", width = '85%', height = "475px"),
        align = "center"
    )
})

comparison_group_third_party_claims_closed_without_payment_data <- eventReactive(c(input$comparison_generate_group_stat_btn, input$group_year_range), {
    comparison_group_data_filtered_summarized() %>%
        filter(identifier == "22")
})

comparison_group_third_party_claims_closed_without_payment_ggplot <- eventReactive(c(input$comparison_generate_group_stat_btn, input$group_year_range), {
    if(nrow(comparison_group_third_party_claims_closed_without_payment_data()) == 0){
        return(ggplot() + theme_void())
    } else{
        ggplot(comparison_group_third_party_claims_closed_without_payment_data(), aes(x = as.factor(year), y = value, fill = groupname, text = glue("<span><b>Value:</b> {comma(value)}"))) +
            geom_bar(stat="identity", position="dodge") +
            scale_fill_manual(values = comparison_group_color_palette()) +
            scale_y_continuous(limits = c(0, max(comparison_group_third_party_claims_closed_without_payment_data()$value) *1.3 ), labels = label_number(prefix = " ", scale_cut = cut_short_scale()), expand = c(0.001,0)) +
            ylab("Third Party Claims Closed without payment (LI 22)") +
            universal_graph_theme_with_legend(0.15, 0.9)
    }
})

output$comparison_third_party_claims_closed_without_payment_group_plot <- renderPlotly({
    if(length(comparison_group_third_party_claims_closed_without_payment_ggplot()$data) == 0){
        ggplotly(comparison_group_third_party_claims_closed_without_payment_ggplot())
    } else{
        plotly <- ggplotly(comparison_group_third_party_claims_closed_without_payment_ggplot(), tooltip = "text", dynamicTicks = TRUE) %>%
            layout(legend = list(x = 0.01, y = 0.99, title = list(text=''), bgcolor = 'rgba(255,255,255,0.2)', font = list(size = 9), dynamicTicks = TRUE))  %>%
            layout(showlegend = TRUE,
                yaxis = list(showline= T, linewidth=1.5, linecolor='black',
                    showticklabels = T,  tickprefix = "  "),
                xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2))

        plotly$x$layout$yaxis$autorange <- FALSE
        plotly$x$layout$yaxis$range <- c(0, max(comparison_group_third_party_claims_closed_without_payment_data()$value) * 1.3)
        plotly
    }
})


## * 1st OPEN CLAIMS ---------
output$comparison_first_party_open_claims_group_ui <- renderUI({
    if (is.null(comparison_group_plots_status$first_party_open_claims_status)) return()
    fluidRow(
        br(),
        div(class = "plot-title", h4(style = 'display: inline-block;', "First Party Open Claims")),
        br(),
        plotlyOutput("comparison_first_party_open_claims_group_plot", width = '85%', height = "475px"),
        align = "center"
    )
})

comparison_group_first_party_open_claims_data <- eventReactive(c(input$comparison_generate_group_stat_btn, input$group_year_range), {
    comparison_group_data_filtered_summarized() %>%
        filter(identifier == "15")
})

comparison_group_first_party_open_claims_ggplot <- eventReactive(c(input$comparison_generate_group_stat_btn, input$group_year_range), {
    if(nrow(comparison_group_first_party_open_claims_data()) == 0){
        return(ggplot() + theme_void())
    } else{
        ggplot(comparison_group_first_party_open_claims_data(), aes(x = as.factor(year), y = value, fill = groupname, text = glue("<span><b>Value:</b> {comma(value)}"))) +
            geom_bar(stat="identity", position="dodge") +
            scale_fill_manual(values = comparison_group_color_palette()) +
            scale_y_continuous(limits = c(0, max(comparison_group_first_party_open_claims_data()$value) *1.3 ), labels = label_number(prefix = " ", scale_cut = cut_short_scale()), expand = c(0.001,0)) +
            ylab("First Party Open Claims (LI 15)") +
            universal_graph_theme_with_legend(0.15, 0.9)
    }
})

output$comparison_first_party_open_claims_group_plot <- renderPlotly({
    if(length(comparison_group_first_party_open_claims_ggplot()$data) == 0){
        ggplotly(comparison_group_first_party_open_claims_ggplot())
    } else{
        plotly <- ggplotly(comparison_group_first_party_open_claims_ggplot(), tooltip = "text", dynamicTicks = TRUE) %>%
            layout(legend = list(x = 0.01, y = 0.99, title = list(text=''), bgcolor = 'rgba(255,255,255,0.2)', font = list(size = 9), dynamicTicks = TRUE))  %>%
            layout(showlegend = TRUE,
                yaxis = list(showline= T, linewidth=1.5, linecolor='black',
                    showticklabels = T,  tickprefix = "  "),
                xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2))

        plotly$x$layout$yaxis$autorange <- FALSE
        plotly$x$layout$yaxis$range <- c(0, max(comparison_group_first_party_open_claims_data()$value) * 1.3)
        plotly
    }
})

## * 3rd OPEN CLAIMS ---------
output$comparison_third_party_open_claims_group_ui <- renderUI({
    if (is.null(comparison_group_plots_status$third_party_open_claims_status)) return()
    fluidRow(
        br(),
        div(class = "plot-title", h4(style = 'display: inline-block;', "Third Party Open Claims")),
        br(),
        plotlyOutput("comparison_third_party_open_claims_group_plot", width = '85%', height = "475px"),
        align = "center"
    )
})

comparison_group_third_party_open_claims_data <- eventReactive(c(input$comparison_generate_group_stat_btn, input$group_year_range), {
    comparison_group_data_filtered_summarized() %>%
        filter(identifier == "16")
})

comparison_group_third_party_open_claims_ggplot <- eventReactive(c(input$comparison_generate_group_stat_btn, input$group_year_range), {
    if(nrow(comparison_group_third_party_open_claims_data()) == 0){
        return(ggplot() + theme_void())
    } else{
        ggplot(comparison_group_third_party_open_claims_data(), aes(x = as.factor(year), y = value, fill = groupname, text = glue("<span><b>Value:</b> {comma(value)}"))) +
            geom_bar(stat="identity", position="dodge") +
            scale_fill_manual(values = comparison_group_color_palette()) +
            scale_y_continuous(limits = c(0, max(comparison_group_third_party_open_claims_data()$value) *1.3 ), labels = label_number(prefix = " ", scale_cut = cut_short_scale()), expand = c(0.001,0)) +
            ylab("Third Party Open Claims (LI 16)") +
            universal_graph_theme_with_legend(0.15, 0.9)
    }
})

output$comparison_third_party_open_claims_group_plot <- renderPlotly({
    if(length(comparison_group_third_party_open_claims_ggplot()$data) == 0){
        ggplotly(comparison_group_third_party_open_claims_ggplot())
    } else{
        plotly <- ggplotly(comparison_group_third_party_open_claims_ggplot(), tooltip = "text", dynamicTicks = TRUE) %>%
            layout(legend = list(x = 0.01, y = 0.99, title = list(text=''), bgcolor = 'rgba(255,255,255,0.2)', font = list(size = 9), dynamicTicks = TRUE))  %>%
            layout(showlegend = TRUE,
                yaxis = list(showline= T, linewidth=1.5, linecolor='black',
                    showticklabels = T,  tickprefix = "  "),
                xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2))

        plotly$x$layout$yaxis$autorange <- FALSE
        plotly$x$layout$yaxis$range <- c(0, max(comparison_group_third_party_open_claims_data()$value) * 1.3)
        plotly
    }
})


###### # COMPARE FIRM ---------

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

observeEvent(input$generate_compare_firm, {
    runjs("$('#firm_stat_modal').modal('hide')")

})


cf_plot <- reactiveValues(
    premium_written_status = NULL,
    premium_earned_status = NULL,
    total_losses_status = NULL,
    loss_ratio_status = NULL
)

observeEvent(input$generate_compare_firm, {
    if("Written Premium" %in% input$comparison_select_firm_statistic){
        cf_plot$premium_written_status <- TRUE
    } else {
        cf_plot$premium_written_status <- NULL
    }

    if("Earned Premium" %in% input$comparison_select_firm_statistic){
        cf_plot$premium_earned_status <- TRUE
    } else {
        cf_plot$premium_earned_status <- NULL
    }

    if("Total Losses" %in% input$comparison_select_firm_statistic){
        cf_plot$total_losses_status <- TRUE
    } else {
        cf_plot$total_losses_status <- NULL
    }

    if("Loss Ratio" %in% input$comparison_select_firm_statistic){
        cf_plot$loss_ratio_status <- TRUE
    } else {
        cf_plot$loss_ratio_status <- NULL
    }

})

output$comparison_premium_written_firm_ui <- renderUI({
    if (is.null(cf_plot$premium_written_status)) return()
    fluidRow(
        br(),
        div(class = "plot-title", h4(style = 'display: inline-block;', "Premiums Written")),
        br(),
        plotlyOutput("written_premium_firm_plot", width = '85%', height = "475px"),
        align = "center"
    )
})

output$comparison_earned_written_firm_ui <- renderUI({
    if (is.null(cf_plot$premium_earned_status)) return()
    fluidRow(
        br(),
        div(class = "plot-title", h4(style = 'display: inline-block;', "Premiums Earned")),
        br(),
        plotlyOutput("earned_premium_firm_plot", width = '85%', height = "475px"),
        align = "center"
    )
})

output$comparison_total_losses_firm_ui <- renderUI({
    if (is.null(cf_plot$total_losses_status)) return()
    fluidRow(
        br(),
        div(class = "plot-title", h4(style = 'display: inline-block;', "Total Losses")),
        br(),
        plotlyOutput("total_losses_firm_plot", width = '85%', height = "475px"),
        align = "center"
    )
})

output$comparison_loss_ratio_firm_ui <- renderUI({
    if (is.null(cf_plot$loss_ratio_status)) return()
    fluidRow(
        br(),
        div(class = "plot-title", h4(style = 'display: inline-block;', "Loss Ratio")),
        br(),
        plotlyOutput("loss_ratio_firm_plot", width = '85%', height = "475px"),
        align = "center"
    )
})


raw_firm_data <- reactive({
    req(input$select_firm_compare)
    data <- all_data %>%
        filter(shortname %in% input$select_firm_compare) %>%
        mutate(shortname = as.factor(shortname))
})


firm_data_year_range <- reactive({
    req(input$select_firm_compare)
    raw_firm_data()$year
})


firm_data <- reactive({
    req(input$select_firm_compare)
    data <- raw_firm_data() %>%
        filter(year >= input$firm_year_range[1] & year <= input$firm_year_range[2])

})


color_palette_firm <- reactive({
    req(input$select_firm_compare)
    palette <- setNames(
        cividis(length(levels(firm_data()$shortname)), alpha = 1, begin = 0, end = 1, direction = 1),
        levels(firm_data()$shortname))

})

## YEAR SLIDER
output$comparison_year_range_firm_ui <- renderUI({
    hidden(div(id = 'firmyearrange',
        sliderInput(
            inputId = "firm_year_range",
            label = "",
            min = min(firm_data_year_range()),
            max = max(firm_data_year_range()),
            value = c(min(firm_data_year_range()), max(firm_data_year_range())),
            step = 1,
            width = '20%',
            sep = ""
        )
        # br()
    )
    )
}
)

observeEvent(input$generate_compare_firm, {
    show('firmyearrange')
})


## WRITTEN PREMIUM PLOT
written_premium_firm_data <- reactive({
    req(c(input$select_firm_compare, input$comparison_select_firm_statistic))
    data <- firm_data() %>%
        filter(identifier == "01") %>%
        group_by(shortname, year) %>%
        summarize(value = sum(value))
})

written_premium_firm_ggplot <- eventReactive(c(input$generate_compare_firm, input$firm_year_range), {
    group_1 <- ggplot(written_premium_firm_data(), aes(x = year, y = value, fill = shortname, text = glue("<span><b>Value:</b> ${comma(value)}"))) +
        geom_bar(stat="identity", position="dodge") +
        scale_fill_manual(values = color_palette_firm()) +
        # scale_fill_viridis(discrete = T, option = "E", drop = F) +
        scale_y_continuous(limits = c(0, max(written_premium_firm_data()$value) + (max(written_premium_firm_data()$value) * 0.3)), labels = label_number(prefix = "$", scale_cut = cut_short_scale()), expand = c(0.001,0)) +
        theme_economist() +
        theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
            legend.title=element_blank(),
            legend.position = c(0.15, 0.9), axis.title.y = element_blank(),
            axis.title.x = element_blank(), axis.text = element_text(size=11),
            panel.grid.major.y = element_line(colour = "grey"),
            axis.line.y.left = element_line(linewidth = 0.5, colour = "black"),
            axis.line.x.bottom = element_line(linewidth = 0.5))
})

output$written_premium_firm_plot <- renderPlotly({
    ggplotly(written_premium_firm_ggplot(), tooltip = "text") %>%
        layout(legend = list(x = 0.01, y = 0.99, title = list(text=''), bgcolor = 'rgba(255,255,255,0.2)', font = list(size = 9)),
            yaxis = list(showline= T, linewidth=1.5, linecolor='black', showticklabels = T),
            xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2))


})

## EARNED PREMIUM PLOT
earned_premium_firm_data <- reactive({
    req(c(input$select_firm_compare, input$comparison_select_firm_statistic))
    data <- firm_data() %>%
        filter(identifier == "02") %>%
        group_by(shortname, year) %>%
        summarize(value = sum(value))
})

earned_premium_firm_ggplot <- eventReactive(c(input$generate_compare_firm, input$firm_year_range), {
    group_2 <- ggplot(earned_premium_firm_data(), aes(x = year, y = value, fill = shortname, text = glue("<span><b>Value:</b> ${comma(value)}"))) +
        geom_bar(stat="identity", position="dodge") +
        scale_fill_manual(values = color_palette_firm()) +
        # scale_fill_viridis(discrete = T, option = "E", drop = F) +
        scale_y_continuous(limits = c(0, max(earned_premium_firm_data()$value) + (max(earned_premium_firm_data()$value) * 0.3)), labels = label_number(prefix = "$", scale_cut = cut_short_scale()), expand = c(0.001,0)) +
        theme_economist() +
        theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
            legend.title=element_blank(),
            legend.position = c(0.15, 0.9), axis.title.y = element_blank(),
            axis.title.x = element_blank(), axis.text = element_text(size=11),
            panel.grid.major.y = element_line(colour = "grey"),
            axis.line.y.left = element_line(linewidth = 0.5, colour = "black"),
            axis.line.x.bottom = element_line(linewidth = 0.5))
})

output$earned_premium_firm_plot <- renderPlotly({
    ggplotly(earned_premium_firm_ggplot(), tooltip = "text") %>%
        layout(legend = list(x = 0.01, y = 0.99, title = list(text=''), bgcolor = 'rgba(255,255,255,0.2)', font = list(size = 9)),
            yaxis = list(showline= T, linewidth=1.5, linecolor='black', showticklabels = T),
            xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2))


})



## TOTAL LOSSES PLOT
total_losses_firm_data <- reactive({
    req(c(input$select_firm_compare, input$comparison_select_firm_statistic))
    data <- firm_data() %>%
        filter(identifier == "05") %>%
        group_by(shortname, year) %>%
        summarize(value = sum(value))
})

total_losses_firm_ggplot <- eventReactive(c(input$generate_compare_firm, input$firm_year_range), {
    group_3 <- ggplot(total_losses_firm_data(), aes(x = year, y = value, fill = shortname, text = glue("<span><b>Value:</b> ${comma(value)}"))) +
        geom_bar(stat="identity", position="dodge") +
        scale_fill_manual(values = color_palette_firm()) +
        # scale_fill_viridis(discrete = T, option = "E", drop = F) +
        scale_y_continuous(limits = c(0, max(total_losses_firm_data()$value) + (max(total_losses_firm_data()$value) * 0.3)), labels = label_number(prefix = "$", scale_cut = cut_short_scale()), expand = c(0.001,0)) +
        scale_x_continuous(breaks = seq(min(firm_data()$year), max(firm_data()$year), 1), limits = c(min(firm_data()$year) - 0.5, max(firm_data()$year) + 0.5)) +
        theme_economist() +
        theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
            legend.title=element_blank(),
            legend.position = c(0.15, 0.9), axis.title.y = element_blank(),
            axis.title.x = element_blank(), axis.text = element_text(size=11),
            panel.grid.major.y = element_line(colour = "grey"),
            axis.line.y.left = element_line(linewidth = 0.5, colour = "black"),
            axis.line.x.bottom = element_line(linewidth = 0.5))
})

output$total_losses_firm_plot <- renderPlotly({
    ggplotly(total_losses_firm_ggplot(), tooltip = "text") %>%
        layout(legend = list(x = 0.01, y = 0.99, title = list(text=''), bgcolor = 'rgba(255,255,255,0.2)', font = list(size = 9)),
            yaxis = list(showline= T, linewidth=1.5, linecolor='black', showticklabels = T),
            xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2))
})

## LOSS RATIO PLOT
loss_ratio_firm_data <- reactive({
    req(c(input$select_firm_compare, input$comparison_select_firm_statistic))
    premium_affiliatedfminsco = firm_data() %>% filter(identifier == "01")
    losses_paid_affiliatedfminsco = firm_data() %>% filter(identifier == "05")

    new_loss_ratio_firm <- left_join(losses_paid_affiliatedfminsco, premium_affiliatedfminsco,
        by = c("shortname","fullname", "groupname", "year")) %>%
        select(shortname, fullname, groupname, year, value.x, value.y) %>%
        group_by(shortname, year) %>%
        summarize(value.x = sum(value.x), value.y = sum(value.y)) %>%
        mutate(lossratio = value.x / value.y)
})

loss_ratio_firm_ggplot <- eventReactive(c(input$generate_compare_firm, input$firm_year_range), {
    group_4 <- ggplot(loss_ratio_firm_data()[!is.na(loss_ratio_firm_data()$lossratio), ], aes(x = year, y = lossratio, group = shortname, color = shortname, text = glue("<span><b>Loss Ratio:</b> {round(lossratio, 4)}"))) +
        geom_line(linewidth = 1.5) +
        scale_color_manual(values = color_palette_firm()) +
        scale_fill_manual(values = color_palette_firm()) +
        geom_point(aes(fill = shortname), shape=21, size=4, color = 'black') +
        geom_label_repel(aes(label= comma(lossratio)), color = 'black') +
        scale_y_continuous(limits = c(0, max(loss_ratio_firm_data()$lossratio, na.rm = T) * 1.2), labels = comma) +
        expand_limits(x = c(min(firm_data()$year) - 0.5, max(firm_data()$year) + 0.5)) +
        scale_x_continuous(breaks = seq(min(firm_data()$year), max(firm_data()$year), 1)) +
        theme_economist() +
        theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(), axis.text = element_text(size=11),
            legend.position = c(0.15, 0.9), legend.title=element_blank(),
            panel.grid.major.y = element_line(colour = "grey"),
            axis.line.y.left = element_line(linewidth = 0.5, colour = "black"),
            axis.line.x.bottom = element_line(linewidth = 0.5))
})

output$loss_ratio_firm_plot <- renderPlotly({
    ggplotly(loss_ratio_firm_ggplot(), tooltip = "text") %>%
        layout(legend = list(x = 0.01, y = 0.99, title = list(text=''), bgcolor = 'rgba(255,255,255,0.2)', font = list(size = 9)),
            yaxis = list(showline= T, linewidth=1.5, linecolor='black', showticklabels = T),
            xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2)) %>%
        fix_legend_plotly()


})
