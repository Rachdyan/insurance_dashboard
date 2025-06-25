## DYNAMIC SELECT FIRM/GROUP ----------

observe({
  req(input$single_graph_box)
  print(input$single_graph_box)
  if(input$single_graph_box == "Single Firm Graph"){
    show("single_firm_input_ui_div")
    hide("single_group_input_ui_div")
  }
  else{
    show("single_group_input_ui_div")
    hide("single_firm_input_ui_div")
  }
})


observeEvent(input$select_single_group_export_type, {
  if(input$select_single_group_export_type == "PDF"){
    show("single_group_pdf_div")
    hide("single_group_excel_div")
  }
  else{
    show("single_group_excel_div")
    hide("single_group_pdf_div")
  }
})



## SINGLE GROUP -----
output$selected_single_group_name <- renderUI({
  req(input$select_single_group)
  div(
    style = "text-align: center;",
    h2(input$select_single_group)
  )
})

selected_single_group_data <- reactive({
  req(input$select_single_group)
  all_data %>%
    filter(groupname == input$select_single_group) %>%
    group_by(groupname,shortname, identifier) %>%
    complete(year = full_seq(2018:2022, 1)) %>%
    mutate(value = replace_na(value, 0)) %>%
    ungroup()
})

selected_single_group_data_filtered <- reactive({
  req(c(input$select_single_group, input$single_group_year_range))
  selected_single_group_data() %>%
    filter(year >= input$single_group_year_range[1] & year <= input$single_group_year_range[2])
})

selected_single_group_data_filtered_summarized <- reactive({
  req(c(input$select_single_group, input$single_group_year_range))
  selected_single_group_data_filtered() %>%
    group_by(groupname, identifier, year) %>%
    summarize(value = sum(value)) %>%
    ungroup()
})


## * WRITTEN PREMIUMS SINGLE GROUP ------

## ** table-----
written_premiums_single_group_data <- reactive({
  selected_single_group_data_filtered_summarized() %>%
    filter(identifier == "01")
})

written_premiums_single_group_export_table <- reactive({
  if(nrow(written_premiums_single_group_data()) == 0){
    zeroGrob()
  } else {
    written_premiums_single_group_data() %>%
      mutate(col_names = paste0("Written Premiums ", seq(min(year), max(year), 1))) %>%
      select(groupname, value, col_names) %>%
      mutate(value = ifelse(!is.na(value), glue("${comma(value)}"), "")) %>%
      rename(`Group Name` = groupname) %>%
      pivot_wider(names_from = col_names, values_from = value) %>%
      table_to_grob()
  }
})

written_premiums_single_group_export_table_excel <- reactive({
  if(nrow(written_premiums_single_group_data()) == 0){
    tibble()
  } else {
    written_premiums_single_group_data() %>%
      select(groupname, value, year) %>%
      rename(`Group Name` = groupname) %>%
      pivot_wider(names_from = year, values_from = value)
  }
})

## ** graph-----
written_premiums_single_group_ggplot <- reactive({
  graph_data <- written_premiums_single_group_data()

  if(identical(graph_data, numeric(0)) || nrow(graph_data) == 0){
    return(ggplot() + theme_void())
  }

  ggplot(graph_data, aes(x = as.factor(year), y = value, fill = groupname, text = glue("<span><b>Value:</b> ${comma(value)}"))) +
    geom_bar(stat="identity", position="dodge", fill='#337ab7') +
    scale_y_continuous(limits = c(0, max(graph_data$value) * 1.2), labels = label_number(prefix = " $", scale_cut = cut_short_scale()), expand = c(0.001,0)) +
    # scale_x_continuous(breaks = seq(min(graph_data$year), max(graph_data$year), 1)) +
    ylab("Written Premium (LI 01)") +
    universal_graph_theme
})


written_premiums_single_group_plotly <- reactive({
  suppressWarnings({
    if(length(written_premiums_single_group_ggplot()$data) == 0){
      ggplotly(written_premiums_single_group_ggplot())
    } else{
      plotly <- ggplotly(written_premiums_single_group_ggplot(), tooltip = "text", dynamicTicks = TRUE)  %>%
        layout(showlegend = FALSE,
          yaxis = list(showline= T, linewidth=1.5, linecolor='black',
            showticklabels = T,  tickprefix = " $"),
          xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2))

      plotly$x$layout$yaxis$autorange <- FALSE
      plotly$x$layout$yaxis$range <- c(0, max(written_premiums_single_group_data()$value) * 1.2)
      plotly
    }
  })
})

written_premiums_single_group_export <- reactive({
  written_premiums_single_group_ggplot() +
    ggtitle("Written Premiums (Quantified)") +
    single_graph_export_theme
})

output$written_premiums_single_group_plot <- renderPlotly(
  written_premiums_single_group_plotly()
)


## * YOY CHANGE WRITTEN PREMIUMS SINGLE GROUP ------

## ** table-----
yoy_change_written_premiums_single_group_data <- reactive({
  selected_single_group_data_filtered_summarized() %>%
    filter(identifier == "01") %>%
    mutate(value = na_if(value, 0),
      yoychange = (value/lag(value) - 1) * 100)
})

yoy_change_written_premiums_single_group_export_table <- reactive({
  if(nrow(yoy_change_written_premiums_single_group_data()) == 0){
    zeroGrob()
  } else {
    yoy_change_written_premiums_single_group_data() %>%
      mutate(col_names = paste0("YOY % Change ", seq(min(year), max(year), 1))) %>%
      select(groupname, yoychange, col_names) %>%
      mutate(yoychange = ifelse(!is.na(yoychange), glue("{round(yoychange, 2)}%"), "")) %>%
      rename(`Group Name` = groupname) %>%
      pivot_wider(names_from = col_names, values_from = yoychange) %>%
      table_to_grob()
  }
})

yoy_change_written_premiums_single_group_export_table_excel <- reactive({
  if(nrow(yoy_change_written_premiums_single_group_data()) == 0){
    tibble()
  } else {
    yoy_change_written_premiums_single_group_data() %>%
      select(groupname, yoychange, year) %>%
      rename(`Group Name` = groupname) %>%
      pivot_wider(names_from = year, values_from = yoychange)
  }
})

## ** graph-----
yoy_change_written_premiums_single_group_ggplot <- reactive({

  graph_data = yoy_change_written_premiums_single_group_data()
  if(identical(graph_data, numeric(0)) || nrow(graph_data) == 0){
    return(ggplot() + theme_void())
  }

  ggplot(graph_data, aes(x = as.factor(year), y = yoychange,  text = glue("<span style='color:white'><b>% Change:</b> {round(yoychange, 2)}%"))) +
    geom_line(linewidth = 1.5, color = '#337ab7', aes(group = 1)) +
    geom_point(color='black', fill='#337ab7', shape=21, size=4) +
    scale_y_continuous(labels = label_number(suffix = "%", prefix = " ")) +
    ylab("Percent") +
    universal_graph_theme
})



yoy_change_written_premiums_single_group_plotly <- reactive({
  suppressWarnings({
    if(length(yoy_change_written_premiums_single_group_ggplot()$data) == 0){
      ggplotly(yoy_change_written_premiums_single_group_ggplot())
    } else{
      ggplotly(yoy_change_written_premiums_single_group_ggplot(), tooltip = "text", dynamicTicks = TRUE)  %>%
        layout(showlegend = FALSE,
          yaxis = list(showline= T, linewidth=1.5, linecolor='black', showticklabels = T,  tickprefix = "  ", ticksuffix = '%'),
          xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2))
    }
  })
})

yoy_change_written_premiums_single_group_export <- reactive({
  yoy_change_written_premiums_single_group_ggplot() +
    ggtitle("% Change in Written Premiums") +
    single_graph_export_theme
})

output$yoy_change_written_premiums_single_group_plot <- renderPlotly(
  yoy_change_written_premiums_single_group_plotly()
)


## * EARNED PREMIUMS SINGLE GROUP -----
## ** table ------------
earned_premiums_single_group_data <- reactive({
  selected_single_group_data_filtered_summarized() %>%
    filter(identifier == "02")
})

earned_premiums_single_group_export_table <- reactive({
  if(nrow(earned_premiums_single_group_data()) == 0){
    zeroGrob()
  } else {
    earned_premiums_single_group_data() %>%
      mutate(col_names = paste0("Earned Premiums ", seq(min(year), max(year), 1))) %>%
      select(groupname, value, col_names) %>%
      mutate(value = ifelse(!is.na(value), glue("${comma(value)}"), "")) %>%
      rename(`Group Name` = groupname) %>%
      pivot_wider(names_from = col_names, values_from = value) %>%
      table_to_grob()
  }
})

earned_premiums_single_group_export_table_excel <- reactive({
  if(nrow(earned_premiums_single_group_data()) == 0){
    tibble()
  } else {
    earned_premiums_single_group_data() %>%
      select(groupname, value, year) %>%
      rename(`Group Name` = groupname) %>%
      pivot_wider(names_from = year, values_from = value)
  }
})

## ** graph ------------
earned_premiums_single_group_ggplot <- reactive({
  graph_data <- earned_premiums_single_group_data()

  if(nrow(graph_data) == 0){
    return(ggplot() + theme_void())
  }

  ggplot(graph_data, aes(x = as.factor(year), y = value, fill = groupname, text = glue("<span><b>Value:</b> ${comma(value)}"))) +
    geom_bar(stat="identity", position="dodge", fill='#B3730E') +
    scale_y_continuous(limits = c(0, max(graph_data$value) * 1.2), labels = label_number(prefix = " $", scale_cut = cut_short_scale()), expand = c(0.001,0)) +
    # scale_x_continuous(breaks = seq(min(graph_data$year), max(graph_data$year), 1)) +
    ylab("Earned Premium (LI 02)") +
    universal_graph_theme

})

earned_premiums_single_group_plotly <- reactive({
  suppressWarnings(
    if(length(earned_premiums_single_group_ggplot()$data) == 0){
      ggplotly(earned_premiums_single_group_ggplot())
    } else{
      plotly <- ggplotly(earned_premiums_single_group_ggplot(), tooltip = "text", dynamicTicks = TRUE) %>%
        layout(showlegend = FALSE,
          yaxis = list(showline= T, linewidth=1.5, linecolor='black',
            showticklabels = T,  tickprefix = "  $"),
          xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2))
      plotly$x$layout$yaxis$autorange <- FALSE
      plotly$x$layout$yaxis$range <- c(0, max(earned_premiums_single_group_data()$value) * 1.2)
      plotly
    }
  )
})

earned_premiums_single_group_export <- reactive({
  earned_premiums_single_group_ggplot() +
    ggtitle("Earned Premiums (Quantified)") +
    single_graph_export_theme
})

output$earned_premiums_single_group_plot <- renderPlotly(
  earned_premiums_single_group_plotly()
)


## * TOTAL PREMIUMS SINGLE GROUP -----
## ** table ----------
total_premiums_single_group_data <- reactive({
  selected_single_group_data_filtered_summarized() %>%
    filter(identifier %in% c("01", "02")) %>%
    mutate(
      `Premiums Type` =
        case_when(identifier == "01" ~ "Written Premiums",
          identifier == "02" ~ "Earned Premiums",
          .default = NA) %>% as.factor())
})

total_premiums_single_group_export_table <- reactive({
  if(nrow(total_premiums_single_group_data()) == 0){
    zeroGrob()
  } else{
    total_premiums_single_group_data() %>%
      group_by(groupname, year) %>%
      summarize(value = sum(value)) %>%
      ungroup() %>%
      mutate(col_names = paste0("Total Premiums ", seq(min(year), max(year), 1))) %>%
      select(groupname, value, col_names) %>%
      mutate(value = ifelse(!is.na(value), glue("${comma(value)}"), "")) %>%
      rename(`Group Name` = groupname) %>%
      pivot_wider(names_from = col_names, values_from = value) %>%
      table_to_grob()
  }
})

total_premiums_single_group_export_table_excel <- reactive({
  if(nrow(total_premiums_single_group_data()) == 0){
    tibble()
  } else {
    total_premiums_single_group_data() %>%
      group_by(groupname, year) %>%
      summarize(value = sum(value)) %>%
      ungroup() %>%
      select(groupname, value, year) %>%
      rename(`Group Name` = groupname) %>%
      pivot_wider(names_from = year, values_from = value)
  }
})

## ** graph ----------
total_premiums_single_group_ggplot <- reactive({

  graph_data <- total_premiums_single_group_data()

  if(identical(graph_data, numeric(0)) || nrow(graph_data) == 0){
    return(ggplot() + theme_void())
  }

  ggplot(graph_data, aes(x = as.factor(year), y = value, fill=`Premiums Type`, text = glue("<span><b>Value:</b> ${comma(value)}</span>"))) +
    geom_bar(stat="identity", position="stack") +
    scale_fill_manual(values = c(`Written Premiums` = '#337ab7', `Earned Premiums` = "#B3730E")) +
    scale_y_continuous(limits = c(0, max(graph_data$value) * 2), labels = label_number(prefix = "  $", scale_cut = cut_short_scale()), expand = c(0.001,0)) +
    # scale_x_continuous(breaks = seq(min(graph_data$year), max(graph_data$year), 1)) +
    ylab("Written & Earned Premium (LI 01 & LI02)") +
    universal_graph_theme_with_legend(0.09, 0.9)
})


total_premiums_single_group_plotly <- reactive({
  suppressWarnings({
    if(length(total_premiums_single_group_ggplot()$data) == 0){
      ggplotly(total_premiums_single_group_ggplot())
    } else{
      plotly <- ggplotly(total_premiums_single_group_ggplot(), tooltip = "text", dynamicTicks = TRUE)  %>%
        layout(
          legend = list(x = 0.01, y = 0.99, title = list(text=''), bgcolor = 'rgba(255,255,255,0.2)', font = list(size = 9.5)),
          yaxis = list(showline= T, linewidth=1.5, linecolor='black', showticklabels = T, tickprefix = "  $"),
          xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2))

      plotly$x$layout$yaxis$autorange <- FALSE
      plotly$x$layout$yaxis$range <- c(0, max(total_premiums_single_group_data()$value) * 2.5)
      plotly
    }
  })
})

total_premiums_single_group_export <- reactive({
  total_premiums_single_group_ggplot() +
    ggtitle("Written & Earned Premiums") +
    single_graph_export_theme
})

output$total_premiums_single_group_plot <- renderPlotly(
  total_premiums_single_group_plotly()
)


## * TOTAL LOSSES SINGLE GROUP ----
## ** table -----
total_losses_single_group_data <- reactive({
  selected_single_group_data_filtered_summarized() %>%
    filter(identifier == "05")
})

total_losses_single_group_export_table <- reactive({
  if(nrow(total_losses_single_group_data()) == 0){
    zeroGrob()
  } else{
    total_losses_single_group_data() %>%
      mutate(col_names = paste0("Losses Paid ", seq(min(year), max(year), 1))) %>%
      select(groupname, value, col_names) %>%
      mutate(value = ifelse(!is.na(value), glue("${comma(value)}"), "")) %>%
      rename(`Group Name` = groupname) %>%
      pivot_wider(names_from = col_names, values_from = value) %>%
      table_to_grob()
  }
})

total_losses_single_group_export_table_excel <- reactive({
  if(nrow(total_losses_single_group_data()) == 0){
    tibble()
  } else {
    total_losses_single_group_data() %>%
      select(groupname, value, year) %>%
      rename(`Group Name` = groupname) %>%
      pivot_wider(names_from = year, values_from = value)
  }
})

## ** graph -----
total_losses_single_group_ggplot <- reactive({
  graph_data <- total_losses_single_group_data()

  if(nrow(graph_data) == 0){
    return(ggplot() + theme_void())
  }

  ggplot(graph_data, aes(x = as.factor(year), y = value, fill = groupname, text = glue("<span><b>Value:</b> ${comma(value)}"))) +
    geom_bar(stat="identity", position="dodge", fill='#337ab7') +
    scale_y_continuous(limits = c(0, max(graph_data$value)* 1.2), labels = label_number(prefix = "  $", scale_cut = cut_short_scale()), expand = c(0.001,0)) +
    # scale_x_continuous(breaks = seq(min(graph_data$year), max(graph_data$year), 1)) +
    ylab("Losses Paid (LI 05)") +
    universal_graph_theme
})


total_losses_single_group_plotly <- reactive({
  suppressWarnings({
    if(length(total_losses_single_group_ggplot()$data) == 0){
      ggplotly(total_losses_single_group_ggplot())
    } else{
      plotly <- ggplotly(total_losses_single_group_ggplot(), tooltip = "text", dynamicTicks = TRUE) %>%
        layout(showlegend = FALSE,
          yaxis = list(showline= T, linewidth=1.5, linecolor='black', showticklabels = T, tickprefix = "  $"),
          xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2))

      plotly$x$layout$yaxis$autorange <- FALSE
      plotly$x$layout$yaxis$range <- c(0, max(total_losses_single_group_data()$value) * 1.2)
      plotly
    }
  })
})

total_losses_single_group_export <- reactive({
  total_losses_single_group_ggplot() +
    ggtitle("Direct Losses Paid") +
    single_graph_export_theme
})

output$total_losses_single_group_plot <- renderPlotly(
  total_losses_single_group_plotly()
)

## * LOSSES RATIO SINGLE GROUP ----
## ** table -----
losses_ratio_single_group_data <- reactive({
  summarized_data <- selected_single_group_data_filtered_summarized()

  premium_affiliatedfminsco = summarized_data %>% filter(identifier == "01")
  losses_ratio_single_group_affiliatedfminsco = summarized_data %>% filter(identifier == "05")

  left_join(losses_ratio_single_group_affiliatedfminsco, premium_affiliatedfminsco,
    by = c("groupname", "year")) %>%
    select(groupname, year, value.x, value.y) %>%
    # group_by(groupname, year) %>%
    # summarize(value.x = sum(value.x), value.y = sum(value.y)) %>%
    mutate(value.x = na_if(value.x, 0), value.y = na_if(value.y, 0)) %>%
    mutate(lossratio = value.x / value.y)
})

losses_ratio_single_group_export_table <- reactive({
  if(nrow(losses_ratio_single_group_data()) == 0){
    zeroGrob()
  } else{
    losses_ratio_single_group_data() %>%
      mutate(col_names = paste0("Loss Ratio ", seq(min(year), max(year), 1))) %>%
      select(groupname, lossratio, col_names) %>%
      mutate(lossratio = ifelse(!is.na(lossratio), glue("{round(lossratio, 4)}"), "")) %>%
      rename(`Group Name` = groupname) %>%
      pivot_wider(names_from = col_names, values_from = lossratio) %>%
      table_to_grob()
  }
})

losses_ratio_single_group_export_table_excel <- reactive({
  if(nrow(losses_ratio_single_group_data()) == 0){
    tibble()
  } else {
    losses_ratio_single_group_data() %>%
      select(groupname, lossratio, year) %>%
      rename(`Group Name` = groupname) %>%
      pivot_wider(names_from = year, values_from = lossratio)
  }
})

## ** graph -----
losses_ratio_single_group_ggplot <- reactive({

  if(identical(losses_ratio_single_group_data(), numeric(0)) || nrow(losses_ratio_single_group_data()) == 0 || all(is.na(losses_ratio_single_group_data()$lossratio))){
    return(ggplot() + theme_void())
  }

  ggplot(losses_ratio_single_group_data(), aes(x = as.factor(year), y = lossratio,  text = glue("<span style='color:white'><b>Loss Ratio:</b> {round(lossratio, 4)}"))) +
    geom_line(linewidth = 1.5, color = '#337ab7', aes(group = 1)) +
    geom_point(color='black', fill='#337ab7', shape=21, size=4) +
    scale_y_continuous(limits = c(0, max(losses_ratio_single_group_data()$lossratio, na.rm = T) * 1.2), labels = comma) +
    # scale_x_continuous(breaks = seq(min(selected_single_group_data_filtered()$year), max(selected_single_group_data_filtered()$year), 1)) +
    ylab("Loss Ratio") +
    universal_graph_theme

})


losses_ratio_single_group_plotly <- reactive({
  suppressWarnings(
    if(length(losses_ratio_single_group_ggplot()$data) == 0){
      ggplotly(losses_ratio_single_group_ggplot())
    } else{
      ggplotly(losses_ratio_single_group_ggplot(), tooltip = "text", dynamicTicks = TRUE) %>%
        layout(showlegend = FALSE,
          yaxis = list(showline= T, linewidth=1.5, linecolor='black', showticklabels = T, tickprefix = "  "),
          xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2))
    }
  )
})

losses_ratio_single_group_export <- reactive({
  losses_ratio_single_group_ggplot() +
    ggtitle("Loss Ratio") +
    single_graph_export_theme
})

output$losses_ratio_single_group_plot <- renderPlotly(
  losses_ratio_single_group_plotly()
)


## * NUMBER OF POLICIES SINGLE GROUP ----------
## ** table -------
number_of_policies_single_group_data <- reactive({
  selected_single_group_data_filtered_summarized() %>%
    filter(identifier == "11")
})

number_of_policies_single_group_export_table <- reactive({
  if(nrow(number_of_policies_single_group_data()) == 0){
    zeroGrob()
  } else {
    number_of_policies_single_group_data() %>%
      mutate(col_names = paste0("(n) Policies ", seq(min(year), max(year), 1))) %>%
      select(groupname, value, col_names) %>%
      mutate(value = ifelse(!is.na(value), glue("{value}"), "")) %>%
      rename(`Group Name` = groupname) %>%
      pivot_wider(names_from = col_names, values_from = value) %>%
      table_to_grob()
  }
})

number_of_policies_single_group_export_table_excel <- reactive({
  if(nrow(number_of_policies_single_group_data()) == 0){
    tibble()
  } else {
    number_of_policies_single_group_data() %>%
      select(groupname, value, year) %>%
      rename(`Group Name` = groupname) %>%
      pivot_wider(names_from = year, values_from = value)
  }
})


## ** graph -------
number_of_policies_single_group_ggplot <- reactive({
  graph_data <- number_of_policies_single_group_data()

  if(identical(graph_data, numeric(0)) || nrow(graph_data) == 0){
    return(ggplot() + theme_void())
  }

  ggplot(graph_data, aes(x = as.factor(year), y = value, fill = groupname, text = glue("<span><b>Value:</b> {comma(value)}"))) +
    geom_bar(stat="identity", position="dodge", fill='#337ab7') +
    scale_y_continuous(limits = c(0, max(graph_data$value) * 1.2), labels = label_number(prefix = " ", scale_cut = cut_short_scale()), expand = c(0.001,0)) +
    # scale_x_continuous(breaks = seq(min(graph_data$year), max(graph_data$year), 1)) +
    ylab("Total Policies in Force") +
    universal_graph_theme
})


number_of_policies_single_group_plotly <- reactive({
  suppressWarnings({
    if(length(number_of_policies_single_group_ggplot()$data) == 0){
      ggplotly(number_of_policies_single_group_ggplot())
    } else{
      plotly <- ggplotly(number_of_policies_single_group_ggplot(), tooltip = "text", dynamicTicks = TRUE)  %>%
        layout(showlegend = FALSE,
          yaxis = list(showline= T, linewidth=1.5, linecolor='black',
            showticklabels = T,  tickprefix = "  "),
          xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2))

      plotly$x$layout$yaxis$autorange <- FALSE
      plotly$x$layout$yaxis$range <- c(0, max(number_of_policies_single_group_data()$value) * 1.2)
      plotly
    }
  })
})

number_of_policies_single_group_export <- reactive({
  number_of_policies_single_group_ggplot() +
    ggtitle("Number of Policies") +
    single_graph_export_theme
})

output$number_of_policies_single_group_plot <- renderPlotly(
  number_of_policies_single_group_plotly()
)


## * AVG PREMIUM --------
## * table --------
avg_premium_single_group_data <- reactive({
  summarized_data <- selected_single_group_data_filtered_summarized()

  written_premium = summarized_data %>% filter(identifier == "01")
  num_of_policies = summarized_data %>% filter(identifier == "11")

  left_join(written_premium, num_of_policies,
    by = c("groupname", "year")) %>%
    select(groupname, year, value.x, value.y) %>%
    # group_by(groupname, year) %>%
    # summarize(value.x = sum(value.x), value.y = sum(value.y)) %>%
    mutate(value.x = na_if(value.x, 0), value.y = na_if(value.y, 0)) %>%
    mutate(avg_premium = value.x / value.y)
})


avg_premium_single_group_export_table <- reactive({
  if(nrow(avg_premium_single_group_data()) == 0){
    zeroGrob()
  } else {
    avg_premium_single_group_data() %>%
      mutate(col_names = paste0("Avg Premiums ", seq(min(year), max(year), 1))) %>%
      select(groupname, avg_premium, col_names) %>%
      mutate(avg_premium = ifelse(!is.na(avg_premium), glue("${comma(avg_premium)}"), "")) %>%
      rename(`Group Name` = groupname) %>%
      pivot_wider(names_from = col_names, values_from = avg_premium) %>%
      table_to_grob()
  }
})


avg_premium_single_group_export_table_excel <- reactive({
  if(nrow(avg_premium_single_group_data()) == 0){
    tibble()
  } else {
    avg_premium_single_group_data() %>%
      select(groupname, avg_premium, year) %>%
      rename(`Group Name` = groupname) %>%
      pivot_wider(names_from = year, values_from = avg_premium)
  }
})

## ** graph---------

avg_premium_single_group_ggplot <- reactive({

  graph_data <- avg_premium_single_group_data()
  if(identical(graph_data, numeric(0)) || nrow(graph_data) == 0 || all(is.na(graph_data$avg_premium))){
    return(ggplot() + theme_void())
  }

  ggplot(graph_data, aes(x = as.factor(year), y = avg_premium,  text = glue("<span><b>Value:</b> ${comma(avg_premium)}</span>"))) +
    geom_line(linewidth = 1.5, color = '#337ab7', aes(group = 1)) +
    geom_point(color='black', fill='#337ab7', shape=21, size=4) +
    scale_y_continuous(limits = c(0, max(graph_data$avg_premium, na.rm = T) * 1.2), labels = label_number(prefix = "  $", scale_cut = cut_short_scale()), expand = c(0.001,0)) +
    # scale_x_continuous(breaks = seq(min(selected_single_group_data_filtered()$year), max(selected_single_group_data_filtered()$year), 1)) +
    ylab("Average Premium per Policy") +
    universal_graph_theme

})

avg_premium_single_group_plotly <- reactive({
  suppressWarnings(
    if(length(avg_premium_single_group_ggplot()$data) == 0){
      ggplotly(avg_premium_single_group_ggplot())
    } else{
      plotly <- ggplotly(avg_premium_single_group_ggplot(), tooltip = "text", dynamicTicks = TRUE) %>%
        layout(showlegend = FALSE,
          yaxis = list(showline= T, linewidth=1.5, linecolor='black', showticklabels = T, tickprefix = "  $"),
          xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2))

    }
  )
})

avg_premium_single_group_export <- reactive({
  avg_premium_single_group_ggplot() +
    ggtitle("Average Premium") +
    single_graph_export_theme
})

output$avg_premium_single_group_plot <- renderPlotly(
  avg_premium_single_group_plotly()
)


## * DIRECT DEFENSE COST containment paid & case reserves--------
## ** table ----------
direct_defense_case_reserves_single_group_data <- reactive({
  selected_single_group_data_filtered_summarized() %>%
    filter(identifier %in% c("07", "08")) %>%
    mutate(
      `Premiums Type` =
        case_when(identifier == "07" ~ "Direct Defense Cost Containment Paid",
          identifier == "08" ~ "Case Reserves",
          .default = NA) %>% as.factor())
})

direct_defense_single_group_export_table <- reactive({
  direct_defense_data <- direct_defense_case_reserves_single_group_data() %>%
    filter(identifier == '07')

  if(nrow(direct_defense_data) == 0){
    zeroGrob()
  } else {
    direct_defense_data %>%
      mutate(col_names = paste0("Defense Cost Paid ", seq(min(year), max(year), 1))) %>%
      select(groupname, value, col_names) %>%
      mutate(value = ifelse(!is.na(value), glue("${comma(value)}"), "")) %>%
      rename(`Group Name` = groupname) %>%
      pivot_wider(names_from = col_names, values_from = value) %>%
      table_to_grob()
  }
})

case_reserves_single_group_export_table <- reactive({
  case_reserves_data <- direct_defense_case_reserves_single_group_data() %>%
    filter(identifier == '08')

  if(nrow(case_reserves_data) == 0){
    zeroGrob()
  } else {
    case_reserves_data %>%
      mutate(col_names = paste0("Case Reserves ", seq(min(year), max(year), 1))) %>%
      select(groupname, value, col_names) %>%
      mutate(value = ifelse(!is.na(value), glue("${comma(value)}"), "")) %>%
      rename(`Group Name` = groupname) %>%
      pivot_wider(names_from = col_names, values_from = value) %>%
      table_to_grob()
  }
})

direct_defense_single_group_export_table_excel <- reactive({
  direct_defense_data <- direct_defense_case_reserves_single_group_data() %>%
    filter(identifier == '07')

  if(nrow(direct_defense_data) == 0){
    tibble()
  } else {
    direct_defense_data %>%
      select(groupname, value, year) %>%
      rename(`Group Name` = groupname) %>%
      pivot_wider(names_from = year, values_from = value)
  }
})

case_reserves_single_group_export_table_excel <- reactive({
  case_reserve_data <- direct_defense_case_reserves_single_group_data() %>%
    filter(identifier == '08')

  if(nrow(case_reserve_data) == 0){
    tibble()
  } else {
    case_reserve_data %>%
      select(groupname, value, year) %>%
      rename(`Group Name` = groupname) %>%
      pivot_wider(names_from = year, values_from = value)
  }
})


## ** graph ----------
direct_defense_case_reserves_single_group_ggplot <- reactive({
  graph_data <- direct_defense_case_reserves_single_group_data()

  if(identical(graph_data, numeric(0)) || nrow(graph_data) == 0){
    return(ggplot() + theme_void())
  }

  ggplot(graph_data, aes(x = as.factor(year), y = value, fill=`Premiums Type`, text = glue("<span><b>Value:</b> ${comma(value)}</span>"))) +
    geom_bar(stat="identity", position="dodge") +
    scale_fill_manual(values = c(`Direct Defense Cost Containment Paid` = '#337ab7', `Case Reserves` = "#B3730E")) +
    scale_y_continuous(limits = c(0, max(graph_data$value) * 2), labels = label_number(prefix = "  $", scale_cut = cut_short_scale()), expand = c(0.001,0)) +
    # scale_x_continuous(breaks = seq(min(graph_data$year), max(graph_data$year), 1)) +
    ylab("Direct Defense Cost Containment \n Paid (LI 07) and Case Reserves(LI 08)") +
    universal_graph_theme_with_legend(0.15, 0.9)
})


direct_defense_case_reserves_single_group_plotly <- reactive({
  suppressWarnings({
    if(length(direct_defense_case_reserves_single_group_ggplot()$data) == 0){
      ggplotly(direct_defense_case_reserves_single_group_ggplot())
    } else{
      plotly <- ggplotly(direct_defense_case_reserves_single_group_ggplot(), tooltip = "text", dynamicTicks = TRUE)  %>%
        layout(
          legend = list(x = 0.01, y = 0.99, title = list(text=''), bgcolor = 'rgba(255,255,255,0.2)', font = list(size = 9.5)),
          yaxis = list(showline= T, linewidth=1.5, linecolor='black', showticklabels = T, tickprefix = "  $"),
          xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2))

      plotly$x$layout$yaxis$autorange <- FALSE
      plotly$x$layout$yaxis$range <- c(0, max(direct_defense_case_reserves_single_group_data()$value) * 1.5)
      plotly
    }
  })
})

direct_defense_case_reserves_single_group_export <- reactive({
  direct_defense_case_reserves_single_group_ggplot() +
    ggtitle("Defense Cost Containment Paid & Case Reserves") +
    single_graph_export_theme
})

output$direct_defense_case_reserves_single_group_plot <- renderPlotly(
  direct_defense_case_reserves_single_group_plotly()
)

## * CLAIMS CLOSED WITH PAYMENT ---------
## ** table----
total_claims_closed_with_payment_single_group_data <- reactive({
  selected_single_group_data_filtered_summarized() %>%
    filter(identifier == "20")
})

total_claims_closed_with_payment_single_group_export_table <- reactive({
  if(nrow(total_claims_closed_with_payment_single_group_data()) == 0){
    zeroGrob()
  } else {
    total_claims_closed_with_payment_single_group_data() %>%
      mutate(col_names = paste0("Closed w/ Payment ", seq(min(year), max(year), 1))) %>%
      select(groupname, value, col_names) %>%
      mutate(value = ifelse(!is.na(value), glue("{value}"), "")) %>%
      rename(`Group Name` = groupname) %>%
      pivot_wider(names_from = col_names, values_from = value) %>%
      table_to_grob()
  }
})

total_claims_closed_with_payment_single_group_export_table_excel <- reactive({
  if(nrow(total_claims_closed_with_payment_single_group_data()) == 0){
    tibble()
  } else {
    total_claims_closed_with_payment_single_group_data() %>%
      select(groupname, value, year) %>%
      rename(`Group Name` = groupname) %>%
      pivot_wider(names_from = year, values_from = value)
  }
})

### ** graph-----
total_claims_closed_with_payment_single_group_ggplot <- reactive({
  graph_data <- total_claims_closed_with_payment_single_group_data()

  if(identical(graph_data, numeric(0)) || nrow(graph_data) == 0){
    return(ggplot() + theme_void())
  }

  ggplot(graph_data, aes(x = as.factor(year), y = value, fill = groupname, text = glue("<span><b>Value:</b> {comma(value)}"))) +
    geom_bar(stat="identity", position="dodge", fill='#373852') +
    scale_y_continuous(limits = c(0, max(graph_data$value) * 1.2), labels = label_number(prefix = " ", scale_cut = cut_short_scale()), expand = c(0.001,0)) +
    # scale_x_continuous(breaks = seq(min(graph_data$year), max(graph_data$year), 1)) +
    ylab("Total Claims Closed with Payment (LI 20)") +
    universal_graph_theme
})


total_claims_closed_with_payment_single_group_plotly <- reactive({
  suppressWarnings({
    if(length(total_claims_closed_with_payment_single_group_ggplot()$data) == 0){
      ggplotly(total_claims_closed_with_payment_single_group_ggplot())
    } else{
      plotly <- ggplotly(total_claims_closed_with_payment_single_group_ggplot(), tooltip = "text", dynamicTicks = TRUE)  %>%
        layout(showlegend = FALSE,
          yaxis = list(showline= T, linewidth=1.5, linecolor='black',
            showticklabels = T,  tickprefix = "  "),
          xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2))

      plotly$x$layout$yaxis$autorange <- FALSE
      plotly$x$layout$yaxis$range <- c(0, max(total_claims_closed_with_payment_single_group_data()$value) * 1.2)
      plotly
    }
  })
})

total_claims_closed_with_payment_single_group_export <- reactive({
  total_claims_closed_with_payment_single_group_ggplot() +
    ggtitle("Total Claims Closed with Payment") +
    single_graph_export_theme
})

output$total_claims_closed_with_payment_single_group_plot <- renderPlotly(
  total_claims_closed_with_payment_single_group_plotly()
)

## * CLAIMS CLOSED WITHOUT PAYMENT ---------
## ** table -----------
total_claims_closed_without_payment_single_group_data <- reactive({
  selected_single_group_data_filtered_summarized() %>%
    filter(identifier == "23")
})

total_claims_closed_without_payment_single_group_export_table <- reactive({
  if(nrow(total_claims_closed_without_payment_single_group_data()) == 0){
    zeroGrob()
  } else {
    total_claims_closed_without_payment_single_group_data() %>%
      mutate(col_names = paste0("Closed w/o Payment ", seq(min(year), max(year), 1))) %>%
      select(groupname, value, col_names) %>%
      mutate(value = ifelse(!is.na(value), glue("{value}"), "")) %>%
      rename(`Group Name` = groupname) %>%
      pivot_wider(names_from = col_names, values_from = value) %>%
      table_to_grob()
  }
})

total_claims_closed_without_payment_single_group_export_table_excel <- reactive({
  if(nrow(total_claims_closed_without_payment_single_group_data()) == 0){
    tibble()
  } else {
    total_claims_closed_without_payment_single_group_data() %>%
      select(groupname, value, year) %>%
      rename(`Group Name` = groupname) %>%
      pivot_wider(names_from = year, values_from = value)
  }
})

## ** graph -----------
total_claims_closed_without_payment_single_group_ggplot <- reactive({
  graph_data <- total_claims_closed_without_payment_single_group_data()

  if(identical(graph_data, numeric(0)) || nrow(graph_data) == 0){
    return(ggplot() + theme_void())
  }

  ggplot(graph_data, aes(x = as.factor(year), y = value, fill = groupname, text = glue("<span><b>Value:</b> {comma(value)}"))) +
    geom_bar(stat="identity", position="dodge", fill='#EB6B2A') +
    scale_y_continuous(limits = c(0, max(graph_data$value) * 1.2), labels = label_number(prefix = " ", scale_cut = cut_short_scale()), expand = c(0.001,0)) +
    # scale_x_continuous(breaks = seq(min(graph_data$year), max(graph_data$year), 1)) +
    ylab("Total Claims Closed without Payment (LI 23)") +
    universal_graph_theme
})


total_claims_closed_without_payment_single_group_plotly <- reactive({
  suppressWarnings({
    if(length(total_claims_closed_without_payment_single_group_ggplot()$data) == 0){
      ggplotly(total_claims_closed_without_payment_single_group_ggplot())
    } else{
      plotly <- ggplotly(total_claims_closed_without_payment_single_group_ggplot(), tooltip = "text", dynamicTicks = TRUE)  %>%
        layout(showlegend = FALSE,
          yaxis = list(showline= T, linewidth=1.5, linecolor='black',
            showticklabels = T,  tickprefix = "  "),
          xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2))

      plotly$x$layout$yaxis$autorange <- FALSE
      plotly$x$layout$yaxis$range <- c(0, max(total_claims_closed_without_payment_single_group_data()$value) * 1.2)
      plotly
    }
  })
})

total_claims_closed_without_payment_single_group_export <- reactive({
  total_claims_closed_without_payment_single_group_ggplot() +
    ggtitle("Total Claims Closed without Payment") +
    single_graph_export_theme
})

output$total_claims_closed_without_payment_single_group_plot <- renderPlotly(
  total_claims_closed_without_payment_single_group_plotly()
)

## * WITH & WITHOUT PAYMENT --------
## ** table ----------
total_claims_closed_with_without_payment_single_group_data <- reactive({
  selected_single_group_data_filtered_summarized() %>%
    filter(identifier %in% c("20", "23")) %>%
    mutate(
      `Premiums Type` =
        case_when(identifier == "20" ~ "Claims Closed with Payment",
          identifier == "23" ~ "Claims Closed without Payment",
          .default = NA) %>% as.factor())
})

total_claims_closed_with_without_payment_single_group_export_table <- reactive({
  if(nrow(total_claims_closed_with_without_payment_single_group_data()) == 0){
    zeroGrob()
  } else{
    total_claims_closed_with_without_payment_single_group_data() %>%
      group_by(groupname, year) %>%
      summarize(value = sum(value)) %>%
      ungroup() %>%
      mutate(col_names = paste0("Total Claims Closed ", seq(min(year), max(year), 1))) %>%
      select(groupname, value, col_names) %>%
      mutate(value = ifelse(!is.na(value), glue("{value}"), "")) %>%
      rename(`Group Name` = groupname) %>%
      pivot_wider(names_from = col_names, values_from = value) %>%
      table_to_grob()
  }
})

total_claims_closed_with_without_payment_single_group_export_table_excel <- reactive({
  if(nrow(total_claims_closed_with_without_payment_single_group_data()) == 0){
    tibble()
  } else {
    total_claims_closed_with_without_payment_single_group_data() %>%
      group_by(groupname, year) %>%
      summarize(value = sum(value)) %>%
      ungroup() %>%
      select(groupname, value, year) %>%
      rename(`Group Name` = groupname) %>%
      pivot_wider(names_from = year, values_from = value)
  }
})

## ** graph ----------
total_claims_closed_with_without_payment_single_group_ggplot <- reactive({
  graph_data <- total_claims_closed_with_without_payment_single_group_data()

  if(identical(graph_data, numeric(0)) || nrow(graph_data) == 0){
    return(ggplot() + theme_void())
  }

  ggplot(graph_data, aes(x = as.factor(year), y = value, fill=`Premiums Type`, text = glue("<span><b>Value:</b> {comma(value)}</span>"))) +
    geom_bar(stat="identity", position="dodge") +
    scale_fill_manual(values = c(`Claims Closed with Payment` = '#373852', `Claims Closed without Payment` = "#EB6B2A")) +
    scale_y_continuous(limits = c(0, max(graph_data$value) * 2), labels = label_number(prefix = "  ", scale_cut = cut_short_scale()), expand = c(0.001,0)) +
    # scale_x_continuous(breaks = seq(min(graph_data$year), max(graph_data$year), 1)) +
    ylab("Total Claims Closed w Payment (LI 20) & \n Total claims Closed without Payment (LI 23)") +
    universal_graph_theme_with_legend(0.13, 0.9)
})


total_claims_closed_with_without_payment_single_group_plotly <- reactive({
  suppressWarnings({
    if(length(total_claims_closed_with_without_payment_single_group_ggplot()$data) == 0){
      ggplotly(total_claims_closed_with_without_payment_single_group_ggplot())
    } else{
      plotly <- ggplotly(total_claims_closed_with_without_payment_single_group_ggplot(), tooltip = "text", dynamicTicks = TRUE)  %>%
        layout(
          legend = list(x = 0.01, y = 0.99, title = list(text=''), bgcolor = 'rgba(255,255,255,0.2)', font = list(size = 9.5)),
          yaxis = list(showline= T, linewidth=1.5, linecolor='black', showticklabels = T, tickprefix = "  "),
          xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2))

      plotly$x$layout$yaxis$autorange <- FALSE
      plotly$x$layout$yaxis$range <- c(0, max(total_claims_closed_with_without_payment_single_group_data()$value) * 1.5)
      plotly
    }

  })
})

total_claims_closed_with_without_payment_single_group_export <- reactive({
  total_claims_closed_with_without_payment_single_group_ggplot() +
    ggtitle("Total Claims Closed With and Without Payment") +
    single_graph_export_theme
})

output$total_claims_closed_with_without_payment_single_group_plot <- renderPlotly(
  total_claims_closed_with_without_payment_single_group_plotly()
)


## * FIRST & THIRD PARTY CLOSED WITHOUT PAYMENT --------
## ** title ------------
first_third_party_closed_without_payment_single_group_data <- reactive({
  selected_single_group_data_filtered_summarized() %>%
    filter(identifier %in% c("21", "22")) %>%
    mutate(
      `Premiums Type` =
        case_when(identifier == "21" ~ "First Party Claims Closed without Payment",
          identifier == "22" ~ "Third Party Claims Closed without Payment",
          .default = NA) %>% as.factor())

})

first_party_closed_without_payment_single_group_export_table <- reactive({
  first_party_data <- first_third_party_closed_without_payment_single_group_data() %>%
    filter(identifier == '21')

  if(nrow(first_party_data) == 0){
    zeroGrob()
  } else {
    first_party_data %>%
      mutate(col_names = paste0("1st Party w/o Payment ", seq(min(year), max(year), 1))) %>%
      select(groupname, value, col_names) %>%
      mutate(value = ifelse(!is.na(value), glue("{value}"), "")) %>%
      rename(`Group Name` = groupname) %>%
      pivot_wider(names_from = col_names, values_from = value) %>%
      table_to_grob(colhead_cex = 0.6)
  }
})

third_party_closed_without_payment_single_group_export_table <- reactive({
  third_party_data <- first_third_party_closed_without_payment_single_group_data() %>%
    filter(identifier == '22')

  if(nrow(third_party_data) == 0){
    zeroGrob()
  } else {
    third_party_data %>%
      mutate(col_names = paste0("3rd Party w/o Payment ", seq(min(year), max(year), 1))) %>%
      select(groupname, value, col_names) %>%
      mutate(value = ifelse(!is.na(value), glue("{value}"), "")) %>%
      rename(`Group Name` = groupname) %>%
      pivot_wider(names_from = col_names, values_from = value) %>%
      table_to_grob(colhead_cex = 0.6)
  }
})


first_party_closed_without_payment_single_group_export_table_excel <- reactive({
  first_party_data <- first_third_party_closed_without_payment_single_group_data() %>%
    filter(identifier == '21')

  if(nrow(first_party_data) == 0){
    tibble()
  } else {
    first_party_data %>%
      select(groupname, value, year) %>%
      rename(`Group Name` = groupname) %>%
      pivot_wider(names_from = year, values_from = value)
  }
})

third_party_closed_without_payment_single_group_export_table_excel <- reactive({
  third_party_data <- first_third_party_closed_without_payment_single_group_data() %>%
    filter(identifier == '22')

  if(nrow(third_party_data) == 0){
    tibble()
  } else {
    third_party_data %>%
      select(groupname, value, year) %>%
      rename(`Group Name` = groupname) %>%
      pivot_wider(names_from = year, values_from = value)
  }
})


## ** graph ------------
first_third_party_closed_without_payment_single_group_ggplot <- reactive({

  graph_data <- first_third_party_closed_without_payment_single_group_data()

  if(identical(graph_data, numeric(0)) || nrow(graph_data) == 0){
    return(ggplot() + theme_void())
  }

  ggplot(graph_data, aes(x = as.factor(year), y = value, fill=`Premiums Type`, text = glue("<span><b>Value:</b> {comma(value)}</span>"))) +
    geom_bar(stat="identity", position="dodge") +
    scale_fill_manual(values = c(`First Party Claims Closed without Payment` = '#90353B', `Third Party Claims Closed without Payment` = "#2D6D66")) +
    scale_y_continuous(limits = c(0, max(graph_data$value) * 2), labels = label_number(prefix = "  ", scale_cut = cut_short_scale()), expand = c(0.001,0)) +
    # scale_x_continuous(breaks = seq(min(graph_data$year), max(graph_data$year), 1)) +
    ylab("1st Party Claims Closed w/o Payment (LI 21) \n & 3rd Party Claims Closed w/o Payment (LI 22)") +
    universal_graph_theme_with_legend(0.17, 0.9)
})


first_third_party_closed_without_payment_single_group_plotly <- reactive({
  suppressWarnings({
    if(length(first_third_party_closed_without_payment_single_group_ggplot()$data) == 0){
      ggplotly(first_third_party_closed_without_payment_single_group_ggplot())
    } else{
      plotly <- ggplotly(first_third_party_closed_without_payment_single_group_ggplot(), tooltip = "text", dynamicTicks = TRUE)  %>%
        layout(
          legend = list(x = 0.01, y = 0.99, title = list(text=''), bgcolor = 'rgba(255,255,255,0.2)', font = list(size = 9.5)),
          yaxis = list(showline= T, linewidth=1.5, linecolor='black', showticklabels = T, tickprefix = "  "),
          xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2))

      plotly$x$layout$yaxis$autorange <- FALSE
      plotly$x$layout$yaxis$range <- c(0, max(first_third_party_closed_without_payment_single_group_data()$value) * 1.5)
      plotly
    }

  })
})

first_third_party_closed_without_payment_single_group_export <- reactive({
  first_third_party_closed_without_payment_single_group_ggplot() +
    ggtitle("1st & 3rd Party Claims Closed w/o Payment") +
    single_graph_export_theme
})

output$first_third_party_closed_without_payment_single_group_plot <- renderPlotly(
  first_third_party_closed_without_payment_single_group_plotly()
)


## * FIRST & THIRD PARTY OPEN CLAIM --------
## ** table ---------
first_third_party_open_claims_single_group_data <- reactive({
  selected_single_group_data_filtered_summarized() %>%
    filter(identifier %in% c("15", "16")) %>%
    mutate(
      `Premiums Type` =
        case_when(identifier == "15" ~ "First Party Open Claims",
          identifier == "16" ~ "Third Party Open Claims",
          .default = NA) %>% as.factor())
})


first_party_open_claims_single_group_export_table <- reactive({
  first_party_data <- first_third_party_open_claims_single_group_data() %>%
    filter(identifier == '15')

  if(nrow(first_party_data) == 0){
    zeroGrob()
  } else {
    first_party_data %>%
      mutate(col_names = paste0("1st Party Open Claims ", seq(min(year), max(year), 1))) %>%
      select(groupname, value, col_names) %>%
      mutate(value = ifelse(!is.na(value), glue("{value}"), "")) %>%
      rename(`Group Name` = groupname) %>%
      pivot_wider(names_from = col_names, values_from = value) %>%
      table_to_grob(colhead_cex = 0.6)
  }
})

third_party_open_claims_single_group_export_table <- reactive({
  third_party_data <- first_third_party_open_claims_single_group_data() %>%
    filter(identifier == '16')

  if(nrow(third_party_data) == 0){
    zeroGrob()
  } else {
    third_party_data %>%
      mutate(col_names = paste0("3rd Party Open Claims ", seq(min(year), max(year), 1))) %>%
      select(groupname, value, col_names) %>%
      mutate(value = ifelse(!is.na(value), glue("{value}"), "")) %>%
      rename(`Group Name` = groupname) %>%
      pivot_wider(names_from = col_names, values_from = value) %>%
      table_to_grob(colhead_cex = 0.6)
  }
})


first_party_open_claims_single_group_export_table_excel <- reactive({
  first_party_data <- first_third_party_open_claims_single_group_data() %>%
    filter(identifier == '15')

  if(nrow(first_party_data) == 0){
    tibble()
  } else {
    first_party_data %>%
      select(groupname, value, year) %>%
      rename(`Group Name` = groupname) %>%
      pivot_wider(names_from = year, values_from = value)
  }
})

third_party_open_claims_single_group_export_table_excel <- reactive({
  third_party_data <- first_third_party_open_claims_single_group_data() %>%
    filter(identifier == '16')

  if(nrow(third_party_data) == 0){
    tibble()
  } else {
    third_party_data %>%
      select(groupname, value, year) %>%
      rename(`Group Name` = groupname) %>%
      pivot_wider(names_from = year, values_from = value)
  }
})



## ** graph -------------

first_third_party_open_claims_single_group_ggplot <- reactive({
  graph_data <- first_third_party_open_claims_single_group_data()

  if(identical(graph_data, numeric(0)) || nrow(graph_data) == 0){
    return(ggplot() + theme_void())
  }

  ggplot(graph_data, aes(x = as.factor(year), y = value, fill=`Premiums Type`, text = glue("<span><b>Value:</b> {comma(value)}</span>"))) +
    geom_bar(stat="identity", position="dodge") +
    scale_fill_manual(values = c(`First Party Open Claims` = '#C10534', `Third Party Open Claims` = "#CAC27E")) +
    scale_y_continuous(limits = c(0, max(graph_data$value) * 2), labels = label_number(prefix = "  ", scale_cut = cut_short_scale()), expand = c(0.001,0)) +
    # scale_x_continuous(breaks = seq(min(graph_data$year), max(graph_data$year), 1)) +
    ylab("Open Claims (LI 15 & LI 16)") +
    universal_graph_theme_with_legend(0.1, 0.9)
})


first_third_party_open_claims_single_group_plotly <- reactive({
  suppressWarnings({
    if(length(first_third_party_open_claims_single_group_ggplot()$data) == 0){
      ggplotly(first_third_party_open_claims_single_group_ggplot())
    } else{
      plotly <- ggplotly(first_third_party_open_claims_single_group_ggplot(), tooltip = "text", dynamicTicks = TRUE)  %>%
        layout(
          legend = list(x = 0.01, y = 0.99, title = list(text=''), bgcolor = 'rgba(255,255,255,0.2)', font = list(size = 9.5)),
          yaxis = list(showline= T, linewidth=1.5, linecolor='black', showticklabels = T, tickprefix = "  "),
          xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2))

      plotly$x$layout$yaxis$autorange <- FALSE
      plotly$x$layout$yaxis$range <- c(0, max(first_third_party_open_claims_single_group_data()$value) * 1.5)
      plotly
    }

  })
})

first_third_party_open_claims_single_group_export <- reactive({
  first_third_party_open_claims_single_group_ggplot() +
    ggtitle("1st & 3rd Party Open Claims") +
    single_graph_export_theme
})

output$first_third_party_open_claims_single_group_plot <- renderPlotly(
  first_third_party_open_claims_single_group_plotly()
)



## * PDF SINGLE GROUP EXPORT ------------------

output$export_single_group_pdf = downloadHandler(
  filename = function() {glue("{input$select_single_group}.pdf")},
  content = function(file) {
    withProgress(message = glue("Exporting {input$select_single_group}.pdf"), {

      cover_title <- textGrob(glue("Cyber Insurance Center"), gp=gpar(fontsize=22, fontface = 'bold'),
        x = unit(0.345, "npc"), y = unit(0.150, "npc"))

      cover_title2 <- textGrob(glue("Cyber Dashboard"), gp=gpar(fontsize=18),
        x = unit(0.055, "npc"), y = unit(0.150, "npc"))

      cover_title3 <- textGrob(glue("{input$select_single_group}"), gp=gpar(fontsize=12.5),
        x = unit(-0.15, "npc"), y = unit(0.150, "npc"), just = "left")

      cover_margin = unit(0.9, "line")

      cover_content <- gtable_col('cover', grobs = list(cover_title,  cover_title2, cover_title3),
        heights = unit.c(grobHeight(cover_title) + 1.2*cover_margin,
          grobHeight(cover_title2) + cover_margin,
          grobHeight(cover_title3) + cover_margin))

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

      logo <- rasterGrob(logo_png,
        height = unit(0.9, "cm"), width = unit(0.9, "cm"),
        x = unit(0.025, "npc"), y = unit(0.25, "npc"))

      title <- textGrob(glue("\t{input$select_single_group}\n\n"), gp=gpar(fontsize=14, fontface = 'bold'), x = unit(0.525, "npc"))

      bottom_text <- c(1:5) %>%
        map(~ textGrob(
          glue("Cyber Insurance Center | {input$select_single_group} | {.x}"),
          gp=gpar(fontsize=9), y = 1))

      content_page_heights = c(0.2, 2.4, 0.1, 0.25, 0.1, 2.4, 0.1, 0.25, 0.1, 2.4, 0.5, 0.1)

      incProgress(1/2)

      ppl <- list(
        p0 = arrangeGrob(zeroGrob(),
          top = cover_content_grob,
          left = cover_logo_with_line,
          nrow = 1),
        p1 = arrangeGrob(
          grobs=list(
            title,
            written_premiums_single_group_export(), zeroGrob(),
            written_premiums_single_group_export_table(), zeroGrob(),
            yoy_change_written_premiums_single_group_export(), zeroGrob(),
            yoy_change_written_premiums_single_group_export_table(), zeroGrob(),
            earned_premiums_single_group_export(),
            earned_premiums_single_group_export_table(), zeroGrob()),
          padding = unit(0.1, "line"),
          top = logo, bottom = bottom_text[[1]],
          left = textGrob("    "), right = textGrob("    "),
          heights = c(0.3, 2.4, 0.1, 0.25, 0.1, 2.4, 0.1, 0.25, 0.1, 2.4, 0.5, 0.1),
          ncol = 1, nrow = 12),
        p2 = arrangeGrob(
          grobs=list(
            zeroGrob(),
            total_premiums_single_group_export(), zeroGrob(),
            total_premiums_single_group_export_table(), zeroGrob(),
            total_losses_single_group_export(), zeroGrob(),
            total_losses_single_group_export_table(), zeroGrob(),
            losses_ratio_single_group_export(),
            losses_ratio_single_group_export_table(), zeroGrob()),
          padding = unit(0.1, "line"),
          top = logo, bottom = bottom_text[[2]],
          left = textGrob("    "), right = textGrob("    "),
          heights = content_page_heights,
          ncol = 1, nrow = 12),
        p3 = arrangeGrob(
          grobs=list(
            zeroGrob(),
            number_of_policies_single_group_export(), zeroGrob(),
            number_of_policies_single_group_export_table(), zeroGrob(),
            avg_premium_single_group_export(), zeroGrob(),
            avg_premium_single_group_export_table(), zeroGrob(),
            direct_defense_case_reserves_single_group_export(),
            direct_defense_single_group_export_table(),
            case_reserves_single_group_export_table(), zeroGrob()),
          padding = unit(0.1, "line"),
          top = logo, bottom = bottom_text[[3]],
          left = textGrob("    "), right = textGrob("    "),
          heights = c(0.2, 2.4, 0.1, 0.25, 0.1, 2.4, 0.1, 0.25, 0.1, 1.8, 0.4, 0.4, 0.1),
          ncol = 1, nrow = 13),
        p4 = arrangeGrob(
          grobs=list(
            zeroGrob(),
            total_claims_closed_with_payment_single_group_export(), zeroGrob(),
            total_claims_closed_with_payment_single_group_export_table(), zeroGrob(),
            total_claims_closed_without_payment_single_group_export(), zeroGrob(),
            total_claims_closed_without_payment_single_group_export_table(), zeroGrob(),
            total_claims_closed_with_without_payment_single_group_export(),
            total_claims_closed_with_without_payment_single_group_export_table(), zeroGrob()),
          padding = unit(0.1, "line"),
          top = logo, bottom = bottom_text[[4]],
          left = textGrob("    "), right = textGrob("    "),
          heights = content_page_heights,
          ncol = 1, nrow = 12),
        p5 = arrangeGrob(
          grobs=list(
            zeroGrob(),
            first_third_party_closed_without_payment_single_group_export(), zeroGrob(),
            first_party_closed_without_payment_single_group_export_table(), zeroGrob(),
            third_party_closed_without_payment_single_group_export_table(), zeroGrob(),
            first_third_party_open_claims_single_group_export(), zeroGrob(),
            first_party_open_claims_single_group_export_table(), zeroGrob(),
            third_party_open_claims_single_group_export_table(), zeroGrob()),
          heights = c(0.2, 2.4, 0.1, 0.25,0.1, 0.25, 0.1, 2.4, 0.1, 0.25, 0.1, 0.25, 0.1),
          padding = unit(0.1, "line"),
          top = logo,
          bottom = bottom_text[[5]],
          left = textGrob("    "), right = textGrob("    "),
          ncol = 1, nrow = 13)
      )

      class(ppl) <- c("arrangelist", "list")
      incProgress(1/2)
      ggsave(file, ppl, device = "pdf", width = 8, height = 12)
    })
  }
)

## * EXCEL SINGLE GROUP EXPORT ------------------


output$export_single_group_excel = downloadHandler(
  filename = function() {glue("{input$select_single_group}.xlsx")},
  content = function(file) {
    withProgress(message = glue("Exporting {input$select_single_group}.xlsx"), {
    my_workbook <- createWorkbook()

    worksheet_name <- c("Written Premiums", "YOY % Change", "Earned Premiums",
      "Total Premiums", "Direct Losses Paid", "Loss Ratio", "Num of Policies",
      "Avg Premiums", "Direct Defense Paid", "Case Reserves", "With Payment",
      "Without Payment", "Total Claims Closed", "1st Without Payment",
      "3rd Without Payment", "1st Open Claims", "3rd Open Claims")
    worksheet_content <- list(
      written_premiums_single_group_export_table_excel(),
      yoy_change_written_premiums_single_group_export_table_excel(),
      earned_premiums_single_group_export_table_excel(),
      total_premiums_single_group_export_table_excel(),
      total_losses_single_group_export_table_excel(),
      losses_ratio_single_group_export_table_excel(),
      number_of_policies_single_group_export_table_excel(),
      avg_premium_single_group_export_table_excel(),
      direct_defense_single_group_export_table_excel(),
      case_reserves_single_group_export_table_excel(),
      total_claims_closed_with_payment_single_group_export_table_excel(),
      total_claims_closed_without_payment_single_group_export_table_excel(),
      total_claims_closed_with_without_payment_single_group_export_table_excel(),
      first_party_closed_without_payment_single_group_export_table_excel(),
      third_party_closed_without_payment_single_group_export_table_excel(),
      first_party_open_claims_single_group_export_table_excel(),
      third_party_open_claims_single_group_export_table_excel()
    )
    incProgress(1/2)
    # print(worksheet_content)

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

