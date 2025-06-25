
## FIRM VIEW TABLE ---------
# firm_table_selected_column <- reactive({
#     req(c(input$firm_table_name_select, input$firm_table_year_range))
#     generate_table_col_name(input$firm_table_name_select, input$firm_table_year_range[1], input$firm_table_year_range[2])
# })

mga_table_filtered_df <- reactive({
    mga_overview_data %>%
        mutate(
            onclick =  paste0("Shiny.onInputChange('mga_button_id','", mga_index, "',{priority:'event'})>"),
            actionable = glue('<button id="mga_custom_btn" onclick= {onclick} {`Firm`}</button>'),
            Website = glue("<a href= {Website}> {Website} </a>")
        ) %>%
        select(-c(`Firm`,onclick)) %>%
        relocate(actionable, .before = Year.Founded) %>%
        rename(`Firm Name` = actionable)
})



output$mga_table_list <- renderDataTable({

    DT::datatable(
        mga_table_filtered_df(),
        style = "auto",
        escape = FALSE,
        selection = "none",
        rownames= FALSE,
        options = list(
            columnDefs = list(list(className = 'dt-center', targets = 0:6),
                list(visible=FALSE, targets=c(0))),
            pageLength = 25,
            searching = TRUE,
            scrollX = TRUE))
})


#
observeEvent(input$mga_button_id, {
    updateTabItems(session, "tabs", "singlemga")

    clicked_mga <- mga_complete_data %>%
        filter(mga_index == input$mga_button_id) %>%
        select(MGA.Name) %>%
        as.character()

    delay(200,
        updatePickerInput(session, 'select_single_mga', selected = clicked_mga)
    )
})
#
