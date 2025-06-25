
output$groupview_table <- renderDataTable({
        group_db <- group_combined_lastyear_stat %>%
                mutate(
                        onclick =  paste0("Shiny.onInputChange('group_button_id','", groupcode, "',{priority:'event'})>"),
                        actionable = glue('<button id="custom_btn" data-target=\"#firm_by_group_modal\" data-toggle=\"modal\" onclick= {onclick} {groupname}</button>')
                ) %>%
                select(-c(groupname,onclick)) %>%
                relocate(actionable, .after = groupcode) %>%
                rename(`Group Name` = actionable)

        DT::datatable(
                group_db,
                style = "auto",
                escape = FALSE,
                selection = "none",
                rownames= FALSE,
                options = list(
                        columnDefs=list(list(targets = c(2:5), class="dt-center"),
                                list(visible=FALSE, targets=c(0))),
                        pageLength = 25,
                        searching = TRUE,
                        autoWidth = TRUE
                )
        ) %>% formatCurrency(c('Written Premium Last Year', 'Earned Premium Last Year', 'Total Losses Last Year'), currency = "$", interval = 3, mark = ",")
})


selected_group_name <- eventReactive(input$group_button_id, {
        group_db %>% filter(groupcode == input$group_button_id) %>%
                select(groupname) %>%
                as.character()

})

output$selected_group_title <- renderUI({
        selected_group_name()
})

# selected_groupview_firm_data <- eventReactive(input$group_button_id, {
#
# })


groupview_firm_db <- reactive({
        groupview_firm_db <- group_firm_db %>% filter(groupcode == input$group_button_id) %>%
                select(companycode, shortname) %>%
                mutate(
                        onclick =  paste0("Shiny.onInputChange('firm_group_button_id','", companycode, "',{priority:'event'})>"),
                        actionable = glue('<button id="group_firm_custom_btn" onclick= {onclick} Value   <i class="fas fa-arrow-right"></i></button>')
                ) %>%
                select(-onclick) %>%
                rename(`Firm` = shortname, `LY Premiums` = actionable)

})

output$groupview_firm_table <- renderDataTable({

        DT::datatable(
                groupview_firm_db(),
                style = "auto",
                escape = FALSE,
                rownames= FALSE,
                options = list(
                        columnDefs=list(list(visible=FALSE, targets=c(0)),
                                list(className = 'dt-right', targets = 2)),
                        dom = 'ft',
                        pageLength = 25,
                        searching = TRUE,
                        autoWidth = TRUE
                )
        )
})

observeEvent(input$firm_group_button_id, {

        runjs("$('#firm_by_group_modal').modal('hide');")
        runjs("window.scrollTo(0, 0)")
        updateTabItems(session, "tabs", "singlefirm")

        clicked_company <- company_db %>%
                filter(companycode == input$firm_group_button_id) %>%
                select(shortname) %>%
                as.character()

        updatePickerInput(session, 'company', selected = clicked_company)
})

observe({
        if(all(is.null(input$groupview_firm_table_rows_selected)) || all(input$groupview_firm_table_rows_selected == "")){
                disable("select_groupview_firm")
        }
        else{
                enable("select_groupview_firm")
                print(input$groupview_firm_table_rows_selected)
        }
})

selected_groupview_firm_table <- reactive({
        test <- groupview_firm_db()[input$groupview_firm_table_rows_selected, 'Firm']

})

observeEvent(input$select_groupview_firm, {
        print(selected_groupview_firm_table())
        runjs("$('#firm_by_group_modal').modal('hide');")
        runjs("window.scrollTo(0, 0)")
        updateTabItems(session, "tabs", "compare")
        delay(500, {
                updateTabsetPanel(session, "compare_box", "Compare by Firm")
                updateVirtualSelect("select_firm_group", selected = selected_group_name())
                delay(300, updateVirtualSelect("select_firm_compare", selected = selected_groupview_firm_table()))
                click('confirm_firm_selection')
                })


})
