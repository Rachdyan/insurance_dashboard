
## GROUPVIEW TABLE ---------
groupview_table_selected_column <- reactive({
  req(c(input$group_table_name_select, input$group_table_year_range))
  generate_table_col_name(input$group_table_name_select, input$group_table_year_range[1], input$group_table_year_range[2])
})

group_table_filtered_df <- reactive({
  overview_table_group_df %>%
    mutate(
      onclick =  paste0("Shiny.onInputChange('group_button_id','", groupcode, "',{priority:'event'})>"),
      actionable = glue('<button id="custom_btn" data-target=\"#company_table_firm_by_group_modal\" data-toggle=\"modal\" onclick= {onclick} {groupname}</button>')
    ) %>%
    select(-c(groupname,onclick)) %>%
    relocate(actionable, .after = groupcode) %>%
    rename(`Group Name` = actionable) %>%
    select(1:2, sort(match(groupview_table_selected_column(), names(.))))
})


output$groupview_table <- renderDataTable({
  n_col <- ncol(group_table_filtered_df())
  scrollX_status <- ifelse(n_col < 8, FALSE, TRUE)
  print(n_col)
  print(scrollX_status)

  DT::datatable(
    group_table_filtered_df(),
    style = "auto",
    escape = FALSE,
    selection = "none",
    rownames= FALSE,
    options = list(
      columnDefs=list(
        list(className = 'dt-center', targets = 1),
        list(visible=FALSE, targets=c(0))),
      pageLength = 25,
      searching = TRUE,
      autoWidth = TRUE,
      scrollX = scrollX_status
    )
  )  %>%
    formatCurrency(
      groupview_table_selected_column(),
      currency = "$", interval = 3, mark = ",")
})


selected_group_name <- eventReactive(input$group_button_id, {
  group_db %>% filter(groupcode == input$group_button_id) %>%
    select(groupname) %>%
    as.character()

})

output$selected_group_title <- renderUI({
  selected_group_name()
})


## * MODAL CONTENT -----

observeEvent(input$view_group_graph_button_btn, {
  runjs("$('#company_table_firm_by_group_modal').modal('hide');")
  updateTabItems(session, "tabs", "singlefirm")

  delay(200,
    updatePickerInput(session, 'select_single_group', selected =  selected_group_name())
  )
})



groupview_firm_db <- reactive({
  groupview_firm_db <- group_firm_modal_db %>%
    filter(groupcode == input$group_button_id) %>%
    select(companycode, shortname, `LY Premiums`) %>%
    mutate(
      ` ` = "",
      onclick =  paste0("Shiny.onInputChange('firm_group_button_id','", companycode, "',{priority:'event'})>"),
      actionable = glue('<button id="group_firm_custom_btn" onclick= {onclick}   <i class="fas fa-arrow-right"></i></button>')
    ) %>%
    select(-onclick) %>%
    relocate(` `, .before = companycode) %>%
    rename(`Firm` = shortname, `   ` = actionable)

})


output$groupview_firm_table <- renderDataTable(server = FALSE, {
  DT::datatable(
    groupview_firm_db(),
    style = "auto",
    escape = FALSE,
    rownames = FALSE,
    extensions = "Select",
    selection = "none",
    options = list(
      columnDefs = list(
        list(targets = 0, orderable = FALSE, className = "select-checkbox"),
        list(visible=FALSE, targets=c(1)),
        list(className = 'dt-right', targets = 3),
        list(className = 'dt-left', targets = 4),
        list(orderable = FALSE, targets = 4)
      ),
      select = list(
        style = "multi", selector = "td:first-child"
      ),
      dom = 'ft',
      pageLength = 25,
      searching = TRUE,
      autoWidth = TRUE
    )
  ) %>% formatCurrency(c('LY Premiums'), currency = "$", interval = 3, mark = ",")
})


observeEvent(input$firm_group_button_id, {
  runjs("$('#company_table_firm_by_group_modal').modal('hide');")
  runjs("window.scrollTo(0, 0)")
  updateTabItems(session, "tabs", "singlefirm")

  clicked_company <- company_db %>%
    filter(companycode == input$firm_group_button_id) %>%
    select(shortname) %>%
    as.character()

  updateTabsetPanel(session, "single_graph_box", "Single Firm Graph")

  delay(200, {
    updatePickerInput(session, 'select_single_firm', selected = clicked_company)
  })
})

observe({
  if(all(is.null(input$groupview_firm_table_rows_selected)) || all(input$groupview_firm_table_rows_selected == "")){
    disable("compare_selected_group_firm_btn")
  }
  else{
    enable("compare_selected_group_firm_btn")
    print(input$groupview_firm_table_rows_selected)
  }
})

selected_groupview_firm_table <- reactive({
  test <- groupview_firm_db()[input$groupview_firm_table_rows_selected, 'Firm']

})

observeEvent(input$compare_selected_group_firm_btn, {
  print(selected_groupview_firm_table())
  runjs("$('#company_table_firm_by_group_modal').modal('hide');")
  runjs("window.scrollTo(0, 0)")
  updateTabItems(session, "tabs", "compare")
  delay(500, {
    updateTabsetPanel(session, "compare_box", "Compare by Firm")
    updateVirtualSelect("comparison_select_firm_group", selected = selected_group_name())
    delay(300, updateVirtualSelect("select_firm_compare", selected = selected_groupview_firm_table()))
    click('confirm_firm_selection')
  })
})




## FIRM VIEW TABLE ---------
firm_table_selected_column <- reactive({
  req(c(input$firm_table_name_select, input$firm_table_year_range))
  generate_table_col_name(input$firm_table_name_select, input$firm_table_year_range[1], input$firm_table_year_range[2])
})

firm_table_filtered_df <- reactive({
  overview_table_single_firm_df %>%
    mutate(
      onclick =  paste0("Shiny.onInputChange('button_id','", companycode, "',{priority:'event'})>"),
      actionable = glue('<button id="custom_btn" onclick= {onclick} {`Firm Name`}</button>')
    ) %>%
    select(-c(`Firm Name`,onclick)) %>%
    relocate(actionable, .after = companycode) %>%
    rename(`Firm Name` = actionable) %>%
    select(1:2, sort(match(firm_table_selected_column(), names(.))))
})



output$firm_table_list <- renderDataTable({
  # company_db <- overview_table_single_firm_df %>%
  #   mutate(
  #     onclick =  paste0("Shiny.onInputChange('button_id','", companycode, "',{priority:'event'})>"),
  #     actionable = glue('<button id="custom_btn" onclick= {onclick} {`Firm Name`}</button>')
  #   ) %>%
  #   select(-c(`Firm Name`,onclick)) %>%
  #   relocate(actionable, .after = companycode) %>%
  #   rename(`Firm Name` = actionable)

  n_col <- ncol(firm_table_filtered_df())
  scrollX_status <- ifelse(n_col < 8, FALSE, TRUE)
  print(n_col)
  print(scrollX_status)

  DT::datatable(
    firm_table_filtered_df(),
    style = "auto",
    escape = FALSE,
    selection = "none",
    rownames= FALSE,
    options = list(
      columnDefs=list(list(targets = 1, class="dt-center"),
        list(visible=FALSE, targets=c(0))),
      pageLength = 25,
      searching = TRUE,
      scrollX = scrollX_status)) %>%
    formatCurrency(
      firm_table_selected_column(), currency = "$", interval = 3, mark = ","
      )
})



observeEvent(input$button_id, {
  updateTabItems(session, "tabs", "singlefirm")

  clicked_company <- company_db %>%
    filter(companycode == input$button_id) %>%
    select(shortname) %>%
    as.character()

  updateTabsetPanel(session, "single_graph_box", "Single Firm Graph")

  delay(200,
    updatePickerInput(session, 'select_single_firm', selected = clicked_company)
  )
})

