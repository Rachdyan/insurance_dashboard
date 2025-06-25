tabItem(
        tabName = "groupview",
        fluidRow(
                box(
                        width = 12,
                        div(id = 'groupview_table_div',
                                DTOutput("groupview_table", width = '85%')
                        )
                )
        ),
        bs_modal(id = 'firm_by_group_modal', title = htmlOutput("selected_group_title"),
                body =  tagList(
                        p("Click on the row to select a firm", style = "display: flex; justify-content: center;"),
                        div(id = 'group_firm_table_div',
                                DTOutput("groupview_firm_table", width = '75%')),
                        div(id = 'groupview_button_div',
                                actionBttn("select_groupview_firm", "Compare Selected Firm", style = 'simple', size = 'sm', color = 'primary')
                        )
                ),
                footer = tagList(
                        bs_modal_closebutton("Close")
                )
        )
)



