tabItem(
    tabName = "mgatable",
    fluidRow(
        box(
            width = 12,
            id = "mga_table_box",
                div(id = 'mga_table_div',
                    DTOutput("mga_table_list", width = '95%')),
        )
    )
)
