tabItem(
  tabName = "companytable",
  fluidRow(
    tabBox(
      width = 12,
      id = "company_table_box",
      tabPanel("Group View", id = 'group_view',
        splitLayout(cellWidths = c('36.3%', '20%', '40%'), style = 'margin-left: 21%;',
          div(
            virtualSelectInput(
            inputId = "group_table_name_select",
            label = "Select columns to show:",
            choices = c("Total Premium", "Standalone Premium", "Package Premium", "Losses Paid"),
            multiple = TRUE,
            selected = c("Total Premium", "Standalone Premium", "Package Premium", "Losses Paid"),
            keepAlwaysOpen = T),
            br()),
          div(class = 'table_year_div',
            sliderInput(
              inputId = "group_table_year_range",
              label = "Select year range",
              min = 2018,
              max = 2022,
              value = c(2018, 2022),
              step = 1,
              width = '85%',
              sep = ""
            )
          )
        ),
        div(id = 'groupview_table_div',
          DTOutput("groupview_table", width = '95%')
        ),
        bs_modal(id = 'company_table_firm_by_group_modal', title = htmlOutput("selected_group_title"),
          body =  tagList(
            div(id = 'groupview_compare_selected_group_button_div',
           actionBttn("view_group_graph_button_btn","View Group Graph",  style = 'simple', size = 'sm', color = 'primary')
              ),
            div(id = 'group_firm_table_div',
              DTOutput("groupview_firm_table", width = '75%')),
            div(id = 'groupview_compare_selected_firm_button_div',
              actionBttn("compare_selected_group_firm_btn", "Compare Selected Firm", style = 'simple', size = 'sm', color = 'primary')
            )
          ),
          footer = tagList(
            bs_modal_closebutton("Close")
          )
        )
      ),
      tabPanel("Firm View", id = 'firm_view',
        splitLayout(cellWidths = c('36.3%', '20%', '40%'), style = 'margin-left: 21%;',
          div(
            virtualSelectInput(
              inputId = "firm_table_name_select",
              label = "Select columns to show:",
              choices = c("Total Premium", "Standalone Premium", "Package Premium", "Losses Paid"),
              multiple = TRUE,
              selected = c("Total Premium", "Standalone Premium", "Package Premium", "Losses Paid"),
              keepAlwaysOpen = T),
            br()),
          div(class = 'table_year_div',
            sliderInput(
              inputId = "firm_table_year_range",
              label = "Select year range",
              min = 2018,
              max = 2022,
              value = c(2018, 2022),
              step = 1,
              width = '85%',
              sep = ""
            )
          )
        ),
        div(id = 'company_table_div',
        DTOutput("firm_table_list", width = '95%')),
      )
    )
  )
)
