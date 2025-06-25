tabItem(
        tabName = "wiki",
        fluidRow(
                box(
                        width = 9,
                        div(id = "faq_content",
                                uiOutput("firm_title_wiki"),
                                htmlOutput("firm_overview_wiki"),
                                uiOutput("company_faq")
                        )
                ),
                box(
                        width = 3,
                        title = "Select a Company",
                        shinyWidgets::pickerInput(
                                inputId = "select_company_faq",
                                choices = sort(unique(all_data$shortname)),
                                options = list(`actions-box` = TRUE, `live-search` = TRUE),
                                multiple = FALSE,
                                selected = NULL
                        )
                )
        )
)
