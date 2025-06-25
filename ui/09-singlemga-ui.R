tabItem(
    tabName = "singlemga",
    fluidRow(
        box(
            id = "single_graph_box",
            width = 9,
            ##SINGLE mga GRAPH ---------
            uiOutput("selected_single_mga_name"),
            br(),
            splitLayout(cellWidths = c('33.3%', '33.3%', '33.3%'),
                uiOutput("single_mga_year_founded_output"),
                uiOutput("single_mga_headquarter_output"),
                uiOutput("single_mga_website_output")
            ),
            br(),
            splitLayout(cellWidths = c('33.3%', '33.3%', '33.3%'),
                uiOutput("single_mga_size_output"),
                uiOutput("single_mga_market_reach_output"),
                uiOutput("single_mga_focus_cyber_output")
            ),
            br(),
            splitLayout(cellWidths = c('33.3%', '33.3%', '33.3%'),
                uiOutput("single_mga_active_output"),
                uiOutput("single_mga_countries_active_output"),
                uiOutput("single_mga_target_segments_output")
            ),
            br(),
            splitLayout(cellWidths = c('33.3%', '33.3%', '33.3%'),
                uiOutput("single_mga_vc_backed_output"),
                uiOutput("single_mga_total_raised_output"),
                uiOutput("single_mga_investors_output")
            ),
            br(),
            splitLayout(cellWidths = c('33.3%', '33.3%', '33.3%'),
                uiOutput("single_mga_capacity_providers_output"),
                uiOutput("single_mga_placeholder_1_output"),
                uiOutput("single_mga_offerings_output")
            ),
            br(),
            splitLayout(cellWidths = c('50%', '50%'),
                uiOutput("single_mga_description_output"),
                uiOutput("single_mga_latest_news_output")
            )
        ),
        box(
            id = "single_mga_input_ui_box",
            width = 3,
            div(class = 'inputdivpicker',
                shinyWidgets::pickerInput(
                    label = "Select an MGA",
                    inputId = "select_single_mga",
                    choices = sort(unique(mga_overview_data$Firm)),
                    options = list(`live-search` = TRUE, `max-options` = 1),
                    multiple = TRUE,
                    selected = NULL,
                )
            )
        )
    )
)
