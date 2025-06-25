tabItem(
    tabName = "geography",
    fluidRow(
        box(
            id = "geography_box",
            width = 9,
            hidden(span(id = 'export_geography_span',
                p("Export", id="geography_export_title"),
                shinyWidgets::pickerInput(
                    label = NULL,
                    inputId = "select_geography_export",
                    choices = c("Results as PDF", "Results as Values"),
                    options = list(`live-search` = FALSE, `max-options` = 1),
                    multiple = TRUE,
                    selected = NULL,
                ),
                hidden(
                    span(id = 'geography_pdf_div',
                        downloadBttn(
                        outputId = "export_geography_pdf",
                        "Export",
                        style = 'simple',
                        size = 'sm',
                        icon = icon("file-pdf"))
                    )
                ),
                hidden(
                    span(id = 'geography_excel_div',
                        downloadBttn(
                            outputId = "export_geography_excel",
                            "Export",
                            style = 'simple',
                            size = 'sm',
                            icon = icon("file-excel"))
                    )
                )
            )
                ),
            br(),
            ##GEOGRAPHY GRAPH ---------
            br(),
            fluidRow(
                div(class = "plot-title", h4(style = 'display: inline-block;', "Premiums by Geography")),
                br(),
                plotlyOutput("premiums_by_geography_plot", width = '85%') %>% withSpinner(),
                align = "center"
            ),
            br(),
            div(id = 'geography_table_div',
                ## DTOutput("geography_table", width = 'auto')
                uiOutput('geography_table_output')
            )
        ),
        box(
            id = "geography_input_ui_box",
            width = 3,
            div(class = 'inputdivpicker',
                shinyWidgets::pickerInput(
                    label = "Select Geography",
                    inputId = "select_geography",
                    choices = unique(geography_pivot_df$Geography),
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE,
                    selected = NULL,
                )
            ),
            div(class = 'inputdivpicker',
                shinyWidgets::pickerInput(
                    label = "Select year(s)",
                    inputId = "select_geography_year",
                    choices = unique(geography_pivot_df$year),
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE,
                    selected = NULL,
                )
            ),
            ## HIDE IF n select_geography == 1
            div(class = 'inputdivpicker',
                shinyWidgets::pickerInput(
                    label = "Select Chart Type",
                    inputId = "select_geography_chart_type",
                    choices = c("Clustered", "Stacked"),
                    options = list(`live-search` = FALSE, `max-options` = 1),
                    multiple = TRUE,
                    selected = NULL,
                )
            )
        )
    )
)
