tabItem(
  tabName = "singlefirm",
  fluidRow(
    tabBox(
      id = "single_graph_box",
      width = 9,
      ##SINGLE GROUP GRAPH ---------
      tabPanel("Single Group Graph", id = 'single_group_graph',
        uiOutput("selected_single_group_name"),
        br(),
        fluidRow(
          div(class = "plot-title", h4(style = 'display: inline-block;', "Written Premiums (Quantified)"),
            uiOutput("written_premiums_single_group_info", inline = T)),
          br(),
          plotlyOutput("written_premiums_single_group_plot", width = '85%') %>% withSpinner(),
          align = "center"
        ),
        br(),
        br(),
        fluidRow(
          div(class = "plot-title", h4(style = 'display: inline-block;', "% Change in Written Premiums"),
            uiOutput("yoy_change_written_premiums_single_group_info", inline = T)),
          br(),
          plotlyOutput("yoy_change_written_premiums_single_group_plot", width = '85%') %>% withSpinner(),
          align = "center"
        ),
        br(),
        br(),
        fluidRow(
          div(class = "plot-title", h4(style = 'display: inline-block;', "Earned Premiums (Quantified)"), uiOutput("earned_premiums_single_group_info", inline = T)),
          br(),
          plotlyOutput("earned_premiums_single_group_plot", width = '85%') %>% withSpinner(),
          align = "center"
        ),
        br(),
        br(),
        fluidRow(
          div(class = "plot-title", h4(style = 'display: inline-block;', "Written & Earned Premiums"),
            uiOutput("total_premiums_single_group_info", inline = T)),
          br(),
          plotlyOutput("total_premiums_single_group_plot", width = '85%') %>% withSpinner(),
          align = "center"
        ),
        br(),
        br(),
        fluidRow(
          div(class = "plot-title", h4(style = 'display: inline-block;', "Direct Losses Paid"), uiOutput("total_losses_single_group_info", inline = T)),
          br(),
          plotlyOutput("total_losses_single_group_plot", width = '85%') %>% withSpinner(),
          align = "center"
        ),
        br(),
        br(),
        fluidRow(
          div(class = "plot-title", h4(style = 'display: inline-block;', "Loss Ratio"), uiOutput("losses_ratio_single_group_info", inline = T)),
          br(),
          plotlyOutput("losses_ratio_single_group_plot", width = '85%') %>% withSpinner(),
          align = "center"
        ),
        br(),
        br(),
        fluidRow(
          div(class = "plot-title", h4(style = 'display: inline-block;', "Number of Policies"),
            uiOutput("number_of_policies_single_group_info", inline = T)),
          br(),
          plotlyOutput("number_of_policies_single_group_plot", width = '85%') %>% withSpinner(),
          align = "center"
        ),
        br(),
        br(),
        fluidRow(
          div(class = "plot-title", h4(style = 'display: inline-block;', "Average Premium"),
            uiOutput("avg_premium_single_group_info", inline = T)),
          br(),
          plotlyOutput("avg_premium_single_group_plot", width = '85%') %>% withSpinner(),
          align = "center"
        ),
        br(),
        br(),
        fluidRow(
          div(class = "plot-title", h4(style = 'display: inline-block;', "Direct Defense Cost Containment Paid & Case Reserves"),
            uiOutput("direct_defense_case_reserves_single_group_info", inline = T)),
          br(),
          plotlyOutput("direct_defense_case_reserves_single_group_plot", width = '85%') %>% withSpinner(),
          align = "center"
        ),
        br(),
        br(),
        fluidRow(
          div(class = "plot-title", h4(style = 'display: inline-block;', "Total Claims Closed with Payment"),
            uiOutput("total_claims_closed_with_payment_single_group_info", inline = T)),
          br(),
          plotlyOutput("total_claims_closed_with_payment_single_group_plot", width = '85%') %>% withSpinner(),
          align = "center"
        ),
        br(),
        br(),
        fluidRow(
          div(class = "plot-title", h4(style = 'display: inline-block;', "Total Claims Closed without Payment"),
            uiOutput("total_claims_closed_without_payment_single_group_info", inline = T)),
          br(),
          plotlyOutput("total_claims_closed_without_payment_single_group_plot", width = '85%') %>% withSpinner(),
          align = "center"
        ),
        br(),
        br(),
        fluidRow(
          div(class = "plot-title", h4(style = 'display: inline-block;', "Total Claims Closed with Payment & without Payment"),
            uiOutput("total_claims_closed_with_without_payment_single_group_info", inline = T)),
          br(),
          plotlyOutput("total_claims_closed_with_without_payment_single_group_plot", width = '85%') %>% withSpinner(),
          align = "center"
        ),
        br(),
        br(),
        fluidRow(
          div(class = "plot-title", h4(style = 'display: inline-block;', "First Party Claims Closed without Payment & Third Party Claims Closed Without payment"),
            uiOutput("first_third_party_closed_without_payment_single_group_info", inline = T)),
          br(),
          plotlyOutput("first_third_party_closed_without_payment_single_group_plot", width = '85%') %>% withSpinner(),
          align = "center"
        ),
        br(),
        br(),
        fluidRow(
          div(class = "plot-title", h4(style = 'display: inline-block;', "First Party Open Claims & Third Party Open Claims"),
            uiOutput("first_third_party_open_claims_single_group_info", inline = T)),
          br(),
          plotlyOutput("first_third_party_open_claims_single_group_plot", width = '85%') %>% withSpinner(),
          align = "center"
        )
      ),
      ##SINGLE FIRM GRAPH ---------
      tabPanel("Single Firm Graph", id = 'single_firm_graph',
        uiOutput("selected_single_firm_name"),
        br(),
        fluidRow(
          div(class = "plot-title", h4(style = 'display: inline-block;', "Written Premiums"),
            uiOutput("written_premiums_single_firm_info", inline = T),
            uiOutput("global_faq_info", inline = T)),
          br(),
          plotlyOutput("written_premiums_single_firm_plot", width = '85%') %>% withSpinner(),
          align = "center"
        ),
        br(),
        br(),
        fluidRow(
          div(class = "plot-title", h4(style = 'display: inline-block;', "% Change in Written Premiums"),
            uiOutput("yoy_change_written_premiums_single_firm_info", inline = T)),
          br(),
          plotlyOutput("yoy_change_written_premiums_single_firm_plot", width = '85%') %>% withSpinner(),
          align = "center"
        ),
        br(),
        br(),
        fluidRow(
          div(class = "plot-title", h4(style = 'display: inline-block;', "Earned Premiums (Quantified)"), uiOutput("earned_premiums_single_firm_info", inline = T)),
          br(),
          plotlyOutput("earned_premiums_single_firm_plot", width = '85%') %>% withSpinner(),
          align = "center"
        ),
        br(),
        br(),
        fluidRow(
          div(class = "plot-title", h4(style = 'display: inline-block;', "Written & Earned Premiums"),
            uiOutput("total_premiums_single_firm_info", inline = T)),
          br(),
          plotlyOutput("total_premiums_single_firm_plot", width = '85%') %>% withSpinner(),
          align = "center"
        ),
        br(),
        br(),
        fluidRow(
          div(class = "plot-title", h4(style = 'display: inline-block;', "Direct Losses Paid"), uiOutput("total_losses_single_firm_info", inline = T)),
          br(),
          plotlyOutput("total_losses_single_firm_plot", width = '85%') %>% withSpinner(),
          align = "center"
        ),
        br(),
        br(),
        fluidRow(
          div(class = "plot-title", h4(style = 'display: inline-block;', "Loss Ratio"), uiOutput("losses_ratio_single_firm_info", inline = T)),
          br(),
          plotlyOutput("losses_ratio_single_firm_plot", width = '85%')  %>% withSpinner(),
          align = "center"
        ),
        br(),
        br(),
        fluidRow(
          div(class = "plot-title", h4(style = 'display: inline-block;', "Number of Policies"),
            uiOutput("number_of_policies_single_firm_info", inline = T)),
          br(),
          plotlyOutput("number_of_policies_single_firm_plot", width = '85%') %>% withSpinner(),
          align = "center"
        ),
        br(),
        br(),
        fluidRow(
          div(class = "plot-title", h4(style = 'display: inline-block;', "Average Premium"),
            uiOutput("avg_premium_single_firm_info", inline = T)),
          br(),
          plotlyOutput("avg_premium_single_firm_plot", width = '85%') %>% withSpinner(),
          align = "center"
        ),
        br(),
        br(),
        fluidRow(
          div(class = "plot-title", h4(style = 'display: inline-block;', "Direct Defense Cost Containment Paid & Case Reserves"),
            uiOutput("direct_defense_case_reserves_single_firm_info", inline = T)),
          br(),
          plotlyOutput("direct_defense_case_reserves_single_firm_plot", width = '85%') %>% withSpinner(),
          align = "center"
        ),
        br(),
        br(),
        fluidRow(
          div(class = "plot-title", h4(style = 'display: inline-block;', "Total Claims Closed with Payment"),
            uiOutput("total_claims_closed_with_payment_single_firm_info", inline = T)),
          br(),
          plotlyOutput("total_claims_closed_with_payment_single_firm_plot", width = '85%') %>% withSpinner(),
          align = "center"
        ),
        br(),
        br(),
        fluidRow(
          div(class = "plot-title", h4(style = 'display: inline-block;', "Total Claims Closed without Payment"),
            uiOutput("total_claims_closed_without_payment_single_firm_info", inline = T)),
          br(),
          plotlyOutput("total_claims_closed_without_payment_single_firm_plot", width = '85%') %>% withSpinner(),
          align = "center"
        ),
        br(),
        br(),
        fluidRow(
          div(class = "plot-title", h4(style = 'display: inline-block;', "Total Claims Closed with Payment & without Payment"),
            uiOutput("total_claims_closed_with_without_payment_single_firm_info", inline = T)),
          br(),
          plotlyOutput("total_claims_closed_with_without_payment_single_firm_plot", width = '85%') %>% withSpinner(),
          align = "center"
        ),
        br(),
        br(),
        fluidRow(
          div(class = "plot-title", h4(style = 'display: inline-block;', "First Party Claims Closed without Payment & Third Party Claims Closed Without payment"),
            uiOutput("first_third_party_closed_without_payment_single_firm_info", inline = T)),
          br(),
          plotlyOutput("first_third_party_closed_without_payment_single_firm_plot", width = '85%') %>% withSpinner(),
          align = "center"
        ),
        br(),
        br(),
        fluidRow(
          div(class = "plot-title", h4(style = 'display: inline-block;', "First Party Open Claims & Third Party Open Claims"),
            uiOutput("first_third_party_open_claims_single_firm_info", inline = T)),
          br(),
          plotlyOutput("first_third_party_open_claims_single_firm_plot", width = '85%') %>% withSpinner(),
          align = "center"
        )
      )
    ),
    ## SINGLE FIRM INPUT --------
    hidden(div(id = "single_firm_input_ui_div",
      box(
        id = "single_firm_input_ui_box",
        width = 3,
        div(class = 'inputdivpicker',
          shinyWidgets::pickerInput(
            label = "Select a firm",
            inputId = "select_single_firm",
            choices = sort(unique(all_data$shortname)),
            options = list(`live-search` = TRUE, `max-options` = 1),
            multiple = TRUE,
            selected = NULL,
          )
        ),
        div(class = 'inputdiv',
          sliderInput(
            inputId = "single_firm_year_range",
            label = "Select year range",
            min = 2018,
            max = 2022,
            value = c(2018, 2022),
            step = 1,
            width = '85%',
            sep = ""
          )
        ),
        div(class = 'inputdivpicker',
          shinyWidgets::pickerInput(
            label = "Select export file",
            inputId = "select_single_firm_export_type",
            choices = c("PDF", "Excel"),
            options = list(`live-search` = FALSE, `max-options` = 1),
            multiple = FALSE,
            selected = NULL,
          )
        ),
        hidden(
          div(id = 'single_firm_pdf_div',
            downloadBttn(
              outputId = "export_single_firm_pdf",
              "Export to PDF",
              style = 'simple',
              size = 'sm',
              icon = icon("file-pdf"))
          )),
        hidden(
          div(id = 'single_firm_excel_div',
            downloadBttn(
              outputId = "export_single_firm_excel",
              "Export to Excel",
              style = 'simple',
              size = 'sm',
              icon = icon("file-excel"))
          )
        )
      )
    )
    ),
    ## SINGLE GROUP INPUT --------
    hidden(div(id = "single_group_input_ui_div",
      box(
        id = "single_group_input_ui_box",
        width = 3,
        div(class = 'inputdivpicker',
          shinyWidgets::pickerInput(
            label = "Select a group",
            inputId = "select_single_group",
            choices = sort(unique(group_db$groupname)),
            options = list(`live-search` = TRUE, `max-options` = 1),
            multiple = TRUE,
            selected = NULL,
          )
        ),
        div(class = 'inputdiv',
          sliderInput(
            inputId = "single_group_year_range",
            label = "Select year range",
            min = 2018,
            max = 2022,
            value = c(2018, 2022),
            step = 1,
            width = '85%',
            sep = ""
          )
        ),
        div(class = 'inputdivpicker',
          shinyWidgets::pickerInput(
            label = "Select export file",
            inputId = "select_single_group_export_type",
            choices = c("PDF", "Excel"),
            options = list(`live-search` = FALSE, `max-options` = 1),
            multiple = FALSE,
            selected = NULL,
          )
        ),
        hidden(
          div(id = 'single_group_pdf_div',
            downloadBttn(
              outputId = "export_single_group_pdf",
              "Export to PDF",
              style = 'simple',
              size = 'sm',
              icon = icon("file-pdf"))
          )),
        hidden(
          div(id = 'single_group_excel_div',
            downloadBttn(
              outputId = "export_single_group_excel",
              "Export to Excel",
              style = 'simple',
              size = 'sm',
              icon = icon("file-excel"))
          )
        )
      )
    )
    )
  )
)
