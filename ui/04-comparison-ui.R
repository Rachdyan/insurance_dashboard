tabItem(
    tabName = "compare",
    fluidRow(
        tabBox(
            width = 12,
            # The id lets us use input$tabset1 on the server to find the current tab
            id = "compare_box",
            tabPanel("Compare by Group", id = 'compare_group',
                splitLayout(cellWidths = c('36.3%', '20%', '40%'), style = 'margin-left: 2%;',
                    div(virtualSelectInput(
                        inputId = "comparison_select_group",
                        label = "Select groups (max 3):",
                        choices = group_db$groupname,
                        multiple = TRUE,
                        search = TRUE,
                        showValueAsTags = T,
                        keepAlwaysOpen = T,
                        maxValues = 3),
                        br()),
                    div(class = 'boxdiv', align = 'center',
                        box(id = "selected_group_box_id", width = 12,  title = "Selected Groups",
                            status = 'primary', align = 'center',
                            uiOutput('selected_group_box')
                        )
                    ),
                    div(id = 'group_statistic',
                        virtualSelectInput(
                            inputId = "comparison_select_group_statistic",
                            label = "Select statistic:",
                            choices = c("Written Premium","% Change in Written Premiums", "Earned Premium", "Total Losses",
                                "Loss Ratio", "Number of Policies", "Average Premium", "Direct Defense Cost Containment Paid",
                                "Case Reserves", "Total Claims Closed with Payment", "Total Claims Closed without Payment",
                                "1st Party Claims Closed Without Payment", "3rd Party Claims Closed Without Payment",
                                "1st Party Open Claims", "3rd Party Open Claims"),
                            multiple = TRUE,
                            keepAlwaysOpen = T),
                        actionBttn("comparison_generate_group_stat_btn", "Generate Report", style = 'simple', size = 'sm', color = 'primary')
                    )
                ),
                fluidRow(
                    box(id = 'group_results' ,width = 12,
                        splitLayout(
                            uiOutput("comparison_export_group_ui",  inline = TRUE),
                            uiOutput("comparison_year_range_group_ui",  inline = TRUE),
                        ),
                        uiOutput("comparison_premium_written_group_ui"),
                        uiOutput("comparison_yoy_change_premium_written_group_ui"),
                        uiOutput("comparison_premium_earned_group_ui"),
                        uiOutput("comparison_total_losses_group_ui"),
                        uiOutput("comparison_loss_ratio_group_ui"),
                        uiOutput("comparison_num_of_policies_group_ui"),
                        uiOutput("comparison_avg_premium_group_ui"),
                        uiOutput("comparison_direct_defense_group_ui"),
                        uiOutput("comparison_case_reserves_group_ui"),
                        uiOutput("comparison_total_claims_closed_with_payment_group_ui"),
                        uiOutput("comparison_total_claims_closed_without_payment_group_ui"),
                        uiOutput("comparison_first_party_claims_closed_without_payment_group_ui"),
                        uiOutput("comparison_third_party_claims_closed_without_payment_group_ui"),
                        uiOutput("comparison_first_party_open_claims_group_ui"),
                        uiOutput("comparison_third_party_open_claims_group_ui")
                    )
                )
            ),
            tabPanel("Compare by Firm", id = 'compare_firm',
                splitLayout(cellWidths = c('36.3%', '36.3%', '23%'), style = 'margin-left: 2%;',
                    div(virtualSelectInput(
                        inputId = "comparison_select_firm_group",
                        label = "Select groups:",
                        choices = group_db$groupname,
                        multiple = TRUE,
                        search = TRUE,
                        showValueAsTags = T,
                        keepAlwaysOpen = T),
                        br()),
                    div(
                        # uiOutput('select_firm_chocies')
                        virtualSelectInput(
                            inputId = "select_firm_compare",
                            label = "Select firm(s) from the selected group(s) (max 3):",
                            choices = NULL,
                            multiple = TRUE,
                            search = TRUE,
                            showValueAsTags = T,
                            keepAlwaysOpen = T,
                            maxValues = 3)
                    ),
                    div(
                        fluidRow(
                            div(class = 'boxdiv', align = 'center',
                                box(id = "selected_firm_box_id", width = 12,  title = "Selected Firm",
                                    status = 'primary', align = 'center',
                                    uiOutput('selected_firm_box_output')
                                )
                            )
                        ),
                        fluidRow(
                            div(align = 'center',
                                actionBttn("confirm_firm_selection", "Confirm Selection", style = 'simple', size = 'sm', color = 'primary') %>%
                                    bs_attach_modal('firm_stat_modal')
                            )
                        )

                    ),
                ),
                fluidRow(
                    box(id = 'compare_firm_results' ,width = 12,
                        splitLayout(
                            uiOutput("comparison_export_firm_ui",  inline = TRUE),
                            uiOutput("comparison_year_range_firm_ui",  inline = TRUE),
                        ),
                        uiOutput("comparison_premium_written_firm_ui"),
                        uiOutput("comparison_yoy_change_premium_written_firm_ui"),
                        uiOutput("comparison_premium_earned_firm_ui"),
                        uiOutput("comparison_total_losses_firm_ui"),
                        uiOutput("comparison_loss_ratio_firm_ui"),
                        uiOutput("comparison_num_of_policies_firm_ui"),
                        uiOutput("comparison_avg_premium_firm_ui"),
                        uiOutput("comparison_direct_defense_firm_ui"),
                        uiOutput("comparison_case_reserves_firm_ui"),
                        uiOutput("comparison_total_claims_closed_with_payment_firm_ui"),
                        uiOutput("comparison_total_claims_closed_without_payment_firm_ui"),
                        uiOutput("comparison_first_party_claims_closed_without_payment_firm_ui"),
                        uiOutput("comparison_third_party_claims_closed_without_payment_firm_ui"),
                        uiOutput("comparison_first_party_open_claims_firm_ui"),
                        uiOutput("comparison_third_party_open_claims_firm_ui")
                    )
                ),
                bs_modal(id = 'firm_stat_modal', title = 'Generate Report',
                    body =  virtualSelectInput(
                        inputId = "comparison_select_firm_statistic",
                        label = "Select statistic:",
                        choices = c("Written Premium","% Change in Written Premiums", "Earned Premium", "Total Losses",
                            "Loss Ratio", "Number of Policies", "Average Premium", "Direct Defense Cost Containment Paid",
                            "Case Reserves", "Total Claims Closed with Payment", "Total Claims Closed without Payment",
                            "1st Party Claims Closed Without Payment", "3rd Party Claims Closed Without Payment",
                            "1st Party Open Claims", "3rd Party Open Claims"),
                        multiple = TRUE,
                        keepAlwaysOpen = T),
                    footer = tagList(
                        actionBttn("comparison_generate_firm_stat_btn", "Generate Report", style = "simple", size = "sm", color = "primary"),
                        bs_modal_closebutton("Close")
                    ))
            )

        )
    )

)
