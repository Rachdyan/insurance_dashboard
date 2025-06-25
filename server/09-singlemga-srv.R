output$selected_single_mga_name <- renderUI({
    req(input$select_single_mga)
    div(
        style = "text-align: center;",
        h2(input$select_single_mga)
    )
})

selected_mga_data <- reactive({
    req(input$select_single_mga)
    mga_complete_data %>%
    filter(MGA.Name == input$select_single_mga)
})

selected_mga_news_data <- reactive({
    req(input$select_single_mga)

    mga_news %>%
        filter(Firm == input$select_single_mga) %>%
        select(Date, Headline)

})

output$single_mga_year_founded_output <- renderUI({
    req(input$select_single_mga)
    div(
        style = "text-align: center;",
        h4("Year Founded:", class = 'mga-title'),
        h4(selected_mga_data()$Year.Founded)
    )
})

output$single_mga_headquarter_output <- renderUI({
    req(input$select_single_mga)
    div(
        style = "text-align: center;",
        h4("Headquartered In:", class = 'mga-title'),
        h4(selected_mga_data()$Country)
    )
})

output$single_mga_website_output <- renderUI({
    req(input$select_single_mga)
    web_link <- selected_mga_data()$Website %>%
        str_remove("^https://") %>%
        str_remove("//?")
    n_char_web <- nchar(web_link)
    div(
        style = "text-align: center;",
        h4("Website:", class = 'mga-title'),
        if(n_char_web >= 30){
            h5(a(web_link, href = selected_mga_data()$Website))
        } else {
            h4(a(web_link, href = selected_mga_data()$Website))
        }
    )
})


output$single_mga_size_output <- renderUI({
    req(input$select_single_mga)
    div(
        style = "text-align: center;",
        h4("Size", class = 'mga-title'),
        div(class = "mga-box-content", p(selected_mga_data()$Size, class = 'mga-content'))
    )
})

output$single_mga_market_reach_output <- renderUI({
    req(input$select_single_mga)
    div(
        style = "text-align: center;",
        h4("Market Reach", class = 'mga-title'),
        div(class = "mga-box-content", p(selected_mga_data()$Market.Reach, class = 'mga-content'))
    )
})

output$single_mga_focus_cyber_output <- renderUI({
    req(input$select_single_mga)
    div(
        style = "text-align: center;",
        h4("Focus on Cyber", class = 'mga-title'),
        div(class = "mga-box-content", p(selected_mga_data()$Focus.on.Cyber, class = 'mga-content'))
    )
})


output$single_mga_active_output <- renderUI({
    req(input$select_single_mga)
    div(
        style = "text-align: center;",
        h4("Size", class = 'mga-title'),
        div(class = "mga-box-content", p(selected_mga_data()$Active...Defunct, class = 'mga-content'))
    )
})

output$single_mga_countries_active_output <- renderUI({
    req(input$select_single_mga)
    div(
        style = "text-align: center;",
        h4("Countries Active", class = 'mga-title'),
        div(class = "mga-box-content", p("", class = 'mga-content'))
    )
})

output$single_mga_target_segments_output <- renderUI({
    req(input$select_single_mga)
    div(
        style = "text-align: center;",
        h4("Target Segments", class = 'mga-title'),
        div(class = "mga-box-content", p(selected_mga_data()$Target.Market.Segment, class = 'mga-content'))
    )
})

output$single_mga_vc_backed_output <- renderUI({
    req(input$select_single_mga)
    div(
        style = "text-align: center;",
        h4("VC-backed", class = 'mga-title'),
        div(class = "mga-box-content", p(selected_mga_data()$VC.backed.yes.no, class = 'mga-content'))
    )
})

output$single_mga_total_raised_output <- renderUI({
    req(input$select_single_mga)
    div(
        style = "text-align: center;",
        h4("Total Raised", class = 'mga-title'),
        div(class = "mga-box-content", p(selected_mga_data()$Total.Raised..VC., class = 'mga-content'))
    )
})

output$single_mga_investors_output <- renderUI({
    req(input$select_single_mga)
    div(
        style = "text-align: center;",
        h4("Investors", class = 'mga-title'),
        div(class = "mga-box-content", p("", class = 'mga-content'))
    )
})


output$single_mga_capacity_providers_output <- renderUI({
    req(input$select_single_mga)
    div(
        style = "text-align: center;",
        h4("Capacity Providers", class = 'mga-title'),
        div(class = "mga-box-content", p("", class = 'mga-content'))
    )
})

output$single_mga_placeholder_1_output <- renderUI({
    req(input$select_single_mga)
    div(
        style = "text-align: center;",
        h4("Placeholder 1", class = 'mga-title'),
        div(class = "mga-box-content", p("", class = 'mga-content'))
    )
})

output$single_mga_offerings_output <- renderUI({
    req(input$select_single_mga)
    div(
        style = "text-align: center;",
        h4("Offerings", class = 'mga-title'),
        div(class = "mga-box-content", p("", class = 'mga-content'))
    )
})

output$single_mga_description_output <- renderUI({
    req(input$select_single_mga)
    div(
        style = "text-align: center;",
        h4("Description", class = 'mga-title'),
        div(class = "mga-box-content", p("", class = 'mga-content'))
    )
})

output$single_mga_latest_news_output <- renderUI({
    req(input$select_single_mga)
    div(
        style = "text-align: center;",
        h4("Latest News", class = 'mga-title'),
        div(
            dataTableOutput("single_mga_news_table")
        )
    )
})

output$single_mga_news_table <- renderDataTable({
    datatable(selected_mga_news_data(),
        escape = FALSE,
        selection = "none",
        rownames= FALSE,
        options = list(
            columnDefs = list(list(className = 'dt-left', targets = 1)),
            pageLength = 5,
            searching = FALSE,
            autoWidth = FALSE,
            scrollX = FALSE,
            lengthChange = FALSE
        )
    )

})
