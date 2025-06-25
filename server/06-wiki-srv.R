
company_faq_data <- reactive({
        req(input$select_company_faq)
        result <- wiki_db %>%
                filter(shortname == input$select_company_faq) %>%
                select(-shortname)
})

firm_overview_text <- reactive({
        req(input$select_company_faq)
        result <- firm_overview_db %>%
                filter(shortname == input$select_company_faq) %>%
                select(Wiki.Text) %>%
                unlist() %>%
                unname() %>%
                str_replace_all(pattern = "\\n", replacement = "<br/>")
})

output$firm_title_wiki <- renderUI({
        h2(input$select_company_faq)
})

output$firm_overview_wiki <- renderText({
        firm_overview_text()
})

output$company_faq <- renderUI({
        faq(company_faq_data(), faqtitle = "", height = '90%', width = "99%")
})



