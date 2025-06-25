

output$global_faq <- renderUI({
        faq(global_wiki_db, faqtitle = 'Global Wiki', width = '98%', height = '90%')

})
