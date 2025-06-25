
server <- function(input, output, session) {

  # callModule(
  #   profile_module,
  #   id = "profile"
  # )

  # call the server part
  # check_credentials returns a function to authenticate users
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )

  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })

  # val_tbl <- reactive({
  #   req(input$val_date)
  #   loss_run(input$val_date)
  # })



  observeEvent(input$tabs, {
    # Drop the leading '#' symbol
    hash <- substring(getUrlHash(), 2)

    # This is so we don't 'update' to the tab we're already in (since clicking on
    # the sidebar already switches tabs.)
    if (hash != input$tabs) {
      # The 'push' argument is necessary so that the hash change event occurs and
      # so that the other observer is triggered.
      updateQueryString(paste0("#", input$tabs), mode = "push")
    }
  }, ignoreInit = TRUE)

  observeEvent(getUrlHash(), {
    hash <- substring(getUrlHash(), 2)
    updateTabItems(session, "tabs", hash)
  })

  source("server/01-dashboard-srv.R", local = TRUE)
  source("server/02-companytable-srv.R", local = TRUE)

  source("server/03-singlegraph-srv/03-singlegraph-group-srv.R", local = TRUE)
  source("server/03-singlegraph-srv/03-singlegraph-firm-srv.R", local = TRUE)

  source("server/04-comparison-srv/04-comparison-group-srv.R", local = TRUE)
  source("server/04-comparison-srv/04-comparison-firm-srv.R", local = TRUE)

  source("server/06-wiki-srv.R", local = TRUE)
  source("server/07-globalwiki-srv.R", local = TRUE)
  source("server/08-mgatable-srv.R", local = TRUE)
  source("server/09-singlemga-srv.R", local = TRUE)
  source("server/10-geography-srv.R", local = TRUE)
}

## secure_server(server)

