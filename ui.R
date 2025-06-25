

header <- dashboardHeader(
  title = span(id = "dash_title",
    "Insurance Dashboard"
  )
  # polished::profile_module_ui("profile")
)

sidebar <- dashboardSidebar(
  sidebarMenu(id = 'tabs',
    #menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Insurance Company Table", tabName = "companytable", icon = icon("table")),
    menuItem("Single Company Graph", tabName = "singlefirm", icon = icon("building")),
    menuItem("Company Comparison", tabName = "compare", icon = icon("code-compare")),
    # menuItem("Group View", tabName = 'groupview', icon = icon("group-arrows-rotate")),
    #menuItem("Wiki", tabName = "wiki", icon = icon("book")),
    #menuItem("FAQ", tabName = "global_wiki", icon = icon("globe")),
    menuItem("Managing General Agent Table", tabName = "mgatable", icon = icon("table")),
    menuItem("Single MGA Overview", tabName = "singlemga", icon = icon("building")),
    menuItem("Summary by Geography", tabName = "geography", icon = icon("table"))
    #menuItem("Iframe", tabName = "iframe", icon = icon("building"))
  )
)

clickFaq_text <- "shinyjs.clickFaq = function(params) {$(`.faqcollapsible:eq(${params})`)[0].click()}"
makeActive_text <- "shinyjs.makeActive = function(params) {$(`.faqcontent:eq(${params})`)[0].style.maxHeight = '38px'}"

# jsCode <- "shinyjs.clickFaq = function(params) {document.querySelector('.faqcollapsible').click()}"
# jsCode <- "shinyjs.clickFaq = function() {$('.faqcollapsible').click()}"


body <- dashboardBody(
  tags$head(
    # tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(src = "custom.js"),
    tags$script(src="https://cdn.jsdelivr.net/npm/lodash@4.17.15/lodash.min.js")
  ),
  includeCSS("www/styles.css"),
  useShinyjs(),
  extendShinyjs(text = clickFaq_text, functions = c("clickFaq")),
  extendShinyjs(text = makeActive_text, functions = c("makeActive")),
  tabItems(
    source("ui/01-dashboard-ui.R", local = TRUE)$value,
    source("ui/02-companytable-ui.R", local = TRUE)$value,
    source("ui/03-singlefirm-ui.R", local = TRUE)$value,
    source("ui/04-comparison-ui.R", local = TRUE)$value,
    source("ui/06-wiki-ui.R", local = TRUE)$value,
    source("ui/07-globalwiki-ui.R", local = TRUE)$value,
    source("ui/08-mgatable-ui.R", local = TRUE)$value,
    source("ui/09-singlemga-ui.R", local = TRUE)$value,
    source("ui/10-geography-ui.R", local = TRUE)$value,
    source("ui/11-iframe-ui.R", local = TRUE)$value
  )

)

ui <- dashboardPage(
  title = "Insurance Dashboard",
  header,
  sidebar,
  body,
  skin = "black"
)

# ui <- secure_app(ui)

# secure_ui(ui,
#   sign_in_page_ui = sign_in_page)
#
