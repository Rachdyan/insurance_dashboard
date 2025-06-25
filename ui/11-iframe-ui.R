tabItem(
    tabName = "iframe",
    fluidRow(
        box(
            width = 12,
            tags$iframe(style="height:600px; width:100%", src="resources/premiums_by_geography.pdf"))
    )
)
