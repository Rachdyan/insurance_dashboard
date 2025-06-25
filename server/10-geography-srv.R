

## UI
observe({
    req(input$select_geography, input$select_geography_year, input$select_geography_chart_type)
    show('export_geography_span')
})

observeEvent(input$select_geography_export, {
    if(input$select_geography_export == "Results as PDF"){
        show("geography_pdf_div")
        hide("geography_excel_div")
    }
    else{
        show("geography_excel_div")
        hide("geography_pdf_div")
    }
})


## DATA ----------
selected_geography_data <- reactive({
    req(input$select_geography, input$select_geography_year, input$select_geography_chart_type)
     geography_pivot_df %>%
        filter(Geography %in% input$select_geography & year %in% input$select_geography_year) %>%
        mutate(Geography = as.factor(Geography))
})

selected_geography_sum_data <- reactive({
    req(input$select_geography, input$select_geography_year, input$select_geography_chart_type)
    selected_geography_data() %>%
    group_by(year) %>%
        summarise(premium_billions = sum(premium_billions))
})


geography_color_palette <- eventReactive(input$select_geography, {
    req(input$select_geography)
    palette <- setNames(
        cividis(length(levels(selected_geography_data()$Geography)), alpha = 1, begin = 0, end = 1, direction = 1),
        levels(selected_geography_data()$Geography))
    palette

})

observeEvent(input$select_geography_chart_type, {
    req("Global" %in% input$select_geography)
    if(input$select_geography_chart_type == "Stacked"){
        updatePickerInput(session = session, inputId = "select_geography",
            choices = unique(geography_pivot_df$Geography), selected = "Global")
        showNotification("Only shows Global when 'Global' geography is selected and the chart type is 'Stacked'.", type = "warning")
    }
}, ignoreInit = TRUE)

observeEvent(input$select_geography, {
    req(input$select_geography_chart_type == "Stacked")
    if("Global" %in% input$select_geography){
        updatePickerInput(session = session, inputId = "select_geography",
            choices = unique(geography_pivot_df$Geography), selected = "Global")
    }
}, ignoreInit = TRUE)

## TABLE -------

selected_geography_display_data <- reactive({
    req(c(input$select_geography, input$select_geography_year))
    geography_raw_df %>%
        filter(Geography %in% input$select_geography) %>%
        select(Geography, all_of(input$select_geography_year)) %>%
        rename('Premium ($Billion)' = Geography)
})

selected_geography_table_export <- reactive({
    selected_geography_display_data() %>%
        tableGrob(
            rows = NULL,
            theme=ttheme_default(base_size = 11,
                colhead = list(fg_params=list(cex = 0.8)),
                core = list(fg_params=list(cex = 0.9))))

})

## PLOT ---------
premiums_by_geography_ggplot <- reactive({
    req(input$select_geography, input$select_geography_year, input$select_geography_chart_type)
    if(identical(selected_geography_data(), numeric(0)) || nrow(selected_geography_data()) == 0 || is.null(input$select_geography_chart_type)){
        return(ggplot() + theme_void())
    }

    if(input$select_geography_chart_type == "Clustered"){
        ggplot(selected_geography_data(), aes(x = as.factor(year), y = premium_billions, fill = Geography, text = glue("<span><b>Premium:</b> ${comma(premium_billions)} B"))) +
            geom_bar(stat="identity", position="dodge") +
            scale_fill_manual(values = geography_color_palette()) +
            scale_y_continuous(limits = c(0, max(selected_geography_data()$premium_billions) * 1.3), labels = label_number(prefix = "$", suffix = "B", scale_cut = cut_short_scale()), expand = c(0.001,0)) +
            ylab("Premium (Billions)") +
            universal_graph_theme_with_legend(0.05, 0.9)
    } else{
        ggplot(selected_geography_data(), aes(x = as.factor(year), y = premium_billions, fill = Geography, text = glue("<span><b>Premium:</b> ${comma(premium_billions)} B"))) +
            geom_bar(stat="identity", position="stack") +
            scale_fill_manual(values = geography_color_palette()) +
            scale_y_continuous(limits = c(0, max(selected_geography_sum_data()$premium_billions) * 1.3), labels = label_number(prefix = "$", suffix = "B", scale_cut = cut_short_scale()), expand = c(0.001,0)) +
            ylab("Premium (Billions)") +
            universal_graph_theme_with_legend(0.05, 0.9)
    }
})

premiums_by_geography_ggplot_export <- reactive({
    premiums_by_geography_ggplot() +
        ggtitle("Premiums by Geography") +
        single_graph_export_theme

})


premiums_by_geography_plotly <- reactive({
    suppressWarnings({
        if(length(premiums_by_geography_ggplot()$data) == 0){
            ggplotly(premiums_by_geography_ggplot())
        } else{
            plotly <- ggplotly(premiums_by_geography_ggplot(), tooltip = "text", dynamicTicks = TRUE)  %>%
                layout(legend = list(x = 0.01, y = 0.99, title = list(text=''), bgcolor = 'rgba(255,255,255,0.2)', font = list(size = 9), tracegroupgap = 1))  %>%
                layout(showlegend = TRUE,
                    yaxis = list(showline= T, linewidth=1.5, linecolor='black',
                        showticklabels = T,  tickprefix = "   $", ticksuffix = "B"),
                    xaxis = list(showline= T, linewidth=1.5, linecolor='black', gridwidth=2))

            plotly$x$layout$yaxis$autorange <- FALSE
            if(input$select_geography_chart_type == "Clustered"){
                plotly$x$layout$yaxis$range <- c(0, max(selected_geography_data()$premium_billions) * 1.3)
            } else{
                plotly$x$layout$yaxis$range <- c(0, max(selected_geography_sum_data()$premium_billions) * 1.3)
            }
            plotly
        }
    })
})

output$premiums_by_geography_plot <- renderPlotly(premiums_by_geography_plotly())

## TABLE RENDER -------

output$geography_table_output <- renderUI({
    req(input$select_geography, input$select_geography_year, input$select_geography_chart_type)
    if(ncol(selected_geography_display_data()) <= 10){
        DTOutput("geography_table", width = 'auto')
    } else{
        DTOutput("geography_table", width = '100%')
    }
})

output$geography_table <- renderDataTable({
    n_col <- ncol(selected_geography_display_data())
    scrollX_status <- ifelse(n_col <= 10, FALSE, TRUE)
    ##print(n_col)
    ##print(scrollX_status)

    DT::datatable(
        selected_geography_display_data(),
        style = "auto",
        escape = FALSE,
        selection = "none",
        rownames= FALSE,
        options = list(
            pageLength = 25,
            searching = FALSE,
            autoWidth = TRUE,
            info = FALSE,
            dom = 't',
            scrollX = scrollX_status
        )
    )
})

## DOWNLOAD --------

output$export_geography_pdf = downloadHandler(
    filename = function() {glue("premiums_by_geography.pdf")},
    content = function(file) {
        withProgress(message = glue("Exporting premiums_by_geography.pdf"), {


            cover_title <- textGrob(glue("Qualrisk Cyber Insurance Center"), gp=gpar(fontsize=22, fontface = 'bold'),
                x = unit(-0.075, "npc"), y = unit(0.150, "npc"))

            cover_title2 <- textGrob(glue("Cyber Dashboard"), gp=gpar(fontsize=18),
                x = unit(-0.365, "npc"), y = unit(0.150, "npc"))

            cover_title3 <- textGrob(glue("Premiums by Geography"), gp=gpar(fontsize=12.5),
                x = unit(-0.57, "npc"), y = unit(0.150, "npc"), just = "left")

            cover_margin = unit(0.9, "line")

            cover_content <- gtable_col('cover', grobs = list(cover_title,  cover_title2, cover_title3),
                heights = unit.c(grobHeight(cover_title) + 1.2*cover_margin,
                    grobHeight(cover_title2) + cover_margin,
                    grobHeight(cover_title3) + cover_margin))

            cover_content_grob = arrangeGrob(zeroGrob(),
                cover_content,
                heights = unit(c(6, 1), c('cm', 'npc')),
                as.table = FALSE)

            logo_png <- readPNG("./www/img/qcclogo_only.png")

            cover_logo <- rasterGrob(logo_png,
                height = unit(1.7, "cm"), width = unit(1.7, "cm"),
                x = unit(1.2, "npc"), y = unit(0.685, "npc"))

            cover_logo_with_line <- arrangeGrob(
                cover_logo,
                left = textGrob("                "),
                right = segmentsGrob(
                    x0 = unit(4, "npc"), y0 = unit(0.74, "npc"),
                    x1 = unit(4, "npc"), y1 = unit(0.61, "npc"))
            )

            logo <- rasterGrob(logo_png,
                height = unit(0.9, "cm"), width = unit(0.9, "cm"),
                x = unit(0.025, "npc"), y = unit(0.25, "npc"))

            bottom_text <- c(1) %>%
                map(~ textGrob(
                    glue("Qualrisk Cyber Insurance Center | {input$select_single_firm} | {.x}"),
                    gp=gpar(fontsize=9), y = 1))

            incProgress(1/2)

            ppl <- list(
                p0 = arrangeGrob(zeroGrob(),
                    top = cover_content_grob,
                    left = cover_logo_with_line,
                    nrow = 1),
                p1 = arrangeGrob(
                    grobs=list(
                        zeroGrob(),
                        premiums_by_geography_ggplot_export(),
                        zeroGrob(),
                        selected_geography_table_export(), zeroGrob(),
                        zeroGrob(),  zeroGrob(),
                        zeroGrob()
                    ),
                    padding = unit(0.1, "line"),
                    top = logo, bottom = bottom_text[[1]],
                    left = textGrob("      "), right = textGrob("      "),
                    heights = c(0.5, 7, 0.75, 2, 1, 1, 1, 1),
                    ncol = 1, nrow = 8)
            )

            class(ppl) <- c("arrangelist", "list")
            incProgress(1/2)
            ggsave(file, ppl, device = "pdf", width = 12, height = 8)
        })
    }
)

output$export_geography_excel = downloadHandler(
    filename = function() {glue("premiums_by_geography.xlsx")},
    content = function(file) {
        withProgress(message = glue("Exporting premiums_by_geography.xlsx"), {
            my_workbook <- createWorkbook()

            worksheet_name <- "Premiums by Geography"
            worksheet_content <- list(
                selected_geography_display_data()
            )
            incProgress(1/2)
            # print(worksheet_content)

            for(i in 1:length(worksheet_name)){
                addWorksheet(
                    wb = my_workbook,
                    sheetName = worksheet_name[i])
                writeData(
                    my_workbook,
                    sheet = i,
                    worksheet_content[[i]])
            }

            incProgress(1/2)
            saveWorkbook(my_workbook, file, overwrite = TRUE)
        })
    }
)
