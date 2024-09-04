shinyServer(function(input, output, session) {
  
  statFN <- reactive({
    fn = c("Minimum" = min, "Median" = median,
           "Mean" = mean, "Maximum" = max)
    fn[[input$stat]]
  })
  
  # Reactive Values ---------------------------------------------------------

  rv <- reactiveValues(last_param = NULL)
  
  # Filter ------------------------------------------------------------------
  
  selDates <- reactive({
    c(date_seq[date_lab == input$date_range[1]],
      date_seq[date_lab == input$date_range[2]])
  })
  
  datSub1 <- reactive({
    filter(df, Level == input$level & Date >= selDates()[1] & Date <= selDates()[2])
  })
  
  observe({
    req(datSub1())
    params = sort(unique(datSub1()$Parameter))
    if (is.null(rv$last_param) || input$parameter != rv$last_param) rv$last_param = input$parameter
    sel = if (rv$last_param %in% params) rv$last_param else params[1]
    updateSelectInput(session, 'parameter', choices = params, selected = sel)
    
    sites = sort(unique(datSub1()$Site))
    freezeReactiveValue(input, "sites")
    updatePickerInput(session, "sites", choices = sites, selected = sites)
  })
  
  datSub2 <- reactive({
    filter(datSub1(), Site %in% input$sites)
  })
  
  datSub3 <- reactive({
    filter(datSub2(), Parameter == input$parameter)
  })
  
  # Time Series Plot --------------------------------------------------------
  
  output$tsPlot <- renderPlotly({
    p = ggplot(datSub3(), aes(x = Date, y = Value, color = Site)) + 
      geom_point() +
      geom_line() +
      labs(x = "", y = input$parameter) +
      theme_bw()
    
    ggplotly(p)
  })
  
  # Box Plot ----------------------------------------------------------------
  
  output$boxPlot <- renderPlotly({
    p = ggplot(datSub3(), aes(x = Site, y = Value)) + 
      geom_boxplot(alpha = 0.3, fill = "grey80") +
      labs(y = input$parameter) +
      scale_x_discrete(limits = rev) +
      coord_flip() +
      theme_bw()
    
    ggplotly(p)
  })
  
  # Bar Plot ----------------------------------------------------------------
  
  barSumm <- reactive({
    datSub3() |> 
      group_by(Site) |> 
      summarise(Value = statFN()(Value, na.rm = TRUE))
  })
  
  output$barPlot <- renderPlotly({
    p = ggplot(barSumm(), aes(y = Site, x = Value)) +
      geom_col() +
      scale_y_discrete(limits = rev) +
      labs(x = input$parameter) +
      theme_bw() 
    
    ggplotly(p)
  })
  
  # Tile Plot ---------------------------------------------------------------
  
  tileSumm <- reactive({
    datSub2() |> 
      group_by(Site, Parameter) |> 
      summarise(Value = statFN()(Value, na.rm = TRUE)) |> 
      group_by(Parameter) |> 
      mutate(Percentile = percentile(Value))
  })
  
  output$tilePlot <- renderPlotly({
    p = ggplot(tileSumm(), aes(y = Site, x = Parameter, fill = Percentile, label = Value)) +
      geom_tile() +
      scale_y_discrete(limits = rev) +
      scale_fill_gradient2(mid = "#f7f7f7", low = scales::muted("blue"), high = scales::muted("red"), midpoint = 50) +
      labs(x = "",) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 0.5))
    
    ggplotly(p)
  })
  
  # Map ---------------------------------------------------------------------
  
  mapData <- reactive({
    req(datSub3())
    datSub3() |> 
      group_by(Site, Parameter) |> 
      summarise(Value = statFN()(Value, na.rm = TRUE)) |>
      mutate(Popup = paste(Parameter, "<br>", Value)) |> 
      left_join(site_locs)
  })
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(attributionControl = FALSE)) |>
      addTiles() |>
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo") |>
      addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") |>
      setView(lat = 28.03, lng = -82.775, zoom = 11) |>
      addLayersControl(baseGroups = c("Topo", "Imagery"),
                       options = layersControlOptions(collapsed = FALSE))
  })
  
  myPalette <- reactive({
    colorNumeric(palette = "Spectral", domain = mapData()$Value, reverse = TRUE)
  })
  
  observe({
    input$panel # take a dependency on panel changes
    leafletProxy("map")|>
      clearShapes() |>
      clearMarkers() |>
      clearControls()
    
    leafletProxy("map")|>
      addCircleMarkers(data = mapData(), lng = ~Lon, lat = ~Lat, label = ~Site, popup = ~Popup,
                       fillColor = ~myPalette()(Value), fillOpacity = 0.9, stroke = FALSE) |> 
      addLegend("bottomright", pal = myPalette(), values = mapData()$Value, 
                title = paste(input$stat, "<br>", input$parameter))
  })
  
  # Table/Download -------------------------------------------------------------------
  
  tableDownload <- reactive({
    mutate(datSub3(), Date = as.character(Date))
  })
  
  output$table <- DT::renderDataTable({
    tableDownload()
  }, options = list(searching = TRUE, bPaginate = TRUE, info = TRUE, scrollX = TRUE))
  
  output$downloadFilteredData <- downloadHandler(
    filename = function() {
      paste0("CHSJS-FilteredData-", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(tableDownload(), file, row.names = FALSE)
    }
  )
  
  output$downloadAllData <- downloadHandler(
    filename = function() {
      paste0("Culebra-AllData-", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(mutate(df, Date = as.character(Date)), file, row.names = FALSE)
    }
  )
  
})