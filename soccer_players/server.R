server = function(input, output, session) {
  
  # Selector by name, league and club
  filter_df <- reactive({
    
    filtered_df = df

    # Filter NA
    if(!input$show_na)
    {
      filtered_df = filtered_df %>% filter(., !is.na(League))
    }
    
    # Filter name
    filtered_df = filtered_df %>% filter(., grepl(tolower(input$search_player), Name))
    
    # Filter age
    filtered_df = filtered_df %>% filter(., Age >= input$age[1], Age <= input$age[2])

    # Filter country
    if(input$country != 'a')
    {
      filtered_df = filtered_df %>% filter(., Country == input$country)
    }

    # Filter league and club
    if(input$league != 'a')
    {
      if(input$club != 'a')
      {
        filtered_df = filtered_df %>% filter(., League == input$league,
                                             Club == input$club)
      }
      else
      {
        filtered_df = filtered_df %>% filter(., League == input$league)
      }
    }
    
    # Filter position
    if(input$position != 'a')
    {
      filtered_df = filtered_df %>%
        filter(., input$position == strsplit(Position, "/"))
    }

    if(input$categories != 'a')
    {
      categ = paste(input$categories, "_agg", sep = "")
      filtered_df %>% 
        select(., Name,
               Categories[[input$categories]],
               categ,
               Prediction,
               'Value after 2 years' = Market.Value..Euros.) %>%
        dplyr::rename(., Aggregate = categ)
    }
    else
    {
      filtered_df %>%
        select(., Name,
               Age,
               Position,
               Country,
               League,
               Club,
               Prediction,
               'Value after 2 years' = Market.Value..Euros.) %>%
        arrange(., by=Name)
    }
  })

  # Update the club list per league
  observe({
    club <- c('Choose a club' = 'a', unique(df %>% filter(., League == input$league & !is.na(League)) %>% select(., Club)) %>% arrange(., Club) %>% select(., Club))
    updateSelectizeInput(
      session, "club",
      choices = club,
      selected = club[1]
    )
  })
  
  observe({
    attribute <- c('Choose an attribute' = 'a', Categories[[input$categories]])
    updateSelectInput(
      session, "attribute",
      choices = attribute,
      selected = attribute[1]
    )
  })
  
  # Render spider graph
  get_aggreg <- reactive({
    df %>%
      filter(., grepl(tolower(input$search_player), Name)) %>%
      select(., Name, contains("_agg") & !contains("Other"))
  })
  
  output$plotspider <- renderPlotly({
    s <- plot_ly(
      type = 'scatterpolar',
      mode = "closest",
      fill = 'toself') %>%
      add_trace(
        r = as.matrix(get_aggreg()[1,2:9]),
        theta = c("Physical", "Technical", "Shooting", "Mental", "Tactical", "Personality", "Goal", "Defense"),
        showlegend = TRUE,
        mode = "markers",
        name = get_aggreg()[1,1]) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0,20)
          )
        ),
        showlegend=TRUE
      )
    s
  })
  
  # Render the players table
  output$table <- DT::renderDataTable({
    table = filter_df()
    
    if(input$attribute != 'a')
    {
      table %>%
        select(., -Prediction,
               -'Value after 2 years')
    }
    
      datatable(table, rownames=FALSE,
                options = list(searching = FALSE,
                               pageLength = 20))
    },options =  list(
    fnRowCallback = I("function( nRow, aData, iDisplayIndex, iDisplayIndexFull )     {f_fnRowCallback( nRow, aData, iDisplayIndex, iDisplayIndexFull ) }")))
    
  observe({   
    if(!is.null(input$request_i)){
      session$sendCustomMessage(type = "showRequested_i", paste( "row:     ",input$request_i[1]))}
  })
  
  # Render the price plots
  get_attribute <- reactive({
    filter_df() %>% 
      select(., input$attribute, 'Value after 2 years')
  })
  
  output$plot_price <- renderPlotly({
    table = filter_df()
      
    if(input$categories == 'a' & input$attribute == 'a')
    {
    pred = table %>% 
      select(., Prediction) %>%
      filter(., !is.na(Prediction)) %>%
      mutate(., Prediction = log(Prediction))
    
    market_value = table %>%
      select(., Value = 'Value after 2 years') %>%
      filter(., !is.na(Value)) %>%
      mutate(., Value = log(Value))
    
      density1 = density(pred$Prediction)
      density2 = density(market_value$Value)
      
      fig <- plot_ly(x = ~density1$x, y = ~density1$y, type = 'scatter', mode = 'lines', name = 'Prediction', fill = 'tozeroy',
                     fillcolor = 'rgba(168, 216, 234, 0.5)',
                     line = list(width = 0.5)) %>%
        add_trace(x = ~density2$x, y = ~density2$y, name = 'Market value', fill = 'tozeroy',
                  fillcolor = 'rgba(255, 212, 96, 0.5)') %>%
        layout(xaxis = list(title = 'Value'),
               yaxis = list(title = 'Density'))
      
      fig
    }
    else if(input$attribute != 'a')
    {
      attr = input$attribute
      
      pred = table %>% 
        select(., input$attribute, Prediction) %>%
        filter(., !is.na(Prediction)) %>%
        mutate(., Prediction = log(Prediction))
      
      market_value = get_attribute() %>%
        dplyr::rename(., Value = 'Value after 2 years') %>%
        filter(., !is.na(Value)) %>%
        mutate(., Value = log(Value))
      
      fig <- plot_ly(market_value, x = ~jitter(market_value[[1]], factor=3), y = ~market_value[[2]], name = "Value", type = "scatter") %>%
        # add_trace(market_value, x = ~input$attribute, y = ~market_value, name = "Market value") %>%
        layout(xaxis = list(title = attr),
                            yaxis = list(title = "Value"))

      fig
    }
    else if(input$categories != 'a')
    {
      # Categ = paste(input$categories, "_agg", sep = "")
      
      pred = table %>%
        select(., Aggregate, Prediction) %>%
        filter(., !is.na(Prediction)) %>%
        mutate(., Prediction = log(Prediction))
      
      market_value = table %>%
        select(., Aggregate, Value = 'Value after 2 years') %>%
        filter(., !is.na(Value)) %>%
        mutate(., Value = log(Value))

      fig <- plot_ly(market_value, x = ~jitter(Aggregate, factor = 3), y = ~Value, name = "Value", type = "scatter") %>%
        # add_trace(pred, x = ~jitter(Aggregate, factor = 3), y = ~Prediction, name = "Prediction", type = "scatter") %>%
        layout(xaxis = list(title = input$categories),
               yaxis = list(title = "Value"))
      
      fig
    }
  })
  
  output$linear_model <- renderPrint({
    summary(lin)
  })
  
  output$clusters <- renderPlotly({
    features.pca = prcomp(features[1:20000,], center = TRUE,scale. = TRUE)
    ggbiplot(features.pca)
  })
  
  output$messi <- renderImage({
    outfile <- tempfile(fileext = '.png')
    
    png(outfile, width = 400, height = 300)
    hist(rnorm(input$obs), main = "Generated in renderImage()")
    dev.off()
    
    # Return a list containing the filename
    list(src = outfile,
         contentType = 'image/png',
         width = 400,
         height = 300,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
}
















