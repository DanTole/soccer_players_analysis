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

    if(input$categories != 'a')
    {
      filtered_df %>% 
        select(., Name,
               Categories[[input$categories]])
    }
    else
    {
      filtered_df %>%
        select(., Name,
               Age,
               Country,
               League,
               Club,
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
  
  # Render the players table
  output$table <- DT::renderDataTable({
    table = filter_df()

    attr = input$attribute
    
    if(0)
    {
      # table = table %>% mutate(.,
      #                          attr = cell_spec(table[attr], 'html',
      #                                             color = ifelse(1,
      #                                                     "green",
      #                                                     "red")))
      datatable(table, rownames=FALSE,
                options = list(searching = FALSE,
                               pageLength = 20)) %>%
        formatStyle(., attr, color = ifelse(attr > 10,
                                      "red",
                                      "blue"))
    }
    else
    {
      datatable(table, rownames=FALSE,
                options = list(searching = FALSE,
                               pageLength = 20))
    }
  })
  
  # Render the map (I hope it does ...)
  output$map <- renderLeaflet({
    table = filter_df()
    if(input$categories != 'a')
    {
      table %>% group_by(., table$Country) %>% summarise(., median())
      leaflet(table) %>% 
        addTiles()  %>% 
        setView( lat=10, lng=0 , zoom=2) %>%
        addPolygons(fillColor = ~mypalette(POP2005), stroke=FALSE )
    }
  else
  {
    palette = "Reds"
    x = table$Market.Value..Euros.
    
    list_nations <- map_data("world", region = table$Country)

    region.lab.data <- list_nations %>%
    group_by(region) %>%
    summarise(long = mean(long), lat = mean(lat))
    
    leaflet(table) %>%
      addTiles(., group = "region.lab.data") %>%
      addProviderTiles("Stamen.Toner", group = "Toner") %>%
      addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
      addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.8, color = ~pal(x)) %>%
      addLegend("bottomright", pal = pal, values = ~x, title = legend.title, labFormat = labelFormat(suffix = ""), opacity = 0.3) %>%
      addLayersControl(baseGroups = c("OSM (default)", "Toner", "Toner Lite", "CartoDB"),options = layersControlOptions(collapsed = FALSE))
    
    # ctry = unique(df$Country)
    # 
    # table = table %>% 
    #   group_by(., Country) %>%
    #   summarise(., median(input$attribute))
    # 
    # ggplot(list_nations, aes(x = long, y = lat)) +
    #   geom_polygon(aes(group = group, fill = region)) +
    #   geom_text(aes(label = region), data = region.lab.data,  size = 3, hjust = 0.5) +
    #   scale_fill_viridis_d() +
    #   theme_void() +
    #   theme(legend.position = "none")
  }
  })
}
















