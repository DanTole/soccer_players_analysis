server = function(input, output, session) {
  
  # Selector by name, league and club
  select_player <- reactive({
    
    filtered_df = df


    # Filter NA
    if(!input$show_na)
    {
      filtered_df = filtered_df %>% filter(., !is.na(League))
    }
    
    # Filter name
    filtered_df = filtered_df %>% filter(., grepl(tolower(input$search_player), Name))

    # Filter country
    if(input$country != 'a')
    {
      filtered_df = filtered_df %>% filter(., Country == input$country)
    }

    # Filter league and club
    if(input$league != 'a')
    {
      if(input$club != '')
      {
        filtered_df = filtered_df %>% filter(., League == input$league,
                                             Club == input$club)
      }
      else
      {
        filtered_df = filtered_df %>% filter(., League == input$league)
      }
    }

    filtered_df %>%
      select(., Name,
             Country,
             League,
             Club,
             'Value after 2 years' = Market.Value..Euros.) %>%
      arrange(., by=Name)
  })

  # Update the club list per league
  observe({
    club <- unique(df %>% filter(., League == input$league & !is.na(League)) %>% select(., Club)) %>% arrange(., Club)
    updateSelectizeInput(
      session, "club",
      choices = club,
      selected = club[1]
    )
  })
  
  # Render the players table
  output$table <- DT::renderDataTable({
    datatable(select_player(), rownames=FALSE,
              options = list(searching = FALSE,
                             pageLength = 20))
  })
}