server = function(input, output, session) {
  
  # Selector by name, league and club
  select_name <- reactive({
    if(input$show_na)
    {
      df %>%
        filter(., grepl(tolower(input$search_player), Name), ifelse(is.na(input$league),is.na(League), c(grepl(input$league, League), grepl(input$club, Club)))) %>%
        select(., Name, League, Club, Market.Value..Euros.) %>%
        arrange(., by=Name)
    }
    else
    {
      df %>%
        filter(., grepl(tolower(input$search_player), Name), grepl(input$league, League), grepl(input$club, Club)) %>%
        select(., Name, League, Club, Market.Value..Euros.) %>%
        arrange(., by=Name)
    }
  })

  # Update the club list per league
  observe({
    club <- unique(df %>% filter(., League == input$league & !is.na(League)) %>% select(., Club))
    updateSelectizeInput(
      session, "club",
      choices = club,
      selected = club[1]
    )
  })
  
  output$table <- DT::renderDataTable({
    datatable(select_name(), rownames=FALSE,
              options = list(searching = FALSE,
                             pageLength = 20))
  })
}