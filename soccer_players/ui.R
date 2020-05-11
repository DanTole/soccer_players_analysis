
# --------------------------------------------------------------------------------

ui <- tags$body(class="skin-blue sidebar-mini control-sidebar-open", dashboardPagePlus(
    header = dashboardHeaderPlus(
        enable_rightsidebar = TRUE,
        rightSidebarIcon = "gears"
    ),
    
    # Left side bar ---------------------------------------------------------------------------------
    sidebar = dashboardSidebar(
        sidebarMenu(
            sidebarUserPanel(
                "Dan Toledano"
            ),
            menuItem("Player", tabName = "player", icon = icon("map")),
            menuItem("Search", tabName = "search", icon = icon("table")),
            menuItem("Visualization", tabName = "visu", icon = icon("fas fa-chart-bar")),
            menuItem("About the author", tabName = "me", icon = icon("fas fa-futbol"))
        )
    ), # end left side bar
    
    # Right side bar --------------------------------------------------------------------------------
    rightsidebar = rightSidebar(
        background = "dark",
        rightSidebarTabContent(
            id = 1,
            title = "Search",
            icon = "search",
            active = TRUE,
            prettyToggle(
                inputId = "show_na",
                label_on = "NAs keeped",
                label_off = "NAs removed",
                icon_on = icon("check"),
                icon_off = icon("remove")),
            textInput("search_player",
                      "Name"),
            selectizeInput("country",
                           "Nationality",
                           choices = c('Choose a country' = 'a', unique(df %>% arrange(., Country) %>% select(., Country))),
                           multiple = F),
            sliderInput("age",
                        "Age",
                        min = 14,
                        max = 60,
                        value = c(14, 60)),
            selectizeInput("league",
                           "League",
                           choices = c('Choose a league' = 'a', unique(df %>% arrange(., League) %>% filter(!is.na(League)) %>% select(., League))),
                           multiple = F),
            selectizeInput("club",
                           "Club",
                           choices = c(),
                           multiple = F)
        ),
        rightSidebarTabContent(
            id = 2,
            title = "Characteristics",
            icon = "fas fa-bars",
            selectizeInput("categories",
                           "Category",
                           choices = c('Choose a category' = 'a', names(Categories)),
                           multiple = F),
            selectizeInput("attribute",
                           "Attribute",
                           choices = c(),
                           multiple = F)
        ),
        title = "Right Sidebar"
    ), # end right side bar
    
    # Body -------------------------------------------------------------------------------------------
    body = dashboardBody(
        tabItems(
            tabItem(
                tabName = "player",
                # box(selectizeInput("compare",
                #                "Select up to 10 players",
                #                choices = c("", df %>% select(Name)),
                #                multiple = T,
                #                options = list(maxItems = 10))),
                column(8, selectizeInput("compare",
                                         "Select up to 10 players",
                                         choices = c("", df %>% select(Name)),
                                         multiple = T,
                                         options = list(maxItems = 10))),
                column(8, plotlyOutput("plotspider", width = 800, height=700))
            ),
            
            tabItem(
                tabName = "search",
                fluidRow(box(DT::dataTableOutput("table"),
                             width = 12)),
                tags$head(tags$script("var f_fnRowCallback = function( nRow, aData, iDisplayIndex,     iDisplayIndexFull ){
      $('td', nRow).click( function(){Shiny.onInputChange('request_i',     [$(this).parent().index(),$(this).index()])} );
}                                        

Shiny.addCustomMessageHandler('showRequested_i', function(x) {

    alert(x)
})"))
            ),
            tabItem(
              tabName = "visu",
              h1("How does a player value depends on his characteristics ?"),
              plotlyOutput("plot_price")
            )
        )
    ) # end body
    
)) # end page
