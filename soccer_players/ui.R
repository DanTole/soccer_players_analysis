



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
            menuItem("Map", tabName = "map", icon = icon("map")),
            menuItem("Players", tabName = "players", icon = icon("table")),
            menuItem("Dad jokes")
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
            checkboxInput("show_na", label = "Show NA", value = F),
            textInput("search_player",
                      "Name"),
            selectizeInput("country",
                           "Nationality",
                           choices = c('Choose a country' = 'a', unique(df %>% arrange(., Country) %>% select(., Country))),
                           multiple = F),
            selectizeInput("league",
                           "League",
                           choices = c('Choose a league' = 'a', unique(df %>% arrange(., League) %>% filter(!is.na(League)) %>% select(., League))),
                           multiple = F),
            selectizeInput("club",
                           "Club",
                           choices = c('Choose a club' = '', unique(df %>% select(., Club))),
                           multiple = F)
        ),
        rightSidebarTabContent(
            id = 2,
            title = "Fepalcon",
            icon = "fas fa-bars"
        ),
        title = "Right Sidebar"
    ), # end right side bar
    
    # Body -------------------------------------------------------------------------------------------
    body = dashboardBody(
        tabItems(
            # tabItem(
            #     tabName = "map",
            #     
            # )
            tabItem(
                tabName = "players",
                fluidRow(box(DT::dataTableOutput("table"),
                             width = 8))
            )
        )
    ) # end body
    
)) # end page
