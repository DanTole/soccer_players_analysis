ui <- tags$body(class="skin-blue sidebar-mini control-sidebar-open", dashboardPagePlus(
    header = dashboardHeaderPlus(
        enable_rightsidebar = TRUE,
        rightSidebarIcon = "gears"
    ),
    
    sidebar = dashboardSidebar(
        sidebarMenu(
            sidebarUserPanel(
                "Dan Toledano"
            ),
            menuItem("Map", tabName = "map", icon = icon("map")),
            menuItem("Players", tabName = "players", icon = icon("table")),
            menuItem("Dad jokes")
        )
    ),
    
    rightsidebar = rightSidebar(
        background = "dark",
        rightSidebarTabContent(
            id = 1,
            title = "Search",
            icon = "search",
            active = TRUE,
            checkboxInput("show_na", label = "Show NA", value = TRUE),
            textInput("search_player",
                      "Name"),
            selectizeInput("league",
                           "League",
                           choices = c('Choose a league' = '', unique(df %>% arrange(., League) %>%  select(., League)))),
            selectizeInput("club",
                           "Club",
                           choices = c('Choose a club' = '', unique(df %>% arrange(., Club) %>% select(., Club))))
        ),
        rightSidebarTabContent(
            id = 2,
            title = "Fepalcon",
            icon = "fas fa-bars"
        ),
        title = "Right Sidebar"
    ),
    body = dashboardBody(
        tabItems(
            tabItem(
                tabName = "players",
                fluidRow(box(DT::dataTableOutput("table"),
                             width = 8))
            )
        )
    )
))