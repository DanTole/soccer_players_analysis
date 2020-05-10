
# --------------------------------------------------------------------------------

ui <- tags$body(class="skin-blue sidebar-mini control-sidebar-closed", dashboardPagePlus(
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
            menuItem("Player", tabName = "Player", icon = icon("map")),
            menuItem("Search", tabName = "search", icon = icon("table")),
            menuItem("")
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
                tabName = "player"
            ),
            tabItem(
                tabName = "search",
                fluidRow(box(DT::dataTableOutput("table"),
                             width = 12))
            )
        )
    ) # end body
    
)) # end page
