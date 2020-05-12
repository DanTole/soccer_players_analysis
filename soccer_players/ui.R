
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
            menuItem("Description", tabName = "descr", icon = icon("fas fa-home")),
            # menuItem("Player", tabName = "player", icon = icon("fas fa-user")),
            menuItem("Visualization", tabName = "visu", icon = icon("fas fa-chart-bar")),
            menuItem("Search", tabName = "search", icon = icon("table")),
            menuItem("Clusters", tabName = "clusters", icon = icon("fas fa-braille")),
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
            selectizeInput("position",
                           "Position",
                           choices = c("Choose position" = "a", list_position),
                           multiple = F),
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
            tabName = "descr",
            h1("Football players value analysis and prediction."),
            box(
              h3("Football manager, a great data collection."),
              p("The data used in this app was collected by Sega for the devellopement of their franchise Football manager. FM is a simulation game letting you play in the role of the manager/director of a club.
                They have been working on collecting a huge amount of data on soccer players all over the world, from the highest to the lowest divisions. It is regularly used by recruiters to find promising young players.
                The data for each players is separated in a few categories such that : technical, mental and physical and some hidden variables relative to the personality of the player and it's prefered positionings on the field. Each one of them including up to 14 different attributes.
                They were collected from the 2017 version on the game, meaning they were last updated in the beggining 2017.
                This first dataset was joined with a second dataset including a list of players and their market valuation during the 2019 summer transfer period.")
            ),
            box(
              h3("Objective."),
              p("The dataset consist of players and their characteristics in the beginning of 2017, and the value of a subset of these players during summer 2019.
                Are we able to predict the value of players given their FM characteristics ? If so, what will make a player valuable ?
                These questions are very complex and depends on many variables. We are trying to give a sketch of understanding with this app.")
            )
          ),
          tabItem(
              tabName = "player",

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
            tabName = "clusters",
            box(title = "How many groups can we form within the players ?",
                p("Deciding which position is best for a player is a difficult decision, that migth even change with time knowing that each player is evolving in a different way.
                  Being able to find in which group a player is with its attributes is then an important question. We are trying here to clusterize the dataset with a k-means algorithms.
                  We then apply a PCA to reduce the dimension in order to being able to visualize the result."),
              width = 12,
              sliderInput("K",
                          "Number of clusters",
                          min = 1,
                          max = 14,
                          value = 1
              ),
              column(12, align="center", plotOutput("clusters_plot", width = 800, height = 700))
            )
          ),
          
          tabItem(
            tabName = "visu",
            h1("How does a player value depends on his characteristics ?"),
            
            box(status = "primary",
              title = "Graph",
              plotlyOutput("plot_price")
            ),
            
            box(
              title = "Linear model", status = "warning",
              collapsible = T, collapsed = T,
              verbatimTextOutput("linear_model")
            )
          )
        )
    ) # end body
    
)) # end page
