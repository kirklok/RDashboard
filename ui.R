require(shiny)
library(shinydashboard)

header <- dashboardHeader(title = "Analytics Dashboard")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Segmentation",  tabName = "Segmentation", icon = icon("tasks")),
    menuItem("Funnels", tabName = "Conversions", icon = icon("filter")),
    menuItem("Retention",  tabName = "Retention", icon = icon("signal")),
    menuItem("Power Users",  tabName = "Power_Users", icon = icon("user")),
    menuItem("Events",  tabName = "Events", icon = icon("sort-amount-desc")),
    menuItem("Users",  tabName = "Users", icon = icon("users")),
    menuItem("Settings",  tabName = "Settings", icon = icon("gear"))
  ),
  dateRangeInput("daterange", "Date range:",
             start = Sys.Date() - 14,
             end   = Sys.Date())
)

body <- dashboardBody(
  tabItems(

    tabItem(tabName = "Segmentation",
      fluidRow(
        column(2,
          selectInput("segmentation.event", "Event",
                c("loading"))
        ),
        column(2,
          selectInput("segmentation.property.1", "Property",
                c("loading"))
        ),
        column(2,
          selectInput("segmentation.expression.1", "Logical expression",
                c("equal", "not equal"))
        ),
        column(2,
          textInput("segmentation.value.1", "Value", c(""))
        ),
        column(2,
          selectInput("segmentation.aggregate.by", "Aggregate by:",
              c("days", "weeks", "months"))
        )
      ),
      fluidRow(
        column(2,
          p("")
        ),
        column(2,
          selectInput("segmentation.property.2", "Property",
                c("loading"))
        ),
        column(2,
          selectInput("segmentation.expression.2", "Logical expression",
                c("equal", "not equal"))
        ),
        column(2,
          textInput("segmentation.value.2", "Value", c(""))
        )
      ),
      fluidRow(
        column(2,
          p("")
        ),
        column(2,
          selectInput("segmentation.property.3", "Property",
                c("loading"))
        ),
        column(2,
          selectInput("segmentation.expression.3", "Logical expression",
                c("equal", "not equal"))
        ),
        column(2,
          textInput("segmentation.value.3", "Value", c(""))
        )
      ),
      fluidRow(
        column(2,
          selectInput("segmentation.event.sub", "Event",
                c("loading"))
        ),
        column(2,
          selectInput("segmentation.property.sub", "Property",
                c("loading"))
        ),
        column(2,
          selectInput("segmentation.expression.sub", "Logical expression",
                c("equal", "not equal"))
        ),
        column(2,
          textInput("segmentation.value.sub", "Value", c(""))
        )
      ),
      actionButton("segmentation.update.button", "Update"),
      p(""),
      plotOutput("segmentation.plot")
    ),

    tabItem(tabName = "Conversions",
      # selectInput("select_funnel", "Select funnel",
      #   c("Custom", "Landed -> Sign up", "Quiz started -> finished")),
      fluidRow(
        column(2,
          selectInput("conversion.event.1", "Step 1 Event",
            c("loading"))
        ),
        column(2,
          selectInput("conversion.property.1", "Property",
            c("loading"))
        ),
        column(2,
          selectInput("conversion.expression.1", "Logical expression",
                c("equal", "not equal"))
        ),
        column(2,
          textInput("conversion.value.1", "Value",
            c(""))
        )
      ),
      fluidRow(
        column(2,
          selectInput("conversion.event.2", "Step 2 Event",
            c("loading"))
        ),
        column(2,
          selectInput("conversion.property.2", "Property",
            c("loading"))
        ),
        column(2,
          selectInput("conversion.expression.2", "Logical expression",
                c("equal", "not equal"))
        ),
        column(2,
          textInput("conversion.value.2", "Value",
            c(""))
        )
      ),
      fluidRow(
        column(2,
          selectInput("conversion.event.3", "Step 3 Event",
            c("loading"))
        ),
        column(2,
          selectInput("conversion.property.3", "Property",
            c("loading"))
        ),
        column(2,
          selectInput("conversion.expression.3", "Logical expression",
                c("equal", "not equal"))
        ),
        column(2,
          textInput("conversion.value.3", "Value",
            c(""))
        )
      ),
      fluidRow(
        column(2,
          selectInput("conversion.event.4", "Step 4 Event",
            c("loading"))
        ),
        column(2,
          selectInput("conversion.property.4", "Property",
            c("loading"))
        ),
        column(2,
          selectInput("conversion.expression.4", "Logical expression",
                c("equal", "not equal"))
        ),
        column(2,
          textInput("conversion.value.4", "Value",
            c(""))
        )
      ),
      actionButton("do", "Update"),
      p(),
      tabsetPanel(
        tabPanel("Chart", 
          plotOutput(outputId = "conversion_plot", width = "75%")
        ), 
        tabPanel("Event #1", 
          htmlOutput("event.1")
        ), 
        tabPanel("Event #2", 
          htmlOutput("event.2")
        ), 
        tabPanel("Event #3", 
          htmlOutput("event.3")
        ),
        tabPanel("Event #4", 
          htmlOutput("event.4")
        )
      )
    ),

    # tabItem(tabName = "Acquisition",
    #   h3("Acquisition TBD")
    # ),

    tabItem(tabName = "Retention",
      fluidRow(
        column(2,
          selectInput("retention.trigger", "Trigger event",
            c("loading"))
        ),
        column(2,
          selectInput("retention.followup", "Followup event",
            c("loading"))
        ),
        column(2,
          selectInput("retention.type", "Retention in",
              c("percent", "numbers"))
        ),
        column(2,
          selectInput("retention.aggregate.by", "Aggregate by",
              c("days", "weeks", "months"))
        )
      ),
      htmlOutput("retention_relative", align = "center")
      # plotOutput("retention_curve_plot")
    ),

    tabItem(tabName = "Power_Users",
      fluidRow(
        column(2,
          selectInput("powerusers.event", "Event",
            c("loading"))
        ),
        column(2,
          selectInput("powerusers.property", "Property",
                c("loading"))
        ),
        column(2,
          selectInput("powerusers.expression", "Logical expression",
                c("equal", "not equal"))
        ),
        column(2,
          textInput("powerusers.value", "Value", c(""))
        )
      ),
      actionButton("powerusers.update.button", "Update"),
      p(""),
      htmlOutput("powerusers.table")
    ),

    tabItem(tabName = "Events",
      fluidRow(
        column(2,
          textInput("events.filter.by.user.id", "Filter by user ID:", c(""))
        ),
        column(2,
          textInput("events.filter.by.key", "Filter by key:", c(""))
        ),
        column(2,
          selectInput("events.filter.by.event", "Filter by event:", c("loading"))
        )
      ),
      actionButton("events.update.button", "Update"),
      p(""),
      htmlOutput("all_events")
    ),

    tabItem(tabName = "Users",
      fluidRow(
        column(2,
          textInput("users.filter.by.user.id", "Filter by user ID:", c(""))
        ),
        column(2,
          textInput("users.filter.by.key", "Filter by key:", c(""))
        )
      ),
      actionButton("users.update.button", "Update"),
      p(""),
      htmlOutput("all_users")
    ),

    tabItem(tabName = "Settings",
      selectInput("exclude_users", "Exclude Kibbit team from all calculations:", c( "Yes", "No")),
      textInput("host", "Host:", value = ""),
      textInput("user", "User:", value = ""),
      textInput("password", "Password:", value = ""),
      textInput("port", "Port:", value = ""),
      textInput("dbname", "DBName:", value = "")
    )
  )
)

dashboardPage(header, sidebar, body)
# #End Kirill Klokov code#