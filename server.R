
require(shiny)
require(plyr)
require(dplyr)
require(ggplot2)
require(reshape2)
require(googleVis)
require(RJSONIO)
require(RPostgreSQL)
require(magrittr)

source("summary_stats.R")
source("database_functions.R")

# Define server logic for random distribution application
shinyServer(function(input, output, session) { 
  
  connection <- ConnectDB()
  list.events <- GetListOfEvents(connection)
  list.events <- list.events[order(list.events[,1]),]

  users <- GetTableDB(connection, "users")
  cross_section <- GetTableDB(connection, "cross") 
  courses <- GetTableDB(connection, "courses")
  articles <- GetTableDB(connection, "articles")

  updateSelectInput(session, "conversion.event.1", choices = c("Landed", list.events))
  updateSelectInput(session, "conversion.event.2", choices = c("Sign up", list.events))
  updateSelectInput(session, "conversion.event.3", choices = c("Quiz", list.events))
  updateSelectInput(session, "conversion.event.4", choices = c("Share", list.events))

  updateSelectInput(session, "segmentation.event", choices = c("Landed", list.events))
  updateSelectInput(session, "segmentation.event.sub", choices = c("all", list.events))

  updateSelectInput(session, "powerusers.event", choices = c("Quiz", list.events))

  updateSelectInput(session, "events.filter.by.event", choices = c("all", list.events))

  updateSelectInput(session, "retention.trigger", choices = c("Sign up", list.events))
  updateSelectInput(session, "retention.followup", choices = c("Sign in", list.events))


  options <- list(
      pageSize = 30,
      page = 'enable'
  )

# ################### FILTER USERS TO EXCLUDE KIBBIT TEAM ###############
  filter_team_out <- observe({
    if (input$exclude_users == "Yes"){   
      legit.users <- cross_section %>% 
      select(user_id, key, country, state, city) %>% 
      filter(!(country == "Russia") & 
          !(country == "Ukraine") & 
          !(state == "California" & 
            city %in% c("Palo Alto", "Cupertino", "San Jose", 
                    "Menlo Park", "Mountain View", "Stanford",
                    "Sunnyvale", "Redwood City"))) %>%
      select(user_id, key)

      legit.user_ids <<- legit.users$user_id[!is.na(legit.users$user_id)]
      legit.keys <<- legit.users$key[!is.na(legit.users$key)]

      cross_section <<- subset(cross_section, user_id %in% legit.user_ids | key %in% legit.keys)
      users <<- subset(users, id %in% legit.user_ids)
    } else {
      users <- GetTableDB(connection, "users")
      cross_section <- GetTableDB(connection, "cross") 
    }
  })


################### FILTER EVENTS ###############
  LoadAllEvents <- reactive({
    start.date <- as.Date(input$daterange[1])
    end.date <- as.Date(input$daterange[2]) + 1

    query = paste("SELECT * FROM analytics_events 
                   WHERE timestamp >= \'", start.date, 
                   "\' AND timestamp <= \'", end.date, "\'", sep = "")
    events <- dbGetQuery(connection, query)
    events$timestamp <- events$timestamp %>% 
                        as.POSIXct(tz="GMT") %>% 
                        format(tz="America/Los_Angeles", usetz=TRUE)                          
    if (input$exclude_users == "Yes"){   
      events <- subset(events, user_id %in% legit.user_ids | key %in% legit.keys)
    }  

    events
  })

  UpdateProperty <- function(items, properties){
    updateSelectInput(session, items[1], choices = properties)
    updateSelectInput(session, items[2], choices = c("equal", "not equal"))
    updateTextInput(session, items[3], value = "")
  }

################### CONVERSIONS TAB ###############

  # select_funnel <- reactive({ 
  #   funnel <- input$select_funnel
  # }) 

  conversion.event.1 <- reactive({ 
    event <- GetPropertiesOfEvent(LoadAllEvents(), input$conversion.event.1)
    UpdateProperty(c("conversion.property.1", "conversion.expression.1", "conversion.value.1"), event$properties)
    event$events
  })  

  conversion.event.2 <- reactive({ 
    event <- GetPropertiesOfEvent(LoadAllEvents(), input$conversion.event.2)
    UpdateProperty(c("conversion.property.2", "conversion.expression.2", "conversion.value.2"), event$properties)
    event$events
  })  

  conversion.event.3 <- reactive({ 
    event <- GetPropertiesOfEvent(LoadAllEvents(), input$conversion.event.3)
    UpdateProperty(c("conversion.property.3", "conversion.expression.3", "conversion.value.3"), event$properties)
    event$events
  })  

  conversion.event.4 <- reactive({ 
    event <- GetPropertiesOfEvent(LoadAllEvents(), input$conversion.event.4)
    UpdateProperty(c("conversion.property.4", "conversion.expression.4", "conversion.value.4"), event$properties)
    event$events
  })  

  conversion <- eventReactive(input$do, {     
    event.1 <- FilterEventsByProperty(conversion.event.1(), input$conversion.property.1, input$conversion.expression.1, input$conversion.value.1)
    event.2 <- FilterEventsByProperty(conversion.event.2(), input$conversion.property.2, input$conversion.expression.2, input$conversion.value.2, users = event.1$users, keys = event.1$keys)
    event.3 <- FilterEventsByProperty(conversion.event.3(), input$conversion.property.3, input$conversion.expression.3, input$conversion.value.3, users = event.2$users, keys = event.2$keys)
    event.4 <- FilterEventsByProperty(conversion.event.4(), input$conversion.property.4, input$conversion.expression.4, input$conversion.value.4, users = event.3$users, keys = event.3$keys)

    result <- c(
      event.1 = SumUniqueUserEvents(event.1$event), 
      event.2 = SumUniqueUserEvents(event.2$event),
      event.3 = SumUniqueUserEvents(event.3$event), 
      event.4 = SumUniqueUserEvents(event.4$event)
    )

    result <- data.frame(page = factor(names(result), levels = names(result)), 
      as.data.frame(result))

    list(result = result, event.1 = event.1, event.2 = event.2, event.3 = event.3, event.4 = event.4)
  })

  output$conversion_plot <- renderPlot({
    result <- conversion()$result
    result$label <- c(as.character(result$result[1]),
           sprintf("%.01f %%", result$result[2:nrow(result)]/result$result[1]*100)) 
    ggplot(data = result, aes(x = page, y = result, fill = page)) +
        geom_bar(stat="identity") +
        geom_text(aes(label = label))
  })


  output$event.1 <- renderGvis({
    gvisTable(as.data.frame(conversion()$event.1[1]), options)
  })

  output$event.2 <- renderGvis({
    gvisTable(as.data.frame(conversion()$event.2[1]), options)
  })

  output$event.3 <- renderGvis({
    gvisTable(as.data.frame(conversion()$event.3[1]), options)
  })

  output$event.4 <- renderGvis({
    gvisTable(as.data.frame(conversion()$event.4[1]), options)
  })


################### RETENTION TAB ###############
  retention_matrix <- reactive({
    trigger <- input$retention.trigger
    followup <- input$retention.followup
    aggregate_by <- input$retention.aggregate.by
    retention_absolute <- retention_matrix_abs(LoadAllEvents(), trigger, followup, aggregate_by)
    retention_relative <- retention_matrix_rel(retention_absolute)

    retention_absolute[,1] <- as.character(as.Date(retention_absolute[,1], origin = "1970-01-01"))

    if (input$retention.type == "numbers"){
      retention_absolute
    } else {
      # sprintf("%1.2f %%", 100*retention_relative)
      cbind(retention_absolute[,c(1:2)], retention_relative)
    }

  })

  # output$retention_curve_plot <- renderPlot({
  #   result <- apply(retention_relative, 2, mean)
  #   print(DrawLinePlot(result))
  # })

  output$retention_relative <- renderGvis({
    options <- list(
      width = '1000'
    )
    m <- as.data.frame(retention_matrix(), stringsAsFactors=FALSE)
    if (input$retention.type == "percent"){
      for (i in 3:ncol(m)){
        m[,i] <- paste(round(as.numeric(m[,i])*100,digits=1),"%",sep="")
      }
    }
    gvisTable(m, options)
  })


################### CONTENT TAB ###############

  output$content_plot <- renderPlot({
    print(GetContentDistribution(users, courses, "courses"))
  })


################### TOP USERS TAB ###############

  powerusers.event <- reactive({
    event <- GetPropertiesOfEvent(LoadAllEvents(), input$powerusers.event)
    UpdateProperty(c("powerusers.property", "powerusers.expression", "powerusers.value"), event$properties)
    event$events
  })

  powerusers <- eventReactive(input$powerusers.update.button, {
    events <- as.data.frame(FilterEventsByProperty(powerusers.event(), input$powerusers.property, input$powerusers.expression, input$powerusers.value)[1])
    names(events) <- c(names(powerusers.event()))
    as.data.frame(IdentifyPowerUsers(users, events))
  })

  output$powerusers.table <- renderGvis({
    gvisTable(powerusers(), options)
  })

################### EVENTS TAB ###############

  filter_events <- eventReactive(input$events.update.button, {
    events <- LoadAllEvents()
    filter_user_id <- input$events.filter.by.user.id
    filter_key <- input$events.filter.by.key
    filter_event.type <- input$events.filter.by.event
    if (filter_user_id != ""){
      events <- subset(events, user_id == as.numeric(filter_user_id))  
    }
    if (filter_key != ""){
      events <- subset(events, key == filter_key)  
    }
    if (filter_event.type != "all"){
      events <- subset(events, event == filter_event.type)  
    }
    events
  })

  output$all_events <- renderGvis({
    gvisTable(filter_events()[,c(1:3,8,9,4)], options)
  })

################### USERS TAB ###############
  filter_users <- eventReactive(input$users.update.button, {  
    filter_user_id <- input$users.filter.by.user.id
    filter_key <- input$users.filter.by.key

    if (filter_user_id == "" && filter_key == ""){
      cross_section[,-c(11:14)]
    } else if (filter_user_id != "") {
      subset(cross_section[,-c(11:14)], user_id == as.numeric(filter_user_id))  
    } else if (filter_key != "") {
      subset(cross_section[,-c(11:14)], key == filter_key)  
    }
  })

  output$all_users <- renderGvis({
    gvisTable(filter_users()[,c(1,36,2,37,4:9,19,35)], options)
  })


################### ENGAGEMENT TAB ###############

  segmentation.event <- reactive({ 
    event <- GetPropertiesOfEvent(LoadAllEvents(), input$segmentation.event)
    UpdateProperty(c("segmentation.property.1", "segmentation.expression.1", "segmentation.value.1"), event$properties)
    UpdateProperty(c("segmentation.property.2", "segmentation.expression.2", "segmentation.value.2"), event$properties)
    UpdateProperty(c("segmentation.property.3", "segmentation.expression.3", "segmentation.value.3"), event$properties)
    event$events
  })  

  segmentation.event.sub <- reactive({ 
    if (input$segmentation.event.sub != "all") {
      event <- GetPropertiesOfEvent(LoadAllEvents(), input$segmentation.event.sub)
      UpdateProperty(c("segmentation.property.sub", "segmentation.expression.sub", "segmentation.value.sub"), event$properties)
      event$events
    }
  })  

  segmentation <- eventReactive(input$segmentation.update.button, {  

    names <- c(names(segmentation.event()))
    events <- as.data.frame(FilterEventsByProperty(segmentation.event(), input$segmentation.property.1, input$segmentation.expression.1, input$segmentation.value.1)[1])
    names(events) <- names
    events <- as.data.frame(FilterEventsByProperty(events, input$segmentation.property.2, input$segmentation.expression.2, input$segmentation.value.2)[1])
    names(events) <- names
    events <- FilterEventsByProperty(events, input$segmentation.property.3, input$segmentation.expression.3, input$segmentation.value.3)
    
    if (input$segmentation.event.sub != "all") {
      events.sub <- as.data.frame(FilterEventsByProperty(segmentation.event.sub(), input$segmentation.property.sub, input$segmentation.expression.sub, input$segmentation.value.sub, users = events$users, keys = events$keys)[1])
      names(events.sub) <- c(names(segmentation.event.sub()))
      events <- as.data.frame(events[1])
      names(events) <- names
      list(events = events, events.sub = events.sub)
    } else {
      events <- as.data.frame(events[1])
      names(events) <- names
      list(events = events)
    }

  })  

  output$segmentation.plot <- renderPlot({
    aggregate_by <- input$segmentation.aggregate.by
    denumerator <- GetTrend(segmentation()$events, aggregate_by)
    if (input$segmentation.event.sub != "all"){
      numerator <- GetTrend(segmentation()$events.sub, aggregate_by)
      temp <- merge(denumerator, numerator, by = "cohort", all = TRUE)
      denumerator[,2] <- temp[,3] / temp[,2]
      denumerator[,2][is.na(denumerator[,2])] <- 0
    }  
    print(DrawLinePlot(denumerator))
  })
  
})
