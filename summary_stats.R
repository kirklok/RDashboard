
########################################## EVENT PROPERTY FUNCTIONS ###############################

AddPropertiesToEvent <- function(events, event.name, property = FALSE){
    df <- subset(events, event == event.name)
    if (property){
        prop <- rbind.fill(lapply(df$property, function(property){
            if (property != "null"){
                x <- fromJSON(property, simplify = FALSE)
            } else {
                x <- "null"
            }
            if (is.null(x[[1]])) x[[1]] <- "null"
            x <- Filter(function(x){Negate(is.null)(x) && length(x) > 0}, x)
            as.data.frame(x)
        })) 
        cbind(df[,c("id", "timestamp", "user_id", "event", "key")], prop)
    } else {
        df[,c("id", "timestamp", "user_id", "event", "key")]
    }
}


GetPropertiesOfEvent <- function(events, event.name){
  events <- AddPropertiesToEvent(events, event.name, TRUE)
  events <- events[order(events[,1]),]
  properties <- c(names(events)[6:length(names(events))])
  list(events = events, properties = properties)
}

###### filter events using property

FilterEventsByProperty <- function(events, property, logical, value, users = NULL, keys = NULL){
    if (value != ""){
      if (logical == "equal"){
          fit.value <- events[, property] == value
      }  
      if (logical == "not equal"){
          fit.value <- events[, property] != value
      } 
      fit.value[is.na(fit.value)] <- FALSE
      
    } else {
      fit.value <- TRUE
    }
    
    result <- events[fit.value,]
    
    if (!is.null(users) | !is.null(keys)) {
      result <- subset(result, user_id %in% users | key %in% keys)
    }

    users <- result$user_id[!is.na(result$user_id)]
    keys <- result$key[!is.na(result$key)]

    list(event = result, users = users, keys = keys)
}

SumUniqueUserEvents <- function(events){
  if (nrow(events)!=0){
    length(unique(events$user_id)) + length(unique(events$key))
  } else {
    0
  }
}

########################################## POWER USERS ###############################

IdentifyPowerUsers <- function(users, events){

    # events <- subset(events, timestamp >= period_start & timestamp <= period_end & event == event.name)

    # identify top users
    powerusers <- sort(table(events$user_id), decreasing = TRUE)
    powerusers <- as.data.frame(cbind(total = powerusers, id = as.numeric(names(powerusers))),
        stringsAsFactors = FALSE)
    powerusers <- merge(powerusers, users, by = "id")
    powerusers <- powerusers[,c("id", "first_name", "last_name", "email", "total")]
    powerusers <- arrange(powerusers,desc(total))

}

########################################## RETENTION ##################################################

###### What is Kibbit's absolute retention by days

retention_matrix_abs <- function(events, trigger, followup, aggregate_by){
    df <- subset(events, event == trigger | event == followup)
    
    df$cohort <- cut(strptime(df$timestamp, "%Y-%m-%d %H:%M:%S"), breaks = aggregate_by)

    m <- ddply(df, .(cohort), function(x) {        
        #identify those users who had trigger during that period
        users_trigger <- subset(x$user_id, x$event == trigger)

        #count those who were active in each of the cohorts
        users_followup <- ddply(df, .(cohort), function(x) {
                id <- unique(subset(x$user_id, x$event == followup))
                length(subset(id, id %in% users_trigger))
        })

    })

    # collapsing data.frame by cohort within data.frame
    m <- ddply(m, .(cohort), function(x){t <- t(x)[2,]})
    colnames(m) <- c("cohort", as.character(as.Date(m$cohort)))
    
    # transposing lower triangular matrix to upper triangular matrix
    names <- m$cohort
    m <- as.data.frame(t(m[,-1]))
    colnames(m) <- names
    
    # adding signed up column
    users_trigger <- ddply(df, "cohort", function(x){
            #count those users who signed up during cohort period
            sum(x$event == trigger)  
    })
    
    #making signed_up the first column
    m <- sapply(m, function(x) as.numeric(as.character(x))) 
    m <- cbind(cohort = as.Date(names, origin = "1900-01-01"), sign_up = users_trigger[,2], m)
    
    #retention matrix in absolute terms
    m
}

###### What is Kibbit's relative retention by days

retention_matrix_rel <- function(retention_matrix_abs){
#Let's convert absolute values to percentages (% of the registered users remaining active)
    m <- retention_matrix_abs[,-1]
    tcols <- ncol(m)
    
    for (i in 2:nrow(m)) {
            #select row from data frame
            df <- m[i, ]
            #remove columns with zeros
            df <- df[-c(2:i)]
            #count number of columns in row (w/o zeros)
            pcols <- length(df)   
            #fill columns after values by zeros
            if (pcols < tcols) df[c((pcols+1):tcols)] <- 0
            #replace GetTableDBial row by new one
            m[i,] <- df #replace GetTableDBial row by new one
    }   
    
    #calculate retention
    x <- m
    y <- m[,1]

    m <- apply(x, 2, function(x) x/y )

    is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))
    m[is.nan(m)] <- 0
    
    #generate period names
    for (i in 1:ncol(m)) colnames(m)[i]<- as.character(i-1)
    
    row.names(m) <- as.character(retention_matrix_abs[,1])

    m
}

GetTrend <- function(events, aggregate_by){
    # get users
    if (nrow(events) > 0){
        events$cohort <- cut(as.Date(events$timestamp,'%Y-%m-%d'), breaks = aggregate_by)
        aggregate(events$event, by = list(cohort = events$cohort), length)
    } else {
        c(0)
    }
    #Calculation of relative matrix
}

DrawLinePlot <- function(data){
    g <- ggplot(data, aes(x=cohort, y=x, group = 1))
    g + geom_line(aes(colour = x)) + geom_point(aes(colour = x)) + 
    scale_colour_gradient(low="red") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    stat_smooth(method = "lm", formula = y ~ x, se = FALSE)
}
