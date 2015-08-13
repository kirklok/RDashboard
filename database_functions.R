########################################## DATA BASE ###############################
# ConnectDB <- function(host, user, password, port, dbname){
ConnectDB <- function(){
        # credentials
        drv <- dbDriver("PostgreSQL")
        host <- "HOST"
        user <- "USER"
        password <- "PASSWORD"
        port <- "PORT"
        dbname <- "DBNAME"
        # create connection
        dbConnect(drv, dbname = dbname, host = host, user = user, password = password, port = port)
}

###### closing connection
DisconnectDB <- function(){

        driver <- dbDriver("PostgreSQL")

        # closes connection
        connections = dbListConnections(driver)

        for (connection in connections){
            dbDisconnect(connection)
        }

        # unload driver
        dbUnloadDriver(driver)

}

###### prints tables that you have in DB
ListTablesDB <- function(connection){
    dbListTables(connection) 
}


GetTableDB <- function(connection, type, ...){
    # get users
    table <- switch(type,
        events = "events",
        cross = "cross_sections",
        stop("No such type"))

    query = paste("SELECT * FROM ", table, sep="")
    dbGetQuery(connection, query)
}

###### list all events in DB analytics_events
GetListOfEvents <- function(connection){
    dbGetQuery(connection, "SELECT DISTINCT event FROM analytics_events")
}
