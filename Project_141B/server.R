server <- function(input, output, session) {
  
  ##############################################################################
  #The air quality data at UC davis, including longitude and latitude at first tab
  url_davis <- paste("api.airvisual.com/v2/nearest_city?lat=", "38.54",
                     "&lon=", "-121.74",
                     "&key=", my_key,
                     sep = ""
  )
  #Find the air quality data
  davis_data <- httpGET(url_davis)
  davis_data <- fromJSON(davis_data)
  whether <- as.data.frame(davis_data$data$current$weather)
  pollution <- as.data.frame(davis_data$data$current$pollution)
  #output the data at the first tab. Including data shown in first tab.
  output$text1 <- renderText({
    paste0(
      "<div style='position:relative;'>",
      "<img src='pic.jpg'/>",
      "<div style='position:absolute; z-index:2; left:500px; top:3px;color:black;font-size:30px'>",
      whether$tp, "°</div>", "<div style='position:absolute; z-index:2; left:408px; top:157px;color:black;font-size:25px'>",
      whether$ws, "</div>", "</div>"
    )
  })
  
  ##############################################################################
  #Create a reactive textbar to retrive longitute and latitude that user input
  data1 <- reactive({
    input$go1
    lat <- isolate(input$lat)
    lon <- isolate(input$lon)
    #Using the longitude and latitude to find data from the API
    url <- paste("api.airvisual.com/v2/nearest_city?lat=", lat,
                 "&lon=", lon,
                 "&key=", my_key,
                 sep = ""
    )
    #Using Json to read the data 
    data <- httpGET(url)
    data <- fromJSON(data)
    #A if else loop function to return city with similar longitude and latitude
    #If there is not suitable city for the longitude and latitude, return "no nearest city"
    if (length(data$data) != 5) {
      output_data <- cbind("warning" = "no nearest city")
    } else {
      output_data <- cbind(
        "Country" = unname(data$data$country),
        "State" = unname(data$data$state),
        "City" = unname(data$data$city),
        as.data.frame(data$data$current$weather),
        "long" = as.numeric(lon),
        "lat" = as.numeric(lat),
        as.data.frame(data$data$current$pollution)
      )
    }
    #return names with actual meaning instead of variables in the API
    output_data <- plyr::rename(output_data, c(
      
      tp = "Temperature", pr = "Pressure", hu = "Humidity",aqius ="AQI(US)" ))
    output_data
  })
  
  ################# output 1
  #Output the table shown in the tab2. containing air quality table for a certain city.
  output$table <- renderTable({
    # country state city ts tp pr hu
    all_col_name <- colnames(data1())
    show_col <- all_col_name[which(all_col_name %in% 
                                     c("Country", "State", 
                                       "City", "Temperature", "Pressure", "Humidity","AQI(US)"))]
    data1()[, show_col]
    
  })
  #Output a map of the city corresponding to the latitude and longitude.
  output$plot1 <- renderLeaflet({
    plotdata <- data1()
    if (length(plotdata) != 1) {
      leaflet() %>%
        addTiles() %>% # Add default OpenStreetMap map tiles
        addMarkers(
          lng = plotdata$long, lat = plotdata$lat,
          popup = paste(
            "tp", plotdata$tp, "<br>",
            "pr:", plotdata$pr, "<br>",
            "hu:", plotdata$hu, "<br>",
            "ws:", plotdata$ws, "<br>",
            "wd:", plotdata$wd
          )
        ) %>%
        setView(lng = plotdata$long, lat = plotdata$lat, zoom = 10)
    }
  })
  
  
  ##############################################################################
  #
  #Using the input data from tab3, search all the cities in the chosen states
  get_city_data <- function(city_name) {
    url_city <- paste("api.airvisual.com/v2/city?city=", city_name,
                      "&state=", input$select_state, "&country=USA&key=", # input$select_state
                      my_key,
                      sep = ""
    )
    #Find cities' names and air quality data using Json
    try_get_city_data <- function(url) {
      city_data <- httpGET(url)
      city_data <- fromJSON(city_data)
      city_data_df <- city_data$data
    }
    #return all the data into a table
    get_city_data <- safely(try_get_city_data, NULL)(url_city)
    get_city_data <- get_city_data$result
    return(get_city_data)
  }
  
  #Creating a reactive select bar to choose states 
  download_show_data <- reactive({
    if (input$select_state != "") {
      #Using the input name of the statem to find air qualities for all cities in the state
      url2 <- paste("api.airvisual.com/v2/cities?state=", input$select_state, # all_state_list,#input$select_state,
                    "&country=USA&key=", my_key,
                    sep = ""
      )
      #Extract the data with Json
      all_city_of_state <- httpGET(url2)
      all_city_of_state <- fromJSON(all_city_of_state)
      #loop function to find and output all cities' air qualities in the state
      if (all_city_of_state$status == "success") {
        #unlist all the cities names 
        all_city_of_state <- unlist(all_city_of_state$data)
        #Showing message while processing the data
        showNotification("start to download data from web")
        message("Dowloading City Data")
        ID2 <- showNotification("Dowloading City Data", duration = NULL)
       #Get all the data
        get_data <- do.call(
          rbind,
          lapply(lapply(all_city_of_state, FUN = get_city_data),
                 FUN = function(x) {
                   cbind(
                     data.frame(
                       "city_name" = x$city,
                       "lon" = x$location$coordinates[1],
                       "lat" = x$location$coordinates[2]
                     ),
                     x$current$weather,
                     x$current$pollution
                   )
                 }
          )
        )
        #output all the cities' air quality in a table
        data <- get_data %>%
          as_tibble(.name_repair = "unique") %>%
          select(-starts_with("ts"))
        removeNotification(ID2)
        showNotification("done !")
      } else {
        showNotification("Failed in get the city of state")
      }
      data <- plyr::rename(data, c(
        #rename all the variables in the API to meaningful names
        tp = "temperature", pr = "pressure", hu = "humidity",
        ws = "wind speed", wd = "wind direction",
        ic = "icon code", aqius = "AQI(US)", mainus = "pollutant(US)",
        aqicn = "AQI(CN)", maincn = "pollutant(CN)"
      ))
      data
    }
  })
  #
  #
  #
  #
  observeEvent(download_show_data(), {
    # showNotification("update bottom")
    show_variable <- colnames(download_show_data())
    show_variable <- show_variable[-which(show_variable %in% c("lon", "city_name", "lat"))]
    # if (is.null(show_variable)) {stop("data is error")}
    updateCheckboxGroupInput(session, "show_num_variable", choices = show_variable)
    updateSelectInput(session, "show_num_variable_map", choices = show_variable)
    showNotification("update bottom")
  })
  #
  #
  #Creating a reactive button for the second selectbar 
  show_plot_1 <- reactive({
    req(input$show_num_variable)
    #store all the data(including cities) for the chosen variables 
    download_show_data() %>%
      dplyr::select(c("city_name", input$show_num_variable)) %>%
      reshape2::melt(id.vars = c("city_name")) %>%
      mutate(value = as.numeric(value)) %>%
      #plot a histogram for all data
      ggplot(aes(
        x = city_name, y = value,
        color = variable, group = variable, fill = variable
      )) +
      geom_col(position = "dodge") + # theme_ft_rc() + scale_fill_ft() + scale_color_ft() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  #
  #Creating a reactive button for the third selectbar
  show_plot_2 <- reactive({
    req(input$show_num_variable_map)
    #Find all the data for the chosen variable 
    g <- list(
      scope = "usa",
      projection = list(type = "albers usa"),
      showland = TRUE,
      landcolor = toRGB("gray85"),
      subunitwidth = 1,
      countrywidth = 1,
      subunitcolor = toRGB("white"),
      countrycolor = toRGB("white")
    )
    #plot the data on a map
    plot_on_map_data <- download_show_data() %>% dplyr::select(c("city_name", "lon", "lat", input$show_num_variable_map))
    # reshape2::melt(id.vars = c("city_name")) %>%
    colnames(plot_on_map_data) <- c("city_name", "lon", "lat", "value")
    plot_geo(plot_on_map_data,
             lat = ~lat, lon = ~lon,
             color = ~value
    ) %>%
      add_markers() %>%
      layout(title = "data on map", geo = g)
  })
  #
  #
  ######################### output 2
  #Final outputs for the third tab
  output$show_table <- renderTable({
    download_show_data()
  })
  output$plot_variable <- renderPlot({
    show_plot_1()
  })
  output$show_map <- renderPlotly({
    show_plot_2()
  })
}
