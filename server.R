
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(devtools)
#devtools::install_github("wch/extrafont")
#devtools::install_github("cran/xkcd") 


library(extrafont)
library(xkcd)
library(shiny)
library(maps)
library(mosaic)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  
  
  #### download texts ####
  output$downloadText <- downloadHandler(
    filename = function() {
      paste("draft-deer-glance-", gsub(" ", "_", Sys.time()), ".txt", sep = "")
    },
    content = function(con) {
      writeLines(input$text, con)
    }
  )

  output$downloadText2 <- downloadHandler(
    filename = function() {
      paste("draft-deer-dive1-", gsub(" ", "_", Sys.time()), ".txt", sep = "")
    },
    content = function(con) {
      writeLines(input$text2, con)
    }
  )


  output$downloadText3 <- downloadHandler(
    filename = function() {
      paste("draft-deer-dive2-", gsub(" ", "_", Sys.time()), ".txt", sep = "")
    },
    content = function(con) {
      writeLines(input$text3, con)
    }
  )

  output$downloadText4 <- downloadHandler(
    filename = function() {
      paste("draft-shark-", gsub(" ", "_", Sys.time()), ".txt", sep = "")
    },
    content = function(con) {
      writeLines(input$text4, con)
    }
  )

  output$downloadText5 <- downloadHandler(
    filename = function() {
      paste("draft-np-", gsub(" ", "_", Sys.time()), ".txt", sep = "")
    },
    content = function(con) {
      writeLines(input$text5, con)
    }
  )
  
  output$downloadTextRoadless <- downloadHandler(
    filename = function() {
      paste("draft-roadless-", gsub(" ", "_", Sys.time()), ".txt", sep = "")
    },
    content = function(con) {
      writeLines(input$textRoadless, con)
    }
  )
  
  output$downloadText_inat <- downloadHandler(
    filename = function() {
      paste("draft-iNat-", gsub(" ", "_", Sys.time()), ".txt", sep = "")
    },
    content = function(con) {
      writeLines(input$text_inat, con)
    }
  )
  
#### roadless ####
  
  output$map1 <- renderLeaflet({getLocation(1)})
  output$map2 <- renderLeaflet({getLocation(2)})
  
  observeEvent(input$action, {
    set.seed(sample(1111:9999,1))
    long <- -runif(nsamp, min = 65, max = 125) # approximate location of continental US
    lat <- runif(nsamp, min = 25, max = 50) # approximate location of continental US
    myroadless <- data.frame(
      sample = 1:nsamp, latitude = round(lat, 4), longitude = round(long, 4),
      withinContinent = c(rep(NA, nsamp)), within1mile = c(rep(NA, nsamp)),
      location = character(nsamp))
    rv$x = myroadless
  })
  
  getLocation <- function(counter) {
    leaflet_map(rv$x[counter, "latitude"], rv$x[counter, "longitude"],
                mark = TRUE, radius = 1, units = "miles"
    )
  }
  nsamp <- 20
  long <- -runif(nsamp, min = 65, max = 125) # approximate location of continental US
  lat <- runif(nsamp, min = 25, max = 50) # approximate location of continental US
  myroadless <- data.frame(
    sample = 1:nsamp, latitude = round(lat, 4), longitude = round(long, 4),
    withinContinent = c(rep(NA, nsamp)), within1mile = c(rep(NA, nsamp)),
    location = character(nsamp)
  )
  rv <- reactiveValues(x = myroadless)
  output$datatab = renderTable({rv$x[1:2,2:3]})
  
#### data ####
  data_nps <- reactive({
    species_data <- read.csv("data/most_visited_nps_species_data.csv", stringsAsFactors = FALSE)
    colnames(species_data) <- gsub("\\s+", ".", colnames(species_data))
    species_data <- species_data %>% filter(Occurrence == "Present")
    # Remove Vascular Plants category
    #species_data <- species_data %>% filter(CategoryName != "Vascular Plant")
    species_data
  })

  data_deer1 <- reactive({
    data(deer)
    deer_coords <- do.call(rbind, st_geometry(deer)) %>%
      as_tibble() %>%
      setNames(c("lon", "lat"))


    data <- cbind.data.frame(id = deer$id, date = deer$date, lon = deer_coords$lon, lat = deer_coords$lat)

    data
  })

  data_deer2 <- reactive({
    data(deer)
    deer_coords <- do.call(rbind, st_geometry(deer)) %>%
      as_tibble() %>%
      setNames(c("lon", "lat"))

    deer2 <- cbind.data.frame(
      start = gsub(" EST", "", deer$date), end = "2005-03-13 18:47:00",
      Longitude = deer_coords$lon, Latitude = deer_coords$lat, color = ifelse(deer$id == "37", "dodgerblue", "orange")
    )

    deer2
  })

  data_toP <- reactive({
    data <- data_deer1()
    d1 <- distHaversine(subset(data, id == 37)[, c("lon", "lat")])
    d2 <- distHaversine(subset(data, id == 38)[, c("lon", "lat")])
    date1 <- subset(data, id == 37)$date
    date2 <- subset(data, id == 38)$date
    t1 <- (date1 - date1[1]) / 60
    t2 <- (date2 - date2[1]) / 60
    t1 <- t1[-1]
    t2 <- t2[-1]
    cd1 <- cumsum(d1)
    cd2 <- cumsum(d2)

    dow1 <- weekdays(date1)[-1]
    dow2 <- weekdays(date2)[-1]

    time1 <- format(as.POSIXct(date1), format = "%H:%M:%S")
    time2 <- format(as.POSIXct(date2), format = "%H:%M:%S")
    time1 <- time1[-1]
    time2 <- time2[-1]


    id <- c(rep("Deer 1", times = length(d1)), rep("Deer 2", times = length(d2)))

    toP <- cbind.data.frame(distT = c(d1, d2), timeD = c(t1, t2), id = id, c_distT = c(cd1, cd2), dow = c(dow1, dow2), time = c(time1, time2))
    toP$time <- as.POSIXct(toP$time, format = "%H:%M:%S")

    toP$dow <- factor(toP$dow, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
    toP$hour <- hour(toP$time)

    toP
  })

#### deer ####

  output$deer_home <- renderLeaflet({
    data <- data_deer1()
    ch1 <- chull(subset(data, id == 37)[, c("lon", "lat")])
    ch2 <- chull(subset(data, id == 38)[, c("lon", "lat")])
    ch1 <- c(ch1, ch1[1])
    ch2 <- c(ch2, ch2[1])



    ch1p <- subset(data, id == 37)[, c("lon", "lat")][ch1, ]
    ch2p <- subset(data, id == 38)[, c("lon", "lat")][ch2, ]
    map_to_use <- ifelse(input$map_type2 == "human", providers$OpenStreetMap, providers$Esri.NatGeoWorldMap)


    leaflet(geojsonio::geojson_json(data_deer2())) %>%
      addProviderTiles(map_to_use) %>%
      addPolylines(data = ch1p, lng = ~lon, lat = ~lat, col = "dodgerblue") %>%
      addPolylines(data = ch2p, lng = ~lon, lat = ~lat, col = "orange") %>%
      setView(-96.39, 34.77, 14) %>%
      addTimeline(
        sliderOpts = sliderOptions(duration = 1000 * 60), ## max duration in milliseconds
        timelineOpts = timelineOptions(
          styleOptions = NULL, # make sure default style does not override
          pointToLayer = htmlwidgets::JS(
            "
function(data, latlng) {
  return L.circleMarker(
    latlng,
    {
      radius: 2,
      color: data.properties.color,
      fillColor: data.properties.color,
      fillOpacity: 1
    }
  );
}
"
          )
        )
      )
  })




  output$deer <- renderLeaflet({
    map_to_use <- ifelse(input$map_type == "human", providers$OpenStreetMap, providers$Esri.NatGeoWorldMap)

    leaflet(geojsonio::geojson_json(data_deer2())) %>%
      addProviderTiles(map_to_use) %>%
      setView(-96.39, 34.77, 14) %>%
      addTimeline(
        sliderOpts = sliderOptions(duration = input$time_to_write * 1000 * 60), ## max duration in milliseconds
        timelineOpts = timelineOptions(
          styleOptions = NULL, # make sure default style does not override
          pointToLayer = htmlwidgets::JS(
            "
function(data, latlng) {
  return L.circleMarker(
    latlng,
    {
      radius: 2,
      color: data.properties.color,
      fillColor: data.properties.color,
      fillOpacity: 1
    }
  );
}
"
          )
        )
      )
  })




  output$distPlot <- renderPlot({
    toP <- data_toP()

    toP2 <- subset(toP, toP$dow %in% input$dowchoice)
    toP3 <- subset(toP2, toP2$hour >= input$timechoice[1] & toP2$hour <= input$timechoice[2])


    if (input$dist_type == "step") {
      ggplot(toP3, aes(time, distT, col = id)) +
        geom_point() +
        geom_line() +
        facet_wrap(~dow) +
        scale_x_datetime(date_labels = "%H:%M") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) +
        scale_color_manual(values = c("Deer 1" = "dodgerblue", "Deer 2" = "orange"))
    } else {
      ggplot(toP3, aes(time, c_distT, col = id)) +
        geom_point() +
        geom_line() +
        facet_wrap(~dow) +
        scale_x_datetime(date_labels = "%H:%M") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) +
        scale_color_manual(values = c("Deer 1" = "dodgerblue", "Deer 2" = "orange"))
    }
  })
  #### national parks ####
  
  output$speciesPlot <- renderPlot({
    species_data <- data_nps()
    # Filter data based on selected category
    filtered_data <- species_data %>%
      filter(CategoryName == input$species_category) %>%
      group_by(ParkName) %>%
      summarise(NumberSpecies = n()) %>%
      #summarise(Observations = sum(Observations, na.rm = TRUE)) %>%
      arrange(desc(NumberSpecies))
    
    # Ensure there is data to plot
    validate(
      need(nrow(filtered_data) > 0, "No data available for this category.")
    )
    
    # Create the bar plot
    ggplot(filtered_data, aes(x = reorder(ParkName, NumberSpecies), y = NumberSpecies)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() + # Flip axis to prevent overlapping names
      labs(
        title = paste("Number of ", input$species_category, "s in National Parks", sep=""),
        x = "National Park", y = "Total Species"
      ) +
      theme_minimal()
  })
  
  #### shark data ####

  output$shark_distPlot <- renderPlotly({
    data <- read.csv("data/great_whites.csv", stringsAsFactors = FALSE)


    data$observed_on <- as.Date(data$observed_on, format = "%Y-%m-%d")
    data$year <- as.numeric(format(data$observed_on, format = "%Y")) # Convert to numeric

    data$decade <- paste0(floor(data$year / 10) * 10)
    decade_colors <- c(
      "1950" = "firebrick4", "1970" = "orange2", "1980" = "maroon",
      "1990" = "darkgreen", "2000" = "darkblue", "2010" = "purple",
      "2020" = "darkcyan"
    )

    decade_shapes <- c(
      "1950" = 1, "1970" = 2, "1980" = 10,
      "1990" = 7, "2000" = 5, "2010" = 6,
      "2020" = 4
    )
    selected_decades <- as.numeric(input$select)
    filtered_data <- data %>% filter(decade %in% selected_decades)

    p <- ggplot() +
      borders("world", colour = "darkgrey", fill = "darkgrey") +
      geom_point(
        data = filtered_data, aes(
          x = longitude, y = latitude, color = factor(decade), shape = factor(decade),
          text = observed_on_string
        ),
        size = 3
      ) +
      xlab("Longitude") +
      ylab("Latitude") +
      theme_void() +
      labs(color = "Decade", shape = "Decade") +
      theme(
        legend.position = "bottom",
        panel.background = element_rect(
          fill = "powderblue",
          colour = "powderblue",
          size = 0.5, linetype = "solid"
        ),
        panel.grid.major = element_line(
          size = 0.5, linetype = "solid",
          colour = "white"
        ),
        panel.grid.minor = element_line(
          size = 0.25, linetype = "solid",
          colour = "white"
        ),
        plot.title = element_text(hjust = 0.5)
      ) +
      # labs(title = 'Great White Shark Spottings by Decade') +
      coord_cartesian(xlim = c(-175, 175), ylim = c(-55, 55)) +
      scale_color_manual(values = decade_colors) +
      scale_shape_manual(values = decade_shapes)
    ggplotly(p, tooltip = "text")
    # https://stackoverflow.com/questions/34605919/formatting-mouse-over-labels-in-plotly-when-using-ggplotly
  })
  
  #### timer ####

  # Countdown timer logic
  timer <- reactiveVal(300)
  active <- reactiveVal(FALSE)

  output$timeleft <- renderText({
    paste("Time left:", seconds_to_period(timer()))
  })

  observe({
    invalidateLater(1000, session)
    isolate({
      if (active()) {
        timer(timer() - 1)
        if (timer() < 1) {
          active(FALSE)
          showModal(modalDialog(
            title = "Time's up!",
            "Countdown completed!"
          ))
        }
      }
    })
  })

  observeEvent(input$start, {
    active(TRUE)
  })
  observeEvent(input$stop, {
    active(FALSE)
  })
  observeEvent(input$reset, {
    timer(input$seconds)
  })
  
  
  ### venn diagram tab ###
  
  # Show/hide logic for tables
  observeEvent(input$toggleShared, toggle("sharedDiv"))
  observeEvent(input$toggleP1, toggle("p1Div"))
  observeEvent(input$toggleP2, toggle("p2Div"))
  
  # Venn Diagram
  output$vennPlot <- renderPlot({
    species_data <- data_nps()
    status_filter <- if (input$endangeredOnly) c("E", "T", "SC") else unique(species_data$TEStatus)
    
    park1_species <- species_data %>%
      filter(ParkName == input$park1, CategoryName == input$venn_category, TEStatus %in% status_filter) %>%
      pull(SciName) %>% unique()
    
    park2_species <- species_data %>%
      filter(ParkName == input$park2, CategoryName == input$venn_category, TEStatus %in% status_filter) %>%
      pull(SciName) %>% unique()
    
    venn_data <- list(Park1 = park1_species, Park2 = park2_species)
    ggvenn(venn_data, fill_color = c("skyblue", "lightgreen"),
           stroke_size = 0.5, set_name_size = 4)
  })
  
  # Venn Summary
  output$vennSummary <- renderPrint({
    species_data <- data_nps()
    status_filter <- if (input$endangeredOnly) c("E", "T", "SC") else unique(species_data$TEStatus)
    
    p1 <- species_data %>% filter(ParkName == input$park1, CategoryName == input$venn_category, TEStatus %in% status_filter)
    p2 <- species_data %>% filter(ParkName == input$park2, CategoryName == input$venn_category, TEStatus %in% status_filter)
    
    o1 <- unique(p1$SciName)
    o2 <- unique(p2$SciName)
    
    shared <- intersect(o1, o2)
    only1 <- setdiff(o1, o2)
    only2 <- setdiff(o2, o1)
    total <- length(unique(c(o1, o2)))
    
    cat("🔍 Summary of Species:\n")
    cat(paste0("Shared Species: ", length(shared), " (", round(length(shared)/total * 100, 1), "%)\n"))
    cat(paste0("Unique to ", input$park1, ": ", length(only1), " (", round(length(only1)/total * 100, 1), "%)\n"))
    cat(paste0("Unique to ", input$park2, ": ", length(only2), " (", round(length(only2)/total * 100, 1), "%)\n"))
  })
  
  # Species Tables
  output$sharedSpecies <- DT::renderDataTable({
    species_data <- data_nps()
    status_filter <- if (input$endangeredOnly) c("E", "T", "SC") else unique(species_data$TEStatus)
    shared_species <- intersect(
      species_data %>% filter(ParkName == input$park1, CategoryName == input$venn_category, TEStatus %in% status_filter) %>% pull(SciName),
      species_data %>% filter(ParkName == input$park2, CategoryName == input$venn_category, TEStatus %in% status_filter) %>% pull(SciName)
    )
    species_data %>%
      filter(SciName %in% shared_species, ParkName %in% c(input$park1, input$park2),
             CategoryName == input$venn_category, TEStatus %in% status_filter) %>%
      select(ParkName, SciName, CommonNames, Order, Family) %>%
      distinct()
  })
  
  output$park1Species <- DT::renderDataTable({
    species_data <- data_nps()
    status_filter <- if (input$endangeredOnly) c("E", "T", "SC") else unique(species_data$TEStatus)
    unique_species <- setdiff(
      species_data %>% filter(ParkName == input$park1, CategoryName == input$venn_category, TEStatus %in% status_filter) %>% pull(SciName),
      species_data %>% filter(ParkName == input$park2, CategoryName == input$venn_category, TEStatus %in% status_filter) %>% pull(SciName)
    )
    species_data %>%
      filter(ParkName == input$park1, SciName %in% unique_species, CategoryName == input$venn_category, TEStatus %in% status_filter) %>%
      select(SciName, CommonNames, Order, Family) %>%
      distinct()
  })
  
  output$park2Species <- DT::renderDataTable({
    species_data <- data_nps()
    status_filter <- if (input$endangeredOnly) c("E", "T", "SC") else unique(species_data$TEStatus)
    unique_species <- setdiff(
      species_data %>% filter(ParkName == input$park2, CategoryName == input$venn_category, TEStatus %in% status_filter) %>% pull(SciName),
      species_data %>% filter(ParkName == input$park1, CategoryName == input$venn_category, TEStatus %in% status_filter) %>% pull(SciName)
    )
    species_data %>%
      filter(ParkName == input$park2, SciName %in% unique_species, CategoryName == input$venn_category, TEStatus %in% status_filter) %>%
      select(SciName, CommonNames, Order, Family) %>%
      distinct()
  })
  
  #### hometown hero ####
  
  urlfile <- "https://raw.githubusercontent.com/sastoudt/dodge_data/main/ecology-data/spec-story.csv"
  storydata = read_csv(url(urlfile))
  
  stateHome = reactive({
    this = input$stateHome
    this})
  
  
  
  hhd = reactive({
    storydata %>% filter(state %in% stateHome())
  })
  
  
  output$hometown_hero<- renderText({paste0("Your hometown hero (the species reported most) is: ", as.character(hhd()$hometown_hero[1]), ".")} )
  
  observeEvent(input$stateHome,{
    output$url_h <-renderUI(a(href=paste0(hhd()$urlH[1]),"Click here for a picture and more information about the species."))
  })
  
  output$underdog <- renderText({paste0("Your hometown underdog (the species reported least) is: ", as.character(hhd()$underdog[1]), ".")} )
  
  observeEvent(input$stateHome,{
    output$url_u <-renderUI(a(href=paste0(hhd()$urlU[1]),"Click here for a picture and more information about the species."))
  })
  
  urlfile <- "https://raw.githubusercontent.com/sastoudt/dodge_data/main/ecology-data/hometown-hero.csv"
  
  hometown_hero_data <- read_csv(url(urlfile))
  
  urlfile <- "https://raw.githubusercontent.com/sastoudt/dodge_data/main/ecology-data/underdog.csv"
  
  underdog_data <- read_csv(url(urlfile))
  
  hhd_p = reactive({
    hometown_hero_data %>% filter(stateProvince %in% stateHome())
  })
  
  ud_p = reactive({
    underdog_data %>% filter(stateProvince %in% stateHome())
  })
  
  output$plotHH<-renderPlot({
    agg <- hhd_p() %>% group_by(year, month) %>% summarise(count = n())  %>% mutate(dateP = as.Date( paste(year, "-", str_pad(width = 2, side = "left",pad = "0",string = month), "-", "01", sep = "")))
    
    
    ggplot(agg,aes(x=dateP,y=count))+geom_point() + geom_line() + xlab("date") + ylab("count of observations in this state") + theme_minimal(base_size = 15)
    
  })
  
  output$plotUD<-renderPlot({
    agg <- ud_p() %>% group_by(year, month) %>% summarise(count = n())  %>% mutate(dateP = as.Date( paste(year, "-", str_pad(width = 2, side = "left",pad = "0",string = month), "-", "01", sep = "")))
    
    
    ggplot(agg,aes(x=dateP,y=count))+geom_point() + geom_line() + xlab("date") + ylab("count of observations in this state") + theme_minimal(base_size = 15)
    
  })
  
  urlfile <- "https://raw.githubusercontent.com/sastoudt/dodge_data/main/ecology-data/stateList.csv"
  stateSimList <- read_csv(url(urlfile))
  
  
  two_state_sim_story <- reactive({
    
    
    data1 <- stateSimList %>% filter(stateProvince == input$stateHome)
    data2 <- stateSimList %>% filter(stateProvince == input$stateMeaning)
    
    
    both <- intersect(data1$species, data2$species)
    justone <- setdiff(data1$species, data2$species)
    justtwo <- setdiff(data2$species, data1$species)
    
    nb <- ifelse(length(both) < 10, length(both), 10)
    njo <- ifelse(length(justone) < 10, length(justone), 10)
    njt <- ifelse(length(justtwo) < 10, length(justtwo), 10)
    
    data <- cbind.data.frame(species = c(
      both[sample(1:length(both), nb)],
      justone[sample(1:length(justone), njo)],
      justtwo[sample(1:length(justtwo), njt)]
    ))
    
    url1 = data1$url_val[which(data1$species %in% data)]
    
    url3 = data2$url_val[which(data2$species %in% justtwo)]
    
    data$info <- c(rep("in_both", nb), rep("in_first_only", njo), rep("in_second_only", njt))
    #data$url <- c(url1,  url3)
    
    data
  })
  
  output$table_two_state_story <- renderDataTable(DT::datatable({
    two_state_sim_story()
  }))
  
}

