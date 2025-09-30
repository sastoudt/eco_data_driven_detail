
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(wildlifeDI)
library(leaflet)
library(leaflet.extras2)
library(sf)
library(geojsonsf)
library(leaftime)
library(htmltools)
library(tidyverse)
library(geosphere)
library(lubridate)
library(plotly)
library(RColorBrewer)
library(shinyjs)
library(ggvenn)
library(DT)
library(dqshiny)

urlfile <- "https://raw.githubusercontent.com/sastoudt/dodge_data/main/ecology-data/byStateYear.csv"
byStateYear <- read_csv(url(urlfile))

urlfile <- "https://raw.githubusercontent.com/sastoudt/dodge_data/main/ecology-data/stateList.csv"
stateSimList <- read_csv(url(urlfile))

opts = unique(stateSimList$species)

 fluidPage(
   useShinyjs(),
  ##theme selector that you can you too see all the options ##
  #shinythemes::themeSelector(),
   
   ##CURRENT THEME CHOSEN ###
  #theme = shinythemes::shinytheme("flatly"),
  
  ###OTHER TAB STYLE###

  page_navbar(
    title = "Facing the blank page? Get inspired by ecological data...",
    #theme = bs_theme(version = 5, bootswatch = "minty"),
    ###OTHER TAB STYLE###
    #tabPanel(
    nav_panel(
      "Who are we, and what is this?",
      card(
        card_header("Our Goal"),
        p("We are a small team from Bucknell University (Elinam Agbo, Shaheryar Asghar, Caitlyn Hickey, and Sara Stoudt) sponsored by the Dominguez Center for Data Science,
          who have partnered with The Dodge to combine our two passions: Data Science and Nature. We have been developing different apps, like this one, to inspire creative writing based on real-world data!"),
        br(" Our team encourages you to explore the data yourself and try to find a story hidden between the data points. We hope to see your stories come to life.")
      ),
      
      card(
        #card_header("Our Inspiration"),
        navset_card_pill( 
          nav_panel("Google Maps", ""), 
          nav_panel("iNaturalist", ""), 
          nav_panel("US National Park Records", ""),
          nav_panel("Movement", p("")
                      ),
        )),
      
      #MAKING TABS FOR EACH OF OUR INSPIRATIONS# 
      
      card(
        #card_header("Our Inspiration"),
        navset_card_pill( 
          nav_panel("Caitlyn", "I was inspired to find data on sharks after seeing a shark-tracking bracelet my sister bought. With the purchase of a bracelet, she was given access to the location of a shark through an app. We watched the shark swim along the East Coast and contrived stories about what it was doing. In our minds, the shark was on a food tour trying to find the tastiest surfers."), 
          nav_panel("Shaheryar", "I’ve always been drawn to the hidden life within national parks—the species that quietly shape each ecosystem. What began as a simple question about how many species live in a single park grew into a deeper curiosity about biodiversity patterns across regions. That’s what led me to build an app: a platform to visualize species data and help people engage with the ecological richness of national parks in a way that’s interactive, informed, and accessible."), 
          nav_panel("Sara", "I had been working with The Dodge, an online literary magazine focusing on eco-writing, to try to develop an interactive web app that helps make exploring different nature-related datasets more accessible with the idea that something in the data might be generative, sparking some ideas about something to write about. Ideally, we would have a few different datasets paired with some writing prompts to get people started and then we would either run some kind of contest to get new people writing for the Dodge or have the app go out in a newsletter/on social media to just give people a way into the writing if they are facing the blank page. I was hitting a wall with my own progress and thought I could use some fresh eyes and some help, so I pitched this project idea to the Dominguez Center for Data Science."),
          nav_panel("What we have read", p("Throughout developing this app, we took inspiration from pieces published in The Dodge. Here is a list of what we read:"),
          HTML("<a href='https://www.thedodgemag.com/christopherlinforth1'>Homesick by Christopher Linforth</a>"),
          HTML("<a href='https://www.thedodgemag.com/piyalimukherjee1'>Death is a Name Spelled in Stripes by Piyali Mukherjee </a>"),
          HTML("<a href='https://www.thedodgemag.com/violetagarciamendoza1'>Deathbed Phenomena and Incomplete Abecedarian at the End of the Once World by Victoria Garcia-Mendoza </a>"),
          HTML("<a href='https://cymbals-corn-mx5t.squarespace.com/joeannhart1'>Ah! by Joeann Hart</a>"),
          HTML("<a href='https://cymbals-corn-mx5t.squarespace.com/brittneycorrigan1'>Why Did the Chicken Remix by Brittney Corrigan</a>"),
          HTML("<a href='https://cymbals-corn-mx5t.squarespace.com/rbminer1'>The Lung Capacity of Aquatic Mammals by R.B. Miner</a>"),
          HTML("<a href='https://www.thedodgemag.com/jondoughboy1'>The North American Blizzard of 1996 by Jon Doughboy</a>"),
          HTML("<a href='https://www.thedodgemag.com/elizabethlevinson1'>like rabbits by Elizabeth Joy Levinson</a>"),
          HTML("<a href='https://www.thedodgemag.com/lieselhamilton1'>On Weeds and Healing by Liesel Hamilton</a>"),
          HTML("<a href='https://www.thedodgemag.com/mollyweisgrau1'>Animal Planet by Molly Weisgrau</a>"),
          HTML("<a href='https://www.thedodgemag.com/brendangillen1'> Tree Food by Brendan Gillen</a>"),
          HTML("<a href='https://www.nytimes.com/column/whats-going-on-in-this-graph'>The 'what do you notice? what do you wonder?' framework is inspired by the New York Times' What's Going On in This Graph series.</a>"),
          HTML("<a href='https://gizmodo.com/the-weirdest-story-ideas-come-from-your-own-obsessions-5565717'>The idea to use a timer to just record as mcuh as possible from the graph to get started is inspired by the 'write 50 first sentences' prompt found here.</a>")
                    ),
                    ))
    ),
    
    ###OTHER TOP TAB STYLE###
    #tabPanel(
    nav_panel(
      "Roadless America",
      card(
        card_header("What am I looking at?"),
        p("Below is an interactive Google Map with a circle of radius one mile drawn around your random location."),
        HTML("<a href='https://www.amstat.org/asa/files/pdfs/stew/PercentWithinMileofRoad.pdf'> This uses an approach from an statistics education activity.  </a>"),
        
      ),
      
      card( 
        card_header("Show me the data!"),
        actionButton("action", label = "Generate data"),
        leafletOutput("map1"),
        leafletOutput("map2"),
        tableOutput("datatab"),
        height = 1200
      ),
      card(
        ##ADDING TABS TO TEXT BOX AND PROMPTS ##
        
        navset_card_pill(
          nav_panel("Time to Write!", 
                    textAreaInput("textRoadless", "", "", height = "200px", width = "1500px"),
                    p("When you are done, feel free to download your ideas so you have them for future reference."),
                    downloadButton("downloadTextRoadless", "Download Notes")),
          nav_panel("Prompts to Consider", p("What *is* and *is not* within one mile of a random location? Is there water? A major highway? A McDonalds? What is just out of reach beyond the one mile radius?"),
                    p("How often does a randomly generated point end up in your state? Does this surprise you? Why or why not?"),
                    p("Write from the point of view of someone who lives at this spot, who is a visitor to this spot, or to someone just passing through this spot. What brings them there? What do they see?"),
                    p("Write from the point of view of the location itself. How has it experienced time? Who has come and gone? How does it relate to its neighbors in space? Does it fit in?")
          ),
          nav_panel("From the Dodge Archive",),
          
        )),
    ),
    nav_panel(
      "Great White Sharks: At a Glance",
      card(
        card_header("What am I looking at?"),
        p("Below is an interactive map of Great White Shark Spottings."),
        HTML("<a href='http://www.inaturalist.org/observations/export'>The raw data was exported from iNaturalist and contains the user data, geolocation, and time of each shark observation.</a>"),
        p(" Each point represents a Great White shark spotting submitted by a user. Hover over the points to find out more information about the sighting!"),
      ),
      
      card( 
        card_header("Show me the data!"),
        selectInput("select",
                    label = h3("Select  Decade(s)"),
                    choices = list(
                      "2020" = 2020, "2010" = 2010, "2000" = 2000, "1990" = 1990,
                      "1980" = 1980, "1970" = 1970, "1950" = 1950
                    ),
                    selected = 1, multiple = TRUE
        ),
        plotlyOutput(outputId = "shark_distPlot"),
        height = 750
      ),
      card(
        ##ADDING TABS TO TEXT BOX AND PROMPTS ##
        
        navset_card_pill(
          nav_panel("Time to Write!", 
                    textAreaInput("text4", "", "", height = "200px", width = "1500px"),
                    p("When you are done, feel free to download your ideas so you have them for future reference."),
                    downloadButton("downloadText4", "Download Notes")),
          nav_panel("Prompts to Consider", p("What do you notice about locations that have many observations? Are these hotspots consistent across decades?"),
                    p("Find a location that has sightings across multiple decades. What if those reoccurring sightings across time were the same shark or group of sharks that visited the same location in multiple decades? Write from the point of view of these sharks from a variety of time points. What changes, and what stays the same? Is there something special about that place?"),
                    p("Find an isolated shark sighting that is not nearby any others. Write from the point of view of the shark that was spotted. Why would they want to travel alone in a new location? How did they get there, and where are they coming from?"),
                    p("Approximate the location of a particular data point and write a legend that explains why this spot has attracted great whites across decades. What would the legend be from the perspective of the shark? The perspective of the nearby humans?"),
                    
                    p("Why are the majority of the spottings on the coast? Why not the middle of the ocean? Is there something the sharks are afraid of? How do they feel about the people on the beach? Consider how the data is collected.")),
          nav_panel("From the Dodge Archive", HTML("<a href='https://www.thedodgemag.com/carolinehockenbury1'> 'Hammerhead' by Caroline Hockenbury is a poem that reflects on the demise of a hammerhead shark, questioning who will discover the creature now that even its bite cannot break the surf.</a>")
                    )
        )),
    ),
    
    nav_panel(
      "iNaturalist Hometown Heroes, Underdogs, and Location Comparisons",
      card(
        card_header("What am I looking at?"),
        HTML("<a href='https://doi.org/10.15468/dl.xh4sme'> This data was collected between 2010 and 2023 and comes from the Global Biodiversity Information Facility.</a>"),
        HTML("<a href='https://www.inaturalist.org/'> The particular source that is displayed here comes from iNaturalist, an app and website that allows people all over the world to identify
and share observations of species.   </a>"),

        
         ),
      
      card( 
        card_header("Where do you call home?"),
        selectInput("stateHome", "Choose a state:",
                    choices = unique(byStateYear$stateProvince)
        ),
        textOutput("hometown_hero"),
        uiOutput("url_h"),
        #plotOutput("plotHH"),
        textOutput("underdog"),
        uiOutput("url_u")#,
        #plotOutput("plotUD"),
        #height = 750
      ),
      card( 
        card_header("Where is another place that holds meaning for you?"),
        p("These are some example species that appear in both states that you chose, or in one but not the other."),
        selectInput("stateMeaning", "Choose a state:",
                    choices = unique(byStateYear$stateProvince)
        ),
        dataTableOutput("table_two_state_story"),
        height = 750
      ),
      card( 
        card_header("Where else can you find your new favorite species?"),
        
        autocomplete_input("auto1", "Species:", opts),
        leafletOutput("chosenSpecMap"),
        height = 750
      ),
      card(
        ##ADDING TABS TO TEXT BOX AND PROMPTS ##
        
        navset_card_pill(
          nav_panel("Time to Write!", 
                    textAreaInput("text_inat", "", "", height = "200px", width = "1500px"),
                    p("When you are done, feel free to download your ideas so you have them for future reference."),
                    downloadButton("downloadText_inat", "Download Notes")),
          nav_panel("Prompts to Consider", 
                    HTML("<a href='https://docs.google.com/forms/d/e/1FAIpQLSdd_SzQmx1KntGJcTP_Lrif2BzM5zj8nt-6zifEQVD65fCmig/viewform?usp=header'> Submit a prompt!  </a>")
          ),
          nav_panel("From the Dodge Archive",
                    HTML("<a href='https://docs.google.com/forms/d/e/1FAIpQLSeIPtgVfMwP0oT6iMj6dptRA5o9_9r99o43uHR5MXgbxcz-PA/viewform?usp=header'> Does this remind you of a past Dodge piece? Recommend it here.  </a>")
                    ,
          )
        )),
    ),
    


nav_panel(
  "iNaturalist Species Richness",
  card(
    card_header("What am I looking at?"),
    p("This data can be used to explore a variety of things, but one thing that it can help us understand is *species richness,* or the number of unique species observed in a region. Between 2010 and 2023, we can count the number of unique species that were collectively observed by iNaturalist users and compare across states in the United States. As you scroll down you can look at more granular measures of species richness, by season (here, winter means December, January and February, Spring means March, April, and May, Summer means June, July, and August, and Fall means September, October, and November), by year, and by year/season combination. Note, that different maps have different color scales.")
    ,
      ),
  
  card( 
    card_header("Species Richness Overall"),
    p("Hover over each state to see how many unique species iNaturalist users have reported there between 2010 and 2023."),
    leafletOutput("mapSR"),
    height = 750
  ),
  card( 
    card_header("Species Richness By Season"),
    p("Choose a season, and then hover over each state to see how many unique species iNaturalist users have reported there between 2010 and 2023 but limited to the season you chose."),
    radioButtons("season", "Season",
                 choices = c(
                   Winter = "winter",
                   Spring = "spring",
                   Summer = "summer",
                   Fall = "fall"
                 )
    ),
    leafletOutput("mapSeasonSR"),
    height = 750
  ),
  card( 
    card_header("Species Richness By Year"),
    p("Choose a year, and then hover over each state to see how many unique species iNaturalist users have reported there in that year."),
    selectInput("year", "Choose a year:",
                choices = 2010:2023
    ),
    leafletOutput("mapYearSR"),
    height = 750
  ),
  card(
    ##ADDING TABS TO TEXT BOX AND PROMPTS ##
    
    navset_card_pill(
      nav_panel("Time to Write!", 
                textAreaInput("text4", "", "", height = "200px", width = "1500px"),
                p("When you are done, feel free to download your ideas so you have them for future reference."),
                downloadButton("downloadText4", "Download Notes")),
      nav_panel("Prompts to Consider",  
                p("Write from the point of view of an iNaturalist user who is getting more and more into using the iNaturalist app over time."),
                p("Write from the point of view of a species in growth (or decline). (In other words, which years have more/less species spotted and speculate from there.)"),
                p("Consider a piece you are writing that unfurls over time. Pick a state that has an environment similar to the setting of your piece and a year that is as close as possible to the time frame of your piece. How might the change in species richness for that place/year over the four seasons impact the narrative?"),
                p("Write from the point of view of a species across the four seasons in a given year. What does the change in the seasons look like in their eyes?"),
                p("Write from the point of view of an iNaturalist user across the four seasons in a given year. How have they changed as their environment changes?"),
                p("Write from the point of view of a species that disappears in a particular season and re-emerges in a different season. (In other words, which seasons have more/less species spotted and speculate from there.)"),
                p("Write from the point of view of an iNaturalist user who faces the transition between seasons and the differences in the species available to them that comes with it."),
                
                
                
                ),
      nav_panel("From the Dodge Archive", HTML("<a href='https://www.thedodgemag.com/carolinehockenbury1'> 'Hammerhead' by Caroline Hockenbury is a poem that reflects on the demise of a hammerhead shark, questioning who will discover the creature now that even its bite cannot break the surf.</a>")
      )
    )),
),
    
    
    
    
    nav_panel(
      "National Parks: At a Glance",
      card(
        card_header("What am I looking at?"),
        HTML("<a href='https://irma.nps.gov/NPSpecies/Search/Advanced'> The National Park Service publishes a database of animal and plant species identified in individual national parks.</a>"),
        p("These species are verified by evidence — observations, vouchers, or reports that document the presence of a species in a park. All park species records are available to the public on the National Park Species portal, with exceptions for sensitive species where data sharing might pose a risk."),
        HTML("<a href='https://www.kaggle.com/datasets/umerhaddii/national-park-species-dataset'>You can get a 'ready to use' version of the data here.</a>"),
      ),
      layout_columns(
        card(
          card_header("Show me the data!"),
          selectInput("species_category", "Choose a Species Category:", choices = c(
            "Mammal", "Bird", "Reptile", "Amphibian", "Fish",
            "Vascular Plant", "Crab/Lobster/Shrimp", "Slug/Snail", "Spider/Scorpion", "Insect",
            "Other Non-vertebrates", "Non-vascular Plant", "Fungi", "Chromista", "Protozoa",
            "Bacteria"
          )),
          plotOutput(outputId = "speciesPlot")
        ),
        card(
          card_header("What do you notice? What do you wonder?"),
          
          ###ADDING A SIDEBAR IN THE CARD TO TOGGLE TIMER###
          
          layout_sidebar(
            fillable = TRUE,
            sidebar = sidebar(
            p("Need some accountability? Use this timer for brainstorming."),
                         actionButton("start", "Start"),
                         actionButton("stop", "Stop"),
                         actionButton("reset", "Reset"),
                         numericInput("seconds", "Seconds:", value = 300, min = 0, max = 99999, step = 1),
                         textOutput("timeleft")),
          #textAreaInput("text5", "", "", height = "500px", width = "100%"),
          #p("When you are done, feel free to download your ideas so you have them for future reference."),
          #downloadButton("downloadText5", "Download Notes"))
        )
      )),
      card(
        ##ADDING TABS TO TEXT BOX AND PROMPTS ##
        
        navset_card_pill(
          nav_panel("Time to Write!", 
                    textAreaInput("text5", "", "", height = "200px", width = "1500px"),
                    p("When you are done, feel free to download your ideas so you have them for future reference."),
                    downloadButton("downloadText5", "Download Notes")),
          nav_panel("Prompts to Consider", 
                    p("The Forgotten Ones: Choose an overlooked species category like fungi, chromista, or protozoa. Write a poetic ode or speculative fiction where these organisms shape the future of the park’s ecosystem."),
                    p("A Secret Between: Pick two vastly different species categories (e.g. spider/scorpion and fish). Invent a tale of quiet communication between them—a warning system, a secret alliance."),
                    p("Missingness: Write a creative nonfiction or investigative speculative essay that interrogates absence in this data. Is a missing species a missing data entry, an oversight in the system, or a deeper commentary on what types of life we value and record?"),
                    p("Being Counted: Use the absence of species to explore what it means to not be counted. Reflect on what disappears when documentation fails—and who is left unseen."),),
          nav_panel("From the Dodge Archive",
                    p(""),
          )
        )), 
     
    ),
    
    nav_panel("National Parks: A Deeper Dive",
              card(
                card_header("Compare Species Orders Across Parks"),
                p("Select two parks and a species category to explore shared and unique species orders."),
                selectInput("park1", "Select First Park:", choices = c( "Acadia National Park"         ,       "Bryce Canyon National Park"    ,      "Cuyahoga Valley National Park"     , 
                                                                        "Glacier National Park"        ,       "Grand Canyon National Park"    ,      "Grand Teton National Park" ,         
                                                                        "Great Smoky Mountains National Park" , "Hot Springs National Park"       ,    "Indiana Dunes National Park" ,       
                                                                        "Joshua Tree National Park"     ,      "Olympic National Park"     ,          "Rocky Mountain National Park"  ,     
                                                                        "Yellowstone National Park"    ,       "Yosemite National Park"  ,           "Zion National Park"   )),
                selectInput("park2", "Select Second Park:", choices = c( "Acadia National Park"         ,       "Bryce Canyon National Park"    ,      "Cuyahoga Valley National Park"     , 
                                                                         "Glacier National Park"        ,       "Grand Canyon National Park"    ,      "Grand Teton National Park" ,         
                                                                         "Great Smoky Mountains National Park" , "Hot Springs National Park"       ,    "Indiana Dunes National Park" ,       
                                                                         "Joshua Tree National Park"     ,      "Olympic National Park"     ,          "Rocky Mountain National Park"  ,     
                                                                         "Yellowstone National Park"    ,       "Yosemite National Park"  ,           "Zion National Park"   ), selected = "Bryce Canyon National Park"),
                selectInput("venn_category", "Select Species Category:", choices = c("Mammal"         ,       "Bird"         ,         "Reptile"         ,      "Amphibian"       ,      "Fish"     ,            
                                                                                     "Crab/Lobster/Shrimp" ,  "Slug/Snail"      ,      "Spider/Scorpion"    ,   "Insect"      ,          "Other Non-vertebrates",
                                                                                     "Non-vascular Plant"  ,  "Fungi"        ,         "Chromista"           ,  "Protozoa"      ,        "Bacteria"           )),
                checkboxInput("endangeredOnly", "Show only endangered/threatened/species of concern", value = FALSE),
                plotOutput("vennPlot"),
                verbatimTextOutput("vennSummary"),
                
                #actionButton("toggleShared", "Show/Hide Shared Species"),
                h2("Shared Species"),
                hidden(div(id = "sharedDiv", DT::dataTableOutput("sharedSpecies"))),
                #actionButton("toggleP1", "Show/Hide Park 1 Unique Species"),
                h2("Park 1 Unique Species"),
                hidden(div(id = "p1Div", DT::dataTableOutput("park1Species"))),
                #actionButton("toggleP2", "Show/Hide Park 2 Unique Species"),
                h2("Park 2 Unique Species"),
                hidden(div(id = "p2Div", DT::dataTableOutput("park2Species")))
              ), 
              card(
                ##ADDING TABS TO TEXT BOX AND PROMPTS ##
                
                navset_card_pill(
                  nav_panel("Time to Write!", 
                            textAreaInput("textNP2", "", "", height = "200px", width = "1500px"),
                            p("When you are done, feel free to download your ideas so you have them for future reference."),
                            downloadButton("downloadTextNP2", "Download Notes")),
                  nav_panel("Prompts to Consider", 
                            p("Pick Your Park: Select a park and scan the animals observed. What’s the first thing that stands out — a species you didn’t expect, a pattern in the numbers, or perhaps the lack of sightings? Use this observation as your entry point."),
                            p("Follow the Trail: Choose one animal and track its presence across multiple parks. Does the data reveal a migration route? What else could explain the patterns?"),
                            p("Moments of Conflict: Select a park where multiple species are observed. Imagine a moment of tension: predator and prey crossing paths, two animals competing for territory, or humans unknowingly disturbing a delicate balance."),
                            p("Movement Mission: Pick two species and write about them working together to travel from one side of the park they both live in to the other. What types of terrain would the group struggle with? How could they work together to overcome these?"),
                            p("Echoes in the Peaks: Look at a subset of the species list for a national park with high species diversity (e.g. Great Smoky Mountains). Write a multi-perspective narrative weaving together voices of species from different taxa—plant, bird, insect—coping with a sudden environmental shift."),
                            p("Endangered Thoughts: Filter for endangered or threatened species. Write a meditative monologue from one such species — its fears, memories, and its hope for the earth."),
                            p("A Game of Species: Pick two parks with very different species. Write a letter exchange between two species — one from each park—exploring how their ecosystems have shaped their identities."),),
                  nav_panel("From the Dodge Archive",
                            p(""),
                  )
                )),
      
      
      
      
      
    ),
    
    
    
    ###OTHER TAB STYLE###
    #tabPanel(
 
    ###OTHER TAB STYLE###
    #tabPanel(
    
    nav_panel(
      "White-tailed Deer: At a Glance",
      card(
        card_header("What am I looking at?"),
        p("
          This data comes from a scientific study of white-tailed deer in Oklahoma, USA who are wearing GPS collars that keep track of each deer's location over time. 
          Below, you'll see the paths of two individual deer over the course of a week in March 2005.
          "),
        HTML("<a href='https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/1365-2656.12198'>Check out the research paper here.</a>"),
      ),
      layout_columns(
        card(
          card_header("Show me the data!"),
          p("A week in the lives of these two deer will unfold for you on the map below.
            Before starting, tailor your point of view by choosing a backdrop for the map. Feel free to zoom in and out to explore the area, but note that the viewing window by default is centered on where the action is.
            In addition, choose the speed at which the data will unfold. A week will be compressed (or drawn out) to fill the time limit you choose for your free writing."),
          radioButtons(
            "map_type", "Map point of view:",
            c(
              "Human Elements" = "human",
              "Nature Elements" = "nature"
            )
          ),
          sliderInput("time_to_write", "I want to write for ____ minutes:",
            min = 1, max = 20, value = 5
          ),
          leafletOutput("deer"),
        ),
        card(
          card_header("Get writing! What do you notice? What do you wonder?"),
          p("Set the timer for 5 minutes, and watch the movie unfold. As you watch, write down anything you notice or wonder. Try to catch a rough time stamp of when these noticings and wonders appear. Now reset the timer for 5 minutes, and change the point of view of the map. Continue to write down anything you notice or wonder but also mark the time stamp of any time you see the two animals get relatively close to one another."),
                    textAreaInput("text", "", "", height = "600px", width = "600px"),
          p("When you are done, feel free to download your ideas so you have them for future reference."),
          downloadButton("downloadText", "Download Notes"),
        ),
      ),
      card(
        card_header("Organizing Your Thoughts"),
        p("Now that you have some ideas down, try to group them by common theme. These themes can help guide you towards a particular slice or summary of this dataset on the next page.")
      ),
    ),
    ###OTHER TAB STYLE###
    #tabPanel(
    nav_panel(
      "White-tailed Deer: A Deeper Dive",
      card(
        card_header("I'm interested in learning more about the deer's home ranges."),
        p(""),
        ## toggles
        radioButtons(
          "map_type2", "Map point of view:",
          c(
            "Human Elements" = "human",
            "Nature Elements" = "nature"
          )
        ),
        leafletOutput("deer_home"),
      ),
      ##ADDING TABS TO TEXT BOX AND PROMPTS ##
      
      card(
        ##ADDING TABS TO TEXT BOX AND PROMPTS ##
        
        navset_card_pill(
          nav_panel("Time to Write!", 
                    textAreaInput("text3", "", "", height = "200px", width = "1500px"),
                    p("When you are done, feel free to download your ideas so you have them for future reference."),
                    downloadButton("downloadText2", "Download Notes")),
          nav_panel("Prompts to Consider", 
                    p("Blend human and animal points of view depending on which choice of map background you choose. What things are nearby that humans might notice or wonder about? What things are nearby that deer might notice or wonder about?"),
                    p("What does home mean to you and how does it relate to your daily and weekly rhythms? What connections can be found with the deer's home and rhythms?"),
          ),
          nav_panel("From the Dodge Archive",
                    p(""),
          )
        )),
      card(
        card_header("I'm interested in learning more about the deer's day-to-day travels."),
      ),
      layout_columns(
        card(
          radioButtons(
            "dist_type", "Travel type:",
            c(
              "Step-by-step" = "step",
              "Cumulative" = "cumul"
            )
          ),
          sliderInput(
            "timechoice", "I'm interested in these times of the day:",
            min = 0, max = 24,
            value = c(0, 24)
          )
        ),
        card(
          checkboxGroupInput(
            "dowchoice", "I'm interested in these days of the week:",
            c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
          ),
        )
      ),
      card(
        plotOutput("distPlot"),
      ),
      ##ADDING TABS TO TEXT BOX AND PROMPTS ##
      
      card(
        ##ADDING TABS TO TEXT BOX AND PROMPTS ##
        
        navset_card_pill(
          nav_panel("Time to Write!", 
                    textAreaInput("text3", "", "", height = "200px", width = "1500px"),
                    p("When you are done, feel free to download your ideas so you have them for future reference."),
                    downloadButton("downloadText3", "Download Notes")),
          nav_panel("Prompts to Consider", 
                    p("Make each line of a poem proportional in length (or in number of words) to the distance traveled in each time step of one deer."),
                    p("Write a two character braid story where the characters come together when the deer get within a certain range of one another. To do this, make the story length before meeting points proportional (in sentences or physically on the page) to the time waiting for the cross."),
                    p("Use the cumulative distances travelled as a constraint for the energy or tone of the piece. In your story, the main character grows wearier and wearing in proportion to the distance travelled. If this brings your story’s energy down too much, consider what other elements in the data might represent a “boost” in energy, for example, when the character meets another character, or when they cross close to a particular spot on the map."),
                    p("Use the day of the week as a way to blend human and animal points of view. What do the `Sunday Scaries' look like for a deer? What about a deer celebrating because... TGIF?"),
          ),
          nav_panel("From the Dodge Archive",
                    p(""),
          )
        )),
 
      
      
    )
  )
)
