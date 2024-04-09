# Australia Public Hospitals Dashboard
# Author: Melvin Galera
# Date: 04-04-2024


# Load libraries
library(shiny)
library(bslib)
library(tidyverse)
library(glue)
library(janitor)
#library(ggpubr)
library(forcats)
library(naniar)
library(ggthemes)
library(shinydashboard)
library(ggrepel)
library(leaflet)



##### Part 1. Data loading and preparation

# 1. Load hospital datasets
hospitals <- read.csv("./data/hospital_mapping_ext.csv")
hospital_services <- read_csv("./data/hospital_services.csv", show_col_types = FALSE)
hospital_bed_ppln <- read_csv("./data/bed_ppln.csv", show_col_types = FALSE)
hospital_workforce <- read_csv("./data/workforce.csv", show_col_types = FALSE)

# 2. Dataset cleaning
hospitals <- hospitals %>% 
  clean_names() %>% 
  mutate(state = as.factor(state)) %>%
  mutate(state = dplyr::recode(state,
                               "NSW" = "New South Wales", 
                               "VIC" = "Victoria", 
                               "QLD" = "Queensland",
                               "SA" = "South Australia",
                               "TAS" = "Tasmania",
                               "WA" = "Western Australia", 
                               "NT" = "Northern Territory",
                               "ACT" = "Australian Capital Territory")) %>% 
  mutate(remoteness = factor(remoteness, levels = c("Major Cities", "Inner Regional", "Outer Regional", "Remote", "Very Remote"))) %>% 
  replace_with_na(replace= list(lhn = "")) %>% 
  mutate(remoteness_group = case_when((remoteness == "Major Cities") ~ "Major Cities",
                                      (remoteness == "Inner Regional") ~ "Regional",
                                      (remoteness == "Outer Regional") ~ "Regional",
                                      (remoteness == "Remote") ~ "Remote",
                                      (remoteness == "Very Remote") ~ "Remote")) %>% 
  mutate(remoteness_group = as.factor(remoteness_group)) %>% 
  mutate(state_add = as.factor(state_add))

# 3. create beds grouping
bed_breaks <- c(0, 10, 50, 100, 200, 500, 1060)

bed_group_tags <- c("10 or fewer beds", 
                    "More than 10 to 50 beds", 
                    "More than 50 to 100 beds",
                    "More than 100 to 200 beds",
                    "More than 200 to 500 beds",
                    "More than 500 beds")

hospitals$bed_group <- cut(hospitals$bed, breaks = bed_breaks,
                           include.lowest = FALSE,
                           right = TRUE,
                           labels = bed_group_tags)

# 4. include NAs in bed in bed_group
hospitals <- hospitals %>% 
  mutate(bed_group = replace_na(bed_group, "10 or fewer beds"))


# 5. assignments

remoteness_opts = c("All", "Major Cities", "Regional", "Remote")

# get state-wide or overall
hospital_bed_ppln_overall <- hospital_bed_ppln %>% 
  filter(remoteness == "State-wide")

hospital_bed_ppln_state <- hospital_bed_ppln %>% 
  filter(remoteness != "State-wide")


#create popup label()
popup_label <- function(selected_hospital) {
  paste0('<strong>',selected_hospital$hospital_name, '</strong>',
         '<br/>', '<strong>', 'Phone number: ', '</strong>', selected_hospital$phone_number,  ' ',
         '<br/>', '<strong>', 'Address: ', '</strong>', selected_hospital$street_address, ', ', selected_hospital$suburb, ' ',
         '<br/>', '<strong>', '         ', '</strong>', selected_hospital$state_add, ', ', selected_hospital$postcode, ' ') %>% 
    lapply(htmltools::HTML)
}


# create icons for markers
HospitalIcons <- awesomeIconList(
  "Major Cities" = makeAwesomeIcon(icon = 'hospital-o', markerColor = "blue", 
                                   library = "fa"),
  "Regional" = makeAwesomeIcon(icon = 'hospital-o', markerColor = "green", 
                               library = "fa"),
  "Remote" = makeAwesomeIcon(icon = 'hospital-o', markerColor = "orange", 
                             library = "fa"))








##### Part 2. SHINY APP CODES

# 1. Define UI for application

ui <- dashboardPage(skin = "green",
  dashboardHeader(
    title = "Australia Public Hospitals Dashboard",
    titleWidth = 500
    ),
  
  dashboardSidebar(width = 230,
    sidebarMenu(
      selectInput(inputId = "selected_state",
                  label = "Select state or territory:",
                  choices = c("All", levels(hospitals$state)),
                  selected = "All"),

      menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-simple")
               ),
      menuItem("Mapping", tabName = "mapping", icon = icon("location-dot"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Roboto Slab", sans-serif;
        font-weight: bold;
        font-size: 24px;
      }
    '))),
    tags$head(tags$style(HTML('.box {margin: 10px;}'))),
    tags$head(tags$style(HTML('.box{-webkit-box-shadow: none; -moz-box-shadow: none;box-shadow: none;}'))),
    tags$head(tags$style(HTML('#title1 .box-header{display: none}'))),
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              tags$head(tags$style(HTML('.info-box {min-height: 50px;} .info-box-icon {height: 50px; line-height: 50px;} .info-box-content {padding-top: 0px; padding-bottom: 0px;}'))),
              
            fluidRow(
              column(width = 4,
                     box(width = NULL, height = 135,
                       infoBoxOutput("hospitals_count", width = "100%"),
                       infoBoxOutput("lhn_count", width = "100%")),
                     box(width = NULL,
                       title = p("Number of public hospitals by remoteness area", style = "font-size:15px;"), status = "success", solidHeader = TRUE,
                       plotOutput("remoteness_plot", height = 250)),
                     box(width = NULL,
                       title = p("Number of public hospitals based on peer groups", style = "font-size:15px;"), status = "success", solidHeader = TRUE,
                       plotOutput("peer_plot", height = 310))
                     ),
              
              column(width = 4,
                     box(width = NULL, height = 68,
                       infoBoxOutput("average_beds", width = "100%")),
                     box(width = NULL,
                       title = p("Number of public hospitals based on average available beds", style = "font-size:15px;"), status = "success", solidHeader = TRUE,
                       uiOutput("remoteness_selector1"),
                       plotOutput("hosp_state_rem_bed_plot", height = 140)),
                     box(width = NULL, height = 68,
                         infoBoxOutput("bed_ppln", width = "100%")),
                     box(width = NULL,
                       title = p("Average available beds per 1,000 population in public hospitals", style = "font-size:15px;"), status = "success", solidHeader = TRUE,
                       plotOutput("bed_ppln_remoteness_plot", height = 130)),
                     box(width = NULL,
                       title = p("Average full-time equivalent staff by staffing category", style = "font-size:15px;"), status = "success", solidHeader = TRUE,
                       plotOutput("hosp_state_staff_plot", height = 170))
                     ),
              
              column(width = 4,
                     box(width = NULL,
                       title = p("Number of public hospitals based on services provided", style = "font-size:15px;"), status = "success", solidHeader = TRUE,
                       uiOutput("remoteness_selector2"),
                       plotOutput("hosp_state_rem_servs_plot", height = 125)),
                     box(width = NULL,
                       title = p("Number of public hospitals providing specialised service units", style = "font-size:15px;"), status = "success", solidHeader = TRUE,
                       plotOutput("hosp_state_servs_plot", height = 560))
                     )
            )
      ),
      
      
      # Second tab content
      tabItem(tabName = "mapping",
            #tags$head(tags$style(HTML('.info-box {min-height: 50px;} .info-box-icon {height: 50px; line-height: 50px;} .info-box-content {padding-top: 0px; padding-bottom: 0px;}'))),
            #tags$head(tags$style(HTML('.box {margin: 10px;}'))),
            #tags$head(tags$style(HTML('#title1 .box-header{display: none}'))),
            fluidRow(
              column(width = 12,
                box(uiOutput("remoteness_selector3"), height = 80),
                box(uiOutput("numbeds_selector"), height = 80))
              ),
            fluidRow(
            box(id= "title1", width = NULL, title = NULL, headerBorder = FALSE, 
              htmlOutput("map_title", height = 80)),  
            ),
            fluidRow(
            box(width = 12,
                  #title = p("Map showing locations of hospitals with beds in Australia", style = "font-size:15px;"), 
                status = "success", solidHeader = TRUE,
                leafletOutput("hospital_mapping", height = 615))
            )  
              
      )
    )
  )
)



# 2. Define the server function

server <- function(input, output) {

    ##### a. create reactive functions
  
    state <- reactive({
      if (input$selected_state == "All") {
        levels(hospitals$state)
      } else
        input$selected_state
    })
  
    state2 <- reactive({input$selected_state})
  
    state3 <- reactive({
      if (input$selected_state == "All") {
        "Australia"
      } else
        input$selected_state
    })
    
    
    state_add <- reactive({
      case_when((input$selected_state == "All")~ levels(hospitals$state_add),
              (input$selected_state == "New South Wales")~ "NSW",
              (input$selected_state == "Victoria")~ "VIC",
              (input$selected_state == "Queensland")~ "QLD",
              (input$selected_state == "Western Australia")~ "WA",
              (input$selected_state == "South Australia")~ "SA",
              (input$selected_state == "Tasmania")~ "TAS",
              (input$selected_state == "Australian Capital Territory")~ "ACT",
              (input$selected_state == "Northern Territory")~ "NT")
    
    })
    
    
    remoteness_group_map <- reactive({
      if(input$selected_rem_mapping == "All"){
        levels(hospitals$remoteness_group)
      } else
        input$selected_rem_mapping
    })
    
    bed_group_map <- reactive({
      if(input$selected_bedgroup == "Any"){
        levels(hospitals$bed_group)
      } else
        input$selected_bedgroup
    })
    
    
    map_state <- reactive({
      case_when((input$selected_state == "All") ~ "Australia",
             (input$selected_state != "All") ~ input$selected_state)
    })
    
    
    map_remoteness <- reactive({
      case_when((input$selected_rem_mapping == "All") ~ "all",
                                (input$selected_rem_mapping != "All") ~ input$selected_rem_mapping)
    })
    
    map_beds <- reactive({
      case_when((input$selected_bedgroup) == "Any" ~ "",
                          (input$selected_bedgroup != "Any") ~ paste0("(", input$selected_bedgroup, ")"))
    })
    
    
    
    
    
    
    
    ######### b. create output render functions
    
    #create remoteness_plot
    output$hospitals_count <- renderValueBox({
      hospital_state <- hospitals %>% filter(state %in% state())
      num_hospitals <- n_distinct(hospital_state$hospital)
      infoBox(title = paste0("Number of Public Hospitals in ", state3()),value = num_hospitals, icon= icon("hospital"), color = "yellow", fill = TRUE)
    })
    
    output$lhn_count <- renderValueBox({
      hospital_state <- hospitals %>% filter(state %in% state())
      lhn_count <- n_distinct(hospital_state$lhn, na.rm = TRUE)
      infoBox(title = paste0("Number of Local health Networks in ", state3()),value = lhn_count, icon= icon("square-h"), color = "yellow", fill = TRUE)
    })
    
    output$remoteness_plot <- renderPlot({

      hospital_state <- hospitals %>%
        filter(state %in% state())

      hospitals_rem1 <-hospital_state %>% 
        select(hospital, remoteness) %>% 
        group_by(remoteness) %>% 
        summarise(hosp_num = n())
      
      hsize <-1.2
      hospitals_rem2 <-hospitals_rem1 %>% 
        mutate(csum = rev(cumsum(rev(hosp_num))), 
               pos = hosp_num/3 + lead(csum, 1),
               pos = if_else(is.na(pos), hosp_num/2, pos))
      
      cols <- c("Major Cities"="deepskyblue3", "Inner Regional"="cyan3", "Outer Regional"="cyan3", "Remote"="gold1", "Very Remote"="gold1")
      ggplot(hospitals_rem1, aes(x = hsize , y = hosp_num, fill = fct_inorder(remoteness))) +
        geom_col(width = 1, color = "white") +
        coord_polar(theta = "y") +
        scale_fill_manual(values = cols) +
        geom_label_repel(data = hospitals_rem2,
                         aes(y = pos, label = paste0(remoteness, ": ", hosp_num)),
                         size = 4, nudge_x = 1, show.legend = FALSE, fill = "white", label.size = 0) +
        guides(fill = guide_legend(title = "Remoteness")) +
        xlim(c(0.1, hsize + 1)) +
        theme_void() +
        theme(legend.position = "none")
      
    })
    
    ########################################
    
    #create peer_plot
    output$peer_plot <- renderPlot({

        hospital_state <- hospitals %>%
          filter(state %in% state())
        
        hosp_state_peer <- hospital_state %>%
          group_by(peer_group) %>%
          summarise(hosp_count = n())

        ggplot(data=hosp_state_peer, aes(x=fct_rev(reorder(peer_group, -hosp_count)), y= hosp_count, label = hosp_count)) +
          geom_col(fill="cyan3", width = 0.7) +
          geom_text(hjust= -0.5)+
          ylim(0, max(hosp_state_peer$hosp_count)*1.15) +
          labs(x = NULL, y= NULL) +
          theme_economist() +
          coord_flip()
    })

    #######################################
    
    #create hosp_state_rem_bed_plot (based on remoteness)
    
    output$average_beds <- renderValueBox({
      hospital_state <- hospitals %>% filter(state %in% state())
      average_beds <- format(round(sum(hospital_state$beds, na.rm = TRUE), 0), big.mark = ",")
      infoBox(title = paste0("Average Available Beds in ", state3()), value = average_beds, icon= icon("bed-pulse"), color = "yellow", fill = TRUE)
    })
    
    output$remoteness_selector1 <- renderUI({
      radioButtons(inputId = "selected_rem_beds", label= NULL, choices = remoteness_opts, inline = TRUE, selected = "All")
    })
    
    output$hosp_state_rem_bed_plot <- renderPlot({
      
      hospital_state <- hospitals %>%
        filter(state %in% state())
      
      
      remoteness_group <- reactive({
        if(input$selected_rem_beds == "All"){
          levels(hospitals$remoteness_group)
        } else
          input$selected_rem_beds
      })
      
      remoteness_color <- reactive({
        if(input$selected_rem_beds == "All"){
          "cyan3"
        } else if (input$selected_rem_beds == "Major Cities"){
          "deepskyblue3"
        } else if (input$selected_rem_beds == "Regional"){
            "cyan3"
        } else 
          "gold2"
      })   
      
      
      hosp_state_rem_bed <- hospital_state %>% 
        filter(remoteness_group %in% remoteness_group()) %>% 
        group_by(bed_group) %>% 
        summarise(hosp_count = n())
      
      ggplot(hosp_state_rem_bed, aes(x=fct_rev(bed_group), y = hosp_count, label = hosp_count)) +
        geom_col(fill= remoteness_color(), width = 0.7) +
        geom_text(hjust= -0.5)+
        ylim(0, max(hosp_state_rem_bed$hosp_count)*1.15) +
        labs(x = NULL, y= NULL) +
        #title = "Average available beds and number of public hospitals")+
        theme_economist() +
        # theme(axis.text.y = element_text(size = 10),
        #       axis.text.x = element_text(size = 8))+
        coord_flip()
    })
    
    #######################################
    
    #create bed_ppln_remoteness_plot (based on remoteness)
    
    output$bed_ppln <- renderValueBox({
      bed_ppln <- case_when((state2() == "All")~ hospital_bed_ppln_overall$ALL,
                            (state2() == "New South Wales")~ hospital_bed_ppln_overall$NSW,
                            (state2() == "Victoria")~ hospital_bed_ppln_overall$VIC,
                            (state2() == "Queensland")~ hospital_bed_ppln_overall$QLD,
                            (state2() == "Western Australia")~ hospital_bed_ppln_overall$WA,
                            (state2() == "South Australia")~ hospital_bed_ppln_overall$SA,
                            (state2() == "Tasmania")~ hospital_bed_ppln_overall$TAS,
                            (state2() == "Australian Capital Territory")~ hospital_bed_ppln_overall$ACT,
                            (state2() == "Northern Territory")~ hospital_bed_ppln_overall$NT)
      
      infoBox(title = paste0("Beds per 1,000 population in ", state3()), value = bed_ppln, icon= icon("hospital-user"), color = "yellow", fill = TRUE)
    })
    
    
    output$bed_ppln_remoteness_plot <- renderPlot({
      
      bed_ppln_remoteness <- case_when((state2() == "All")~ hospital_bed_ppln_state$ALL,
                                       (state2() == "New South Wales")~ hospital_bed_ppln_state$NSW,
                                       (state2() == "Victoria")~ hospital_bed_ppln_state$VIC,
                                       (state2() == "Queensland")~ hospital_bed_ppln_state$QLD,
                                       (state2() == "Western Australia")~ hospital_bed_ppln_state$WA,
                                       (state2() == "South Australia")~ hospital_bed_ppln_state$SA,
                                       (state2() == "Tasmania")~ hospital_bed_ppln_state$TAS,
                                       (state2() == "Australian Capital Territory")~ hospital_bed_ppln_state$ACT,
                                       (state2() == "Northern Territory")~ hospital_bed_ppln_state$NT)
      
      cols <- c("Major Cities"="deepskyblue3", "Regional"="cyan3", "Remote"="gold2")
      
      ggplot(hospital_bed_ppln_state, aes(x= remoteness, y = bed_ppln_remoteness, label = bed_ppln_remoteness)) +
        geom_col(fill= cols, width = 0.7) +
        geom_text(vjust= -0.5)+
        ylim(0, max(bed_ppln_remoteness)*1.15) +
        labs(x = NULL, y= NULL) +
        #title = "Average available beds and number of public hospitals")+
        theme_economist() +
        theme(axis.text.y = element_blank(),
              axis.text.x = element_text(size = 10))
    })  
      
      
    #######################################
      
    #create hosp_state_servs_plot (specialised service unit)
    output$hosp_state_servs_plot <- renderPlot({
    
      state_spec_servs <- case_when((state2() == "All")~ hospital_services$ALL,
                                    (state2() == "New South Wales")~ hospital_services$NSW,
                                    (state2() == "Victoria")~ hospital_services$VIC,
                                    (state2() == "Queensland")~ hospital_services$QLD,
                                    (state2() == "Western Australia")~ hospital_services$WA,
                                    (state2() == "South Australia")~ hospital_services$SA,
                                    (state2() == "Tasmania")~ hospital_services$TAS,
                                    (state2() == "Australian Capital Territory")~ hospital_services$ACT,
                                    (state2() == "Northern Territory")~ hospital_services$NT)
      
      ggplot(hospital_services, aes(x=fct_rev(reorder(spec_services, -state_spec_servs)), y = state_spec_servs, label = state_spec_servs)) +
        geom_col(fill="cyan3", width = 0.7) +
        geom_text(hjust= -0.5)+
        ylim(0, max(state_spec_servs)*1.35) +
        labs(x = NULL, y= NULL) +
        #title = "Average available beds and number of public hospitals")+
        theme_economist() +
        theme(axis.text.y = element_text(size = 13),
              axis.text.x = element_blank())+
        coord_flip()
      
    })
      
    #######################################
    
    #create hosp_state_staff_plot (workforce)
    output$hosp_state_staff_plot <- renderPlot({
      
      state_staff <- case_when((state2() == "All")~ hospital_workforce$ALL,
                               (state2() == "New South Wales")~ hospital_workforce$NSW,
                               (state2() == "Victoria")~ hospital_workforce$VIC,
                               (state2() == "Queensland")~ hospital_workforce$QLD,
                               (state2() == "Western Australia")~ hospital_workforce$WA,
                               (state2() == "South Australia")~ hospital_workforce$SA,
                               (state2() == "Tasmania")~ hospital_workforce$TAS,
                               (state2() == "Australian Capital Territory")~ hospital_workforce$ACT,
                               (state2() == "Northern Territory")~ hospital_workforce$NT)
      
      ggplot(hospital_workforce, aes(x=fct_rev(reorder(staff, -state_staff)), y = state_staff, label = format(state_staff, big.mark = ','))) +
        geom_col(fill="cyan3", width = 0.7) +
        geom_text(hjust= -0.5)+
        ylim(0, max(state_staff)*1.35) +
        labs(x = NULL, y= NULL) +
        #title = "Average available beds and number of public hospitals")+
        theme_economist() +
        # theme(axis.text.y = element_text(size = 10),
        #       axis.text.x = element_text(size = 8))+
        coord_flip()
      
    })
  
    
    #######################################
    
    #create hosp_state_rem_servs_plot  (based on remoteness)
    output$remoteness_selector2 <- renderUI({
      radioButtons(inputId = "selected_rem_servs", label= NULL, choices = remoteness_opts, inline = TRUE, selected = "All")
    })
    
    output$hosp_state_rem_servs_plot  <- renderPlot({
      
      hospital_state <- hospitals %>%
        filter(state %in% state())
      
      remoteness_group <- reactive({
        if(input$selected_rem_servs == "All"){
          levels(hospitals$remoteness_group)
        } else
          input$selected_rem_servs
      })
      
      remoteness_color <- reactive({
        if(input$selected_rem_servs == "All"){
          "cyan3"
        } else if (input$selected_rem_servs == "Major Cities"){
          "deepskyblue3"
        } else if (input$selected_rem_servs == "Regional"){
          "cyan3"
        } else 
          "gold2"
      })   
      
      
      # filter the data
      hosp_state_rem_servs <- hospital_state %>% 
        filter(remoteness_group %in% remoteness_group())
      
      num_ed <- hosp_state_rem_servs %>% 
        filter(ed == "Yes") %>% 
        count()
      
      num_es <- hosp_state_rem_servs %>% 
        filter(es == "Yes") %>% 
        count()
      
      num_ed <- hosp_state_rem_servs %>% 
        filter(ed == "Yes") %>% 
        count()
      
      num_nap <- hosp_state_rem_servs %>% 
        filter(nap == "Yes") %>% 
        count()
      
      servs_df = data.frame(services = c("Emergency departments", "Elective surgery", "Non-admitted patient clinics"),
                            hosp_count = c(num_ed$n, num_es$n, num_nap$n))
      
      ggplot(servs_df, aes(x= services, y = hosp_count, label = hosp_count)) +
        geom_col(fill= remoteness_color(), width = 0.7) +
        geom_text(vjust= -0.5)+
        ylim(0, max(servs_df$hosp_count)*1.15) +
        labs(x = NULL, y= NULL) +
        #title = "Average available beds and number of public hospitals")+
        theme_economist() +
        theme(axis.text.y = element_blank(),
              axis.text.x = element_text(size = 10))
    })
    
    
    
    ##################### MAPPING TAB #########
    
    output$remoteness_selector3 <- renderUI({
      radioButtons(inputId = "selected_rem_mapping", label= "Select: ", choices = remoteness_opts, inline = TRUE, selected = "All")
    })
    
    output$numbeds_selector <- renderUI({
      selectInput(inputId = "selected_bedgroup",
                label = "Select available beds:",
                choices = c("Any", levels(hospitals$bed_group)),
                selected = "Any")
    })
    
    output$map_title <- renderText({
      
      # paste("Map showing locations of", tolower(map_remoteness()),
      #       "public hospitals", tolower(map_beds()), "in", map_state())

      paste("<br/> <b> <font size = 4> <font-family = 'Roboto'> Map showing locations of", tolower(map_remoteness()),
            "public hospitals", tolower(map_beds()), "in", map_state(),"</font> </b>")
    })    
      
      
    
    ### leaflet
    output$hospital_mapping <- renderLeaflet({
      
      # create dataset of hospitals in the selected state
      state_hospitals <- hospitals %>% 
        filter(state_add %in% state_add()) %>% 
        filter(remoteness_group %in% remoteness_group_map()) %>% 
        filter(bed_group %in% bed_group_map())
      
      
      # generate map
      if (dim(state_hospitals)[1] != 0) { 
        
        state_hospitals$popup_label <- popup_label(state_hospitals)
        
        # create leaflet
        hospital_mapping <- leaflet(data = state_hospitals) %>% 
          addTiles() %>% 
          addAwesomeMarkers(lng = ~longitude,
                            lat = ~latitude,
                            icon = ~HospitalIcons[remoteness_group],
                            label = ~hospital_name,
                            labelOptions = labelOptions(style = list("font-family" = "Roboto", 
                                                                     "font-size" = "12px")),
                            popup = ~popup_label, 
                            clusterOptions = markerClusterOptions(
                              showCoverageOnHover = FALSE,
                              maxClusterRadius = 40,
                              disableClusteringAtZoom = 9,
                              spiderfyOnMaxZoom = FALSE)) 
        
      } else
        
        # in case there is no hospital based on selected inputs
        if (dim(state_hospitals)[1] == 0) {
          
          hospital_mapping <- leaflet(data = state_hospitals) %>%
            addTiles() %>%
            setView(lng = 140.2016, lat = -29.86052, zoom = 4.2) %>%  # to center 'Australia' in the mainpanel output 
            addPopups(133.8373, -23.94099, 
                      popup = paste(sep = "<br/>", "No hospital available by this filter"),
                      options = popupOptions(closeButton = FALSE))
      
      }
      
    hospital_mapping
    })
    
}


# 3. Run the application
shinyApp(ui = ui, server = server)



