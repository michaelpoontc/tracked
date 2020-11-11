# This is a Shiny app that present National Records of Scotland data
# Developed by Michael Poon (Usher Institute, University of Edinburgh)
# Team members include:
#   Dr Kai Jin
#   Professor Cathie Sudlow
#   Dr Paul Brennan
#   Dr Jonine Figueroa

# Sourcing scripts ===============================================================
source("data.R", local=TRUE)
# source("previous_data.R", local=TRUE)
# source("location.R", local=TRUE)

# Start of shiny app UI codes ======================================================

ui <- dashboardPagePlus(
  dashboardHeaderPlus(title = "TRACKED",
                      left_menu = NULL
                      ),
  dashboardSidebar(
    # Custom colour
    tags$head(tags$style(HTML('.logo {
                                            background-color: #00325f !important;
                                            }
                                            .navbar {
                                            background-color: #00325f !important;
                                            }
                                            '))),
    width = "16vw",
    sidebarMenu(id="tab",
                menuItem("Overview", tabName="home", icon=icon("chart-line")
                ),
                menuItem("Overview settings", icon=icon("cog"),
                         checkboxGroupInput("options",
                                            label =NULL,
                                            choices = c("Non-COVID deaths" = 1,
                                                        "4-week average"=2))
                ),
                menuItem("Nations", icon=icon("map-marker-alt"),
                         menuSubItem("England", tabName="england"),
                         menuSubItem("Scotland", tabName="scotland"),
                         menuSubItem("Wales", tabName="wales"),
                         menuSubItem("Northern Ireland", tabName="ni")
                ),
                menuItem("Regional data", tabName = "regional", icon=icon("map")),
                menuItem("Data info", tabName="info", icon=icon("database")),
                menuItem("About authors", tabName="about", icon=icon("address-card")),
                menuItem("Contact", tabName="contact", icon=icon("envelope-open"))
    )
  ),
  dashboardBody(
    # Custom dashboard appaearances
    tags$head(tags$style(
                         "body {overflow-x: hidden;}",
                         HTML('
                                
                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #2b2b2b;
                                }

                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #8d744a;
                                }

                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #2b2b2b;
                                color: #ffffff;
                                }

                                /* other links in the sidebarmenu when hovered */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color: #959696;
                                }
                                /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #959696;
                                }

                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #dae7f7;
                                }
                                
                                /* box appearance */
                                .box.box-solid.box-primary{
                                border-bottom-color:#666666;
                                border-left-color:#666666;
                                border-right-color:#666666;
                                border-top-color:#666666;
                                }
                                
                                .box.box-solid.box-primary{
                                  background:#f5f9ff
                                  }

                                '))),
    
    tabItems(
      # Overview tab
      tabItem(tabName = "home",
              fluidRow(
                column(6, h1("Tracking Excess Deaths", style="padding:3px;")),
                column(6, div(img(src="usher.png", height = 70, align="right")))
              ),
              fluidRow(
                box(status = "primary", solidHeader = T, height="38vh", 
                    plotOutput("england_graph", height="36vh")),
                box(status = "primary", solidHeader = T, height="38vh",
                    plotOutput("scotland_graph",height="36vh")),
              ),
              fluidRow(
                box(status = "primary", solidHeader = T, height="38vh",
                    plotOutput("wales_graph", height="36vh")),
                box(status = "primary", solidHeader = T, height="38vh",
                    plotOutput("ni_graph", height="36vh"))
              )
      ),
      
      # England tab
      tabItem(tabName = "england",
              fluidRow(
                column(6, h1("England", style="padding:3px;")),
                column(6, div(img(src="usher.png", height = 70, align="right")))
              ),
              # Number of deaths boxes & plot
              fluidRow(
                valueBoxOutput("total_box_england", width = 4),
                valueBoxOutput("excess_box_england", width = 4),
                valueBoxOutput("expected_box_england", width = 4),
                valueBoxOutput("non_covid_box_england", width = 4),
                valueBoxOutput("non_covid_excess_box_england", width = 4),
                valueBoxOutput("latest_england", width = 4)),
              # England plot
              fluidRow(
                column(4, box(title = "Options",
                              status = "primary", solidHeader = T, width = 12,
                              p("Numbers displayed correspond to the selected period of time."),
                              checkboxInput("noncovid_eng",
                                            "Non-COVID deaths", FALSE),
                              checkboxInput("movingaverage_eng",
                                            "4-week average", FALSE),
                              p("Select period (end of week dates)"),
                              sliderInput("slider_eng",
                                          label=NULL,
                                          min = as.Date(min(ons$week)),
                                          max = as.Date(max(ons$week)),
                                          value = c(as.Date(min(ons$week)), as.Date(max(ons$week))),
                                          step = 7,
                                          timeFormat = "%d %b"),
                                          )
                       ),
                column(8, align="center",
                       plotOutput("ons_cod_graph", height="60vh", width = "90%"))
              )
      ),
      
      # Scotland tab
      tabItem(tabName = "scotland",
              fluidRow(
                column(6, h1("Scotland", style="padding:3px;")),
                column(6, div(img(src="usher.png", height = 70, align="right")))
              ),
              # Number of deaths
              fluidRow(
                valueBoxOutput("total_box_scotland", width = 3),
                valueBoxOutput("excess_box_scotland", width = 3),
                valueBoxOutput("expected_box_scotland", width = 3),
                valueBoxOutput("latest_scotland", width = 3)
              ),
              
              # Scotland Plot
              fluidRow(
                column(3, box(title = "Options",
                              status = "primary", solidHeader = T, width = 12,
                              p("Numbers displayed above correspond to the selected cause of death
                                within the selected period."
                                ),
                              selectInput("cod",
                                          "Cause of death",
                                          list("All",
                                               "Non-COVID",
                                               "Cancer",
                                               "Cardiovascular",
                                               "Dementia",
                                               "Respiratory",
                                               "Others")),
                              p("Select period (end of week dates)"),
                              sliderInput("slider_scotland",
                                          label=NULL,
                                          min = as.Date(min(nrs$week)),
                                          max = as.Date(max(nrs$week)),
                                          value = c(as.Date(min(nrs$week)), as.Date(max(nrs$week))),
                                          step = 7,
                                          timeFormat = "%d %b"),
                              checkboxInput("movingaverage_scotland",
                                            "4-week average", FALSE),
                              )
                       ),
                column(9, align="center",
                       plotOutput("nrs_cod_graph", height="58vh", width = "85%"))
                # box(status = "primary", solidHeader = T, height="60vh", width = 9, align = "center",
                #     plotOutput("nrs_cod_graph", height="58vh", width = "70%")
                # )
              )
      ),
      
      # Wales tab
      tabItem(tabName = "wales",
              fluidRow(
                column(6, h1("Wales", style="padding:3px;")),
                column(6, div(img(src="usher.png", height = 70, align="right")))
              ),
              # Number of deaths boxes & plot
              fluidRow(
                valueBoxOutput("total_box_wales", width = 4),
                valueBoxOutput("excess_box_wales", width = 4),
                valueBoxOutput("expected_box_wales", width = 4),
                valueBoxOutput("non_covid_box_wales", width = 4),
                valueBoxOutput("non_covid_excess_box_wales", width = 4),
                valueBoxOutput("latest_wales", width = 4)),
              # Wales plot
              fluidRow(
                column(4, box(title = "Options",
                              status = "primary", solidHeader = T, width = 12,
                              p("Numbers displayed correspond to the selected period of time."),
                              checkboxInput("noncovid_wales",
                                            "Non-COVID deaths", FALSE),
                              checkboxInput("movingaverage_wales",
                                            "4-week average", FALSE),
                              p("Select period (end of week dates)"),
                              sliderInput("slider_wales",
                                          label=NULL,
                                          min = as.Date(min(ons$week)),
                                          max = as.Date(max(ons$week)),
                                          value = c(as.Date(min(ons$week)), as.Date(max(ons$week))),
                                          step = 7,
                                          timeFormat = "%d %b"),
                              )
                        ),
                column(8, align="center",
                       plotOutput("ons_cod_graph_wales", height="60vh", width = "90%"))
              )
      ),
      
      # Northern Ireland tab
      tabItem(tabName = "ni",
              fluidRow(
                column(6, h1("Northern Ireland", style="padding:3px;")),
                column(6, div(img(src="usher.png", height = 70, align="right")))
              ),
              # Number of deaths boxes & plot
              fluidRow(
                valueBoxOutput("total_box_ni", width = 4),
                valueBoxOutput("excess_box_ni", width = 4),
                valueBoxOutput("expected_box_ni", width = 4),
                valueBoxOutput("non_covid_box_ni", width = 4),
                valueBoxOutput("non_covid_excess_box_ni", width = 4),
                valueBoxOutput("latest_ni", width = 4)),
              # Northern Ireland plot
              fluidRow(
                column(4, box(title = "Options",
                              status = "primary", solidHeader = T, width = 12,
                              p("Numbers displayed correspond to the selected period of time."),
                              checkboxInput("noncovid_ni",
                                            "Non-COVID deaths", FALSE),
                              checkboxInput("movingaverage_ni",
                                            "4-week average", FALSE),
                              p("Select period (end of week dates)"),
                              sliderInput("slider_ni",
                                          label=NULL,
                                          min = as.Date(min(nisra$week)),
                                          max = as.Date(max(nisra$week)),
                                          value = c(as.Date(min(nisra$week)), as.Date(max(nisra$week))),
                                          step = 7,
                                          timeFormat = "%d %b"),
                )
                ),
                column(8, align="center",
                       plotOutput("nisra_cod_graph", height="60vh", width = "90%"))
              )
      ),
      
      # tabItem(tabName = "regional",
      #         plotOutput("difference_plot")
      #         ),
      tabItem(tabName = "info",
              fluidRow(
                column(6, h1("Data information", style="padding:3px;")),
                column(6, div(img(src="usher.png", height = 70, align="right")))
              ),
              p(br(),
                em("Background"), ": We aimed to describe trends of excess mortality in the United Kingdom (UK) stratified by nation and cause of death, and to develop an online tool", 
                                  " for reporting the most up to date data on excess mortality.",
               em("Methods"), ": Population statistics agencies in the UK including the Office for National Statistics (ONS), National Records of Scotland (NRS), and Northern Ireland Statistics and Research Agency (NISRA)",
                                "publish weekly data on deaths. We used mortality data up to 22nd May in the ONS and the NISRA and 24th May in the NRS.",
                                " Crude mortality for non-COVID deaths (where there is no mention of COVID-19 on the death certificate) calculated.", 
                                " Excess mortality defined as difference between observed mortality and expected average of mortality from previous 5 years.",
                  em("Results"), ": There were 56,961 excess deaths and 8,986 were non-COVID excess deaths. England had the highest number of excess deaths per 100,000 population (85) and Northern Ireland the lowest (34).",
                                  " Non-COVID mortality increased from 23rd March and returned to the 5-year average on 10th May. In Scotland, where underlying cause mortality data besides COVID-related deaths was available,",
                                  " the percentage excess over the 8-week period when COVID-related mortality peaked was: dementia 49%, other causes 21%, circulatory diseases 10%, and cancer 5%.",
                                  " We developed an online tool (TRACKing Excess Deaths - TRACKED) to allow dynamic exploration and visualisation of the latest mortality trends.",
               em("Conclusions"), ":  Continuous monitoring of excess mortality trends and further integration of age- and gender-stratified and underlying cause of death data beyond COVID-19",
                                  " will allow dynamic assessment of the impacts of indirect and direct mortality of the COVID-19 pandemic."),
              p(br(),
                "Our interpretation of the findings are available in our preprint."),
              p(a("MedRxiv preprint", href="https://www.medrxiv.org/content/10.1101/2020.06.05.20121962v1", target="_blank")),
              p(br(),
                "Source for this application can be viewed and downloaded from github."),
              p(a("GitHub page", href="https://github.com/michaelpoontc/tracked", target="_blank")),
              p(br(),  
                "Direct URLs of the data sources are available below:"),
              p(a("Office for National Statistics (ONS)", href="https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/weeklyprovisionalfiguresondeathsregisteredinenglandandwales", target="_blank")),
              p(a("National Records of Scotland (NRS)", href="https://www.nrscotland.gov.uk/covid19stats", target="_blank")),
              p(a("Northern Ireland Statistics and Research Agency (NISRA)", href="https://www.nisra.gov.uk/publications/weekly-deaths", target="_blank")),
              p(strong("Please note that mortality data from ONS/ NRS/ NISRA are provisional and will likely change due to delays in registrations. Rates are crude and not standardised for age."))
      ),
      
      tabItem(tabName = "about",
              fluidRow(
                column(6, h1("Research team", style="padding:3px;")),
                column(6, div(img(src="usher.png", height = 70, align="right")))
              ),
              fluidRow(
                column(8, 
                       p(
                          "The creators are a group of clinical and non-clinical academics, based at the University of Edinburgh and Health Data Research UK, ",
                          "with expertise in cancer and cardiovascular epidemiology, health informatics, and analysis of routine linked healthcare data.",
                          "Please read our ",
                         a("MedRxiv preprint", href="https://www.medrxiv.org/content/10.1101/2020.06.05.20121962v1", target="_blank"), "for more information.
                         The code supporting this app was written by Michael Poon, who maintains this website.",
                         br(),
                         br()),
                       box(title="Team members", status = "primary", solidHeader = T, width = 12,
                             p(a("Dr Michael Poon", href="https://www.research.ed.ac.uk/portal/en/persons/michael-poon(1c64fb66-b332-44cf-bc0e-0ba4110b5ef4).html", target="_blank")),
                             p(a("Dr Paul Brennan", href="https://www.ed.ac.uk/profile/dr-paul-brennan", target="_blank")),
                             p(a("Dr Kai Jin", href="https://www.researchgate.net/profile/Kai_Jin13", target="_blank")),
                             p(a("Dr Jonine Figueroa", href="https://www.research.ed.ac.uk/portal/en/persons/jonine-figueroa(af64d0a3-49c3-428d-9b71-b755ae531023).html", target="_blank")),
                             p(a("Professor Cathie Sudlow", href="https://www.hdruk.org/people/cathie-sudlow/", target="_blank"))
                       )
                ),
                column(4, align="right",
                       p(
                         a(img(src="twitter.png", height = 60), href="https://twitter.com/MchaelPoon", target="_blank"),
                         a(img(src="github.png", height = 65), href="https://github.com/michaelpoontc/tracked", target="_blank")
                       )
                )
              )
      ),
      tabItem(tabName = "contact",
              tags$iframe(id = "googleform",
                          src = "https://docs.google.com/forms/d/e/1FAIpQLSerm53QaTMcRSzIuX_rQUe4o9iNk4JDjJaxyJLS9XCgAkUx5w/viewform?embedded=true",
                          width = 1000,
                          height = 800,
                          frameborder = 0,
                          marginheight = 0,
                          align = "center"))
    )
  )
)



# Server functions ========================================================

server <- function(input, output) {
  
  # Setting graphical parameters =================================================
  label_size <- 12
  axis_title_size <- 12
  plot_title_size <- 15
  legend_size <- 12
  
  # Number of weeks common between nations ====================================
  xmax_common <- as.Date(min(max(nrs$week), max(ons$week), max(nisra$week)))
  
  # Maximum y value in common graphs
  ymax_common <- as.Date(max(max(ons$total_wales_std_ma_ub, na.rm = T), 
                         max(nrs$total_std_ma_ub, na.rm = T),
                         max(ons$total_england_ub, na.rm = T),
                         max(ons$total_wales_ub, na.rm = T))
                         )
  
  # Maximum y value in common graphs with smoothing ==============================
  ymax_common_ma <- as.Date(max(max(nrs$total_std_ma_ub, na.rm = T), 
                        max(ons$total_wales_ub, na.rm = T), 
                        max(nisra$total_std_ma_ub, na.rm = T),
                        max(ons$total_england_ub, na.rm = T))
                        )
  
  # Defining maximum x value in Scotland graph
  xmax <- as.Date(max(deaths$week))
  
  # Defining maximum value for y axis in number graph (Scotland) ========================
  ymax <- reactive ({
    ifelse(as.Date(max(deaths[[input$cod]]) < max(deaths$Covid),
           max(deaths$Covid)+100,
           max(deaths[[input$cod]])+100))
  })
  
  # Defining maximum value for y axis in difference graph=====================
  ymax_diff <- reactive ({
    max(
      abs(difference[[input$cod]])
    )
  })
  
  # Defining maximum value for y axis in comparison graphs ============================
  ymax_eng <- as.Date(max(ons$total_england_std))
  
  # Overview england graph ==================================
  output$england_graph <- renderPlot ({
    p <- ggplot(ons) +
      geom_ribbon(aes(x=week, ymin=expected_england_lb, ymax=expected_england_ub),
                  fill="#a1c5ff", alpha=0.3) +
      geom_ribbon(aes(x=week, ymin=covid_england_lb, ymax=covid_england_ub),
                  fill="#b1debe", alpha=0.3) +
      geom_line(aes(x=week, expected_england_std, colour="Total deaths expected"),
                size=1, linetype="dashed") +
      geom_line(aes(x=week, covid_england_std, colour="COVID-19 deaths"),
                size=1) +
      scale_y_continuous(name="Deaths per 100,000", 
                         limits=c(0, ymax_common),
                         minor_breaks=NULL) +
      scale_x_date(name="Month in 2020",
                   limits= as.Date(c("2020-01-01", xmax_common)),
                   labels = date_format("%b"),
                   breaks = "months",
                   minor_breaks=NULL) +
      scale_color_manual(name = "",
                         values = c("Total deaths expected"="#01478c", "COVID-19 deaths"="#2a4030"),
                         guide = guide_legend(reverse=FALSE)) +
      labs(title = "Mortality in England") +
      theme(
        panel.background = element_rect(fill="#f2f7fc", colour = "#002169",
                                        size=2, linetype = "solid"),
        plot.margin = unit(c(0.5, 1, 0.1, 0.7), "cm"),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
        axis.title.y = element_text(size=axis_title_size, vjust=3, margin = margin(t = 20, r = 0, b=30, l=0)),
        axis.title.x = element_text(size=axis_title_size, vjust=-1.5),
        axis.text = element_text(size=label_size),
        legend.key = element_rect(fill = "transparent", color = NA),
        legend.key.width = unit(1, "cm"),
        legend.text = element_text(size = legend_size),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.position="bottom"
      )
    
    q <- ggplot(ons) +
      geom_ribbon(aes(x=week, ymin=expected_england_std_ma_lb, ymax=expected_england_std_ma_ub),
                  fill="#a1c5ff", alpha=0.3) +
      geom_ribbon(aes(x=week, ymin=covid_england_std_ma_lb, ymax=covid_england_std_ma_ub),
                  fill="#b1debe", alpha=0.3) +
      geom_line(aes(x=week, expected_england_std_ma, colour="Total deaths expected"),
                size=1, linetype="dashed") +
      geom_line(aes(x=week, covid_england_std_ma, colour="COVID-19 deaths"),
                size=1) +
      scale_y_continuous(name="Deaths per 100,000", 
                         limits=c(0, ymax_common_ma),
                         minor_breaks=NULL) +
      scale_x_date(name="Month in 2020",
                   limits= as.Date(c("2020-01-01", xmax_common)),
                   labels = date_format("%b"),
                   breaks = "months",
                   minor_breaks=NULL) +
      scale_color_manual(name = "",
                         values = c("Total deaths expected"="#01478c", "COVID-19 deaths"="#2a4030"),
                         guide = guide_legend(reverse=FALSE)) +
      labs(title = "Mortality in England") +
      theme(
        panel.background = element_rect(fill="#f2f7fc", colour = "#002169",
                                        size=2, linetype = "solid"),
        plot.background = element_rect(fill = "transparent", colour = NA),
        plot.margin = unit(c(0.5, 1, 0.1, 0.7), "cm"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
        axis.title.y = element_text(size=axis_title_size, vjust=3, margin = margin(t = 20, r = 0, b=30, l=0)),
        axis.title.x = element_text(size=axis_title_size, vjust=-1.5),
        axis.text = element_text(size=label_size),
        legend.key = element_rect(fill = "transparent", color = NA),
        legend.key.width = unit(1, "cm"),
        legend.text = element_text(size = legend_size),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.position="bottom"
      ) 
    
    if(sum(as.numeric(input$options))==0){
      p +
        geom_ribbon(aes(x=week, ymin=total_england_lb, ymax=total_england_ub),
                    fill="#ff87b1", alpha=0.3) +
        geom_line(aes(x=week, total_england_std, colour="Total deaths observed"),
                  size=1) +
        scale_color_manual(name = "",
                           values = c("Total deaths observed"="#c10044", "Total deaths expected"="#01478c", "COVID-19 deaths"="#2a4030"),
                           guide = guide_legend(override.aes = list(linetype=c("solid", "dashed", "solid")),
                                                                     reverse=T))
    }
    
    else if (sum(as.numeric(input$options))==1){
      p +
        geom_ribbon(aes(x=week, ymin=non_covid_england_lb, ymax=non_covid_england_ub),
                    fill="#ff87b1", alpha=0.3) +
        geom_line(aes(week, non_covid_england_std, colour="Non-COVID-deaths"),
                  size=1) +
        scale_color_manual(name = "",
                           values = c("Non-COVID-deaths"="#c10044", "Total deaths expected"="#01478c", "COVID-19 deaths"="#2a4030"),
                           guide = guide_legend(override.aes = list(linetype=c("dashed", "solid", "solid")), reverse=T))
    }
    
    else if (sum(as.numeric(input$options))==2){
      q +
        geom_ribbon(aes(x=week, ymin=total_england_std_ma_lb, ymax=total_england_std_ma_ub),
                    fill="#ff87b1", alpha=0.3) +
        geom_line(aes(week, total_england_std_ma, colour="Total deaths observed"),
                  size=1) +
        scale_color_manual(name = "",
                           values = c("Total deaths observed"="#c10044", "Total deaths expected"="#01478c", "COVID-19 deaths"="#2a4030"),
                           guide = guide_legend(override.aes = list(linetype=c("solid", "dashed", "solid")),reverse=T))
    }
    
    else if (sum(as.numeric(input$options))==3){
      q +
        geom_ribbon(aes(x=week, ymin=non_covid_england_std_ma_lb, ymax=non_covid_england_std_ma_ub),
                    fill="#ff87b1", alpha=0.3) +
        geom_line(aes(week, non_covid_england_std_ma, colour="Non-COVID-deaths"),
                  size=1) +
        scale_color_manual(name = "",
                           values = c("Non-COVID-deaths"="#c10044", "Total deaths expected"="#01478c", "COVID-19 deaths"="#2a4030"),
                           guide = guide_legend(override.aes = list(linetype=c("dashed", "solid", "solid")),reverse=T))
    }
    
  }, bg="transparent")
  
  
  
  # Scotland graph ========================================================
  output$scotland_graph <- renderPlot ({
    p <- ggplot(nrs) +
      geom_ribbon(aes(x=week, ymin=expected_lb, ymax=expected_ub),
                  fill="#a1c5ff", alpha=0.3) +
      geom_ribbon(aes(x=week, ymin=covid_lb, ymax=covid_ub),
                  fill="#b1debe", alpha=0.3) +
      geom_line(aes(week, expected_std, colour="Total deaths expected"),
                size=1, linetype="dashed") +
      geom_line(aes(week, covid_std, colour="COVID-19 deaths"),
                size=1) +
      scale_y_continuous(name="Deaths per 100,000", 
                         limits=c(0, ymax_common_ma),
                         minor_breaks=NULL) +
      scale_x_date(name="Month in 2020",
                   limits= as.Date(c("2020-01-01", xmax_common)),
                   labels = date_format("%b"),
                   breaks = "months",
                   minor_breaks=NULL) +
      scale_color_manual(name = "",
                         values = c("Total deaths expected"="#01478c", "COVID-19 deaths"="#2a4030"),
                         guide = guide_legend(reverse=FALSE)) +
      labs(title = "Mortality in Scotland") +
      theme(
        panel.background = element_rect(fill="#f2f7fc", colour = "#002169",
                                        size=2, linetype = "solid"),
        plot.margin = unit(c(0.5, 1, 0.1, 0.7), "cm"),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
        axis.title.y = element_text(size=axis_title_size, vjust=3, margin = margin(t = 20, r = 0, b=30, l=0)),
        axis.title.x = element_text(size=axis_title_size, vjust=-1.5),
        axis.text = element_text(size=label_size),
        legend.key = element_rect(fill = "transparent", color = NA),
        legend.key.width = unit(1, "cm"),
        legend.text = element_text(size = legend_size),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.position="bottom"
      )
    
    q <- ggplot(nrs) +
      geom_ribbon(aes(x=week, ymin=expected_std_ma_lb, ymax=expected_std_ma_ub),
                  fill="#a1c5ff", alpha=0.3) +
      geom_ribbon(aes(x=week, ymin=covid_std_ma_lb, ymax=covid_std_ma_ub),
                  fill="#b1debe", alpha=0.3) +
      geom_line(aes(week, expected_std_ma, colour="Total deaths expected"),
                size=1, linetype="dashed") +
      geom_line(aes(week, covid_std_ma, colour="COVID-19 deaths"),
                size=1) +
      scale_y_continuous(name="Deaths per 100,000", 
                         limits=c(0, ymax_common_ma),
                         minor_breaks=NULL) +
      scale_x_date(name="Month in 2020",
                   limits= as.Date(c("2020-01-01", xmax_common)),
                   labels = date_format("%b"),
                   breaks = "months",
                   minor_breaks=NULL) +
      scale_color_manual(name = "",
                         values = c("Total deaths expected"="#01478c", "COVID-19 deaths"="#2a4030"),
                         guide = guide_legend(reverse=FALSE)) +
      labs(title = "Mortality in Scotland") +
      theme(
        panel.background = element_rect(fill="#f2f7fc", colour = "#002169",
                                        size=2, linetype = "solid"),
        plot.margin = unit(c(0.5, 1, 0.1, 0.7), "cm"),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
        axis.title.y = element_text(size=axis_title_size, vjust=3, margin = margin(t = 20, r = 0, b=30, l=0)),
        axis.title.x = element_text(size=axis_title_size, vjust=-1.5),
        axis.text = element_text(size=label_size),
        legend.key = element_rect(fill = "transparent", color = NA),
        legend.key.width = unit(1, "cm"),
        legend.text = element_text(size = legend_size),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.position="bottom"
      )
    
    if(sum(as.numeric(input$options))==0){
      p +
        geom_ribbon(aes(x=week, ymin=total_lb, ymax=total_ub),
                    fill="#ff87b1", alpha=0.3) +
        geom_line(aes(week, total_std, colour="Total deaths observed"),
                  size=1) +
        scale_color_manual(name = "",
                           values = c("Total deaths observed"="#c10044", "Total deaths expected"="#01478c", "COVID-19 deaths"="#2a4030"),
                           guide = guide_legend(override.aes = list(linetype=c("solid", "dashed", "solid")),reverse=T))
    }
    
    else if (sum(as.numeric(input$options))==1){
      p +
        geom_ribbon(aes(x=week, ymin=non_covid_lb, ymax=non_covid_ub),
                    fill="#ff87b1", alpha=0.3) +
        geom_line(aes(week, non_covid_std, colour="Non-COVID-deaths"),
                  size=1) +
        scale_color_manual(name = "",
                           values = c("Non-COVID-deaths"="#c10044", "Total deaths expected"="#01478c", "COVID-19 deaths"="#2a4030"),
                           guide = guide_legend(override.aes = list(linetype=c("dashed", "solid", "solid")),reverse=T))
    }
    
    else if (sum(as.numeric(input$options))==2){
      q +
        geom_ribbon(aes(x=week, ymin=total_std_ma_lb, ymax=total_std_ma_ub),
                    fill="#ff87b1", alpha=0.3) +
        geom_line(aes(week, total_std_ma, colour="Total deaths observed"),
                  size=1) +
        scale_color_manual(name = "",
                           values = c("Total deaths observed"="#c10044", "Total deaths expected"="#01478c", "COVID-19 deaths"="#2a4030"),
                           guide = guide_legend(override.aes = list(linetype=c("solid", "dashed", "solid")),reverse=T))
    }
    
    else if (sum(as.numeric(input$options))==3){
      q +
        geom_ribbon(aes(x=week, ymin=non_covid_std_ma_lb, ymax=non_covid_std_ma_ub),
                    fill="#ff87b1", alpha=0.3) +
        geom_line(aes(week, non_covid_std_ma, colour="Non-COVID-deaths"),
                  size=1) +
        scale_color_manual(name = "",
                           values = c("Non-COVID-deaths"="#c10044", "Total deaths expected"="#01478c", "COVID-19 deaths"="#2a4030"),
                           guide = guide_legend(override.aes = list(linetype=c("dashed", "solid", "solid")),reverse=T))
    }
    
  }, bg="transparent")
  
  
  # Wales graph =========================================================================
  output$wales_graph <- renderPlot ({
    p <- ggplot(ons) +
      geom_ribbon(aes(x=week, ymin=expected_wales_lb, ymax=expected_wales_ub),
                  fill="#a1c5ff", alpha=0.3) +
      geom_ribbon(aes(x=week, ymin=covid_wales_lb, ymax=covid_wales_ub),
                  fill="#b1debe", alpha=0.3) +
      geom_line(aes(week, expected_wales_std, colour="Total deaths expected"),
                size=1, linetype="dashed") +
      geom_line(aes(week, covid_wales_std, colour="COVID-19 deaths"),
                size=1) +
      scale_y_continuous(name="Deaths per 100,000", 
                         limits=c(0, ymax_common),
                         minor_breaks=NULL) +
      scale_x_date(name="Month in 2020",
                   limits= as.Date(c("2020-01-01", xmax_common)),
                   labels = date_format("%b"),
                   breaks = "months",
                   minor_breaks=NULL) +
      scale_color_manual(name = "",
                         values = c("Total deaths expected"="#01478c", "COVID-19 deaths"="#2a4030"),
                         guide = guide_legend(reverse=FALSE)) +
      labs(title = "Mortality in Wales") +
      theme(
        panel.background = element_rect(fill="#f2f7fc", colour = "#002169",
                                        size=2, linetype = "solid"),
        plot.margin = unit(c(0.5, 1, 0.1, 0.7), "cm"),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
        axis.title.y = element_text(size=axis_title_size, vjust=3, margin = margin(t = 20, r = 0, b=30, l=0)),
        axis.title.x = element_text(size=axis_title_size, vjust=-1.5),
        axis.text = element_text(size=label_size),
        legend.key = element_rect(fill = "transparent", color = NA),
        legend.key.width = unit(1, "cm"),
        legend.text = element_text(size = legend_size),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.position="bottom"
      )
    
    q <- ggplot(ons) +
      geom_ribbon(aes(x=week, ymin=expected_wales_std_ma_lb, ymax=expected_wales_std_ma_ub),
                  fill="#a1c5ff", alpha=0.3) +
      geom_ribbon(aes(x=week, ymin=covid_wales_std_ma_lb, ymax=covid_wales_std_ma_ub),
                  fill="#b1debe", alpha=0.3) +
      geom_line(aes(week, expected_wales_std_ma, colour="Total deaths expected"),
                size=1, linetype="dashed") +
      geom_line(aes(ons$week, covid_wales_std_ma, colour="COVID-19 deaths"),
                size=1) +
      scale_y_continuous(name="Deaths per 100,000", 
                         limits=c(0, ymax_common_ma),
                         minor_breaks=NULL) +
      scale_x_date(name="Month in 2020",
                   limits= as.Date(c("2020-01-01", xmax_common)),
                   labels = date_format("%b"),
                   breaks = "months",
                   minor_breaks=NULL) +
      scale_color_manual(name = "",
                         values = c("Total deaths expected"="#01478c", "COVID-19 deaths"="#2a4030"),
                         guide = guide_legend(reverse=FALSE)) +
      labs(title = "Mortality in Wales") +
      theme(
        panel.background = element_rect(fill="#f2f7fc", colour = "#002169",
                                        size=2, linetype = "solid"),
        plot.margin = unit(c(0.5, 1, 0.1, 0.7), "cm"),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
        axis.title.y = element_text(size=axis_title_size, vjust=3, margin = margin(t = 20, r = 0, b=30, l=0)),
        axis.title.x = element_text(size=axis_title_size, vjust=-1.5),
        axis.text = element_text(size=label_size),
        legend.key = element_rect(fill = "transparent", color = NA),
        legend.key.width = unit(1, "cm"),
        legend.text = element_text(size = legend_size),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.position="bottom"
      )
    
    if(sum(as.numeric(input$options))==0){
      p +
        geom_ribbon(aes(x=week, ymin=total_wales_lb, ymax=total_wales_ub),
                    fill="#ff87b1", alpha=0.3) +
        geom_line(aes(week, total_wales_std, colour="Total deaths observed"),
                  size=1) +
        scale_color_manual(name = "",
                           values = c("Total deaths observed"="#c10044", "Total deaths expected"="#01478c", "COVID-19 deaths"="#2a4030"),
                           guide = guide_legend(override.aes = list(linetype=c("solid", "dashed", "solid")),reverse=T))
    }
    
    else if (sum(as.numeric(input$options))==1){
      p +
        geom_ribbon(aes(x=week, ymin=non_covid_wales_lb, ymax=non_covid_wales_ub),
                    fill="#ff87b1", alpha=0.3) +
        geom_line(aes(week, non_covid_wales_std, colour="Non-COVID-deaths"),
                  size=1) +
        scale_color_manual(name = "",
                           values = c("Non-COVID-deaths"="#c10044", "Total deaths expected"="#01478c", "COVID-19 deaths"="#2a4030"),
                           guide = guide_legend(override.aes = list(linetype=c("dashed", "solid", "solid")),reverse=T))
    }
    
    else if (sum(as.numeric(input$options))==2){
      q +
        geom_ribbon(aes(x=week, ymin=total_wales_std_ma_lb, ymax=total_wales_std_ma_ub),
                    fill="#ff87b1", alpha=0.3) +
        geom_line(aes(week, total_wales_std_ma, colour="Total deaths observed"),
                  size=1) +
        scale_color_manual(name = "",
                           values = c("Total deaths observed"="#c10044", "Total deaths expected"="#01478c", "COVID-19 deaths"="#2a4030"),
                           guide = guide_legend(override.aes = list(linetype=c("solid", "dashed", "solid")),reverse=T))
    }
    
    else if (sum(as.numeric(input$options))==3){
      q +
        geom_ribbon(aes(x=week, ymin=non_covid_wales_std_ma_lb, ymax=non_covid_wales_std_ma_ub),
                    fill="#ff87b1", alpha=0.3) +
        geom_line(aes(week, non_covid_wales_std_ma, colour="Non-COVID-deaths"),
                  size=1) +
        scale_color_manual(name = "",
                           values = c("Non-COVID-deaths"="#c10044", "Total deaths expected"="#01478c", "COVID-19 deaths"="#2a4030"),
                           guide = guide_legend(override.aes = list(linetype=c("dashed", "solid", "solid")),reverse=T))
    }
    
    
  }, bg="transparent")
  
  
  # Northern Ireland graph =============================================================
  output$ni_graph <- renderPlot ({
    p <- ggplot(nisra) +
      geom_ribbon(aes(x=week, ymin=expected_lb, ymax=expected_ub),
                  fill="#a1c5ff", alpha=0.3) +
      geom_ribbon(aes(x=week, ymin=covid_lb, ymax=covid_ub),
                  fill="#b1debe", alpha=0.3) +
      geom_line(aes(week, expected_std, colour="Total deaths expected"),
                size=1, linetype="dashed") +
      geom_line(aes(week, covid_std, colour="COVID-19 deaths"),
                size=1) +
      scale_y_continuous(name="Deaths per 100,000", 
                         limits=c(0, ymax_common),
                         minor_breaks=NULL) +
      scale_x_date(name="Month in 2020",
                   limits= as.Date(c("2020-01-01", xmax_common)),
                   labels = date_format("%b"),
                   breaks = "months",
                   minor_breaks=NULL) +
      scale_color_manual(name = "",
                         values = c("Total deaths observed"="#c10044", "Total deaths expected"="#01478c", "COVID-19 deaths"="#2a4030"),
                         guide = guide_legend(reverse=FALSE)) +
      labs(title = "Mortality in Northern Ireland") +
      theme(
        panel.background = element_rect(fill="#f2f7fc", colour = "#002169",
                                        size=2, linetype = "solid"),
        plot.margin = unit(c(0.5, 1, 0.1, 0.7), "cm"),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
        axis.title.y = element_text(size=axis_title_size, vjust=3, margin = margin(t = 20, r = 0, b=30, l=0)),
        axis.title.x = element_text(size=axis_title_size, vjust=-1.5),
        axis.text = element_text(size=label_size),
        legend.key = element_rect(fill = "transparent", color = NA),
        legend.key.width = unit(1, "cm"),
        legend.text = element_text(size = legend_size),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.position="bottom"
      )
    
    q <- ggplot(nisra) +
      geom_ribbon(aes(x=week, ymin=expected_std_ma_lb, ymax=expected_std_ma_ub),
                  fill="#a1c5ff", alpha=0.3) +
      geom_ribbon(aes(x=week, ymin=covid_std_ma_lb, ymax=covid_std_ma_ub),
                  fill="#b1debe", alpha=0.3) +
      geom_line(aes(week, expected_std_ma, colour="Total deaths expected"),
                size=1, linetype="dashed") +
      geom_line(aes(week, covid_std_ma, colour="COVID-19 deaths"),
                size=1) +
      scale_y_continuous(name="Deaths per 100,000", 
                         limits=c(0, ymax_common_ma),
                         minor_breaks=NULL) +
      scale_x_date(name="Month in 2020",
                   limits= as.Date(c("2020-01-01", xmax_common)),
                   labels = date_format("%b"),
                   breaks = "months",
                   minor_breaks=NULL) +
      scale_color_manual(name = "",
                         values = c("Total deaths observed"="#c10044", "Total deaths expected"="#01478c", "COVID-19 deaths"="#2a4030"),
                         guide = guide_legend(reverse=FALSE)) +
      labs(title = "Mortality in Northern Ireland") +
      theme(
        panel.background = element_rect(fill="#f2f7fc", colour = "#002169",
                                        size=2, linetype = "solid"),
        plot.margin = unit(c(0.5, 1, 0.1, 0.7), "cm"),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
        axis.title.y = element_text(size=axis_title_size, vjust=3, margin = margin(t = 20, r = 0, b=30, l=0)),
        axis.title.x = element_text(size=axis_title_size, vjust=-1.5),
        axis.text = element_text(size=label_size),
        legend.key = element_rect(fill = "transparent", color = NA),
        legend.key.width = unit(1, "cm"),
        legend.text = element_text(size = legend_size),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.position="bottom"
      )
    
    if(sum(as.numeric(input$options))==0){
      p +
        geom_ribbon(aes(x=week, ymin=total_lb, ymax=total_ub),
                    fill="#ff87b1", alpha=0.3) +
        geom_line(aes(week, total_std, colour="Total deaths observed"),
                  size=1) +
        scale_color_manual(name = "",
                           values = c("Total deaths observed"="#c10044", "Total deaths expected"="#01478c", "COVID-19 deaths"="#2a4030"),
                           guide = guide_legend(override.aes = list(linetype=c("solid", "dashed", "dashed")),reverse=T))
    }
    
    else if (sum(as.numeric(input$options))==1){
      p +
        geom_ribbon(aes(x=week, ymin=non_covid_lb, ymax=non_covid_ub),
                    fill="#ff87b1", alpha=0.3) +
        geom_line(aes(week, non_covid_std, colour="Non-COVID-deaths"),
                  size=1) +
        scale_color_manual(name = "",
                           values = c("Non-COVID-deaths"="#c10044", "Total deaths expected"="#01478c", "COVID-19 deaths"="#2a4030"),
                           guide = guide_legend(override.aes = list(linetype=c("dashed", "solid", "solid")),reverse=T))
    }
    
    else if (sum(as.numeric(input$options))==2){
      q +
        geom_ribbon(aes(x=week, ymin=total_std_ma_lb, ymax=total_std_ma_ub),
                    fill="#ff87b1", alpha=0.3) +
        geom_line(aes(week, total_std_ma, colour="Total deaths observed"),
                  size=1) +
        scale_color_manual(name = "",
                           values = c("Total deaths observed"="#c10044", "Total deaths expected"="#01478c", "COVID-19 deaths"="#2a4030"),
                           guide = guide_legend(override.aes = list(linetype=c("solid", "dashed", "solid")),reverse=T))
    }
    
    else if (sum(as.numeric(input$options))==3){
      q +
        geom_ribbon(aes(x=week, ymin=non_covid_std_ma_lb, ymax=non_covid_std_ma_ub),
                    fill="#ff87b1", alpha=0.3) +
        geom_line(aes(week, non_covid_std_ma, colour="Non-COVID-deaths"),
                  size=1) +
        scale_color_manual(name = "",
                           values = c("Non-COVID-deaths"="#c10044", "Total deaths expected"="#01478c", "COVID-19 deaths"="#2a4030"),
                           guide = guide_legend(override.aes = list(linetype=c("dashed", "solid", "solid")),reverse=T))
    }
    
  }, bg="transparent")
  
  # England tab ==============================================================================================
  # Number of total deaths England
  output$total_box_england <- renderValueBox({
    t <- ons %>% select(week, total_england) %>% 
      filter(week>=input$slider_eng[1]) %>%
      filter(week<=input$slider_eng[2]) %>%
      select(total_england) %>% sum() %>% as.character()
    valueBox(t, 
             "Total deaths observed", color = "blue", icon = icon("chart-bar") 
    )
  })
  
  # Number of non-covid deaths England
  output$non_covid_box_england <- renderValueBox({
    t <- ons %>% select(week, non_covid_england) %>% 
      filter(week>=input$slider_eng[1]) %>%
      filter(week<=input$slider_eng[2]) %>%
      select(non_covid_england) %>% sum() %>% as.character()
    valueBox(t, 
             "Non-COVID deaths", color = "olive", icon = icon("chart-bar") 
    )
  })
  
  # Number of expected deaths England
  output$expected_box_england <- renderValueBox ({
    expected <- ons %>% select(week, expected_england) %>% 
      filter(week>=input$slider_eng[1]) %>%
      filter(week<=input$slider_eng[2]) %>%
      select(expected_england) %>% sum() %>% as.character()
    valueBox(expected,
             "Total deaths expected", color = "light-blue", icon = icon("signal")
    )
  })
  
  # Number of excess deaths England
  output$excess_box_england <- renderValueBox ({
    tot <- ons %>% select(week, total_england) %>% 
      filter(week>=input$slider_eng[1]) %>%
      filter(week<=input$slider_eng[2]) %>%
      select(total_england) %>% sum() %>% as.numeric()
    e <- ons %>% select(week, expected_england) %>% 
      filter(week>=input$slider_eng[1]) %>%
      filter(week<=input$slider_eng[2]) %>%
      select(expected_england) %>% sum() %>% as.numeric()
    excess <- tot-e 
    excess_pct <- as.character(round(excess/e*100))
    excess_pct <- paste0("(", excess_pct, "%", ")")
    excess <- as.character(excess)
    valueBox(paste0(excess, " ", excess_pct),
             "Excess deaths (% of expected)", color = "aqua", icon = icon("calendar-plus") 
    )
  })
  
  
  # Number of excess non-COVID deaths England
  output$non_covid_excess_box_england <- renderValueBox ({
    tot <- ons %>% select(week, non_covid_england) %>% 
      filter(week>=input$slider_eng[1]) %>%
      filter(week<=input$slider_eng[2]) %>%
      select(non_covid_england) %>% sum() %>% as.numeric()
    e <- ons %>% select(week, expected_england) %>% 
      filter(week>=input$slider_eng[1]) %>%
      filter(week<=input$slider_eng[2]) %>%
      select(expected_england) %>% sum() %>% as.numeric()
    excess <- tot-e 
    excess_pct <- as.character(round(excess/e*100))
    excess_pct <- paste0("(", excess_pct, "%", ")")
    excess <- as.character(excess)
    valueBox(paste0(excess, " ", excess_pct),
             "Non-COVID excess (% of expected)", color = "teal", icon = icon("calendar-plus")  
    )
  })
  
  # Latest week
  output$latest_england <- renderValueBox ({
    wk <- max(ons$week)
    wk <- format(wk, format="%d %b %Y")
    valueBox(wk, 
             "Latest data", color = "yellow", icon = icon("calendar-alt") 
    )
  })
  

  
  # England COD graph
  output$ons_cod_graph <- renderPlot ({
    p <- ggplot(ons) +
      geom_vline(aes(xintercept=as.Date("2020-01-31"), linetype="1st case", fill="1st case"),
                 colour="#ab6100", size=1) +
      geom_vline(aes(xintercept=as.Date("2020-03-26"), linetype="Lockdown", fill="Lockdown"),
                 colour="#4f009e", size=1) +
      geom_ribbon(aes(x=week, ymin=expected_england_lb, ymax=expected_england_ub),
                  fill="#a1c5ff", alpha=0.3) +
      geom_line(aes(week, expected_england_std, colour="Total deaths expected"),
                size=1, linetype="dashed") +
      scale_y_continuous(name="Deaths per 100,000", 
                         limits = c(10, NA),
                         minor_breaks=NULL) +
      scale_x_date(name="Month in 2020",
                   limits= as.Date(c(input$slider_eng[1], input$slider_eng[2])),
                   labels = date_format("%b"),
                   breaks = "months",
                   minor_breaks=NULL) +
      scale_color_manual(name = "",
                         values = c("Total deaths expected"="#01478c"),
                         guide = guide_legend(reverse=FALSE)) +
      scale_linetype_manual(name="", labels = c("1st case", "Lockdown"), 
                            values = c("1st case"="dashed", "Lockdown"="dashed")) +
      scale_fill_manual("", 
                        values=rep(1,4),
                        guide=
                          guide_legend(override.aes = list(colour=c("#ab6100", "#4f009e")))
      ) +
      theme(
        panel.background = element_rect(fill="#f2f7fc", colour = "#002169",
                                        size=2, linetype = "solid"),
        plot.margin = unit(c(0.5, 1, 0.1, 0.7), "cm"),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
        axis.title.y = element_text(size=axis_title_size, vjust=3, margin = margin(t = 20, r = 0, b=30, l=0)),
        axis.title.x = element_text(size=axis_title_size, vjust=-1.5),
        axis.text = element_text(size=label_size),
        legend.key = element_rect(fill = "transparent", color = NA),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = legend_size),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.position="bottom"
      )
    
    q <- ggplot(ons) +
      geom_vline(aes(xintercept=as.Date("2020-01-31"), linetype="1st case", fill="1st case"),
                 colour="#ab6100", size=1) +
      geom_vline(aes(xintercept=as.Date("2020-03-26"), linetype="Lockdown", fill="Lockdown"),
                 colour="#4f009e", size=1) +
      geom_ribbon(aes(x=week, ymin=expected_england_std_ma_lb, ymax=expected_england_std_ma_ub),
                  fill="#a1c5ff", alpha=0.3) +
      geom_line(aes(week, expected_england_std_ma, colour="Total deaths expected"),
                size=1, linetype = "dashed") +
      scale_y_continuous(name="Deaths per 100,000", 
                         limits = c(10, NA),
                         minor_breaks=NULL) +
      scale_x_date(name="Month in 2020",
                   limits= as.Date(c(input$slider_eng[1], input$slider_eng[2])),
                   labels = date_format("%b"),
                   breaks = "months",
                   minor_breaks=NULL) +
      scale_color_manual(name = "",
                         values = c("Total deaths expected"="#01478c"),
                         guide = guide_legend(reverse=FALSE)) +
      scale_linetype_manual(name="", labels = c("1st case", "Lockdown"), 
                            values = c("1st case"="dashed", "Lockdown"="dashed")) +
      scale_fill_manual("", 
                        values=rep(1,4),
                        guide=
                          guide_legend(override.aes = list(colour=c("#ab6100", "#4f009e")))
      ) +
      theme(
        panel.background = element_rect(fill="#f2f7fc", colour = "#002169",
                                        size=2, linetype = "solid"),
        plot.background = element_rect(fill = "transparent", colour = NA),
        plot.margin = unit(c(0.5, 1, 0.1, 0.7), "cm"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
        axis.title.y = element_text(size=axis_title_size, vjust=3, margin = margin(t = 20, r = 0, b=30, l=0)),
        axis.title.x = element_text(size=axis_title_size, vjust=-1.5),
        axis.text = element_text(size=label_size),
        legend.key = element_rect(fill = "transparent", color = NA),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = legend_size),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.position="bottom"
      ) 
    
    if(input$noncovid_eng == FALSE & input$movingaverage_eng == FALSE){
      p +
        geom_ribbon(aes(x=week, ymin=total_england_lb, ymax=total_england_ub),
                    fill="#ff87b1", alpha=0.3) +
        geom_line(aes(week, total_england_std, colour="Total deaths observed"),
                  size=1) +
        scale_color_manual(name = "",
                           values = c("Total deaths observed"="#c10044", "Total deaths expected"="#01478c"),
                           breaks = c("Total deaths observed", "Total deaths expected"),
                           guide = guide_legend(override.aes = list(linetype=c("solid", "dashed")),reverse=F))
    }
    
    else if(input$noncovid_eng == TRUE & input$movingaverage_eng == FALSE){
      p +
        geom_ribbon(aes(x=week, ymin=non_covid_england_lb, ymax=non_covid_england_ub),
                    fill="#ff87b1", alpha=0.3) +
        geom_line(aes(week, non_covid_england_std, colour="Non-COVID-deaths"),
                  size=1) +
        scale_color_manual(name = "",
                           values = c("Non-COVID-deaths"="#c10044", "Total deaths expected"="#01478c"),
                           guide = guide_legend(override.aes = list(linetype=c("solid", "dashed")),reverse=F))
    }
    
    else if(input$noncovid_eng == FALSE & input$movingaverage_eng == TRUE){
      q +
        geom_ribbon(aes(x=week, ymin=total_england_std_ma_lb, ymax=total_england_std_ma_ub),
                    fill="#ff87b1", alpha=0.3) +
        geom_line(aes(week, total_england_std_ma, colour="Total deaths observed"),
                  size=1) +
        scale_color_manual(name = "",
                           values = c("Total deaths observed"="#c10044", "Total deaths expected"="#01478c"),
                           guide = guide_legend(override.aes = list(linetype=c("solid", "dashed")),reverse=T))
    }
    
    else if(input$noncovid_eng == TRUE & input$movingaverage_eng == TRUE){
      q +
        geom_ribbon(aes(x=week, ymin=non_covid_england_std_ma_lb, ymax=non_covid_england_std_ma_ub),
                    fill="#ff87b1", alpha=0.3) +
        geom_line(aes(week, non_covid_england_std_ma, colour="Non-COVID-deaths"),
                  size=1) +
        scale_color_manual(name = "",
                           values = c("Non-COVID-deaths"="#c10044", "Total deaths expected"="#01478c"),
                           guide = guide_legend(override.aes = list(linetype=c("solid", "dashed")),reverse=F))
    }
    
  }, bg="transparent")
  
  
  # Scotland tab ==============================================================================
  # Number of total deaths Scotland
  output$total_box_scotland <- renderValueBox({
    t <- deaths %>% select(week, input$cod) %>%
      filter(week>=input$slider_scotland[1]) %>%
      filter(week<=input$slider_scotland[2]) %>%
      select(input$cod) %>% sum()
    valueBox(t , 
             "Total deaths observed", color = "maroon", icon = icon("chart-bar")
    )
  })

  
  # Number of excess deaths Scotland
  output$excess_box_scotland <- renderValueBox ({
    tot <- deaths %>% select(week, input$cod) %>% 
      filter(week>=input$slider_scotland[1]) %>%
      filter(week<=input$slider_scotland[2]) %>%
      select(input$cod) %>% sum() %>% as.numeric()
    e <- five_year_average %>% select(week, input$cod) %>% 
      filter(week>=input$slider_scotland[1]) %>%
      filter(week<=input$slider_scotland[2]) %>%
      select(input$cod) %>% sum() %>% as.numeric()
    excess <- tot-e 
    excess_pct <- as.character(round(excess/e*100))
    excess_pct <- paste0("(", excess_pct, "%", ")")
    excess <- as.character(excess)
    valueBox(paste0(excess, " ", excess_pct),
             "Excess deaths (% of expected)", color = "red", icon = icon("calendar-plus")
    )
  })
  
  
  # Number of expected deaths Scotland
  output$expected_box_scotland <- renderValueBox ({
    expected <- five_year_average %>% select(week, input$cod) %>% 
      filter(week>=input$slider_scotland[1]) %>%
      filter(week<=input$slider_scotland[2]) %>%
      select(input$cod) %>% sum() %>% as.numeric()
    valueBox(expected,
             "Total deaths expected", color = "purple", icon = icon("signal")
    )
  })
  
  
  
  # Latest week
  output$latest_scotland <- renderValueBox ({
    wk <- max(nrs$week)
    wk <- format(wk, format="%d %b %Y")
    valueBox(wk,
             "Latest data", color = "yellow", icon = icon("calendar-alt")
    )
  })
  

  # Scotland COD graph Scotland
  output$nrs_cod_graph <- renderPlot ({
    if (input$cod == "All"){
      nrs_data <- nrs %>% select(c("week", "expected_std", "expected_lb", "expected_ub",
                                   "expected_std_ma", "expected_std_ma_lb", "expected_std_ma_ub",
                                   "total_std", "total_lb", "total_ub", 
                                   "total_std_ma", "total_std_ma_lb", "total_std_ma_ub"
      ))
      colnames(nrs_data) <- c("week", "expected_std", "expected_lb", "expected_ub",
                              "expected_std_ma", "expected_std_ma_lb", "expected_std_ma_ub",
                              "std", "lb", "ub", 
                              "ma", "ma_lb", "ma_ub")
    }
    
    else if (input$cod == "Non-COVID"){
      nrs_data <- nrs %>% select(c("week", "expected_std", "expected_lb", "expected_ub",
                                   "expected_std_ma", "expected_std_ma_lb", "expected_std_ma_ub",
                                   "non_covid_std", "non_covid_lb", "non_covid_ub", 
                                   "non_covid_std_ma", "non_covid_std_ma_lb", "non_covid_std_ma_ub"
      ))
      colnames(nrs_data) <- c("week", "expected_std", "expected_lb", "expected_ub",
                              "expected_std_ma", "expected_std_ma_lb", "expected_std_ma_ub",
                              "std", "lb", "ub", 
                              "ma", "ma_lb", "ma_ub")
    }
    
    else if (input$cod == "Cancer"){
      nrs_data <- nrs %>% select(c("week", "expected_cancer_std", "expected_cancer_lb", "expected_cancer_ub",
                                   "expected_cancer_std_ma", "expected_cancer_std_ma_lb", "expected_cancer_std_ma_ub",
                                   "cancer_std", "cancer_lb", "cancer_ub", 
                                   "cancer_std_ma", "cancer_std_ma_lb", "cancer_std_ma_ub"
      ))
      colnames(nrs_data) <- c("week", "expected_std", "expected_lb", "expected_ub",
                              "expected_std_ma", "expected_std_ma_lb", "expected_std_ma_ub",
                              "std", "lb", "ub", 
                              "ma", "ma_lb", "ma_ub")
    }
    
    else if (input$cod == "Cardiovascular"){
      nrs_data <- nrs %>% select(c("week", "expected_cardiovascular_std", "expected_cardiovascular_lb", "expected_cardiovascular_ub",
                                   "expected_cardiovascular_std_ma", "expected_cardiovascular_std_ma_lb", "expected_cardiovascular_std_ma_ub",
                                   "cardiovascular_std", "cardiovascular_lb", "cardiovascular_ub", 
                                   "cardiovascular_std_ma", "cardiovascular_std_ma_lb", "cardiovascular_std_ma_ub"
      ))
      colnames(nrs_data) <- c("week", "expected_std", "expected_lb", "expected_ub",
                              "expected_std_ma", "expected_std_ma_lb", "expected_std_ma_ub",
                              "std", "lb", "ub", 
                              "ma", "ma_lb", "ma_ub")
    }
    
    else if (input$cod == "Dementia"){
      nrs_data <- nrs %>% select(c("week", "expected_dementia_std", "expected_dementia_lb", "expected_dementia_ub",
                                   "expected_dementia_std_ma", "expected_dementia_std_ma_lb", "expected_dementia_std_ma_ub",
                                   "dementia_std", "dementia_lb", "dementia_ub", 
                                   "dementia_std_ma", "dementia_std_ma_lb", "dementia_std_ma_ub"
      ))
      colnames(nrs_data) <- c("week", "expected_std", "expected_lb", "expected_ub",
                              "expected_std_ma", "expected_std_ma_lb", "expected_std_ma_ub",
                              "std", "lb", "ub", 
                              "ma", "ma_lb", "ma_ub")
    }
    
    else if (input$cod == "Respiratory"){
      nrs_data <- nrs %>% select(c("week", "expected_respiratory_std", "expected_respiratory_lb", "expected_respiratory_ub",
                                   "expected_respiratory_std_ma", "expected_respiratory_std_ma_lb", "expected_respiratory_std_ma_ub",
                                   "respiratory_std", "respiratory_lb", "respiratory_ub", 
                                   "respiratory_std_ma", "respiratory_std_ma_lb", "respiratory_std_ma_ub"
      ))
      colnames(nrs_data) <- c("week", "expected_std", "expected_lb", "expected_ub",
                              "expected_std_ma", "expected_std_ma_lb", "expected_std_ma_ub",
                              "std", "lb", "ub", 
                              "ma", "ma_lb", "ma_ub")
    }
    
    else if (input$cod == "Others"){
      nrs_data <- nrs %>% select(c("week", "expected_others_std", "expected_others_lb", "expected_others_ub",
                                   "expected_others_std_ma", "expected_others_std_ma_lb", "expected_others_std_ma_ub",
                                   "others_std", "others_lb", "others_ub", 
                                   "others_std_ma", "others_std_ma_lb", "others_std_ma_ub"
      ))
      colnames(nrs_data) <- c("week", "expected_std", "expected_lb", "expected_ub",
                              "expected_std_ma", "expected_std_ma_lb", "expected_std_ma_ub",
                              "std", "lb", "ub", 
                              "ma", "ma_lb", "ma_ub")
    }
    
    p <- ggplot(nrs_data) +
      geom_vline(aes(xintercept=as.Date("2020-03-01"), linetype="1st case", fill="1st case"),
                 colour="#ab6100", size=1) +
      geom_vline(aes(xintercept=as.Date("2020-03-26"), linetype="Lockdown", fill="Lockdown"),
                 colour="#4f009e", size=1) +
      geom_ribbon(aes(x=week, ymin=expected_lb, ymax=expected_ub),
                  fill="#a1c5ff", alpha=0.3) +
      geom_line(aes(week, expected_std, colour="Expected deaths"),
                size=1, linetype = "dashed") +
      scale_y_continuous(name="Deaths per 100,000",
                         minor_breaks=NULL) +
      scale_x_date(name="Month in 2020",
                   limits= as.Date(c(input$slider_scotland[1], input$slider_scotland[2])),
                   labels = date_format("%b"),
                   breaks = "months",
                   minor_breaks=NULL) +
      scale_color_manual(name = "",
                         values = c("Cause-specific deaths"="#c10044", "Expected deaths"="#01478c"),
                         guide = guide_legend(reverse=FALSE)) +
      scale_linetype_manual(name="", labels = c("1st case", "Lockdown"), 
                            values = c("1st case"="dashed", "Lockdown"="dashed")) +
      scale_fill_manual("", 
                        values=rep(1,4),
                        guide=
                          guide_legend(override.aes = list(colour=c("#ab6100", "#4f009e")))
      ) +
      theme(
        panel.background = element_rect(fill="#f2f7fc", colour = "#002169",
                                        size=2, linetype = "solid"),
        plot.margin = unit(c(1, 1, 0.1, 0.7), "cm"),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
        axis.title.y = element_text(size=axis_title_size, vjust=3, margin = margin(t = 20, r = 0, b=30, l=0)),
        axis.title.x = element_text(size=axis_title_size, vjust=-1.5),
        axis.text = element_text(size=label_size),
        legend.key = element_rect(fill = "transparent", color = NA),
        legend.key.size = unit(1, "cm"),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.text = element_text(size = axis_title_size),
        legend.position="bottom"
      )
    
    q <- ggplot(nrs_data) +
      geom_vline(aes(xintercept=as.Date("2020-03-01"), linetype="1st case", fill="1st case"),
                 colour="#ab6100", size=1) +
      geom_vline(aes(xintercept=as.Date("2020-03-26"), linetype="Lockdown", fill="Lockdown"),
                 colour="#4f009e", size=1) +
      geom_ribbon(aes(x=week, ymin=expected_std_ma_lb, ymax=expected_std_ma_ub),
                  fill="#a1c5ff", alpha=0.3) +
      geom_line(aes(week, expected_std_ma, colour="Expected deaths"),
                size=1, linetype = "dashed") +
      scale_y_continuous(name="Deaths per 100,000", 
                         minor_breaks=NULL) +
      scale_x_date(name="Month in 2020",
                   limits= as.Date(c(input$slider_scotland[1], input$slider_scotland[2])),
                   labels = date_format("%b"),
                   breaks = "months",
                   minor_breaks=NULL) +
      scale_color_manual(name = "",
                         values = c("Expected deaths"="#01478c"),
                         guide = guide_legend(reverse=FALSE)) +
      scale_linetype_manual(name="", labels = c("1st case", "Lockdown"), 
                            values = c("1st case"="dashed", "Lockdown"="dashed")) +
      scale_fill_manual("", 
                        values=rep(1,4),
                        guide=
                          guide_legend(override.aes = list(colour=c("#ab6100", "#4f009e")))
      ) +
      theme(
        panel.background = element_rect(fill="#f2f7fc", colour = "#002169",
                                        size=2, linetype = "solid"),
        plot.background = element_rect(fill = "transparent", colour = NA),
        plot.margin = unit(c(0.5, 1, 0.1, 0.7), "cm"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
        axis.title.y = element_text(size=axis_title_size, vjust=3, margin = margin(t = 20, r = 0, b=30, l=0)),
        axis.title.x = element_text(size=axis_title_size, vjust=-1.5),
        axis.text = element_text(size=label_size),
        legend.key = element_rect(fill = "transparent", color = NA),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = axis_title_size),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.position="bottom"
      )
    
    if(input$movingaverage_scotland == FALSE){
      p +
        geom_ribbon(aes(x=week, ymin=lb, ymax=ub),
                    fill="#ff87b1", alpha=0.3) +
        geom_line(aes(week, std, colour="Observed deaths"),
                  size=1) +
        scale_color_manual(name = "",
                           values = c("Observed deaths"="#c10044", "Expected deaths"="#01478c"),
                           guide = guide_legend(override.aes = list(linetype=c("solid", "dashed")),reverse=T))
    }
    
    
    else if(input$movingaverage_scotland == TRUE){
      q +
        geom_ribbon(aes(x=week, ymin=ma_lb, ymax=ma_ub),
                    fill="#ff87b1", alpha=0.3) +
        geom_line(aes(week, ma, colour="Observed deaths"),
                  size=1) +
        scale_color_manual(name = "",
                           values = c("Observed deaths"="#c10044", "Expected deaths"="#01478c"),
                           guide = guide_legend(override.aes = list(linetype=c("solid", "dashed")),reverse=T))
    }
    
  }, bg="transparent")
  
  # Scotland health board map of non-COVID deaths
  
  

  
  # Wales tab
  ## Wales tab ===============================================================================
  #Number of total deaths Wales
  output$total_box_wales <- renderValueBox({
    t <- ons %>% select(week, total_wales) %>%
      filter(week>=input$slider_wales[1]) %>%
      filter(week<=input$slider_wales[2]) %>%
      select(total_wales) %>% sum() %>% as.character()
    valueBox(t,
             "Total deaths observed", color = "blue", icon = icon("chart-bar")
    )
  })
  
  # Number of non-covid deaths Wales
  output$non_covid_box_wales <- renderValueBox({
    t <- ons %>% select(week, non_covid_wales) %>%
      filter(week>=input$slider_wales[1]) %>%
      filter(week<=input$slider_wales[2]) %>%
      select(non_covid_wales) %>% sum() %>% as.character()
    valueBox(t,
             "Non-COVID deaths", color = "olive", icon = icon("chart-bar")
    )
  })
  
  # Number of expected deaths Wales
  output$expected_box_wales <- renderValueBox ({
    expected <- ons %>% select(week, expected_wales) %>%
      filter(week>=input$slider_wales[1]) %>%
      filter(week<=input$slider_wales[2]) %>%
      select(expected_wales) %>% sum() %>% as.character()
    valueBox(expected,
             "Total deaths expected", color = "light-blue", icon = icon("signal")
    )
  })
  
  # Number of excess deaths Wales
  output$excess_box_wales <- renderValueBox ({
    tot <- ons %>% select(week, total_wales) %>%
      filter(week>=input$slider_wales[1]) %>%
      filter(week<=input$slider_wales[2]) %>%
      select(total_wales) %>% sum() %>% as.numeric()
    e <- ons %>% select(week, expected_wales) %>%
      filter(week>=input$slider_wales[1]) %>%
      filter(week<=input$slider_wales[2]) %>%
      select(expected_wales) %>% sum() %>% as.numeric()
    excess <- tot-e
    excess_pct <- as.character(round(excess/e*100))
    excess_pct <- paste0("(", excess_pct, "%", ")")
    excess <- as.character(excess)
    valueBox(paste0(excess, " ", excess_pct),
             "Excess deaths (% of expected)", color = "aqua", icon = icon("calendar-plus")
    )
  })
  
  
  # Number of excess non-COVID deaths Wales
  output$non_covid_excess_box_wales <- renderValueBox ({
    tot <- ons %>% select(week, non_covid_wales) %>%
      filter(week>=input$slider_wales[1]) %>%
      filter(week<=input$slider_wales[2]) %>%
      select(non_covid_wales) %>% sum() %>% as.numeric()
    e <- ons %>% select(week, expected_wales) %>%
      filter(week>=input$slider_wales[1]) %>%
      filter(week<=input$slider_wales[2]) %>%
      select(expected_wales) %>% sum() %>% as.numeric()
    excess <- tot-e
    excess_pct <- as.character(round(excess/e*100))
    excess_pct <- paste0("(", excess_pct, "%", ")")
    excess <- as.character(excess)
    valueBox(paste0(excess, " ", excess_pct),
             "Non-COVID excess (% of expected)", color = "teal", icon = icon("calendar-plus"),
    )
  })
  
  # Latest week
  output$latest_wales <- renderValueBox ({
    wk <- max(ons$week)
    wk <- format(wk, "%d %b %Y")
    valueBox(wk,
             "Latest data", color = "yellow", icon = icon("calendar-alt")
    )
  })
  
  # Wales COD graph
  output$ons_cod_graph_wales <- renderPlot ({
    p <- ggplot(ons) +
      geom_vline(aes(xintercept=as.Date("2020-02-28"), linetype="1st case", fill="1st case"),
                 colour="#ab6100", size=1) +
      geom_vline(aes(xintercept=as.Date("2020-03-26"), linetype="Lockdown", fill="Lockdown"),
                 colour="#4f009e", size=1) +
      geom_ribbon(aes(x=week, ymin=expected_wales_lb, ymax=expected_wales_ub),
                  fill="#a1c5ff", alpha=0.3) +
      geom_line(aes(week, expected_wales_std, colour="Total deaths expected"),
                size=1, linetype="dashed") +
      scale_y_continuous(name="Deaths per 100,000",
                         limits = c(10, NA),
                         minor_breaks=NULL) +
      scale_x_date(name="Month in 2020",
                   limits= as.Date(c(input$slider_wales[1], input$slider_wales[2])),
                   labels = date_format("%b"),
                   breaks = "months",
                   minor_breaks=NULL) +
      scale_color_manual(name = "",
                         values = c("Total deaths expected"="#01478c"),
                         guide = guide_legend(reverse=FALSE)) +
      scale_linetype_manual(name="", labels = c("1st case", "Lockdown"), 
                            values = c("1st case"="dashed", "Lockdown"="dashed")) +
      scale_fill_manual("", 
                        values=rep(1,4),
                        guide=
                          guide_legend(override.aes = list(colour=c("#ab6100", "#4f009e")))
      ) +
      theme(
        panel.background = element_rect(fill="#f2f7fc", colour = "#002169",
                                        size=2, linetype = "solid"),
        plot.margin = unit(c(0.5, 1, 0.1, 0.7), "cm"),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
        axis.title.y = element_text(size=axis_title_size, vjust=3, margin = margin(t = 20, r = 0, b=30, l=0)),
        axis.title.x = element_text(size=axis_title_size, vjust=-1.5),
        axis.text = element_text(size=label_size),
        legend.key = element_rect(fill = "transparent", color = NA),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = legend_size),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.position="bottom"
      )
    
    q <- ggplot(ons) +
      geom_vline(aes(xintercept=as.Date("2020-02-28"), linetype="1st case", fill="1st case"),
                 colour="#ab6100", size=1) +
      geom_vline(aes(xintercept=as.Date("2020-03-26"), linetype="Lockdown", fill="Lockdown"),
                 colour="#4f009e", size=1) +
      geom_ribbon(aes(x=week, ymin=expected_wales_std_ma_lb, ymax=expected_wales_std_ma_ub),
                  fill="#a1c5ff", alpha=0.3) +
      geom_line(aes(week, expected_wales_std_ma, colour="Total deaths expected"),
                size=1, linetype = "dashed") +
      scale_y_continuous(name="Deaths per 100,000",
                         limits = c(10, NA),
                         minor_breaks=NULL) +
      scale_x_date(name="Month in 2020",
                   limits= as.Date(c(input$slider_wales[1], input$slider_wales[2])),
                   labels = date_format("%b"),
                   breaks = "months",
                   minor_breaks=NULL) +
      scale_color_manual(name = "",
                         values = c("Total deaths expected"="#01478c"),
                         guide = guide_legend(reverse=FALSE)) +
      scale_linetype_manual(name="", labels = c("1st case", "Lockdown"), 
                            values = c("1st case"="dashed", "Lockdown"="dashed")) +
      scale_fill_manual("", 
                        values=rep(1,4),
                        guide=
                          guide_legend(override.aes = list(colour=c("#ab6100", "#4f009e")))
      ) +
      theme(
        panel.background = element_rect(fill="#f2f7fc", colour = "#002169",
                                        size=2, linetype = "solid"),
        plot.background = element_rect(fill = "transparent", colour = NA),
        plot.margin = unit(c(0.5, 1, 0.1, 0.7), "cm"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
        axis.title.y = element_text(size=axis_title_size, vjust=3, margin = margin(t = 20, r = 0, b=30, l=0)),
        axis.title.x = element_text(size=axis_title_size, vjust=-1.5),
        axis.text = element_text(size=label_size),
        legend.key = element_rect(fill = "transparent", color = NA),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = legend_size),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.position="bottom"
      )
    
    if(input$noncovid_wales == FALSE & input$movingaverage_wales == FALSE){
      p +
        geom_ribbon(aes(x=week, ymin=total_wales_lb, ymax=total_wales_ub),
                    fill="#ff87b1", alpha=0.3) +
        geom_line(aes(week, total_wales_std, colour="Total deaths observed"),
                  size=1) +
        scale_color_manual(name = "",
                           values = c("Total deaths observed"="#c10044", "Total deaths expected"="#01478c"),
                           guide = guide_legend(override.aes = list(linetype=c("solid", "dashed")),reverse=T))
    }
    
    else if(input$noncovid_wales == TRUE & input$movingaverage_wales == FALSE){
      p +
        geom_ribbon(aes(x=week, ymin=ons$non_covid_wales_lb, ymax=ons$non_covid_wales_ub),
                    fill="#ff87b1", alpha=0.3) +
        geom_line(aes(week, ons$non_covid_wales_std, colour="Non-COVID-deaths"),
                  size=1) +
        scale_color_manual(name = "",
                           values = c("Non-COVID-deaths"="#c10044", "Total deaths expected"="#01478c"),
                           guide = guide_legend(override.aes = list(linetype=c("solid", "dashed")),reverse=F))
    }
    
    else if(input$noncovid_wales == FALSE & input$movingaverage_wales == TRUE){
      q +
        geom_ribbon(aes(x=week, ymin=total_wales_std_ma_lb, ymax=total_wales_std_ma_ub),
                    fill="#ff87b1", alpha=0.3) +
        geom_line(aes(week, total_wales_std_ma, colour="Total deaths observed"),
                  size=1) +
        scale_color_manual(name = "",
                           values = c("Total deaths observed"="#c10044", "Total deaths expected"="#01478c"),
                           guide = guide_legend(override.aes = list(linetype=c("solid", "dashed")),reverse=T))
    }
    
    else if(input$noncovid_wales == TRUE & input$movingaverage_wales == TRUE){
      q +
        geom_ribbon(aes(x=week, ymin=non_covid_wales_std_ma_lb, ymax=non_covid_wales_std_ma_ub),
                    fill="#ff87b1", alpha=0.3) +
        geom_line(aes(week, non_covid_wales_std_ma, colour="Non-COVID-deaths"),
                  size=1) +
        scale_color_manual(name = "",
                           values = c("Non-COVID-deaths"="#c10044", "Total deaths expected"="#01478c"),
                           guide = guide_legend(override.aes = list(linetype=c("solid", "dashed")),reverse=F))
    }
    
  }, bg="transparent")
  
  
  # Northern Ireland tab =============================================================
  #Number of total deaths Northern Ireland
  output$total_box_ni <- renderValueBox({
    t <- nisra %>% select(week, total) %>%
      filter(week>=input$slider_ni[1]) %>%
      filter(week<=input$slider_ni[2]) %>%
      select(total) %>% sum() %>% as.character()
    valueBox(t,
             "Total deaths observed", color = "blue", icon = icon("chart-bar")
    )
  })
  
  # Number of non-covid deaths Northern Ireland
  output$non_covid_box_ni <- renderValueBox({
    t <- nisra %>% select(week, non_covid) %>%
      filter(week>=input$slider_ni[1]) %>%
      filter(week<=input$slider_ni[2]) %>%
      select(non_covid) %>% sum() %>% as.character()
    valueBox(t,
             "Non-COVID deaths", color = "olive", icon = icon("chart-bar")
    )
  })
  
  # Number of expected deaths Northern Ireland
  output$expected_box_ni <- renderValueBox ({
    expected <- nisra %>% select(week, expected) %>%
      filter(week>=input$slider_ni[1]) %>%
      filter(week<=input$slider_ni[2]) %>%
      select(expected) %>% sum() %>% round() %>% as.character()
    valueBox(expected,
             "Total deaths expected", color = "light-blue", icon = icon("signal")
    )
  })
  
  # Number of excess deaths Northern Ireland
  output$excess_box_ni <- renderValueBox ({
    tot <- nisra %>% select(week, total) %>%
      filter(week>=input$slider_ni[1]) %>%
      filter(week<=input$slider_ni[2]) %>%
      select(total) %>% sum() %>% as.numeric()
    e <- nisra %>% select(week, expected) %>%
      filter(week>=input$slider_ni[1]) %>%
      filter(week<=input$slider_ni[2]) %>%
      select(expected) %>% sum() %>% as.numeric()
    excess <- round(tot-e)
    excess_pct <- as.character(round(excess/e*100))
    excess_pct <- paste0("(", excess_pct, "%", ")")
    excess <- as.character(excess)
    valueBox(paste0(excess, " ", excess_pct),
             "Excess deaths (% of expected)", color = "aqua", icon = icon("calendar-plus")
    )
  })
  
  
  # Number of excess non-COVID deaths Northern Ireland
  output$non_covid_excess_box_ni <- renderValueBox ({
    tot <- nisra %>% select(week, non_covid) %>%
      filter(week>=input$slider_ni[1]) %>%
      filter(week<=input$slider_ni[2]) %>%
      select(non_covid) %>% sum() %>% as.numeric()
    e <- nisra %>% select(week, expected) %>%
      filter(week>=input$slider_ni[1]) %>%
      filter(week<=input$slider_ni[2]) %>%
      select(expected) %>% sum() %>% as.numeric()
    excess <- round(tot-e)
    excess_pct <- as.character(round(excess/e*100))
    excess_pct <- paste0("(", excess_pct, "%", ")")
    excess <- as.character(excess)
    valueBox(paste0(excess, " ", excess_pct),
             "Non-COVID excess (% of expected)", color = "teal", icon = icon("calendar-plus"),
    )
  })
  
  # Latest week Northern Ireland
  output$latest_ni <- renderValueBox ({
    wk <- max(nisra$week)
    wk <- format(wk, "%d %b %Y")
    valueBox(wk, 
             "Latest data", color = "yellow", icon = icon("calendar-alt")
    )
  })
  
  # Northern Ireland COD graph
  output$nisra_cod_graph <- renderPlot ({
    p <- ggplot(nisra) +
      geom_vline(aes(xintercept=as.Date("2020-02-27"), linetype="1st case", fill="1st case"),
                 colour="#ab6100", size=1) +
      geom_vline(aes(xintercept=as.Date("2020-03-26"), linetype="Lockdown", fill="Lockdown"),
                 colour="#4f009e", size=1) +
      geom_ribbon(aes(x=week, ymin=expected_lb, ymax=expected_ub),
                  fill="#a1c5ff", alpha=0.3) +
      geom_line(aes(week, expected_std, colour="Total deaths expected"),
                size=1, linetype="dashed") +
      scale_y_continuous(name="Deaths per 100,000",
                         limits = c(10,NA),
                         minor_breaks=NULL) +
      scale_x_date(name="Month in 2020",
                   limits= as.Date(c(input$slider_ni[1], input$slider_ni[2])),
                   labels = date_format("%b"),
                   breaks = "months",
                   minor_breaks=NULL) +
      scale_color_manual(name = "",
                         values = c("Total deaths expected"="#01478c"),
                         guide = guide_legend(reverse=FALSE)) +
      scale_linetype_manual(name="", labels = c("1st case", "Lockdown"), 
                            values = c("1st case"="dashed", "Lockdown"="dashed")) +
      scale_fill_manual("", 
                        values=rep(1,4),
                        guide=
                          guide_legend(override.aes = list(colour=c("#ab6100", "#4f009e")))
      ) +
      theme(
        panel.background = element_rect(fill="#f2f7fc", colour = "#002169",
                                        size=2, linetype = "solid"),
        plot.margin = unit(c(0.5, 1, 0.1, 0.7), "cm"),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
        axis.title.y = element_text(size=axis_title_size, vjust=3, margin = margin(t = 20, r = 0, b=30, l=0)),
        axis.title.x = element_text(size=axis_title_size, vjust=-1.5),
        axis.text = element_text(size=label_size),
        legend.key = element_rect(fill = "transparent", color = NA),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = axis_title_size),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.position="bottom"
      )
    
    q <- ggplot(nisra) +
      geom_vline(aes(xintercept=as.Date("2020-02-27"), linetype="1st case", fill="1st case"),
                 colour="#ab6100", size=1) +
      geom_vline(aes(xintercept=as.Date("2020-03-26"), linetype="Lockdown", fill="Lockdown"),
                 colour="#4f009e", size=1) +
      geom_ribbon(aes(x=week, ymin=expected_std_ma_lb, ymax=expected_std_ma_ub),
                  fill="#a1c5ff", alpha=0.3) +
      geom_line(aes(week, expected_std_ma, colour="Total deaths expected"),
                size=1, linetype = "dashed") +
      scale_y_continuous(name="Deaths per 100,000",
                         limits = c(10, NA),
                         minor_breaks=NULL) +
      scale_x_date(name="Month in 2020",
                   limits= as.Date(c(input$slider_ni[1], input$slider_ni[2])),
                   labels = date_format("%b"),
                   breaks = "months",
                   minor_breaks=NULL) +
      scale_color_manual(name = "",
                         values = c("Total deaths expected"="#01478c"),
                         guide = guide_legend(reverse=FALSE)) +
      scale_linetype_manual(name="", labels = c("1st case", "Lockdown"), 
                            values = c("1st case"="dashed", "Lockdown"="dashed")) +
      scale_fill_manual("", 
                        values=rep(1,4),
                        guide=
                          guide_legend(override.aes = list(colour=c("#ab6100", "#4f009e")))
      ) +
      theme(
        panel.background = element_rect(fill="#f2f7fc", colour = "#002169",
                                        size=2, linetype = "solid"),
        plot.margin = unit(c(0.5, 1, 0.1, 0.7), "cm"),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
        axis.title.y = element_text(size=axis_title_size, vjust=3, margin = margin(t = 20, r = 0, b=30, l=0)),
        axis.title.x = element_text(size=axis_title_size, vjust=-1.5),
        axis.text = element_text(size=label_size),
        legend.key = element_rect(fill = "transparent", color = NA),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = axis_title_size),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.position="bottom"
      )
    
    if(input$noncovid_ni == FALSE & input$movingaverage_ni == FALSE){
      p +
        geom_ribbon(aes(x=week, ymin=total_lb, ymax=nisra$total_ub),
                    fill="#ff87b1", alpha=0.3) +
        geom_line(aes(week, total_std, colour="Total deaths observed"),
                  size=1) +
        scale_color_manual(name = "",
                           values = c("Total deaths observed"="#c10044", "Total deaths expected"="#01478c"),
                           guide = guide_legend(override.aes = list(linetype=c("solid", "dashed")),reverse=T))
    }
    
    else if(input$noncovid_ni == TRUE & input$movingaverage_ni == FALSE){
      p +
        geom_ribbon(aes(x=week, ymin=nisra$non_covid_lb, ymax=nisra$non_covid_ub),
                    fill="#ff87b1", alpha=0.3) +
        geom_line(aes(week, non_covid_std, colour="Non-COVID-deaths"),
                  size=1) +
        scale_color_manual(name = "",
                           values = c("Non-COVID-deaths"="#c10044", "Total deaths expected"="#01478c"),
                           guide = guide_legend(override.aes = list(linetype=c("solid", "dashed")),reverse=F))
    }
    
    else if(input$noncovid_ni == FALSE & input$movingaverage_ni == TRUE){
      q +
        geom_ribbon(aes(x=week, ymin=nisra$total_std_ma_lb, ymax=nisra$total_std_ma_ub),
                    fill="#ff87b1", alpha=0.3) +
        geom_line(aes(week, total_std_ma, colour="Total deaths observed"),
                  size=1) +
        scale_color_manual(name = "",
                           values = c("Total deaths observed"="#c10044", "Total deaths expected"="#01478c"),
                           guide = guide_legend(override.aes = list(linetype=c("solid", "dashed")),reverse=T))
    }
    
    else if(input$noncovid_ni == TRUE & input$movingaverage_ni == TRUE){
      q +
        geom_ribbon(aes(x=week, ymin=nisra$non_covid_std_ma_lb, ymax=nisra$non_covid_std_ma_ub),
                    fill="#ff87b1", alpha=0.3) +
        geom_line(aes(week, non_covid_std_ma, colour="Non-COVID-deaths"),
                  size=1) +
        scale_color_manual(name = "",
                           values = c("Non-COVID-deaths"="#c10044", "Total deaths expected"="#01478c"),
                           guide = guide_legend(override.aes = list(linetype=c("solid", "dashed")),reverse=F))
    }
    
  }, bg="transparent")

  # # Regional graphical outputs ================================================
  # output$difference_plot <- renderPlot ({
  #     ggplot(uk) +
  #       geom_line(aes(week, difference, colour=location), size=1) +
  #       facet_wrap(~location, ncol=3) +
  #       scale_y_continuous(name="% excess of non-COVID deaths", 
  #                          minor_breaks=NULL) +
  #       scale_x_date(name="Month in 2020",
  #                    labels = date_format("%b"),
  #                    breaks = "months",
  #                    minor_breaks=NULL) +
  #       theme(
  #         panel.background = element_rect(fill="#f2f7fc", colour = "#002169",
  #                                         size=1.5, linetype = "solid"),
  #         plot.background = element_rect(fill = "transparent", colour = NA),
  #         panel.border = element_rect(colour = "black", fill=NA, size=0.5),
  #         panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
  #         panel.grid.major.x = element_blank(),
  #         strip.background = element_rect(
  #           color="#002169", fill="#002169", size=1.5, linetype="solid"
  #         ),
  #         strip.text.x = element_text(
  #           size = 10, color = "white", face = "bold"
  #         ),
  #         legend.position="none"
  #         )
  # }, bg="transparent")
  # 
}

# Run the application 
shinyApp(ui = ui, server = server)
