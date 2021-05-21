#### UI #####
ui <- navbarPage("Bundestagswahl 2017",
                 
                 tabPanel("Ergebnisse auf Bundes- und Landesebene",
                          sidebarLayout(sidebarPanel(radioButtons(inputId = 'state',  
                                                                  label = 'Bundesland', 
                                                                  choices = sort(unique(final_parties$state)), 
                                                                  #multiple = FALSE,
                                                                  selected = "Baden-Wuerttemberg"), width = 2),
                                        mainPanel(plotOutput('plotresults'), width = 4, fluid = T),
                          )
                 ),
                 
                 
                 
                 
                 
                 navbarMenu("Raeumliche Verteilung der Wahlergebnisse",
                            tabPanel("Erstimmen (Direktmandate)", plotOutput("map_first_votes",
                                                                             height = "600px",
                                                                             dblclick = "plotspatial_dblclick",
                                                                             brush = brushOpts(id = "plotspatial_brush",
                                                                                               resetOnNew = TRUE),
                                                                             hover = hoverOpts(id = "plot_hover1111"))),
                            tabPanel("Zweitstimmen", 
                                     sidebarLayout(sidebarPanel(radioButtons(inputId = 'party',  
                                                                             label = 'Partei', 
                                                                             choices = unique(final_parties$party), 
                                                                             #multiple = FALSE,
                                                                             selected = "CDU/CSU"), width = 2),
                                                   mainPanel(plotOutput('plotspatial',
                                                                        height = "600px",
                                                                        dblclick = "plotspatial_dblclick",
                                                                        brush = brushOpts(id = "plotspatial_brush",
                                                                                          resetOnNew = TRUE),
                                                                        hover = hoverOpts(id = "plot_hover")
                                                   )
                                                   )
                                                   
                                     )
                            )
                            
                 )
)
