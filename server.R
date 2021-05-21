server <- function(input, output, session){
  
  
  output$plotresults <- renderPlot({
    
    wb <- as.numeric( final_parties %>% filter(state == input$state) %>% summarise(wb = sum(second_vote)) )
    
    # calculating total second votes for selected state
    state_results <- final_parties %>% filter(state == input$state) %>% 
      group_by(party) %>%
      summarize(Zweitstimmen = sum(second_vote) / wb * 100)
    
    # creating plot of state results
    plot_state  <- state_results %>% 
      ggplot(aes(party, Zweitstimmen, fill = party)) + 
      geom_col(color = "black") +
      geom_hline(yintercept = 5, color = "darkgrey", alpha = 0.8, size = 1) + 
      geom_text(aes(label = round(Zweitstimmen, 1)),
                position = position_dodge(0.9),
                vjust = -0.3) +
      ylim(c(0,40)) +
      scale_fill_manual(values = party_colors) +
      labs(x = element_blank(), 
           y = element_blank(),
           title = paste(input$state)) +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5))  
    
    # calculating difference between state and whole-country results 
    diff_results <- mutate(germany_results, diff_results = state_results$Zweitstimmen - germany_results$Zweitstimmen)
    
    
    plot_diff <- diff_results %>% 
      ggplot(aes(party, diff_results, fill = party)) + 
      geom_col(color = "black") +
      geom_hline(yintercept = 0, color = "darkgrey", alpha = 0.8, size = 1) + 
      geom_text(aes(label = round(diff_results, 1)), y = -15) +
      ylim(c(-20,20)) +
      scale_fill_manual(values = party_colors) +
      labs(x = element_blank(), 
           y = element_blank(),
           title = "Differenz") +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5))  
    
    # plotting all plots
    grid.arrange(plot_germany, plot_state, plot_diff)
  })
  
  output$map_first_votes <- renderPlot({ 
    
    map_first_votes + theme(plot.title = element_text(hjust = 0.5),
                            axis.line.x = element_blank(), 
                            axis.line.y = element_blank(), 
                            axis.text.x = element_blank(),
                            axis.text.y = element_blank(),
                            axis.ticks.x = element_blank(),
                            axis.ticks.y = element_blank()) +
      labs(title = "Gewonnene Direktmandate der Wahlbezirke", 
           fill = "Partei",
           x = "Zoom: Doppelklick auf markiertem Rechteck. Erneuter Doppelklick setzt Karte zurück") +
      coord_sf(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
    
    
  })
  
  ranges <- reactiveValues(x = NULL, y = NULL) #input of drawn rectangle
  
  output$plotspatial <- renderPlot({ 
    
    second_votes <- final_parties %>% 
      filter(party == input$party)
    
    mp_second_votes <- inner_join(districts, second_votes, by = c("WKR_NR" = "area_id")) %>% 
      mutate(Zweitstimmen = second_vote / overall$valid_second_votes * 100)
    
    ggplot(mp_second_votes) + geom_sf(aes(fill = Zweitstimmen), lwd = 0.1) +
      scale_fill_continuous(name = "Zweitstimmen in %", low = "#FFFFFF", high = party_colors[input$party]) +
      labs(x = "Zoom: Doppelklick auf markiertem Rechteck. Erneuter Doppelklick setzt Karte zurück",
           y = element_blank(),
           title = paste(input$party)) +
      theme(plot.title = element_text(hjust = 0.5),
            axis.line.x = element_blank(), 
            axis.line.y = element_blank(), 
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank()) +
      coord_sf(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
    
  })
  
  
  observeEvent(input$plotspatial_dblclick, {
    brush <- input$plotspatial_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
}
#shinyApp(ui, server)
