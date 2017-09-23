function(input, output) {

  output$main_plot <- renderPlot({

    a <- c()
    for (i in 1:500)
      a[i] = mean(runif(300, min =0, max = 1 ))
    hist(a,
         probability = TRUE,
         breaks = as.numeric(input$n_breaks),
         xlab = "Duration (minutes)",
         main = "Geyser eruption duration")

    if (input$individual_obs) {
      rug(faithful$eruptions)
    }

    if (input$density) {
      dens <- density(a,
                      adjust = input$bw_adjust)
      lines(dens, col = "blue")
    }

  })
}
