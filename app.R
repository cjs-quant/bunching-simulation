
# clear
rm(list=ls())

# load packages
library(ggplot2)
library(shiny)

# source helper functions
source("bunching_data_gen.R")
source("bunching_bins.R")
source("bunching_optimal_bw.R")
source("bunching_estimation.R")

# create ui
ui = fluidPage(
  
  # app title
  headerPanel("Bunching Simulation"),
  
  # sidebar panel for inputs
  sidebarPanel(sliderInput("bunch_point", "Set Bunch Point:", min=10, max=60, value=20, step=0.1),
               sliderInput("n_bunch", "# of Bunchers:", min=0, max=2000, value=500, step=1),
               sliderInput("variance_bunch", "Variance of Bunchers:", min=0.1, max=0.5, value=0.15, step=0.01),
               sliderInput("t_above", "Marginal Tax Rate above Bunch Point:", min=0, max=0.99, value=0.25),
               sliderInput("t_below", "Marginal Tax Rate below Bunch Point:", min=0, max=0.99, value=0.20),
               sliderInput("n_bins", "Number of Bins:", min=50, max=150, value=100)
  ),
  
  # main panel for outputs
  mainPanel(fluidRow(
                    column(6,plotOutput("income_density")),
                    column(6,plotOutput("bunch_plot"))),
            fluidRow(align="center", tableOutput("out_table")),
            fluidRow(uiOutput("note"))
  )
  
)

# server logic to plot variables
server = function(session, input, output) {
  
  # draw data
  x_set = reactive({bunching_data_gen(input$bunch_point, input$n_bunch, input$variance_bunch)})
  
  # plot income density
  output$income_density<-renderPlot({
    ggplot(x_set()) + 
      geom_density(mapping=aes(x), fill="red", color="red", alpha=0.1) + 
      xlab("Income (in 000s)") + ylab("Density") + 
      geom_vline(xintercept = input$bunch_point)
  })
  
  # create bunching regression data
  data = reactive({bunching_bins(x_set(), input$bunch_point, input$n_bins)})
  y = reactive({data()$y})
  x = reactive({data()$x})
  
  # compute optimal bunching window
  results = reactive({bunching_optimal_bw(y(), x(), input$bunch_point, input$n_bins)})
  bin_mid = reactive({results()$bin_mid})
  bw_l = reactive({results()$bw_l})
  bw_r = reactive({results()$bw_r})
  mu = reactive({results()$mu})
  y_hat_all = reactive({results()$y_hat_all})
  ci_low_all = reactive({results()$ci_low_all})
  ci_high_all = reactive({results()$ci_high_all})
  
  # plot bunching data
  output$bunch_plot<-renderPlot({
    ggplot() + 
      geom_point(mapping = aes(x()$x_1[-c((bin_mid()-bw_l()):(bin_mid()+bw_r()))], 
                               y()$y[-c((bin_mid()-bw_l()):(bin_mid()+bw_r()))])) + 
      geom_point(mapping=aes(x()$x_1[c((bin_mid()-bw_l()):(bin_mid()+bw_r()))],
                               y()$y[c((bin_mid()-bw_l()):(bin_mid()+bw_r()))]), 
                               col=ifelse(((bw_l() == 0) & (bw_r() == 0)), "black", "red")) + 
      geom_line(mapping = aes(mu()$mu, y_hat_all())) + 
      geom_line(mapping = aes(mu()$mu, ci_low_all()), col="gray") + 
      geom_line(mapping = aes(mu()$mu, ci_high_all()), col="gray") + 
      geom_vline(xintercept = input$bunch_point, col="red") + 
      {if (bw_r() > 0) geom_vline(xintercept = max(x()$x_1[c((bin_mid()-bw_l()):(bin_mid()+bw_r()))]), 
                                  linetype="dashed", col="black")} + 
      {if (bw_l() > 0) geom_vline(xintercept = min(x()$x_1[c((bin_mid()-bw_l()):(bin_mid()+bw_r()))]), 
                                  linetype="dashed", col="black")} + 
      xlab("Income (in 000s)") + ylab("Count")
  })
  
  # compute bunching estimator
  estimates = reactive({bunching_estimation(y(), x(), bin_mid(), bw_l(), bw_r(), input$bunch_point, input$t_above, input$t_below)})
  
  # create output table
  output$out_table = renderTable({estimates()}, rownames=FALSE)
  
  # links
  url_chetty = a("Chetty (2009).", 
                 href="https://arxiv.org/pdf/1911.09511.pdf")
  url_bosch = a("Bosch et. al. (2020).", 
                href="https://arxiv.org/pdf/1911.09511.pdf")
  chris_email = a("christopher.simard@ny.frb.org,", href="christopher.simard@ny.frb.org")
  github = a("here.", href="https://github.com/csimard-econ/bunching-simulation")
  
  # create note
  output$note = renderUI({
    tagList("This simulation demonstrates the optimal bunching window selection 
            algorithm of ", url_bosch, "After selecting the optimal bunching window, 
            the (naive) bunching estimator from is computed as in ", url_chetty,
            "Simulation author: Christopher Simard, contact:", chris_email, 
            "The source code for this simulation can be found", github, 
            "The views expressed here are my own and do not necessarily 
            represent the views of the Federal Reserve Bank of New York or the Federal Reserve System.",
            tags$br(), tags$br(), "Notes: (1) The simulation assumes the smooth
            income data generating process is log-normal. (2) Bunching is simulated 
            with draws from the normal distribution with mean and variance 
            selected by the user.")
  })
  
}

# creates shinyapp object
shinyApp(ui, server)

