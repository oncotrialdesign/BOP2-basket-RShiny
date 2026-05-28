library(shiny)
library(DT)

shinyUI(
  navbarPage(
    title = "BOP2-basket Trial Design",
    
    tabPanel("Home",
             h2("Bayesian Optimal Phase II (BOP2) Design for Basket Trials"),
             p("Basket trials test a single drug across multiple tumor subtypes 
        simultaneously. This app implements the BOP2-basket design 
        (He, Ren & Zhou, 2025), which uses Bayesian Hierarchical Models 
        to borrow information across baskets while controlling the 
        family-wise error rate (FWER)."),
             h4("Reference:"),
             p("He T, Ren Y, Zhou H. Bayesian optimal phase II (BOP2) design 
        for basket trials. Stat Biopharm Res. 2025;1-10.")
    ),
    
    tabPanel("Trial Design",
             sidebarLayout(
               sidebarPanel(
                 h4("Trial Parameters"),
                 numericInput("p0", "Null Response Rate (p0)", 
                              value = 0.15, min = 0, max = 1, step = 0.05),
                 numericInput("p1", "Target Response Rate (p1)", 
                              value = 0.35, min = 0, max = 1, step = 0.05),
                 numericInput("num_baskets", "Number of Baskets", 
                              value = 5, min = 2, max = 20, step = 1),
                 numericInput("n_per_basket", "Sample Size per Basket", 
                              value = 20, min = 5, max = 100, step = 5),
                 numericInput("num_interim", "Number of Interim Analyses", 
                              value = 2, min = 1, max = 5, step = 1),
                 numericInput("fwer", "FWER Control Level", 
                              value = 0.10, min = 0.01, max = 0.20, step = 0.01),
                 numericInput("nsim", "Number of Simulations", 
                              value = 1000, min = 100, max = 10000, step = 100),
                 selectInput("design_method", "Design Method",
                             choices = c("Independent", "BHM", "CBHM"),
                             selected = "BHM"),
                 actionButton("run", "Run Simulation", 
                              class = "btn-primary", width = "100%")
               ),
               mainPanel(
                 h4("Results will appear here after running simulation."),
                 DTOutput("results_table")
               )
             )
    ),
    
    tabPanel("Operating Characteristics",
             h4("Power and Error Summary"),
             DTOutput("oc_table")
    )
  )
)