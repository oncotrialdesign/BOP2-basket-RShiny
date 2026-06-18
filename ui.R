library(shiny)
library(DT)

shinyUI(
  navbarPage(
    title = "BOP2-basket Trial Design",

    # ---- Home tab ----
    tabPanel("Home",
             h2("Bayesian Optimal Phase II (BOP2) Design for Basket Trials"),
             p("Basket trials test a single drug across multiple tumor subtypes
                simultaneously. This app implements the BOP2-basket design
                (He, Ren & Zhou, 2025), which uses Bayesian Hierarchical Models
                (BHM) or Calibrated BHM (CBHM) to borrow information across
                baskets while controlling the family-wise error rate (FWER)."),
             h4("How to use this app:"),
             tags$ol(
               tags$li("Set the number of baskets and interim looks."),
               tags$li("Enter the cohort size, null response rate, and target
                        response rate for each basket."),
               tags$li("Choose BHM or CBHM. If CBHM, calibrate (a, b) first using
                        the Calibration tab, or enter (a, b) directly under BHM."),
               tags$li("Set lam/gam (interim decision tuning parameters) and
                        run the simulation.")
             ),
             h4("Reference:"),
             p("He T, Ren Y, Zhou H. Bayesian optimal phase II (BOP2) design
                for basket trials. Stat Biopharm Res. 2025;1-10.")
    ),

    # ---- Calibration tab (decidePar) ----
    tabPanel("Calibration (CBHM)",
             sidebarLayout(
               sidebarPanel(
                 h4("Calibrate CBHM variance parameters (a, b)"),
                 p("Only needed if you plan to use the CBHM method on the
                    Trial Design tab. Computes (a, b) linking between-basket
                    variance to a homogeneity statistic T."),
                 numericInput("cal_ntype", "Number of Baskets",
                              value = 3, min = 2, max = 20, step = 1),
                 numericInput("cal_ntrial", "Number of Calibration Trials",
                              value = 1000, min = 100, max = 50000, step = 100),
                 uiOutput("cal_cohortsize_ui"),
                 uiOutput("cal_p0_ui"),
                 uiOutput("cal_p1_ui"),
                 numericInput("var_small", "Small Variance Target",
                              value = 0.1, min = 0, step = 0.01),
                 numericInput("var_big", "Large Variance Target",
                              value = 1, min = 0, step = 0.01),
                 actionButton("run_calibration", "Calibrate (a, b)",
                              class = "btn-primary", width = "100%")
               ),
               mainPanel(
                 h4("Calibration result"),
                 verbatimTextOutput("calibration_result"),
                 helpText("Copy the resulting a and b values into the Trial
                           Design tab if using CBHM.")
               )
             )
    ),

    # ---- Trial Design tab (design) ----
    tabPanel("Trial Design",
             sidebarLayout(
               sidebarPanel(
                 h4("Trial Structure"),
                 numericInput("num_baskets", "Number of Baskets",
                              value = 3, min = 2, max = 20, step = 1),
                 numericInput("num_looks", "Number of Interim Looks",
                              value = 2, min = 1, max = 6, step = 1),

                 uiOutput("cohortsize_ui"),
                 uiOutput("p_true_ui"),
                 uiOutput("p_null_ui"),

                 hr(),
                 h4("Bayesian Model Settings"),
                 selectInput("design_method", "Design Method",
                             choices = c("BHM", "CBHM"),
                             selected = "BHM"),
                 numericInput("mu_par", "Prior Mean for Common Effect (mu.par, log-odds scale)",
                              value = 0, step = 0.1),
                 numericInput("v_par", "Prior Precision for mu (v)",
                              value = 0.01, min = 0, step = 0.001),
                 numericInput("a_par", "Variance Hyperparameter (a)",
                              value = 1, step = 0.1),
                 numericInput("b_par", "Variance Hyperparameter (b)",
                              value = 1, step = 0.1),

                 hr(),
                 h4("Interim Decision Rules"),
                 numericInput("lam_par", "Lambda (lam) - futility/efficacy tuning",
                              value = 0.95, min = 0, max = 1, step = 0.01),
                 numericInput("gam_par", "Gamma (gam) - futility/efficacy tuning",
                              value = 0.9, min = 0, max = 1, step = 0.01),
                 numericInput("type1_par", "Target Type I Error / FWER",
                              value = 0.1, min = 0.01, max = 0.5, step = 0.01),

                 hr(),
                 numericInput("ntrial", "Number of Simulated Trials",
                              value = 5000, min = 100, max = 50000, step = 100),

                 actionButton("run_design", "Run Simulation",
                              class = "btn-success", width = "100%")
               ),

               mainPanel(
                 h4("Operating Characteristics"),
                 uiOutput("design_status"),
                 tableOutput("oc_summary_table"),
                 h4("Probability of Claiming Efficacy by Basket"),
                 tableOutput("eff_table"),
                 h4("Average Sample Size by Basket"),
                 tableOutput("sp_basket_table"),
                 h4("Early Termination Rate by Basket"),
                 tableOutput("terminate_table")
               )
             )
    )
  )
)
