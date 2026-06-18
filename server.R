library(shiny)
library(DT)
library(BOP2basket)

shinyServer(function(input, output, session) {

  # ============================================================
  # ---- TRIAL DESIGN TAB: dynamic inputs ----
  # ============================================================

  # Build one numeric input per basket, per look, for cohort sizes
  output$cohortsize_ui <- renderUI({
    nb <- input$num_baskets
    nl <- input$num_looks
    if (is.null(nb) || is.null(nl) || nb < 1 || nl < 1) return(NULL)

    tagList(
      h5("Cohort Size per Look (rows) x per Basket (cols)"),
      lapply(1:nl, function(look) {
        fluidRow(
          column(2, p(paste("Look", look))),
          lapply(1:nb, function(basket) {
            column(
              floor(10 / nb),
              numericInput(
                inputId = paste0("cohort_", look, "_", basket),
                label = paste("Basket", basket),
                value = 5, min = 1, step = 1
              )
            )
          })
        )
      })
    )
  })

  # Build one numeric input per basket for true response rate
  output$p_true_ui <- renderUI({
    nb <- input$num_baskets
    if (is.null(nb) || nb < 1) return(NULL)
    tagList(
      h5("True / Alternative Response Rate per Basket (p.true)"),
      fluidRow(
        lapply(1:nb, function(basket) {
          column(
            floor(12 / nb),
            numericInput(
              inputId = paste0("ptrue_", basket),
              label = paste("Basket", basket),
              value = 0.3, min = 0, max = 1, step = 0.01
            )
          )
        })
      )
    )
  })

  # Build one numeric input per basket for null response rate
  output$p_null_ui <- renderUI({
    nb <- input$num_baskets
    if (is.null(nb) || nb < 1) return(NULL)
    tagList(
      h5("Null Response Rate per Basket (p.null)"),
      fluidRow(
        lapply(1:nb, function(basket) {
          column(
            floor(12 / nb),
            numericInput(
              inputId = paste0("pnull_", basket),
              label = paste("Basket", basket),
              value = 0.2, min = 0, max = 1, step = 0.01
            )
          )
        })
      )
    )
  })

  # Helper: assemble the cohortsize matrix from the dynamic inputs above
  build_cohortsize_matrix <- function(nb, nl) {
    mat <- matrix(NA, nrow = nl, ncol = nb)
    for (look in 1:nl) {
      for (basket in 1:nb) {
        val <- input[[paste0("cohort_", look, "_", basket)]]
        mat[look, basket] <- if (is.null(val)) NA else val
      }
    }
    mat
  }

  build_vector <- function(prefix, nb) {
    vals <- sapply(1:nb, function(basket) {
      v <- input[[paste0(prefix, "_", basket)]]
      if (is.null(v)) NA else v
    })
    vals
  }

  # ============================================================
  # ---- TRIAL DESIGN TAB: run design() ----
  # ============================================================

  design_result <- eventReactive(input$run_design, {

    nb <- input$num_baskets
    nl <- input$num_looks

    cohortsize <- build_cohortsize_matrix(nb, nl)
    p_true <- build_vector("ptrue", nb)
    p_null <- build_vector("pnull", nb)

    # Basic validation before calling the (slow) simulation
    validate(
      need(!any(is.na(cohortsize)), "All cohort size fields must be filled in."),
      need(!any(is.na(p_true)), "All p.true fields must be filled in."),
      need(!any(is.na(p_null)), "All p.null fields must be filled in."),
      need(input$num_baskets >= 2, "Need at least 2 baskets."),
      need(input$ntrial >= 1, "Number of trials must be positive.")
    )

    withProgress(message = "Running BOP2-basket simulation...", value = 0.3, {

      result <- design(
        cohortsize = cohortsize,
        ntype       = nb,
        p.true      = p_true,
        p.null      = p_null,
        ntrial      = input$ntrial,
        mu.par      = input$mu_par,
        v           = input$v_par,
        a           = input$a_par,
        b           = input$b_par,
        lam         = input$lam_par,
        gam         = input$gam_par,
        type1       = input$type1_par,
        method      = input$design_method
      )

      incProgress(0.7)
      result
    })
  })

  output$design_status <- renderUI({
    if (input$run_design == 0) {
      return(p("Set your parameters, then click 'Run Simulation'."))
    }
    res <- tryCatch(design_result(), error = function(e) e)
    if (inherits(res, "error")) {
      return(div(style = "color:red;", paste("Error:", res$message)))
    }
    div(style = "color:green;", "Simulation complete.")
  })

  output$oc_summary_table <- renderTable({
    res <- tryCatch(design_result(), error = function(e) NULL)
    if (is.null(res)) return(NULL)
    data.frame(
      Metric = c("Type I Error", "FWER", "Power", "Avg. Total Sample Size"),
      Value  = c(res$type1.error, res$FWER, res$power, res$sp.est)
    )
  })

  output$eff_table <- renderTable({
    res <- tryCatch(design_result(), error = function(e) NULL)
    if (is.null(res)) return(NULL)
    data.frame(
      Basket = paste("Basket", seq_along(res$eff.est)),
      `P(Claim Efficacy)` = res$eff.est,
      check.names = FALSE
    )
  })

  output$sp_basket_table <- renderTable({
    res <- tryCatch(design_result(), error = function(e) NULL)
    if (is.null(res)) return(NULL)
    data.frame(
      Basket = paste("Basket", seq_along(res$sp.basket)),
      `Avg. Sample Size` = res$sp.basket,
      check.names = FALSE
    )
  })

  output$terminate_table <- renderTable({
    res <- tryCatch(design_result(), error = function(e) NULL)
    if (is.null(res)) return(NULL)
    term_rate <- colMeans(res$terminate)
    data.frame(
      Basket = paste("Basket", seq_along(term_rate)),
      `Early Termination Rate` = term_rate,
      check.names = FALSE
    )
  })

  # ============================================================
  # ---- CALIBRATION TAB: dynamic inputs ----
  # ============================================================

  output$cal_cohortsize_ui <- renderUI({
    nb <- input$cal_ntype
    if (is.null(nb) || nb < 1) return(NULL)
    tagList(
      h5("Cohort Size per Basket (single look, comma-separated not needed)"),
      fluidRow(
        lapply(1:nb, function(basket) {
          column(
            floor(12 / nb),
            numericInput(
              inputId = paste0("cal_cohort_", basket),
              label = paste("Basket", basket),
              value = 5, min = 1, step = 1
            )
          )
        })
      )
    )
  })

  output$cal_p0_ui <- renderUI({
    nb <- input$cal_ntype
    if (is.null(nb) || nb < 1) return(NULL)
    tagList(
      h5("Null Response Rate per Basket (p0)"),
      fluidRow(
        lapply(1:nb, function(basket) {
          column(
            floor(12 / nb),
            numericInput(
              inputId = paste0("cal_p0_", basket),
              label = paste("Basket", basket),
              value = 0.2, min = 0, max = 1, step = 0.01
            )
          )
        })
      )
    )
  })

  output$cal_p1_ui <- renderUI({
    nb <- input$cal_ntype
    if (is.null(nb) || nb < 1) return(NULL)
    tagList(
      h5("Alternative Response Rate per Basket (p1)"),
      fluidRow(
        lapply(1:nb, function(basket) {
          column(
            floor(12 / nb),
            numericInput(
              inputId = paste0("cal_p1_", basket),
              label = paste("Basket", basket),
              value = 0.4, min = 0, max = 1, step = 0.01
            )
          )
        })
      )
    )
  })

  # ============================================================
  # ---- CALIBRATION TAB: run decidePar() ----
  # ============================================================

  calibration_result <- eventReactive(input$run_calibration, {

    nb <- input$cal_ntype

    cohort <- sapply(1:nb, function(b) {
      v <- input[[paste0("cal_cohort_", b)]]
      if (is.null(v)) NA else v
    })
    p0 <- sapply(1:nb, function(b) {
      v <- input[[paste0("cal_p0_", b)]]
      if (is.null(v)) NA else v
    })
    p1 <- sapply(1:nb, function(b) {
      v <- input[[paste0("cal_p1_", b)]]
      if (is.null(v)) NA else v
    })

    validate(
      need(!any(is.na(cohort)), "All cohort size fields must be filled in."),
      need(!any(is.na(p0)), "All p0 fields must be filled in."),
      need(!any(is.na(p1)), "All p1 fields must be filled in.")
    )

    # decidePar expects cohortsize as a matrix (rows = looks); single look here
    cohort_matrix <- matrix(cohort, nrow = 1)

    withProgress(message = "Calibrating (a, b)...", value = 0.3, {
      result <- decidePar(
        cohortsize = cohort_matrix,
        ntype       = nb,
        ntrial      = input$cal_ntrial,
        p0          = p0,
        p1          = p1,
        var.small   = input$var_small,
        var.big     = input$var_big
      )
      incProgress(0.7)
      result
    })
  })

  output$calibration_result <- renderPrint({
    if (input$run_calibration == 0) {
      cat("Click 'Calibrate (a, b)' to compute values.")
      return(invisible(NULL))
    }
    res <- tryCatch(calibration_result(), error = function(e) e)
    if (inherits(res, "error")) {
      cat("Error:", res$message)
      return(invisible(NULL))
    }
    cat("a =", res$a, "\n")
    cat("b =", res$b, "\n")
  })

})
