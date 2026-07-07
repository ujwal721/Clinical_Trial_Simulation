library(shiny)

# Browser-only educational simulator for Shinylive / GitHub Pages.
# No external APIs, tokens, or server-side packages are required.

ink <- "#16213E"
navy <- "#22365E"
blue <- "#3A75C4"
indigo <- "#5A4FCF"
cyan <- "#2C9AB7"
orange <- "#D97706"
bg <- "#EDF2F7"
card <- "#F2F5F9"
muted <- "#607086"

boin_boundaries <- function(target) {
  list(
    lambda1 = max(0, target - 0.05),
    lambda2 = min(1, target + 0.05)
  )
}

boin_decision <- function(y, n, target) {
  if (n == 0) return("Treat")
  p_hat <- y / n
  b <- boin_boundaries(target)
  if (p_hat < b$lambda1) return("Escalate")
  if (p_hat > b$lambda2) return("De-escalate")
  "Stay"
}

simulate_boin_trial <- function(true_tox, target = 0.30, cohort_size = 3, max_patients = 30) {
  k <- length(true_tox)
  n <- rep(0, k)
  y <- rep(0, k)
  eliminated <- rep(FALSE, k)
  dose <- 1
  step <- 1

  steps <- data.frame(
    Step = integer(), Dose = integer(), NewPatients = integer(), NewDLTs = integer(),
    TotalPatients = integer(), TotalDLTs = integer(), ToxicityRate = numeric(),
    Decision = character(), stringsAsFactors = FALSE
  )

  while (sum(n) < max_patients && !all(eliminated)) {
    if (eliminated[dose]) break

    treat_n <- min(cohort_size, max_patients - sum(n))
    new_dlts <- rbinom(1, treat_n, true_tox[dose])
    n[dose] <- n[dose] + treat_n
    y[dose] <- y[dose] + new_dlts

    p_hat <- y[dose] / n[dose]
    decision <- boin_decision(y[dose], n[dose], target)

    # Illustrative safety elimination rule for teaching only.
    if (n[dose] >= 3 && p_hat > target + 0.25) {
      eliminated[dose:k] <- TRUE
      decision <- "Eliminate"
    }

    steps <- rbind(
      steps,
      data.frame(
        Step = step, Dose = dose, NewPatients = treat_n, NewDLTs = new_dlts,
        TotalPatients = n[dose], TotalDLTs = y[dose], ToxicityRate = round(p_hat, 3),
        Decision = decision, stringsAsFactors = FALSE
      )
    )

    if (decision == "Escalate") {
      candidates <- which(!eliminated & seq_len(k) > dose)
      if (length(candidates)) dose <- min(candidates)
    } else if (decision %in% c("De-escalate", "Eliminate")) {
      candidates <- which(!eliminated & seq_len(k) < dose)
      dose <- if (length(candidates)) max(candidates) else 1
    }

    step <- step + 1
  }

  eligible <- which(!eliminated & n > 0)
  mtd <- if (!length(eligible)) NA_integer_ else {
    eligible[which.min(abs((y[eligible] / n[eligible]) - target))]
  }

  list(n = n, y = y, mtd = mtd, eliminated = eliminated, steps = steps)
}

simulate_3plus3_trial <- function(true_tox, max_patients = 30) {
  k <- length(true_tox)
  n <- rep(0, k)
  y <- rep(0, k)
  dose <- 1
  prev_safe <- NA_integer_
  step <- 1

  steps <- data.frame(
    Step = integer(), Dose = integer(), NewPatients = integer(), NewDLTs = integer(),
    TotalPatients = integer(), TotalDLTs = integer(), ToxicityRate = numeric(),
    Decision = character(), stringsAsFactors = FALSE
  )

  repeat {
    if (dose > k || sum(n) >= max_patients) break

    treat_n <- min(3, max_patients - sum(n))
    new_dlts <- rbinom(1, treat_n, true_tox[dose])
    n[dose] <- n[dose] + treat_n
    y[dose] <- y[dose] + new_dlts

    total_n <- n[dose]
    total_y <- y[dose]
    p_hat <- total_y / total_n

    if (total_n == 3) {
      if (total_y == 0) {
        decision <- "Escalate"
        prev_safe <- dose
        next_dose <- dose + 1
      } else if (total_y == 1) {
        decision <- "Expand"
        next_dose <- dose
      } else {
        decision <- "Stop"
        next_dose <- dose
      }
    } else {
      if (total_y <= 1) {
        decision <- "Escalate"
        prev_safe <- dose
        next_dose <- dose + 1
      } else {
        decision <- "Stop"
        next_dose <- dose
      }
    }

    steps <- rbind(
      steps,
      data.frame(
        Step = step, Dose = dose, NewPatients = treat_n, NewDLTs = new_dlts,
        TotalPatients = total_n, TotalDLTs = total_y, ToxicityRate = round(p_hat, 3),
        Decision = decision, stringsAsFactors = FALSE
      )
    )

    if (decision == "Stop") break
    dose <- next_dose
    step <- step + 1
  }

  mtd <- if (is.na(prev_safe)) 1L else min(prev_safe, k)
  list(n = n, y = y, mtd = mtd, steps = steps)
}

run_comparison <- function(nsim, true_tox, target, cohort_size, max_patients) {
  k <- length(true_tox)
  true_mtd <- which.min(abs(true_tox - target))
  boin_sel <- plus_sel <- rep(NA_integer_, nsim)
  boin_alloc <- plus_alloc <- matrix(0, nrow = nsim, ncol = k)
  boin_over <- plus_over <- numeric(nsim)

  for (i in seq_len(nsim)) {
    b <- simulate_boin_trial(true_tox, target, cohort_size, max_patients)
    p <- simulate_3plus3_trial(true_tox, max_patients)
    boin_sel[i] <- b$mtd
    plus_sel[i] <- p$mtd
    boin_alloc[i, ] <- b$n
    plus_alloc[i, ] <- p$n
    boin_over[i] <- sum(b$n[true_tox > target])
    plus_over[i] <- sum(p$n[true_tox > target])
  }

  summary <- data.frame(
    Design = c("BOIN-style interval", "3+3"),
    `True MTD` = c(true_mtd, true_mtd),
    `Correct selection (%)` = round(c(mean(boin_sel == true_mtd, na.rm = TRUE), mean(plus_sel == true_mtd, na.rm = TRUE)) * 100, 1),
    `Avg. overdose patients` = round(c(mean(boin_over), mean(plus_over)), 2),
    `Avg. total patients` = round(c(mean(rowSums(boin_alloc)), mean(rowSums(plus_alloc))), 2),
    check.names = FALSE
  )

  list(
    summary = summary,
    boin_alloc = colMeans(boin_alloc),
    plus_alloc = colMeans(plus_alloc),
    true_mtd = true_mtd
  )
}

ui <- fluidPage(
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML(paste0("
      :root { color-scheme: light; }
      body { background: ", bg, "; color: ", ink, "; font-family: Inter, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif; }
      .page-shell { max-width: 1380px; margin: 18px auto; padding: 18px; }
      .topbar, .soft-card, .side-card { background: ", card, "; border-radius: 24px; box-shadow: 12px 12px 26px #cbd3df, -12px -12px 26px #ffffff; }
      .topbar { padding: 22px 26px; margin-bottom: 20px; }
      .eyebrow { color: ", indigo, "; letter-spacing: .11em; text-transform: uppercase; font-size: 11px; font-weight: 800; margin-bottom: 6px; }
      h1 { margin: 0; font-size: clamp(27px, 4vw, 40px); letter-spacing: -.04em; }
      .subhead { margin: 7px 0 0; color: ", muted, "; font-size: 15px; max-width: 830px; line-height: 1.55; }
      .side-card { padding: 20px; margin-bottom: 20px; }
      .side-card h2 { font-size: 17px; margin: 0 0 4px; }
      .side-note { color: ", muted, "; font-size: 13px; line-height: 1.5; margin-bottom: 15px; }
      .soft-card { padding: 20px; margin-bottom: 20px; }
      .metric { padding: 16px; text-align: center; min-height: 112px; }
      .metric .label { color: ", muted, "; font-size: 12px; font-weight: 700; text-transform: uppercase; letter-spacing: .05em; }
      .metric .value { color: ", navy, "; font-size: 28px; font-weight: 800; margin-top: 7px; }
      .section-title { font-size: 18px; font-weight: 800; margin: 0 0 12px; }
      .disclaimer { background: #FFF8E7; color: #744B00; border-radius: 16px; padding: 13px 15px; font-size: 13px; line-height: 1.5; margin-top: 15px; }
      .nav-tabs { border: 0; margin-bottom: 16px; }
      .nav-tabs > li > a { border: 0 !important; border-radius: 14px; color: ", navy, "; background: #e6ebf2; margin-right: 8px; font-weight: 750; }
      .nav-tabs > li.active > a, .nav-tabs > li.active > a:hover, .nav-tabs > li.active > a:focus { background: ", indigo, " !important; color: white !important; }
      .btn-primary { background: ", indigo, " !important; border: 0 !important; border-radius: 13px !important; font-weight: 800 !important; box-shadow: 4px 4px 10px #c0c8d5, -4px -4px 10px #fff !important; }
      .form-group { margin-bottom: 14px; }
      .control-label { color: ", navy, "; font-weight: 750; font-size: 13px; }
      .irs--shiny .irs-bar, .irs--shiny .irs-single, .irs--shiny .irs-from, .irs--shiny .irs-to { background: ", indigo, "; border-color: ", indigo, "; }
      .irs--shiny .irs-handle { border-color: ", indigo, "; }
      table { width: 100%; font-size: 13px; } th { background: #e6ebf2; } th, td { padding: 8px; border-bottom: 1px solid #d9e0e9; text-align: left; }
      @media(max-width: 767px) { .page-shell { margin: 0; padding: 12px; } .topbar, .side-card, .soft-card { border-radius: 18px; } }
    "))
  ),
  div(class = "page-shell",
      div(class = "topbar",
          div(class = "eyebrow", "Browser-only · GitHub Pages · Shinylive"),
          tags$h1("Phase I Dose-Finding Simulator"),
          div(class = "subhead", "Explore an educational BOIN-style interval simulation alongside the traditional 3+3 approach. All calculations run in your browser; no data are sent to a server.")
      ),
      fluidRow(
        column(3,
          div(class = "side-card",
              tags$h2("Scenario controls"),
              div(class = "side-note", "Set the target toxicity, scenario, and simulation size. Then run a single trial or repeated comparison."),
              actionButton("run_trial", "Run one BOIN-style trial", class = "btn-primary"), tags$br(), tags$br(),
              actionButton("run_compare", "Run comparison study", class = "btn-primary"), tags$hr(),
              sliderInput("target", "Target toxicity", min = 0.10, max = 0.50, value = 0.30, step = 0.01),
              sliderInput("n_dose", "Number of dose levels", min = 3, max = 8, value = 5, step = 1),
              sliderInput("cohort", "BOIN-style cohort size", min = 1, max = 6, value = 3, step = 1),
              sliderInput("maxp", "Maximum patients", min = 12, max = 54, value = 30, step = 3),
              sliderInput("nsim", "Simulation runs", min = 100, max = 1000, value = 300, step = 100),
              uiOutput("dose_sliders"),
              div(class = "disclaimer", tags$b("Educational use only. "), "This simplified simulator is not protocol-ready clinical decision software and must not be used for patient-specific treatment decisions.")
          )
        ),
        column(9,
          fluidRow(
            column(3, div(class = "soft-card metric", div(class = "label", "Lower interval"), div(class = "value", textOutput("lambda1")))),
            column(3, div(class = "soft-card metric", div(class = "label", "Upper interval"), div(class = "value", textOutput("lambda2")))),
            column(3, div(class = "soft-card metric", div(class = "label", "True MTD"), div(class = "value", textOutput("true_mtd")))),
            column(3, div(class = "soft-card metric", div(class = "label", "Dose levels"), div(class = "value", textOutput("dose_ct"))))
          ),
          tabsetPanel(
            tabPanel("Single trial", tags$br(),
              fluidRow(
                column(7, div(class = "soft-card", div(class = "section-title", "BOIN-style dose path"), plotOutput("path_plot", height = "330px"))),
                column(5, div(class = "soft-card", div(class = "section-title", "True toxicity scenario"), plotOutput("tox_plot", height = "330px")))
              ),
              div(class = "soft-card", div(class = "section-title", "Allocation by dose"), plotOutput("alloc_plot", height = "350px")),
              div(class = "soft-card", div(class = "section-title", "Step-by-step decisions"), tableOutput("trial_table"))
            ),
            tabPanel("BOIN-style vs 3+3", tags$br(),
              fluidRow(
                column(7, div(class = "soft-card", div(class = "section-title", "Average patient allocation"), plotOutput("compare_plot", height = "360px"))),
                column(5, div(class = "soft-card", div(class = "section-title", "Simulation summary"), tableOutput("summary_table")))
              ),
              div(class = "soft-card", div(class = "section-title", "Interpretation note"),
                  tags$p("The comparison summarizes simulation behavior under the selected toxicity scenario. Selection and overdose metrics should be interpreted as educational outputs of this simplified model, not a validated design recommendation."))
            ),
            tabPanel("About", tags$br(),
              div(class = "soft-card",
                div(class = "section-title", "Why this runs on GitHub Pages"),
                tags$p("This version uses only Shiny and base R graphics so it can run locally in the visitor's browser through Shinylive. It intentionally excludes the Gemini API, Plotly, and DT dependencies used in the server-hosted version."),
                tags$ul(tags$li("No API key is embedded or required."), tags$li("No participant or visitor data are transmitted by the app."), tags$li("The browser may take a moment to load R/WebAssembly on first visit."))
              )
            )
          )
        )
      )
  )
)

server <- function(input, output, session) {
  output$dose_sliders <- renderUI({
    k <- input$n_dose
    defaults <- round(seq(0.05, 0.45, length.out = k), 2)
    tagList(lapply(seq_len(k), function(i) {
      sliderInput(paste0("dose", i), paste("Dose", i, "true toxicity"), min = 0.01, max = 0.80, value = defaults[i], step = 0.01)
    }))
  })

  true_tox <- reactive({
    k <- input$n_dose
    values <- vapply(seq_len(k), function(i) {
      x <- input[[paste0("dose", i)]]
      if (is.null(x)) NA_real_ else as.numeric(x)
    }, numeric(1))
    defaults <- seq(0.05, 0.45, length.out = k)
    values[is.na(values)] <- defaults[is.na(values)]
    values
  })

  output$lambda1 <- renderText(sprintf("%.2f", boin_boundaries(input$target)$lambda1))
  output$lambda2 <- renderText(sprintf("%.2f", boin_boundaries(input$target)$lambda2))
  output$true_mtd <- renderText(which.min(abs(true_tox() - input$target)))
  output$dose_ct <- renderText(input$n_dose)

  trial_res <- eventReactive(input$run_trial, {
    simulate_boin_trial(true_tox(), input$target, input$cohort, input$maxp)
  }, ignoreNULL = FALSE)

  compare_res <- eventReactive(input$run_compare, {
    run_comparison(input$nsim, true_tox(), input$target, input$cohort, input$maxp)
  }, ignoreNULL = FALSE)

  output$tox_plot <- renderPlot({
    vals <- true_tox()
    ylim_top <- max(0.60, max(vals) + 0.08)
    plot(seq_along(vals), vals, type = "b", pch = 19, lwd = 3, col = cyan,
         ylim = c(0, ylim_top), xlab = "Dose level", ylab = "True toxicity probability", xaxt = "n")
    axis(1, at = seq_along(vals), labels = paste("Dose", seq_along(vals)))
    abline(h = input$target, lty = 2, lwd = 2, col = orange)
    legend("topleft", legend = c("True toxicity", "Target"), col = c(cyan, orange), lty = c(1, 2), pch = c(19, NA), bty = "n", cex = 0.85)
  })

  output$path_plot <- renderPlot({
    tr <- trial_res()
    if (!nrow(tr$steps)) return(invisible())
    with(tr$steps, {
      plot(Step, Dose, type = "b", pch = 19, lwd = 3, col = indigo,
           ylim = c(1, input$n_dose), xlab = "Trial step", ylab = "Dose level", yaxt = "n")
      axis(2, at = seq_len(input$n_dose), labels = paste("Dose", seq_len(input$n_dose)))
      text(Step, Dose, labels = Decision, pos = 3, cex = 0.72, col = muted)
    })
  })

  output$alloc_plot <- renderPlot({
    tr <- trial_res()
    labels <- paste0("D", seq_along(tr$n), "\nDLT=", tr$y)
    mids <- barplot(tr$n, names.arg = labels, col = blue, border = NA, ylim = c(0, max(1, max(tr$n) + 4)),
                    xlab = "Dose level and observed DLTs", ylab = "Patients treated")
    text(mids, tr$n, labels = tr$n, pos = 3, cex = 0.9, col = ink)
  })

  output$trial_table <- renderTable({
    tr <- trial_res()$steps
    tr$ToxicityRate <- sprintf("%.3f", tr$ToxicityRate)
    tr
  }, striped = TRUE, bordered = FALSE, spacing = "s")

  output$compare_plot <- renderPlot({
    res <- compare_res()
    k <- length(res$boin_alloc)
    ymax <- max(c(res$boin_alloc, res$plus_alloc, 1)) * 1.15
    plot(seq_len(k), res$boin_alloc, type = "b", pch = 19, lwd = 3, col = indigo,
         ylim = c(0, ymax), xlab = "Dose level", ylab = "Average patients", xaxt = "n")
    axis(1, at = seq_len(k), labels = paste("Dose", seq_len(k)))
    lines(seq_len(k), res$plus_alloc, type = "b", pch = 17, lwd = 3, col = cyan)
    abline(v = res$true_mtd, lty = 3, col = orange, lwd = 2)
    legend("topright", legend = c("BOIN-style interval", "3+3", "True MTD"), col = c(indigo, cyan, orange),
           lty = c(1, 1, 3), pch = c(19, 17, NA), bty = "n", cex = 0.88)
  })

  output$summary_table <- renderTable({
    compare_res()$summary
  }, striped = TRUE, bordered = FALSE, spacing = "s")
}

shinyApp(ui, server)
