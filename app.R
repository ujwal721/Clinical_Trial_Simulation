library(shiny)
library(ggplot2)
library(DT)
library(httr2)
library(jsonlite)
library(plotly)
nyu_purple <- "#57068C"
nyu_lavender <- "#B58AE6"
teal_blue <- "#14B8A6"
soft_teal <- "#99F6E4"
warm_orange <- "#F59E0B"
card_bg <- "#FFFFFF"
text_dark <- "#1F1F29"
muted <- "#6B7280"
border_col <- "#ECE7F2"
bg_main <- "#F4F1F8"
bg_shell <- "#EEF1F7"
gemini_enabled <- function() {
  nzchar(Sys.getenv("GEMINI_API_KEY", unset = ""))
}

call_gemini <- function(prompt_text, model = "gemini-2.5-flash") {
  api_key <- Sys.getenv("GEMINI_API_KEY", unset = "")

  if (!nzchar(api_key)) {
    return(
      paste(
        "AI Assistant is disabled in this public demo.",
        "The BOIN and 3+3 simulation tools remain fully available.",
        "For private local testing, define GEMINI_API_KEY in your user-level .Renviron file and restart RStudio."
      )
    )
  }

  url <- paste0(
    "https://generativelanguage.googleapis.com/v1beta/models/",
    model,
    ":generateContent"
  )

  body <- list(
    contents = list(
      list(parts = list(list(text = prompt_text)))
    )
  )

  safe_request <- function() {
    req <- request(url) |>
      req_headers(
        "x-goog-api-key" = api_key,
        "Content-Type" = "application/json"
      ) |>
      req_body_json(body) |>
      req_perform()

    res <- resp_body_json(req)
    res$candidates[[1]]$content$parts[[1]]$text
  }

  tryCatch(
    safe_request(),
    error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("503", msg, fixed = TRUE)) {
        Sys.sleep(2)
        tryCatch(
          safe_request(),
          error = function(e2) "Gemini is temporarily busy. Please try again in a few seconds."
        )
      } else {
        paste("Gemini API call failed:", msg)
      }
    }
  )
}

boin_boundaries <- function(target) {
  lambda1 <- max(0, target - 0.05)
  lambda2 <- min(1, target + 0.05)
  list(lambda1 = lambda1, lambda2 = lambda2)
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
    Step = integer(),
    Dose = integer(),
    NewPatients = integer(),
    NewDLTs = integer(),
    TotalPatients = integer(),
    TotalDLTs = integer(),
    ToxicityRate = numeric(),
    Decision = character(),
    stringsAsFactors = FALSE
  )
  
  while (sum(n) < max_patients) {
    if (eliminated[dose]) break
    
    treat_n <- min(cohort_size, max_patients - sum(n))
    new_dlts <- rbinom(1, treat_n, true_tox[dose])
    
    n[dose] <- n[dose] + treat_n
    y[dose] <- y[dose] + new_dlts
    
    p_hat <- y[dose] / n[dose]
    decision <- boin_decision(y[dose], n[dose], target)
    
    if (n[dose] >= 3 && p_hat > target + 0.25) {
      eliminated[dose:k] <- TRUE
      decision <- "Eliminate"
    }
    
    steps <- rbind(
      steps,
      data.frame(
        Step = step,
        Dose = dose,
        NewPatients = treat_n,
        NewDLTs = new_dlts,
        TotalPatients = n[dose],
        TotalDLTs = y[dose],
        ToxicityRate = round(p_hat, 3),
        Decision = decision,
        stringsAsFactors = FALSE
      )
    )
    
    if (decision == "Escalate") {
      cand <- which(!eliminated & seq_len(k) > dose)
      if (length(cand) > 0) dose <- min(cand)
    } else if (decision %in% c("De-escalate", "Eliminate")) {
      cand <- which(!eliminated & seq_len(k) < dose)
      if (length(cand) > 0) dose <- max(cand) else dose <- 1
    }
    
    step <- step + 1
    if (all(eliminated)) break
  }
  
  eligible <- which(!eliminated & n > 0)
  if (length(eligible) == 0) {
    mtd <- NA
  } else {
    phat <- y[eligible] / n[eligible]
    mtd <- eligible[which.min(abs(phat - target))]
  }
  
  list(n = n, y = y, mtd = mtd, eliminated = eliminated, steps = steps)
}

simulate_3plus3_trial <- function(true_tox, max_patients = 30) {
  k <- length(true_tox)
  n <- rep(0, k)
  y <- rep(0, k)
  dose <- 1
  prev_safe <- NA
  step <- 1
  
  steps <- data.frame(
    Step = integer(),
    Dose = integer(),
    NewPatients = integer(),
    NewDLTs = integer(),
    TotalPatients = integer(),
    TotalDLTs = integer(),
    ToxicityRate = numeric(),
    Decision = character(),
    stringsAsFactors = FALSE
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
        Step = step,
        Dose = dose,
        NewPatients = treat_n,
        NewDLTs = new_dlts,
        TotalPatients = total_n,
        TotalDLTs = total_y,
        ToxicityRate = round(p_hat, 3),
        Decision = decision,
        stringsAsFactors = FALSE
      )
    )
    
    if (decision == "Stop") break
    dose <- next_dose
    step <- step + 1
  }
  
  if (is.na(prev_safe)) {
    mtd <- 1
  } else if (prev_safe > k) {
    mtd <- k
  } else {
    mtd <- prev_safe
  }
  
  list(n = n, y = y, mtd = mtd, steps = steps)
}

run_comparison <- function(nsim, true_tox, target, cohort_size, max_patients) {
  k <- length(true_tox)
  true_mtd <- which.min(abs(true_tox - target))
  
  boin_sel <- rep(NA, nsim)
  plus_sel <- rep(NA, nsim)
  boin_alloc <- matrix(0, nsim, k)
  plus_alloc <- matrix(0, nsim, k)
  boin_over <- rep(0, nsim)
  plus_over <- rep(0, nsim)
  
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
  
  summary_df <- data.frame(
    Design = c("BOIN", "3+3"),
    TrueMTD = c(true_mtd, true_mtd),
    SelectionRate = c(
      round(mean(boin_sel == true_mtd, na.rm = TRUE) * 100, 1),
      round(mean(plus_sel == true_mtd, na.rm = TRUE) * 100, 1)
    ),
    AvgOverdosePatients = c(
      round(mean(boin_over), 2),
      round(mean(plus_over), 2)
    ),
    AvgTotalPatients = c(
      round(mean(rowSums(boin_alloc)), 2),
      round(mean(rowSums(plus_alloc)), 2)
    )
  )
  
  alloc_df <- rbind(
    data.frame(Design = "BOIN", Dose = factor(seq_len(k)), AvgPatients = colMeans(boin_alloc)),
    data.frame(Design = "3+3", Dose = factor(seq_len(k)), AvgPatients = colMeans(plus_alloc))
  )
  
  list(summary = summary_df, alloc = alloc_df, true_mtd = true_mtd)
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML(paste0("
      body {
        background:
          radial-gradient(circle at 8% 6%, #F4EEFF 0, transparent 34%),
          radial-gradient(circle at 92% 10%, #E8F7F7 0, transparent 31%),
          #EEF1F7;
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif;
        color: #242338;
      }
      .app-shell {
        background: #EEF1F7;
        border: 1px solid rgba(255,255,255,0.75);
        border-radius: 30px;
        padding: 20px;
        min-height: 96vh;
        box-shadow: 18px 18px 38px rgba(160, 171, 192, 0.28),
                    -18px -18px 38px rgba(255, 255, 255, 0.92);
      }
      .sidebar-wrap {
        background: linear-gradient(160deg, #4A166F 0%, #642A90 55%, #8149AB 100%);
        border: 1px solid rgba(255,255,255,0.22);
        border-radius: 24px;
        padding: 24px 18px;
        min-height: 88vh;
        color: white;
        box-shadow: inset 1px 1px 1px rgba(255,255,255,0.18),
                    12px 12px 24px rgba(94, 55, 117, 0.24);
      }
      .brand-mark {
        width: 42px; height: 42px; border-radius: 15px; display: inline-flex;
        align-items: center; justify-content: center; margin-bottom: 16px;
        background: rgba(255,255,255,0.16); border: 1px solid rgba(255,255,255,0.20);
        box-shadow: inset 2px 2px 4px rgba(255,255,255,0.10),
                    inset -3px -3px 7px rgba(32, 6, 56, 0.16);
        font-size: 18px; font-weight: 800;
      }
      .brand-title { font-size: 24px; font-weight: 760; letter-spacing: -0.025em; margin-bottom: 4px; }
      .brand-sub { font-size: 12px; color: #F0E5FB; margin-bottom: 20px; }
      .main-wrap { padding-left: 12px; padding-right: 8px; }
      .topbar {
        background: #EEF1F7; border: 1px solid rgba(255,255,255,0.82); border-radius: 23px;
        padding: 20px 24px; margin-bottom: 17px;
        box-shadow: 10px 10px 22px rgba(160, 171, 192, 0.26), -10px -10px 22px rgba(255,255,255,0.95);
      }
      .page-title { font-size: 29px; font-weight: 780; letter-spacing: -0.035em; color: #29233C; }
      .page-sub { font-size: 13px; color: #6F7182; margin-top: 4px; max-width: 880px; }
      .card, .metric-card {
        background: #EEF1F7; border: 1px solid rgba(255,255,255,0.86);
        box-shadow: 9px 9px 18px rgba(163, 174, 196, 0.26), -9px -9px 18px rgba(255,255,255,0.95);
      }
      .card { border-radius: 22px; padding: 19px; margin-bottom: 17px; overflow: hidden; }
      .metric-card { border-radius: 19px; padding: 17px 12px; margin-bottom: 15px; text-align: center; }
      .metric-label { font-size: 11.5px; color: #74768A; letter-spacing: 0.035em; text-transform: uppercase; font-weight: 700; margin-bottom: 9px; }
      .metric-value { font-size: 25px; font-weight: 780; color: #581F7E; letter-spacing: -0.03em; }
      .section-title { font-size: 16px; font-weight: 760; color: #2B2837; margin-bottom: 13px; letter-spacing: -0.012em; }
      .clinical-notice, .security-notice { border-radius: 18px; padding: 12px 15px; margin-bottom: 16px; font-size: 12.5px; line-height: 1.5; }
      .clinical-notice { background: #FFF7E6; border: 1px solid #F5D69A; color: #76510B; box-shadow: inset 2px 2px 4px rgba(232,191,96,0.12); }
      .security-notice { background: #EEF7FF; border: 1px solid #BEDDF4; color: #24547D; box-shadow: inset 2px 2px 4px rgba(117,181,227,0.10); }
      .control-label { color: #F9F4FF !important; font-size: 12px; font-weight: 700; }
      .irs--shiny .irs-bar, .irs--shiny .irs-single, .irs--shiny .irs-from, .irs--shiny .irs-to {
        background: #C8A6E8 !important; border-color: #C8A6E8 !important;
      }
      .irs--shiny .irs-handle { border-color: white !important; box-shadow: 0 2px 6px rgba(48,14,74,0.22); }
      .btn-navigate {
        width: 100%; background: rgba(255,255,255,0.11); color: white; border: 1px solid rgba(255,255,255,0.14);
        border-radius: 14px; padding: 12px 14px; margin-bottom: 10px; text-align: left; font-weight: 680;
        box-shadow: inset 2px 2px 4px rgba(255,255,255,0.06), inset -3px -3px 6px rgba(31,5,53,0.12);
        transition: transform .15s ease, background .15s ease;
      }
      .btn-navigate:hover, .btn-navigate:focus { color: white; background: rgba(255,255,255,0.18); transform: translateY(-1px); }
      .btn-primary, .btn-default {
        border: 1px solid rgba(255,255,255,0.86) !important; border-radius: 14px !important; font-weight: 750 !important;
        padding: 10px 15px !important; box-shadow: 5px 5px 12px rgba(155,165,186,0.22), -5px -5px 12px rgba(255,255,255,0.90);
      }
      .btn-primary { background: #5A1F80 !important; color: white !important; }
      .btn-default { background: #EEF1F7 !important; color: #5A1F80 !important; }
      .shiny-input-container { margin-bottom: 12px !important; }
      .nav-tabs { border-bottom: none; margin-bottom: 12px; }
      .nav-tabs > li > a {
        border-radius: 15px; border: 1px solid rgba(255,255,255,0.8) !important; background: #EEF1F7; color: #581F7E;
        margin-right: 8px; font-weight: 730; box-shadow: 4px 4px 9px rgba(164,174,194,0.18), -4px -4px 9px rgba(255,255,255,0.86);
      }
      .nav-tabs > li.active > a, .nav-tabs > li.active > a:hover, .nav-tabs > li.active > a:focus {
        background: #5A1F80 !important; color: white !important; box-shadow: inset 2px 2px 5px rgba(44,9,65,0.35) !important;
      }
      .credit-box { font-size: 12px; color: #F2E9FC; line-height: 1.55; margin-top: 17px; padding-top: 14px; border-top: 1px solid rgba(255,255,255,0.18); }
      .table-wrap { width: 100%; overflow-x: auto; overflow-y: hidden; }
      .dataTables_wrapper, table.dataTable { width: 100% !important; }
      .dataTables_wrapper .dataTables_paginate .paginate_button.current { background: #5A1F80 !important; color: white !important; border: none !important; }
      .ai-response-box {
        background: #EEF1F7; border: 1px solid rgba(255,255,255,0.86); border-left: 5px solid #5A1F80; border-radius: 18px;
        padding: 18px 20px; color: #2B2233; font-size: 16px; line-height: 1.65; white-space: pre-wrap;
        box-shadow: inset 3px 3px 7px rgba(159,170,192,0.15), inset -3px -3px 7px rgba(255,255,255,0.88);
      }
      .ai-label { font-size: 11.5px; font-weight: 800; color: #5A1F80; text-transform: uppercase; letter-spacing: 0.08em; margin-bottom: 10px; }
      .ask-box textarea {
        font-size: 16px !important; line-height: 1.55 !important; color: #2B2233 !important; background: #F6F8FC !important;
        border: 1px solid #DDE2EB !important; box-shadow: inset 3px 3px 7px rgba(159,170,192,0.13) !important; border-radius: 14px !important;
      }
      .plotly.html-widget, .html-widget { width: 100% !important; }
      .tox-card .plotly.html-widget { min-width: 0 !important; }
      /* Keep charts readable at narrow browser widths instead of squeezing the toxicity plot. */
      @media (max-width: 1199px) {
        .trial-chart-row > [class*='col-'] { width: 100%; float: none; }
        .trial-chart-row .card { margin-bottom: 17px; }
      }
      @media (max-width: 1099px) {
        .app-layout-row > .col-sm-3,
        .app-layout-row > .col-sm-9 { width: 100%; float: none; }
        .sidebar-wrap { min-height: auto; margin-bottom: 16px; }
        .main-wrap { padding-left: 0; padding-right: 0; }
      }
      @media (max-width: 768px) {
        .app-shell { padding: 10px; border-radius: 0; } .sidebar-wrap { min-height: auto; margin-bottom: 14px; }
        .main-wrap { padding-left: 0; padding-right: 0; } .page-title { font-size: 24px; }
      }
    ")))
  ),
  
  div(
    class = "app-shell",
    fluidRow(
      class = "app-layout-row",
      column(
        3,
        div(
          class = "sidebar-wrap",
          div(class = "brand-mark", "B"),
          div(class = "brand-title", "BOIN Dashboard"),
          div(class = "brand-sub", "Phase I dose-finding app"),
          actionButton("run_trial", "Run One Trial", class = "btn-navigate"),
          actionButton("run_compare", "Run Simulation Study", class = "btn-navigate"),
          br(),
          sliderInput("target", "Target toxicity", min = 0.10, max = 0.50, value = 0.30, step = 0.01),
          sliderInput("n_dose", "Number of dose levels", min = 3, max = 8, value = 5, step = 1),
          sliderInput("cohort", "BOIN cohort size", min = 1, max = 6, value = 3, step = 1),
          sliderInput("maxp", "Maximum patients", min = 12, max = 60, value = 30, step = 3),
          sliderInput("nsim", "Number of simulations", min = 100, max = 3000, value = 500, step = 100),
          uiOutput("dose_sliders"),
          div(
            class = "credit-box",
            HTML("<b>Credits</b><br>
                 Built for a Phase I dose-finding course project.<br>
                 Conceptual reference: <b>Dr. Yajun Mei</b>, <i>Sequential Methods in Clinical Trials</i>, NYU School of Global Public Health.<br>
                 Topics reflected here include MTD, 3+3 design, and BOIN.")
          )
        )
      ),
      
      column(
        9,
        div(
          class = "main-wrap",
          div(
            class = "topbar",
            div(class = "page-title", "Interactive BOIN and 3+3 Visualizer"),
            div(class = "page-sub", "Phase I clinical trial dashboard for MTD exploration, dose-escalation logic, simulation comparison, and AI-guided interpretation")
          ),
          
          div(
            class = "clinical-notice",
            HTML("<b>Educational simulation only.</b> This dashboard illustrates dose-finding logic for teaching and research. It is not clinical decision support and must not be used for patient-specific treatment decisions.")
          ),

          fluidRow(
            column(3, div(class = "metric-card", div(class = "metric-label", "Lower BOIN Boundary"), div(class = "metric-value", textOutput("lambda1")))),
            column(3, div(class = "metric-card", div(class = "metric-label", "Upper BOIN Boundary"), div(class = "metric-value", textOutput("lambda2")))),
            column(3, div(class = "metric-card", div(class = "metric-label", "True MTD"), div(class = "metric-value", textOutput("true_mtd")))),
            column(3, div(class = "metric-card", div(class = "metric-label", "Dose Levels"), div(class = "metric-value", textOutput("dose_ct"))))
          ),
          
          tabsetPanel(
            tabPanel(
              "Single Trial",
              br(),
              fluidRow(
                class = "trial-chart-row",
                column(7, div(class = "card", div(class = "section-title", "BOIN Dose Path"), plotlyOutput("path_plot", height = "310px"))),
                column(5, div(class = "card tox-card", div(class = "section-title", "Current Toxicity Setup"), plotlyOutput("tox_plot", height = "290px")))
              ),
              fluidRow(
                column(12, div(class = "card", div(class = "section-title", "Trial Allocation Summary"), plotlyOutput("alloc_plot", height = "360px")))
              ),
              fluidRow(
                column(12, div(class = "card", div(class = "section-title", "Step-by-Step BOIN Decisions"), DTOutput("trial_table")))
              )
            ),
            
            tabPanel(
              "BOIN vs 3+3",
              br(),
              fluidRow(
                column(8, div(class = "card", div(class = "section-title", "Average Patient Allocation"), plotlyOutput("compare_plot", height = "340px"))),
                column(
                  4,
                  div(
                    class = "card",
                    div(class = "section-title", "Simulation Summary"),
                    div(class = "table-wrap", DTOutput("summary_table"))
                  )
                )
              )
            ),
            
            tabPanel(
              "AI Assistant",
              br(),
              uiOutput("ai_status"),
              fluidRow(
                column(
                  12,
                  div(
                    class = "card",
                    div(class = "section-title", "Ask AI About the Phase I Analysis"),
                    div(
                      class = "ask-box",
                      textAreaInput(
                        "ai_question",
                        "Type your question",
                        placeholder = "Example: Why did BOIN escalate at the current dose? Compare BOIN and 3+3 in simple words. Which dose appears closest to the target toxicity for MTD selection?",
                        rows = 5,
                        width = "100%"
                      )
                    ),
                    actionButton("ask_ai", "Ask AI"),
                    br(), br(),
                    div(class = "section-title", "AI Response"),
                    uiOutput("ai_answer")
                  )
                )
              ),
              fluidRow(
                column(
                  12,
                  div(
                    class = "card",
                    div(class = "section-title", "Suggested Questions"),
                    HTML("
                      <ul>
                        <li>Why did BOIN escalate, stay, or de-escalate in the current Phase I trial?</li>
                        <li>Which dose appears closest to the target toxicity and why?</li>
                        <li>Compare BOIN and 3+3 for MTD selection in simple language.</li>
                        <li>What does the simulation summary suggest about safety and efficiency?</li>
                        <li>Suggest a realistic Phase I toxicity scenario for teaching.</li>
                      </ul>
                    ")
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {

  output$ai_status <- renderUI({
    if (gemini_enabled()) {
      div(
        class = "security-notice",
        HTML("<b>Privacy reminder:</b> Questions are sent to an external AI service. Do not enter patient identifiers, protected health information, or confidential study information. AI answers are educational and should be independently verified.")
      )
    } else {
      div(
        class = "security-notice",
        HTML("<b>Public-demo mode:</b> AI chat is disabled because no server-side API key is configured. The BOIN and 3+3 simulation tabs remain fully functional. Never place a Gemini key directly in this file or deploy a .Renviron file with a public app.")
      )
    }
  })

  output$dose_sliders <- renderUI({
    k <- input$n_dose
    defaults <- round(seq(0.05, 0.45, length.out = k), 2)
    sliders <- lapply(seq_len(k), function(i) {
      sliderInput(
        inputId = paste0("dose", i),
        label = paste("Dose", i, "true toxicity"),
        min = 0.01,
        max = 0.80,
        value = defaults[i],
        step = 0.01
      )
    })
    do.call(tagList, sliders)
  })
  
  true_tox <- reactive({
    k <- input$n_dose
    vals <- unlist(lapply(seq_len(k), function(i) input[[paste0("dose", i)]]), use.names = FALSE)
    
    if (length(vals) == 0) {
      vals <- seq(0.05, 0.45, length.out = k)
    }
    
    vals <- as.numeric(vals)
    
    if (length(vals) < k) {
      defaults <- seq(0.05, 0.45, length.out = k)
      defaults[seq_along(vals)] <- vals
      vals <- defaults
    }
    
    vals
  })
  
  output$lambda1 <- renderText({
    round(boin_boundaries(input$target)$lambda1, 3)
  })
  
  output$lambda2 <- renderText({
    round(boin_boundaries(input$target)$lambda2, 3)
  })
  
  output$true_mtd <- renderText({
    req(length(true_tox()) == input$n_dose)
    which.min(abs(as.numeric(true_tox()) - input$target))
  })
  
  output$dose_ct <- renderText({
    input$n_dose
  })
  
  trial_res <- eventReactive(input$run_trial, {
    req(length(true_tox()) == input$n_dose)
    simulate_boin_trial(
      true_tox = as.numeric(true_tox()),
      target = input$target,
      cohort_size = input$cohort,
      max_patients = input$maxp
    )
  }, ignoreNULL = FALSE)
  
  compare_res <- eventReactive(input$run_compare, {
    req(length(true_tox()) == input$n_dose)
    run_comparison(
      nsim = input$nsim,
      true_tox = as.numeric(true_tox()),
      target = input$target,
      cohort_size = input$cohort,
      max_patients = input$maxp
    )
  }, ignoreNULL = FALSE)
  
  output$tox_plot <- renderPlotly({
    req(length(true_tox()) == input$n_dose)
    
    df <- data.frame(
      Dose = factor(seq_along(true_tox())),
      Toxicity = as.numeric(true_tox())
    )
    df$hover <- paste0(
      "Dose: ", df$Dose,
      "<br>True toxicity: ", round(df$Toxicity, 3),
      "<br>Target toxicity: ", input$target
    )
    
    p <- ggplot(df, aes(Dose, Toxicity, group = 1, text = hover)) +
      geom_line(color = teal_blue, linewidth = 1.4) +
      geom_point(color = teal_blue, size = 3.5) +
      geom_hline(yintercept = input$target, linetype = "dashed", color = warm_orange, linewidth = 1) +
      ylim(0, max(0.85, max(df$Toxicity) + 0.05)) +
      labs(x = "Dose level", y = "Toxicity") +
      theme_minimal(base_size = 12.5) +
      theme(
        plot.margin = margin(12, 10, 10, 8),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title = element_text(face = "bold"),
        axis.title.y = element_text(margin = margin(r = 8))
      )
    
    ggplotly(p, tooltip = "text") |>
      layout(
        autosize = TRUE,
        margin = list(l = 52, r = 12, b = 48, t = 10),
        xaxis = list(automargin = TRUE),
        yaxis = list(automargin = TRUE),
        hoverlabel = list(bgcolor = "white", font = list(color = text_dark))
      ) |>
      config(displayModeBar = FALSE, responsive = TRUE)
  })
  
  output$path_plot <- renderPlotly({
    tr <- trial_res()
    req(nrow(tr$steps) > 0)
    req(length(true_tox()) == input$n_dose)
    
    df <- tr$steps
    df$hover <- paste0(
      "Step: ", df$Step,
      "<br>Dose: ", df$Dose,
      "<br>New patients: ", df$NewPatients,
      "<br>New DLTs: ", df$NewDLTs,
      "<br>Total patients at dose: ", df$TotalPatients,
      "<br>Total DLTs at dose: ", df$TotalDLTs,
      "<br>Toxicity rate: ", df$ToxicityRate,
      "<br>Decision: ", df$Decision
    )
    
    p <- ggplot(df, aes(x = Step, y = Dose, text = hover)) +
      geom_line(color = nyu_purple, linewidth = 1.4) +
      geom_point(size = 3.8, color = nyu_purple) +
      scale_y_continuous(breaks = seq_len(input$n_dose)) +
      labs(x = "Trial step", y = "Dose level") +
      theme_minimal(base_size = 14) +
      theme(
        plot.margin = margin(15, 20, 15, 15),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title = element_text(face = "bold")
      )
    
    ggplotly(p, tooltip = "text") |>
      layout(
        autosize = TRUE,
        margin = list(l = 60, r = 20, b = 50, t = 10),
        xaxis = list(automargin = TRUE),
        yaxis = list(automargin = TRUE),
        hoverlabel = list(bgcolor = "white", font = list(color = text_dark))
      ) |>
      config(displayModeBar = FALSE, responsive = TRUE)
  })
  
  output$alloc_plot <- renderPlotly({
    tr <- trial_res()
    req(length(true_tox()) == input$n_dose)
    
    df <- data.frame(
      Dose = factor(seq_along(tr$n)),
      Patients = as.numeric(tr$n),
      DLTs = as.numeric(tr$y),
      TrueToxicity = round(as.numeric(true_tox()), 2)
    )
    
    ymax <- max(df$Patients, na.rm = TRUE)
    if (ymax < 1) ymax <- 1
    
    df$label_y <- df$Patients + max(1, 0.08 * ymax)
    df$hover <- paste0(
      "Dose: ", df$Dose,
      "<br>Patients treated: ", df$Patients,
      "<br>DLTs: ", df$DLTs,
      "<br>True toxicity: ", df$TrueToxicity
    )
    
    p <- ggplot(df, aes(x = Dose, y = Patients, text = hover)) +
      geom_col(fill = nyu_purple, alpha = 0.9, width = 0.7) +
      geom_text(
        aes(
          y = label_y,
          label = paste0("DLT=", DLTs, "\nTox=", TrueToxicity)
        ),
        size = 4.5,
        lineheight = 1.05,
        color = text_dark
      ) +
      scale_y_continuous(
        limits = c(0, ymax + max(4, 0.20 * ymax)),
        expand = expansion(mult = c(0, 0.02))
      ) +
      coord_cartesian(clip = "off") +
      labs(x = "Dose level", y = "Patients treated") +
      theme_minimal(base_size = 14) +
      theme(
        plot.margin = margin(20, 25, 20, 20),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title = element_text(face = "bold")
      )
    
    ggplotly(p, tooltip = "text") |>
      layout(
        margin = list(l = 70, r = 20, b = 60, t = 20),
        hoverlabel = list(bgcolor = "white", font = list(color = text_dark))
      )
  })
  
  output$trial_table <- renderDT({
    tr <- trial_res()
    datatable(
      tr$steps,
      rownames = FALSE,
      class = "compact stripe hover",
      options = list(
        pageLength = 7,
        dom = "tip",
        autoWidth = TRUE,
        scrollX = TRUE
      )
    )
  })
  
  output$compare_plot <- renderPlotly({
    res <- compare_res()
    
    df <- res$alloc
    df$hover <- paste0(
      "Design: ", df$Design,
      "<br>Dose: ", df$Dose,
      "<br>Average patients: ", round(df$AvgPatients, 2)
    )
    
    p <- ggplot(df, aes(x = Dose, y = AvgPatients, group = Design, color = Design, text = hover)) +
      geom_line(linewidth = 1.4) +
      geom_point(size = 3.8) +
      scale_color_manual(values = c("BOIN" = nyu_purple, "3+3" = teal_blue)) +
      labs(x = "Dose level", y = "Average patients") +
      theme_minimal(base_size = 14) +
      theme(
        plot.margin = margin(15, 20, 15, 15),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title = element_text(face = "bold"),
        legend.title = element_blank(),
        legend.position = "top"
      )
    
    ggplotly(p, tooltip = "text") |>
      layout(
        margin = list(l = 70, r = 20, b = 60, t = 20),
        hoverlabel = list(bgcolor = "white", font = list(color = text_dark))
      )
  })
  
  output$summary_table <- renderDT({
    res <- compare_res()
    datatable(
      res$summary,
      rownames = FALSE,
      class = "compact stripe hover",
      options = list(
        dom = "t",
        paging = FALSE,
        ordering = FALSE,
        scrollX = TRUE,
        autoWidth = TRUE
      )
    )
  })
  
  ai_answer_text <- eventReactive(input$ask_ai, {
    req(nchar(trimws(input$ai_question)) > 0)
    req(length(true_tox()) == input$n_dose)
    
    tr <- trial_res()
    cmp <- compare_res()
    b <- boin_boundaries(input$target)
    
    latest_trial_info <- ""
    if (!is.null(tr) && nrow(tr$steps) > 0) {
      last_row <- tr$steps[nrow(tr$steps), ]
      latest_trial_info <- paste0(
        "Current Phase I BOIN trial summary:\n",
        "- Target toxicity: ", input$target, "\n",
        "- Lower boundary: ", round(b$lambda1, 3), "\n",
        "- Upper boundary: ", round(b$lambda2, 3), "\n",
        "- Current dose: ", last_row$Dose, "\n",
        "- Total patients at current dose: ", last_row$TotalPatients, "\n",
        "- Total DLTs at current dose: ", last_row$TotalDLTs, "\n",
        "- Observed toxicity rate: ", last_row$ToxicityRate, "\n",
        "- BOIN decision: ", last_row$Decision, "\n",
        "- Current estimated MTD: ", tr$mtd, "\n\n"
      )
    }
    
    compare_info <- ""
    if (!is.null(cmp)) {
      compare_info <- paste0(
        "BOIN vs 3+3 comparison summary for Phase I dose finding:\n",
        paste(capture.output(print(cmp$summary)), collapse = "\n"),
        "\n\n"
      )
    }
    
    tox_info <- paste0(
      "True toxicity probabilities by dose:\n",
      paste0("Dose ", seq_along(true_tox()), ": ", round(as.numeric(true_tox()), 2), collapse = "; "),
      "\n\n"
    )
    
    prompt <- paste0(
      "You are assisting with a graduate biostatistics Shiny app on Phase I clinical trials.\n",
      "BOIN means Bayesian Optimal Interval design for dose finding and MTD selection.\n",
      "3+3 is a traditional Phase I rule-based dose-escalation design.\n",
      "Do not confuse BOIN with BOINC.\n",
      "Answer in a short, correct, student-friendly way.\n",
      "Keep the answer under 180 words unless the user asks for more detail.\n",
      "Do not provide patient-specific treatment advice. Do not repeat or request personal health information.\n\n",
      tox_info,
      latest_trial_info,
      compare_info,
      "User question:\n",
      input$ai_question
    )
    
    call_gemini(prompt)
  })
  
  output$ai_answer <- renderUI({
    req(ai_answer_text())
    
    div(
      style = "display:flex; gap:12px; align-items:flex-start;",
      div(
        style = paste0(
          "background:", nyu_purple, 
          "; color:white; width:42px; height:42px; border-radius:50%; ",
          "display:flex; align-items:center; justify-content:center; ",
          "font-weight:700; font-size:16px; flex-shrink:0;"
        ),
        "AI"
      ),
      div(
        class = "ai-response-box",
        div(class = "ai-label", "Gemini Response"),
        HTML(gsub("\n", "<br/>", htmltools::htmlEscape(ai_answer_text()), fixed = TRUE))
      )
    )
  })
}

shinyApp(ui = ui, server = server)
