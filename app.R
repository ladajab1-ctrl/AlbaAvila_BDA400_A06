# =============================================================================
# BDA400 - Data Science Tools and Techniques
# Assignment 6: Technical Analysis using R, Visualization Phase
# Portfolio Dashboard (R Shiny)
#
# Author : Alba Avila
# Run    : open this file in RStudio and click "Run App",
#          or run  shiny::runApp("app.R")
#
# Sections (they follow the tasks in the assignment):
#   Step 1 - Data collection and setup
#   Step 2 - Visualizing stock data (UI widgets + chart types)
#   Step 3 - Overlay technical indicators (SMA, RSI, MACD) with on/off toggles
#   Step 4 - Trading rules (SMA crossover) and chart annotations
# =============================================================================


# =============================================================================
# STEP 1: DATA COLLECTION AND SETUP
# =============================================================================

# ---- 1.1 Data source ---------------------------------------------------------
# Yahoo Finance, accessed through the 'quantmod' package.

# ---- 1.2 Install (if missing) and load the necessary packages ----------------
required_packages <- c("shiny", "ggplot2", "quantmod", "TTR", "patchwork")
missing_packages  <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(shiny)      # web application framework
library(ggplot2)    # charts
library(quantmod)   # getSymbols() -> Yahoo Finance data (also loads xts/zoo/TTR)
library(TTR)        # SMA(), RSI(), MACD()
library(patchwork)  # stacks the price / RSI / MACD panels vertically

# ---- 1.3 Global settings -----------------------------------------------------
# Extra history downloaded before the chosen start date. Indicators such as a
# 50-period SMA need previous data ("warm-up"); without it the lines would start
# empty. 4 years is enough even for a 50-period SMA on monthly bars.
WARMUP_DAYS <- 365 * 4

DEFAULT_TICKERS <- c("AAPL", "MSFT", "GOOGL", "AMZN", "TSLA", "NVDA", "META")

# ---- 1.4 Fetch historical stock data -----------------------------------------
# Downloads OHLCV data for ONE symbol. Throws an informative error when the
# symbol is invalid, Yahoo is unreachable, or no rows come back.
fetch_stock <- function(symbol, from, to) {
  x <- tryCatch(
    suppressWarnings(
      getSymbols(symbol, src = "yahoo", from = from, to = to + 1,
                 auto.assign = FALSE)
    ),
    error = function(e) {
      stop(sprintf("Could not download '%s' from Yahoo Finance (%s)",
                   symbol, conditionMessage(e)), call. = FALSE)
    }
  )

  if (is.null(x) || NROW(x) == 0) {
    stop(sprintf("No data returned for '%s'", symbol), call. = FALSE)
  }

  # Yahoo columns: Open, High, Low, Close, Volume, Adjusted -> keep first five
  x <- x[, 1:5]
  colnames(x) <- c("Open", "High", "Low", "Close", "Volume")

  # Missing data handling: drop incomplete rows
  x <- x[complete.cases(x), ]
  if (NROW(x) < 2) {
    stop(sprintf("Not enough valid rows for '%s'", symbol), call. = FALSE)
  }
  x
}

# Aggregates daily OHLCV data to the selected time frame.
to_timeframe <- function(x, time_frame) {
  if (time_frame == "Daily") return(x)

  agg <- if (time_frame == "Weekly") {
    to.weekly(x)
  } else {
    to.monthly(x, indexAt = "endof")
  }
  colnames(agg) <- c("Open", "High", "Low", "Close", "Volume")
  agg
}


# =============================================================================
# STEP 4 (helper): TRADING RULES
# =============================================================================
# Moving Average Crossover rule:
#   * Buy  : short-term MA crosses ABOVE the long-term MA
#   * Sell : short-term MA crosses BELOW the long-term MA
#   * Hold : otherwise
# Optional RSI confirmation (customizable):
#   * a Buy is ignored when RSI >= overbought level
#   * a Sell is ignored when RSI <= oversold level
generate_signals <- function(short_ma, long_ma, rsi = NULL,
                             use_rsi_filter = FALSE,
                             overbought = 70, oversold = 30) {
  n <- length(short_ma)
  above <- short_ma > long_ma
  prev_above <- c(NA, above[-n])

  valid <- !is.na(above) & !is.na(prev_above)
  buy  <- valid & above & !prev_above   # cross up
  sell <- valid & !above & prev_above   # cross down

  if (use_rsi_filter && !is.null(rsi)) {
    buy  <- buy  & !is.na(rsi) & rsi < overbought
    sell <- sell & !is.na(rsi) & rsi > oversold
  }

  signal <- rep("Hold", n)
  signal[buy]  <- "Buy"
  signal[sell] <- "Sell"
  signal
}


# =============================================================================
# STEP 2: SHINY APP - USER INTERFACE (widgets)
# =============================================================================
ui <- fluidPage(
  titlePanel("Portfolio Dashboard - Technical Analysis"),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      h4("Data"),
      selectizeInput("symbol", "Stock symbol:",
                     choices = DEFAULT_TICKERS, selected = "AAPL",
                     options = list(create = TRUE, placeholder = "e.g. AAPL")),
      dateRangeInput("date_range", "Select Date Range:",
                     start = Sys.Date() - 365, end = Sys.Date(),
                     max = Sys.Date()),
      selectInput("time_frame", "Select Time Frame:",
                  choices = c("Daily", "Weekly", "Monthly")),
      selectizeInput("portfolio", "Portfolio (for the Portfolio tab):",
                     choices = DEFAULT_TICKERS,
                     selected = c("AAPL", "MSFT", "GOOGL"),
                     multiple = TRUE,
                     options = list(create = TRUE)),
      actionButton("load", "Load / Update data", class = "btn-primary"),

      hr(),
      h4("Chart"),
      radioButtons("chart_type", "Chart type:",
                   choices = c("Line", "Candlestick", "Area"),
                   selected = "Candlestick", inline = TRUE),

      # ---- STEP 3: widgets that turn each indicator overlay on / off ----
      checkboxGroupInput("indicators", "Technical indicators:",
                         choices = c("Moving Averages", "RSI", "MACD"),
                         selected = c("Moving Averages")),

      conditionalPanel(
        condition = "input.indicators && input.indicators.indexOf('Moving Averages') > -1",
        numericInput("short_n", "Short MA period:", value = 20, min = 2, max = 200),
        numericInput("long_n",  "Long MA period:",  value = 50, min = 3, max = 400)
      ),
      conditionalPanel(
        condition = "(input.indicators && input.indicators.indexOf('RSI') > -1) || input.use_rsi_filter",
        numericInput("rsi_n", "RSI period:", value = 14, min = 2, max = 100)
      ),
      conditionalPanel(
        condition = "input.indicators && input.indicators.indexOf('MACD') > -1",
        numericInput("macd_fast",   "MACD fast EMA:",   value = 12, min = 2, max = 100),
        numericInput("macd_slow",   "MACD slow EMA:",   value = 26, min = 3, max = 200),
        numericInput("macd_signal", "MACD signal EMA:", value = 9,  min = 2, max = 100)
      ),

      hr(),
      h4("Trading rules (SMA crossover)"),
      checkboxInput("show_signals", "Annotate Buy / Sell signals on chart", TRUE),
      checkboxInput("use_rsi_filter", "Confirm signals with RSI", FALSE),
      conditionalPanel(
        condition = "input.use_rsi_filter",
        sliderInput("rsi_levels", "RSI oversold / overbought levels:",
                    min = 10, max = 90, value = c(30, 70))
      )
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Chart",
                 br(),
                 plotOutput("stock_chart", height = "700px"),
                 uiOutput("signal_summary")),
        tabPanel("Signals",
                 br(),
                 p("Buy / Sell signals generated by the moving average crossover rule ",
                   "(only the dates where a signal fired are listed)."),
                 tableOutput("signal_table")),
        tabPanel("Portfolio",
                 br(),
                 p("Performance of the selected portfolio, rebased to 100 at the ",
                   "start of the date range."),
                 plotOutput("portfolio_chart", height = "450px"),
                 tableOutput("portfolio_table")),
        tabPanel("Data",
                 br(),
                 tableOutput("data_table"))
      )
    )
  )
)


# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {

  # ---------------------------------------------------------------------------
  # STEP 1/2: reactive data download (runs at start and on every button click)
  # ---------------------------------------------------------------------------
  raw_data <- eventReactive(input$load, {
    req(input$symbol, input$date_range)
    sym <- toupper(trimws(input$symbol))

    if (input$date_range[1] >= input$date_range[2]) {
      showNotification("The start date must be before the end date.",
                       type = "error")
      return(NULL)
    }

    withProgress(message = paste("Downloading", sym, "..."), value = 0.5, {
      res <- tryCatch(
        fetch_stock(sym, from = input$date_range[1] - WARMUP_DAYS,
                    to = input$date_range[2]),
        error = function(e) e
      )
    })

    if (inherits(res, "error")) {
      showNotification(conditionMessage(res), type = "error", duration = 10)
      return(NULL)
    }
    list(symbol = sym, data = res)
  }, ignoreNULL = FALSE)

  # ---------------------------------------------------------------------------
  # STEP 2-4: time-frame aggregation, indicators and signals
  # Indicators are computed on the full (warm-up) history and the result is
  # then cut to the selected date range.
  # ---------------------------------------------------------------------------
  analysis <- reactive({
    rd <- raw_data()
    validate(need(!is.null(rd),
                  "No data to show. Check the symbol / dates and press 'Load / Update data'."))

    validate(
      need(!is.na(input$short_n) && !is.na(input$long_n) &&
             input$short_n < input$long_n,
           "The short MA period must be smaller than the long MA period."),
      need(!is.na(input$rsi_n), "Enter a valid RSI period."),
      need(!is.na(input$macd_fast) && !is.na(input$macd_slow) &&
             !is.na(input$macd_signal) && input$macd_fast < input$macd_slow,
           "MACD: the fast EMA must be smaller than the slow EMA.")
    )

    x  <- to_timeframe(rd$data, input$time_frame)
    cl <- as.numeric(Cl(x))

    df <- as.data.frame(coredata(x))
    df$Date <- as.Date(index(x))

    # ---- STEP 3: technical indicators ----
    df$SMA_short <- as.numeric(SMA(cl, n = input$short_n))
    df$SMA_long  <- as.numeric(SMA(cl, n = input$long_n))
    df$RSI       <- as.numeric(RSI(cl, n = input$rsi_n))

    macd <- MACD(cl, nFast = input$macd_fast, nSlow = input$macd_slow,
                 nSig = input$macd_signal)
    df$MACD        <- as.numeric(macd[, "macd"])
    df$MACD_signal <- as.numeric(macd[, "signal"])
    df$MACD_hist   <- df$MACD - df$MACD_signal

    # ---- STEP 4: trading rules ----
    levels <- if (is.null(input$rsi_levels)) c(30, 70) else input$rsi_levels
    df$Signal <- generate_signals(df$SMA_short, df$SMA_long, df$RSI,
                                  use_rsi_filter = isTRUE(input$use_rsi_filter),
                                  oversold = levels[1], overbought = levels[2])

    # Cut to the user's date range (warm-up rows are dropped here)
    df <- df[df$Date >= input$date_range[1] & df$Date <= input$date_range[2], ]
    validate(need(nrow(df) > 1,
                  "Not enough data in the selected range for this time frame."))

    list(symbol = rd$symbol, df = df)
  })

  # ---------------------------------------------------------------------------
  # STEP 2-4: main chart (price panel + optional RSI and MACD panels)
  # ---------------------------------------------------------------------------
  output$stock_chart <- renderPlot({
    a  <- analysis()
    df <- a$df
    ind <- input$indicators

    # Shared x range so that all stacked panels line up
    bar_width <- median(as.numeric(diff(df$Date)))
    xr <- range(df$Date) + c(-bar_width, bar_width)

    # ---------------- Price panel: chart type ----------------
    p <- ggplot(df, aes(x = Date))

    if (input$chart_type == "Line") {
      p <- p + geom_line(aes(y = Close), color = "#1f4e79", linewidth = 0.8)

    } else if (input$chart_type == "Area") {
      df$ymin_area <- min(df$Low) * 0.99
      p <- p +
        geom_ribbon(data = df, aes(ymin = ymin_area, ymax = Close),
                    fill = "#1f4e79", alpha = 0.25) +
        geom_line(aes(y = Close), color = "#1f4e79", linewidth = 0.8)

    } else {  # Candlestick
      w <- bar_width * 0.6
      up   <- df[df$Close >= df$Open, ]
      down <- df[df$Close <  df$Open, ]
      p <- p +
        geom_segment(aes(xend = Date, y = Low, yend = High), color = "grey30") +
        geom_rect(data = up,
                  aes(xmin = Date - w / 2, xmax = Date + w / 2,
                      ymin = Open, ymax = Close),
                  fill = "#2e7d32", color = "grey30") +
        geom_rect(data = down,
                  aes(xmin = Date - w / 2, xmax = Date + w / 2,
                      ymin = Close, ymax = Open),
                  fill = "#c62828", color = "grey30")
    }

    # ---------------- STEP 3: overlay Moving Averages ----------------
    if ("Moving Averages" %in% ind) {
      p <- p +
        geom_line(aes(y = SMA_short, color = "Short MA"), linewidth = 0.9,
                  na.rm = TRUE) +
        geom_line(aes(y = SMA_long, color = "Long MA"), linewidth = 0.9,
                  na.rm = TRUE) +
        scale_color_manual(
          name = NULL,
          values = c("Short MA" = "#ff9800", "Long MA" = "#7b1fa2"),
          labels = c("Short MA" = paste0("SMA ", input$short_n),
                     "Long MA"  = paste0("SMA ", input$long_n))
        )
    }

    # ---------------- STEP 4: annotate bars with Buy / Sell signals --------
    if (isTRUE(input$show_signals)) {
      sig <- df[df$Signal != "Hold", ]
      if (nrow(sig) > 0) {
        offset <- 0.03 * diff(range(c(df$Low, df$High)))
        sig$y <- ifelse(sig$Signal == "Buy", sig$Low - offset, sig$High + offset)
        sig$label_y <- ifelse(sig$Signal == "Buy", sig$y - offset * 0.8,
                              sig$y + offset * 0.8)
        p <- p +
          geom_point(data = sig, aes(y = y, fill = Signal, shape = Signal),
                     color = "black", size = 3.5) +
          geom_text(data = sig, aes(y = label_y, label = toupper(Signal)),
                    size = 3, fontface = "bold", color = "grey15") +
          scale_fill_manual(name = "Signal",
                            values = c(Buy = "#2e7d32", Sell = "#c62828")) +
          scale_shape_manual(name = "Signal", values = c(Buy = 24, Sell = 25))
      }
    }

    p <- p +
      labs(title = sprintf("%s - %s (%s to %s)", a$symbol, input$time_frame,
                           format(min(df$Date)), format(max(df$Date))),
           y = "Price (USD)", x = NULL) +
      coord_cartesian(xlim = xr) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top")

    panels  <- list(p)
    heights <- 3

    # ---------------- STEP 3: overlay RSI (own panel) ----------------
    if ("RSI" %in% ind) {
      levels <- if (is.null(input$rsi_levels)) c(30, 70) else input$rsi_levels
      p_rsi <- ggplot(df, aes(x = Date, y = RSI)) +
        geom_hline(yintercept = c(30, 70), linetype = "dashed", color = "grey50") +
        geom_line(color = "#00796b", linewidth = 0.8, na.rm = TRUE) +
        labs(y = paste0("RSI (", input$rsi_n, ")"), x = NULL) +
        coord_cartesian(xlim = xr, ylim = c(0, 100)) +
        theme_minimal(base_size = 13)
      panels  <- c(panels, list(p_rsi))
      heights <- c(heights, 1)
    }

    # ---------------- STEP 3: overlay MACD (own panel) ----------------
    if ("MACD" %in% ind) {
      p_macd <- ggplot(df, aes(x = Date)) +
        geom_col(aes(y = MACD_hist, fill = MACD_hist >= 0), width = bar_width * 0.8,
                 na.rm = TRUE, show.legend = FALSE) +
        scale_fill_manual(values = c("TRUE" = "#81c784", "FALSE" = "#e57373")) +
        geom_line(aes(y = MACD), color = "#1565c0", linewidth = 0.8, na.rm = TRUE) +
        geom_line(aes(y = MACD_signal), color = "#ef6c00", linewidth = 0.8,
                  na.rm = TRUE) +
        labs(y = "MACD", x = NULL) +
        coord_cartesian(xlim = xr) +
        theme_minimal(base_size = 13)
      panels  <- c(panels, list(p_macd))
      heights <- c(heights, 1)
    }

    wrap_plots(panels, ncol = 1, heights = heights)
  })

  # ---------------------------------------------------------------------------
  # Signal summary + table (STEP 4)
  # ---------------------------------------------------------------------------
  output$signal_summary <- renderUI({
    df <- analysis()$df
    n_buy  <- sum(df$Signal == "Buy")
    n_sell <- sum(df$Signal == "Sell")
    last_sig <- df[df$Signal != "Hold", ]
    last_txt <- if (nrow(last_sig) == 0) "no signal in this range" else
      sprintf("%s on %s", tail(last_sig$Signal, 1), format(tail(last_sig$Date, 1)))
    tags$p(strong("Signals in range: "),
           sprintf("%d Buy, %d Sell. Latest: %s. ", n_buy, n_sell, last_txt),
           tags$em("Educational use only - not financial advice."))
  })

  output$signal_table <- renderTable({
    df <- analysis()$df
    sig <- df[df$Signal != "Hold", c("Date", "Signal", "Close", "SMA_short", "SMA_long", "RSI")]
    validate(need(nrow(sig) > 0, "No crossover signals in the selected range."))
    sig$Date <- format(sig$Date)
    names(sig) <- c("Date", "Signal", "Close", "Short MA", "Long MA", "RSI")
    sig
  }, digits = 2)

  output$data_table <- renderTable({
    df <- analysis()$df
    out <- df[order(df$Date, decreasing = TRUE),
              c("Date", "Open", "High", "Low", "Close", "Volume", "Signal")]
    out$Date <- format(out$Date)
    head(out, 200)
  }, digits = 2)

  # ---------------------------------------------------------------------------
  # Portfolio tab: several symbols, each one fetched with its own error handling
  # ---------------------------------------------------------------------------
  portfolio_data <- eventReactive(input$load, {
    syms <- unique(toupper(trimws(input$portfolio)))
    req(length(syms) > 0, input$date_range)

    results <- list()
    failed  <- character(0)

    withProgress(message = "Downloading portfolio...", value = 0, {
      for (s in syms) {
        incProgress(1 / length(syms), detail = s)
        res <- tryCatch(
          fetch_stock(s, from = input$date_range[1], to = input$date_range[2]),
          error = function(e) NULL
        )
        if (is.null(res)) failed <- c(failed, s) else results[[s]] <- res
      }
    })

    if (length(failed) > 0) {
      showNotification(paste("Could not load:", paste(failed, collapse = ", ")),
                       type = "warning", duration = 8)
    }
    results
  }, ignoreNULL = FALSE)

  portfolio_long <- reactive({
    res <- portfolio_data()
    validate(need(length(res) > 0, "No portfolio data available."))
    do.call(rbind, lapply(names(res), function(s) {
      cl <- as.numeric(Cl(res[[s]]))
      data.frame(Symbol = s, Date = as.Date(index(res[[s]])),
                 Close = cl, Rebased = cl / cl[1] * 100)
    }))
  })

  output$portfolio_chart <- renderPlot({
    d <- portfolio_long()
    ggplot(d, aes(Date, Rebased, color = Symbol)) +
      geom_hline(yintercept = 100, linetype = "dashed", color = "grey60") +
      geom_line(linewidth = 0.9) +
      labs(title = "Portfolio performance (rebased to 100)",
           y = "Index (start = 100)", x = NULL, color = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top")
  })

  output$portfolio_table <- renderTable({
    d <- portfolio_long()
    do.call(rbind, lapply(split(d, d$Symbol), function(g) {
      r <- diff(log(g$Close))
      data.frame(Symbol = g$Symbol[1],
                 `Start price` = g$Close[1],
                 `End price`   = g$Close[nrow(g)],
                 `Return (%)`  = (g$Close[nrow(g)] / g$Close[1] - 1) * 100,
                 `Annualized volatility (%)` = sd(r) * sqrt(252) * 100,
                 check.names = FALSE)
    }))
  }, digits = 2)
}

shinyApp(ui, server)
