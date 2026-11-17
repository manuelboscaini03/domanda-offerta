# =====================================================================
#  Domanda e offerta lineari: equilibrio, surplus e interventi pubblici
#  Avvio: shiny::runApp("app.R")  oppure  aprire il file in RStudio e
#  premere "Run App".
#  Pacchetti richiesti: shiny, ggplot2
# =====================================================================

library(shiny)
library(ggplot2)

# ---- Valori iniziali -------------------------------------------------
DEF <- list(a = 100, b = 1, c = 10, d = 1,
            t = 20, pmax = 40, pmin = 70, qmax = 30)

# Formattazione numeri all'italiana
fmt <- function(x, k = 2) {
  ifelse(is.na(x) | !is.finite(x), "–",
         formatC(x, format = "f", digits = k, big.mark = ".", decimal.mark = ","))
}

# ---- Interfaccia -----------------------------------------------------
ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { background:#f7f7f9; }
    .well { background:#ffffff; border:1px solid #e3e3e8; }
    .well h4 { margin-top:6px; font-weight:600; color:#333; }
    .box { background:#fff; border:1px solid #e3e3e8; border-radius:6px;
           padding:10px 14px; margin-bottom:12px; }
    .msg-info { border-left:4px solid #3182bd; }
    .msg-warn { border-left:4px solid #e6550d; }
    table { width:100%; }
    .small-note { color:#666; font-size:12px; }
  "))),
  titlePanel("Domanda e offerta: equilibrio, surplus e interventi pubblici"),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Domanda:  P = a − b·Q"),
      sliderInput("a", "Intercetta a (disponibilità massima a pagare)",
                  min = 10, max = 200, value = DEF$a, step = 1),
      sliderInput("b", "Pendenza b", min = 0.1, max = 5, value = DEF$b, step = 0.05),
      h4("Offerta:  P = c + d·Q"),
      sliderInput("c", "Intercetta c (costo marginale minimo)",
                  min = 0, max = 150, value = DEF$c, step = 1),
      sliderInput("d", "Pendenza d", min = 0.1, max = 5, value = DEF$d, step = 0.05),
      hr(),
      h4("Tassa per unità"),
      checkboxInput("tax_on", "Introduci una tassa", FALSE),
      conditionalPanel(
        "input.tax_on",
        sliderInput("t", "Importo della tassa (per unità)",
                    min = 0, max = 100, value = DEF$t, step = 1),
        radioButtons("tax_who", "Chi versa la tassa allo Stato?",
                     choices = c("Venditore" = "seller", "Compratore" = "buyer"),
                     inline = TRUE)
      ),
      hr(),
      h4("Intervento sul mercato"),
      checkboxInput("ceil_on", "Prezzo massimo (tetto di prezzo)", FALSE),
      conditionalPanel("input.ceil_on",
        sliderInput("pmax", "Tetto di prezzo", min = 0, max = 200, value = DEF$pmax, step = 1)),
      checkboxInput("floor_on", "Prezzo minimo (prezzo pavimento)", FALSE),
      conditionalPanel("input.floor_on",
        sliderInput("pmin", "Prezzo minimo", min = 0, max = 200, value = DEF$pmin, step = 1)),
      checkboxInput("quota_on", "Limite alla quantità (quota / contingentamento)", FALSE),
      conditionalPanel("input.quota_on",
        sliderInput("qmax", "Quantità massima", min = 0, max = 200, value = DEF$qmax, step = 1)),
      hr(),
      actionButton("reset", "Ripristina valori iniziali", icon = icon("rotate-left")),
      br(), br(),
      p(class = "small-note",
        "I controlli di prezzo si applicano al prezzo di mercato (quello scambiato tra",
        "compratore e venditore, al netto della tassa se a versarla è il compratore).",
        "Con tetto o pavimento si assume razionamento efficiente.")
    ),
    mainPanel(
      width = 8,
      div(class = "box", plotOutput("plot", height = "540px")),
      uiOutput("messages"),
      fluidRow(
        column(6, div(class = "box", h4("Equilibrio"), tableOutput("eq_table"))),
        column(6, div(class = "box", h4("Benessere"), tableOutput("welfare_table")))
      )
    )
  )
)

# ---- Server ----------------------------------------------------------
server <- function(input, output, session) {

  observeEvent(input$reset, {
    for (id in c("a", "b", "c", "d", "t", "pmax", "pmin", "qmax"))
      updateSliderInput(session, id, value = DEF[[id]])
    for (id in c("tax_on", "ceil_on", "floor_on", "quota_on"))
      updateCheckboxInput(session, id, value = FALSE)
    updateRadioButtons(session, "tax_who", selected = "seller")
  })

  calc <- reactive({
    a <- input$a; b <- input$b; c0 <- input$c; d <- input$d
    t   <- if (isTRUE(input$tax_on)) input$t else 0
    who <- if (is.null(input$tax_who)) "seller" else input$tax_who

    D  <- function(q) a - b * q          # prezzo di domanda
    S  <- function(q) c0 + d * q         # prezzo di offerta (costo marginale)
    Qd <- function(p) max(0, (a - p) / b)
    Qs <- function(p) max(0, (p - c0) / d)
    pb_of <- function(pm) if (who == "buyer") pm + t else pm   # pagato dai compratori
    ps_of <- function(pm) if (who == "buyer") pm else pm - t   # incassato dai venditori

    msgs <- list()
    add  <- function(txt, type = "info") msgs[[length(msgs) + 1]] <<- list(txt = txt, type = type)

    # --- Mercato libero ---
    Q0 <- max(0, (a - c0) / (b + d))
    P0 <- if (Q0 > 0) D(Q0) else NA
    W0 <- if (a > c0) (a - c0)^2 / (2 * (b + d)) else 0
    CS0 <- if (Q0 > 0) b * Q0^2 / 2 else 0
    PS0 <- if (Q0 > 0) d * Q0^2 / 2 else 0
    if (a <= c0) add("La disponibilità massima a pagare (a) non supera il costo minimo (c): il mercato non esiste.", "warn")

    # --- Equilibrio con eventuale tassa ---
    Qt   <- max(0, (a - c0 - t) / (b + d))
    Pb_t <- D(Qt)
    Pm   <- if (who == "buyer") Pb_t - t else Pb_t      # prezzo di mercato
    if (t > 0 && Qt == 0 && Q0 > 0)
      add("La tassa è così alta da azzerare gli scambi: il mercato scompare.", "warn")

    # --- Controlli di prezzo ---
    ceil <- if (isTRUE(input$ceil_on))  input$pmax else Inf
    flo  <- if (isTRUE(input$floor_on)) input$pmin else -Inf
    if (flo > ceil) {
      add("Il prezzo minimo è superiore al tetto massimo: il prezzo minimo viene ignorato.", "warn")
      flo <- -Inf
    }
    regime <- "mercato"
    if (Pm > ceil) { Pm <- ceil; regime <- "tetto" }
    else if (Pm < flo) { Pm <- flo; regime <- "pavimento" }
    if (is.finite(ceil) && regime != "tetto")
      add("Il tetto di prezzo è sopra il prezzo di mercato: non è vincolante e non ha effetti.")
    if (is.finite(flo) && regime != "pavimento")
      add("Il prezzo minimo è sotto il prezzo di mercato: non è vincolante e non ha effetti.")

    Pb <- pb_of(Pm); Ps <- ps_of(Pm)
    qd <- Qd(Pb); qs <- Qs(Ps)
    Q  <- min(qd, qs)
    if (regime == "tetto")
      add(sprintf("Tetto vincolante: domanda %s, offerta %s → carenza di %s unità (vengono scambiate solo le unità offerte).",
                  fmt(qd), fmt(qs), fmt(qd - qs)), "warn")
    if (regime == "pavimento")
      add(sprintf("Prezzo minimo vincolante: offerta %s, domanda %s → eccesso di offerta di %s unità (vengono scambiate solo le unità domandate).",
                  fmt(qs), fmt(qd), fmt(qs - qd)), "warn")

    # --- Quota ---
    quota <- if (isTRUE(input$quota_on)) input$qmax else Inf
    quota_bind <- FALSE
    if (Q > quota) {
      Q <- quota; quota_bind <- TRUE
      if (regime == "mercato") {           # il prezzo lo fissa la domanda
        Pb <- D(Q); Pm <- if (who == "buyer") Pb - t else Pb; Ps <- ps_of(Pm)
        qd <- Q; qs <- Qs(Ps)
      }
      add(sprintf("Quota vincolante: si scambiano solo %s unità. La differenza tra prezzo incassato e costo marginale è una rendita per i venditori autorizzati.",
                  fmt(Q)), "warn")
    } else if (is.finite(quota)) {
      add("Il limite di quantità è sopra la quantità scambiata: non è vincolante.")
    }

    # --- Benessere ---
    if (Q > 0) {
      CS  <- (a - Pb) * Q - b * Q^2 / 2
      PS  <- (Ps - c0) * Q - d * Q^2 / 2
      rent <- if (quota_bind) max(0, (Ps - S(Q)) * Q) else 0
    } else { CS <- PS <- rent <- 0 }
    Tax <- t * Q
    W   <- CS + PS + Tax
    DWL <- max(0, W0 - W)

    if (t > 0 && Qt > 0 && regime == "mercato" && !quota_bind && Q0 > 0) {
      sh_b <- (Pb - P0) / t
      add(sprintf("Incidenza della tassa: %s%% a carico dei compratori, %s%% a carico dei venditori (indipendentemente da chi la versa).",
                  fmt(100 * sh_b, 1), fmt(100 * (1 - sh_b), 1)))
    }
    if (Q0 > 0) {
      add(sprintf("Elasticità al punto di equilibrio libero: domanda %s, offerta %s.",
                  fmt(-(1 / b) * P0 / Q0), fmt((1 / d) * P0 / Q0)))
    }

    list(a = a, b = b, c0 = c0, d = d, t = t, who = who, D = D, S = S,
         Q0 = Q0, P0 = P0, W0 = W0, CS0 = CS0, PS0 = PS0,
         Q = Q, Pb = Pb, Ps = Ps, Pm = Pm, qd = qd, qs = qs,
         regime = regime, ceil = ceil, flo = flo, quota = quota, quota_bind = quota_bind,
         CS = CS, PS = PS, rent = rent, Tax = Tax, W = W, DWL = DWL, msgs = msgs)
  })

  # ---- Grafico ----
  output$plot <- renderPlot({
    r <- calc()
    a <- r$a; b <- r$b; c0 <- r$c0; d <- r$d; t <- r$t
    D <- r$D; S <- r$S; Q <- r$Q

    xcand <- c(1.7 * r$Q0, r$qd, r$qs, if (is.finite(r$quota)) r$quota else 0)
    xmax  <- max(xcand, na.rm = TRUE) * 1.08
    if (!is.finite(xmax) || xmax <= 0) xmax <- a / b
    xmax  <- min(xmax, max(a / b, r$quota * 1.08, na.rm = TRUE))
    ycand <- c(a, if (is.finite(r$ceil)) r$ceil, if (is.finite(r$flo)) r$flo, c0 + t)
    ymax  <- max(ycand) * 1.1

    # Aree
    areas <- list()
    addp <- function(x, y, lab) areas[[length(areas) + 1]] <<-
      data.frame(x = x, y = y, area = lab, id = length(areas) + 1)
    if (Q > 0) {
      addp(c(0, Q, Q, 0), c(a, D(Q), r$Pb, r$Pb), "Surplus consumatori")
      if (r$rent > 1e-9) {
        addp(c(0, Q, 0), c(c0, S(Q), S(Q)), "Surplus produttori")
        addp(c(0, Q, Q, 0), c(S(Q), S(Q), r$Ps, r$Ps), "Rendita da quota")
      } else {
        addp(c(0, Q, Q, 0), c(c0, S(Q), r$Ps, r$Ps), "Surplus produttori")
      }
      if (r$Tax > 1e-9) addp(c(0, Q, Q, 0), c(r$Ps, r$Ps, r$Pb, r$Pb), "Gettito fiscale")
    }
    if (r$DWL > 1e-9 && r$Q0 > 0)
      addp(c(Q, r$Q0, Q), c(D(Q), r$P0, S(Q)), "Perdita secca")

    levs <- c("Surplus consumatori", "Surplus produttori", "Rendita da quota",
              "Gettito fiscale", "Perdita secca")
    cols <- c("Surplus consumatori" = "#9ecae1", "Surplus produttori" = "#fcae91",
              "Rendita da quota" = "#bcbddc", "Gettito fiscale" = "#fed976",
              "Perdita secca" = "#969696")
    vals <- c("Surplus consumatori" = r$CS - 0, "Surplus produttori" = r$PS - r$rent,
              "Rendita da quota" = r$rent, "Gettito fiscale" = r$Tax, "Perdita secca" = r$DWL)
    labs <- setNames(paste0(levs, ": ", fmt(vals[levs])), levs)

    p <- ggplot()
    if (length(areas)) {
      ad <- do.call(rbind, areas)
      ad$area <- factor(ad$area, levels = levs)
      p <- p + geom_polygon(data = ad, aes(x, y, group = id, fill = area),
                            alpha = 0.75, colour = NA) +
        scale_fill_manual(values = cols, labels = labs, drop = TRUE, name = NULL)
    }

    # Curve
    p <- p +
      annotate("segment", x = 0, xend = xmax, y = a, yend = D(xmax), colour = "#1f4fd1", linewidth = 1.3) +
      annotate("segment", x = 0, xend = xmax, y = c0, yend = S(xmax), colour = "#d7191c", linewidth = 1.3) +
      annotate("text", x = xmax, y = D(xmax), label = "D", colour = "#1f4fd1",
               hjust = -0.4, fontface = "bold", size = 5) +
      annotate("text", x = xmax, y = min(S(xmax), ymax * 0.98), label = "S", colour = "#d7191c",
               hjust = -0.4, fontface = "bold", size = 5)
    if (t > 0) {
      if (r$who == "seller") {
        p <- p + annotate("segment", x = 0, xend = xmax, y = c0 + t, yend = S(xmax) + t,
                          colour = "#d7191c", linewidth = 1, linetype = "dashed") +
          annotate("text", x = (ymax * 0.97 - c0 - t) / d, y = ymax * 0.97,
                   label = "S + t", colour = "#d7191c", hjust = 1.1, size = 4.5)
      } else {
        p <- p + annotate("segment", x = 0, xend = xmax, y = a - t, yend = D(xmax) - t,
                          colour = "#1f4fd1", linewidth = 1, linetype = "dashed") +
          annotate("text", x = xmax * 0.6, y = D(xmax * 0.6) - t, label = "D − t", colour = "#1f4fd1",
                   hjust = 1.3, vjust = 1.4, size = 4.5)
      }
    }

    # Interventi
    if (is.finite(r$ceil))
      p <- p + geom_hline(yintercept = r$ceil, colour = "#238b45", linetype = "longdash", linewidth = 0.9) +
        annotate("label", x = xmax * 0.98, y = r$ceil, label = paste("Tetto", fmt(r$ceil, 0)),
                 colour = "#238b45", hjust = 1, size = 3.6)
    if (is.finite(r$flo))
      p <- p + geom_hline(yintercept = r$flo, colour = "#d95f0e", linetype = "longdash", linewidth = 0.9) +
        annotate("label", x = xmax * 0.98, y = r$flo, label = paste("Prezzo minimo", fmt(r$flo, 0)),
                 colour = "#d95f0e", hjust = 1, size = 3.6)
    if (is.finite(r$quota))
      p <- p + geom_vline(xintercept = r$quota, colour = "#6a51a3", linetype = "longdash", linewidth = 0.9) +
        annotate("label", x = r$quota, y = ymax * 0.97, label = paste("Quota", fmt(r$quota, 0)),
                 colour = "#6a51a3", size = 3.6)
    if (r$regime %in% c("tetto", "pavimento") && abs(r$qd - r$qs) > 1e-9) {
      lab <- if (r$regime == "tetto") "Carenza" else "Eccesso di offerta"
      p <- p + annotate("segment", x = min(r$qd, r$qs), xend = max(r$qd, r$qs), y = r$Pm, yend = r$Pm,
                        colour = "black", linewidth = 0.8,
                        arrow = arrow(ends = "both", length = unit(0.18, "cm"))) +
        annotate("text", x = (r$qd + r$qs) / 2, y = r$Pm, label = lab, vjust = -0.7, size = 4)
    }

    # Punti di equilibrio e guide
    if (r$Q0 > 0) {
      p <- p +
        annotate("segment", x = r$Q0, xend = r$Q0, y = 0, yend = r$P0, linetype = "dotted") +
        annotate("segment", x = 0, xend = r$Q0, y = r$P0, yend = r$P0, linetype = "dotted") +
        annotate("point", x = r$Q0, y = r$P0, size = 3.5, shape = 21, fill = "white", stroke = 1.3) +
        annotate("text", x = r$Q0, y = r$P0, label = "E*", hjust = -0.5, vjust = -0.3, fontface = "bold")
    }
    changed <- Q > 0 && (abs(Q - r$Q0) > 1e-9 || abs(r$Pb - r$Ps) > 1e-9)
    if (changed) {
      p <- p +
        annotate("segment", x = Q, xend = Q, y = 0, yend = max(r$Pb, D(Q)), linetype = "dotted") +
        annotate("segment", x = 0, xend = Q, y = r$Pb, yend = r$Pb, linetype = "dotted") +
        annotate("segment", x = 0, xend = Q, y = r$Ps, yend = r$Ps, linetype = "dotted") +
        annotate("point", x = Q, y = r$Pb, size = 3, colour = "#1f4fd1") +
        annotate("point", x = Q, y = r$Ps, size = 3, colour = "#d7191c")
      if (abs(r$Pb - r$Ps) > 1e-9) {
        p <- p +
          annotate("text", x = 0, y = r$Pb, label = paste0("Pc = ", fmt(r$Pb)),
                   hjust = -0.05, vjust = -0.4, size = 3.6, colour = "#1f4fd1") +
          annotate("text", x = 0, y = r$Ps, label = paste0("Pv = ", fmt(r$Ps)),
                   hjust = -0.05, vjust = 1.3, size = 3.6, colour = "#d7191c")
      } else {
        p <- p + annotate("text", x = 0, y = r$Pb, label = paste0("Pc = Pv = ", fmt(r$Pb)),
                          hjust = -0.05, vjust = -0.4, size = 3.6)
      }
    }

    p + coord_cartesian(xlim = c(0, xmax * 1.05), ylim = c(0, ymax), expand = FALSE) +
      labs(x = "Quantità (Q)", y = "Prezzo (P)",
           subtitle = "Pc = prezzo pagato dai compratori, Pv = prezzo ricevuto dai venditori") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom", legend.text = element_text(size = 11),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "grey30")) +
      guides(fill = guide_legend(nrow = 2))
  })

  # ---- Messaggi ----
  output$messages <- renderUI({
    r <- calc()
    if (!length(r$msgs)) return(NULL)
    tagList(lapply(r$msgs, function(m)
      div(class = paste("box", if (m$type == "warn") "msg-warn" else "msg-info"), m$txt)))
  })

  # ---- Tabelle ----
  output$eq_table <- renderTable({
    r <- calc()
    data.frame(
      Grandezza = c("Quantità scambiata", "Prezzo pagato dai compratori",
                    "Prezzo ricevuto dai venditori", "Quantità domandata", "Quantità offerta"),
      `Mercato libero` = fmt(c(r$Q0, r$P0, r$P0, r$Q0, r$Q0)),
      `Con interventi` = fmt(c(r$Q, r$Pb, r$Ps, r$qd, r$qs)),
      check.names = FALSE)
  }, align = "lrr", striped = TRUE)

  output$welfare_table <- renderTable({
    r <- calc()
    data.frame(
      Voce = c("Surplus consumatori", "Surplus produttori", "  di cui rendita da quota",
               "Gettito fiscale", "Benessere totale", "Perdita secca"),
      `Mercato libero` = fmt(c(r$CS0, r$PS0, 0, 0, r$W0, 0)),
      `Con interventi` = fmt(c(r$CS, r$PS, r$rent, r$Tax, r$W, r$DWL)),
      Variazione = fmt(c(r$CS - r$CS0, r$PS - r$PS0, r$rent, r$Tax, r$W - r$W0, r$DWL)),
      check.names = FALSE)
  }, align = "lrrr", striped = TRUE)
}

shinyApp(ui, server)
