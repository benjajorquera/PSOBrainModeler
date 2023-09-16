
require(ggplot2)
require(gridExtra)
require(RColorBrewer)
require(pracma)
require(scales)



get.mfARI <- function(Ks, Delta.tau, Phi)
{
  #
  # From regression on bounded mfARI parameters made
  # on mar 06 2015
  #
  # Call:
  # lm(formula = ATARI ~ Ks + Delta.tau + Phi, data = params)
  # 
  # Residuals:
  #      Min       1Q   Median       3Q      Max 
  # -0.24540 -0.08135  0.01454  0.07764  0.19562 
  # 
  # Coefficients:
  #             Estimate Std. Error t value Pr(>|t|)    
  # (Intercept)  1.48740    0.31454   4.729 8.65e-06 ***
  # Ks           3.85688    0.11339  34.014  < 2e-16 ***
  # Delta.tau   -0.12420    0.02910  -4.269 4.99e-05 ***
  # Phi          0.10999    0.00608  18.091  < 2e-16 ***
  # ---
  # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  # 
  # Residual standard error: 0.1161 on 87 degrees of freedom
  # Multiple R-squared:  0.9981,	Adjusted R-squared:  0.9981 
  # F-statistic: 1.55e+04 on 3 and 87 DF,  p-value: < 2.2e-16
  
  invisible(1.48740 + 3.85688 * Ks - 0.12420 * Delta.tau + 0.10999 * Phi)
}


get.mfARI.no.Phi <- function(Ks, Delta.tau)
{
  # From regression on bounded mfARI parameters (discarding Phi)
  # made on mar 10 2015
  #
  # Call:
  # lm(formula = ATARI ~ Ks + Delta.tau, data = params)
  # 
  # Residuals:
  #      Min       1Q   Median       3Q      Max 
  # -0.56369 -0.15209 -0.03217  0.16018  0.60636 
  # 
  # Coefficients:
  #             Estimate Std. Error t value Pr(>|t|)    
  # (Intercept)  6.75148    0.25915   26.05   <2e-16 ***
  # Ks           2.51501    0.18608   13.52   <2e-16 ***
  # Delta.tau   -0.61895    0.02155  -28.72   <2e-16 ***
  # ---
  # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  # 
  # Residual standard error: 0.2519 on 88 degrees of freedom
  # Multiple R-squared:  0.9911,	Adjusted R-squared:  0.9909 
  # F-statistic:  4904 on 2 and 88 DF,  p-value: < 2.2e-16
  
  invisible(6.75148 + 2.51501 * Ks - 0.61895 * Delta.tau)
}


#
# Returns a plot with the CBFV parameters for a specific Delta-Tau.
#
get.plot.CBFV.parameters.search <- function(
      time.instants,
      normalised.CBFV.signal,
      min.CBFV.sample,
      min.CBFV.time.instant,
      search.results,
      cbfv.palette = brewer.pal(n = 9, name = "Blues"),
      id = NULL,
      ...
      )
{
  ymin <- min(normalised.CBFV.signal)
  ymax <- max(normalised.CBFV.signal)
  dy <- (ymax - ymin) / 100
  dx <- (tail(time.instants, 1) - time.instants[1]) / 100
  
  # Plots CBFV
  d <- data.frame(Time = time.instants, CBFV = normalised.CBFV.signal)
  p <- ggplot(data = d, mapping = aes(x = Time, y = CBFV))
  p <- p + geom_line(colour = cbfv.palette[4])
  p <- p + geom_point(colour = cbfv.palette[4])
  
  # If a transient and a steady CBFV were found
  if(is.finite(search.results[["CBFV.response.MSE"]]))
  {
    # Plots transient CBFV line
    dtl <- data.frame(
      Time = search.results[["transient.CBFV.time.instants"]],
      CBFV = search.results[["transient.CBFV.line"]]
    )
    p <- p + geom_line(data = dtl, colour = cbfv.palette[7])
    
    # Plots initial and final points of the transient CBFV
    dti <- data.frame(
      Time = search.results[["transient.CBFV.time.instants"]][1],
      CBFV = search.results[["transient.normalised.CBFV.signal"]][1]
    )
    p <- p + geom_point(data = dti, colour = cbfv.palette[7])
    dtf <- data.frame(
      Time = tail(search.results[["transient.CBFV.time.instants"]], 1),
      CBFV = tail(search.results[["transient.normalised.CBFV.signal"]], 1)
    )
    p <- p + geom_point(data = dtf, colour = cbfv.palette[7])
    
    # Adds time annotations
    d <- dti
    d[["CBFV"]] <- d[["CBFV"]] - 2 * dy
    d <- cbind(d, Text = sprintf("%.1f", min.CBFV.time.instant))
    p <- p + geom_text(
      data = d,
      mapping = aes(label = Text),
      colour = cbfv.palette[7],
      size = 2,
      hjust = 0.5,
      vjust = 1
    )
    d <- dtf
    u <- ifelse(dti[["CBFV"]] < dtf[["CBFV"]], -2 * dy, 3 * dy)
    d[["CBFV"]] <- dti[["CBFV"]] + u
    d <- cbind(d, Text = sprintf("%.1f", search.results[["tau"]]))
    p <- p + geom_text(
      data = d,
      mapping = aes(label = Text),
      colour = cbfv.palette[7],
      size = 2,
      hjust = 0.5,
      vjust = 1
    )
    d <- dtf
    d[["Time"]] <- d[["Time"]] - dx
    u <- ifelse(dti[["CBFV"]] < dtf[["CBFV"]], 2 * dy, -3 * dy)
    d[["CBFV"]] <- d[["CBFV"]] + u
    d <- cbind(d, Text = "Tau")
    p <- p + geom_text(
      data = d,
      mapping = aes(label = Text),
      colour = cbfv.palette[7],
      size = 2,
      hjust = 0.5,
      vjust = 0
    )
    
    # Adds lines to annotations
    d <- rbind(dtf, dtf)
    d[["CBFV"]][2] <- dti[["CBFV"]]
    p <- p + geom_line(
      data = d,
      colour = cbfv.palette[7],
      linetype = "dashed"
    )
    
    # Plots steady CBFV
    dsl <- data.frame(
      Time = search.results[["steady.CBFV.time.instants"]],
      CBFV = search.results[["steady.CBFV.line"]]
    )
    p <- p + geom_line(data = dsl, colour = cbfv.palette[7])
    
    # Adds time annotations
    d <- tail(dsl, 1)
    u <- ifelse(dti[["CBFV"]] < dtf[["CBFV"]], -2 * dy, 3 * dy)
    d[["CBFV"]] <- dti[["CBFV"]] + u
    s <- sprintf("%.1f", d[["Time"]])
    d <- cbind(d, Text = s)
    p <- p + geom_text(
      data = d,
      mapping = aes(label = Text),
      colour = cbfv.palette[7],
      size = 2,
      hjust = 0.5,
      vjust = 1
    )
    
    # Adds lines to annotations
    u <- ifelse(dti[["CBFV"]] < dtf[["CBFV"]], 2 * dy, -3 * dy)
    d[["CBFV"]] <- d[["CBFV"]] + u
    d <- rbind(d[, c(1, 2)], tail(dsl, 1))
    p <- p + geom_line(
      data = d,
      colour = cbfv.palette[7],
      linetype = "dashed"
    )
    
    # Adds summary table
    s <- sprintf("%.1fs", search.results[["Delta.tau"]])
    t <- data.frame(l = "Delta Tau", d = s)
    s <- sprintf("%.3f", search.results[["Ks"]])
    t <- rbind(t, data.frame(l = "Ks", d = s))
    s <- sprintf("%.2f°",
      atan(search.results[["transient.CBFV.slope"]]) * 180 / pi
    )
    t <- rbind(t, data.frame(l = "Angle", d = s))
    s <- sprintf("%.4f", search.results[["transient.CBFV.MSE"]])
    t <- rbind(t, data.frame(l = "Trans. MSE", d = s))
    s <- sprintf("%.4f", search.results[["steady.CBFV.MSE"]])
    t <- rbind(t, data.frame(l = "Steady MSE", d = s))
    s <- sprintf("%.4f", search.results[["CBFV.response.MSE"]])
    t <- rbind(t, data.frame(l = "Total MSE", d = s))
    
    gt <- tableGrob(
      d = t,
      show.rownames = FALSE,
      show.colnames = FALSE,
      gpar.coretext = gpar(col = cbfv.palette[7], cex = 0.5),
      padding.v = unit(2, "mm")
    )
    p <- p + annotation_custom(
      grob = gt,
      xmin = time.instants[1],
      xmax = time.instants[1] + 9,
      ymin = ymin,
      ymax = ymin + 25 * dy
    )
  }
  
  if(!is.null(id))
    p <- p + ggtitle(id)
  p <- p + xlab("Time") + ylab("Normalised CBFV")
  
  p
}


#
# Returns a plot with the CBFV parameters for a specific Delta-Tau.
#
get.plots.mfARI.parameters <- function(
      time.instants,
      ABP.signal,
      CBFV.signal,
      parameter.results,
      index.estimation.function = NULL,
      abp.palette = brewer.pal(n = 9, name = "Reds"),
      cbfv.palette = brewer.pal(n = 9, name = "Blues"),
      ann.palette = brewer.pal(n = 9, name = "Greens")[seq(3, 9, 2)],
      time.tol = min(diff(time.instants)) / 100,
      id = NULL,
      ...
      )
{
  #
  # Plot 1 (original signals)
  #
  
  abp.ymin <- min(ABP.signal, na.rm = TRUE)
  abp.ymax <- max(ABP.signal, na.rm = TRUE)
  abp.ry <- abp.ymax - abp.ymin
  abp.dy <- abp.ry / 100
  
  cbfv.ymin <- min(CBFV.signal, na.rm = TRUE)
  cbfv.ymax <- max(CBFV.signal, na.rm = TRUE)
  cbfv.ry <- cbfv.ymax - cbfv.ymin
  cbfv.dy <- cbfv.ry / 100
  
  rx <- tail(time.instants, 1) - time.instants[1]
  dx <- rx / 100
  
  # Plots ABP
  dabp <- data.frame(
    Time = time.instants,
    Signal = ABP.signal,
    Segment = "ABP Signal"
  )
  p1 <- ggplot(
    data = dabp,
    mapping = aes(x = Time, y = Signal, colour = Segment)
  )
  p1 <- p1 + geom_line(linetype = "dashed")
  
  # Plots CBFV
  dcbfv <- data.frame(
    Time = time.instants,
    Signal = CBFV.signal,
    Segment = "CBFV Signal"
  )
  p2 <- ggplot(
    data = dabp,
    mapping = aes(x = Time, y = Signal, colour = Segment)
  )
  p2 <- p2 + geom_line(data = dcbfv, linetype = "dashed")
  
  # Plots ABP baseline
  d <- dabp[parameter.results[["ABP.baseline.samples"]], ]
  d[["Segment"]] <- "Baseline"
  p1 <- p1 + geom_point(data = d)
  d[["Signal"]] <- parameter.results[["ABP.baseline.value"]]
  p1 <- p1 + geom_line(data = d)
  
  # Plots CBFV baseline
  d <- dcbfv[parameter.results[["CBFV.baseline.samples"]], ]
  d[["Segment"]] <- "Baseline"
  p2 <- p2 + geom_point(data = d)
  d[["Signal"]] <- parameter.results[["CBFV.baseline.value"]]
  p2 <- p2 + geom_line(data = d)
    
  # Plots min ABP window
  d <- dabp[parameter.results[["min.ABP.samples"]], ]
  d[["Segment"]] <- "Min. Search Window"
  p1 <- p1 + geom_point(data = d)
  d[["Signal"]] <- parameter.results[["min.ABP.value"]]
  p1 <- p1 + geom_line(data = d, linetype = "dashed")
  
  # Plots min CBFV window
  d <- dcbfv[parameter.results[["min.CBFV.samples"]], ]
  d[["Segment"]] <- "Min. Search Window"
  p2 <- p2 + geom_point(data = d)
  
  # Adds min ABP time annotation
  d <- dabp[parameter.results[["min.ABP.sample"]], ]
  d[["Signal"]] <- d[["Signal"]] - 2 * abp.dy
  d[["Segment"]] <- "Min. Search Window"
  d <- cbind(d, Text = sprintf("%.1f", d[["Time"]]))
  p1 <- p1 + geom_text(
    data = d,
    mapping = aes(label = Text),
    size = 3,
    hjust = 0.5,
    vjust = 1
  )
  
  # Adds min CBFV time annotation
  d <- dcbfv[parameter.results[["min.CBFV.sample"]], ]
  d[["Signal"]] <- d[["Signal"]] - 2 * cbfv.dy
  d[["Segment"]] <- "Min. Search Window"
  d <- cbind(d, Text = sprintf("%.1f", d[["Time"]]))
  p2 <- p2 + geom_text(
    data = d,
    mapping = aes(label = Text),
    size = 3,
    hjust = 0.5,
    vjust = 1
  )
  
  # Adds min ABP drop annotation
  d <- dabp[parameter.results[["min.ABP.samples"]][1], ]
  d[["Segment"]] <- "Min. Search Window"
  d[["Signal"]] <- parameter.results[["min.ABP.value"]] - 4 * cbfv.dy
  d <- cbind(
    d,
    Text = sprintf("%.1f%% drop", parameter.results[["min.ABP.drop.pc"]])
  )
  p1 <- p1 + geom_text(
    data = d,
    mapping = aes(label = Text),
    size = 2,
    hjust = 0.5,
    vjust = 1
  )
  
  p1 <- p1 + xlab("Time") + ylab("ABP")
  p2 <- p2 + xlab("Time") + ylab("CBFV")
  cmatch1 <- c(
    "ABP Signal" = abp.palette[4],
    "CBFV Signal" = cbfv.palette[4],
    "Baseline" = ann.palette[2],
    "Min. Search Window" = ann.palette[3]
  )
  p1 <- p1 + scale_colour_manual(values = cmatch1, breaks = names(cmatch1))
  p2 <- p2 + scale_colour_manual(values = cmatch1, breaks = names(cmatch1))
  p1 <- p1 + theme(
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = alpha('grey90', 0.0)),
    legend.position = c(1, 1),
    legend.key.size =  unit(0.3, "cm"),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 6)
  )
  p2 <- p2 + theme(
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = alpha('grey90', 0.0)),
    legend.position = c(1, 1),
    legend.key.size =  unit(0.4, "cm"),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 6)
  )
  
  #
  # Plot 3 (normalised signals)
  #
  
  ABP.time <- time.instants -
              time.instants[parameter.results[["min.ABP.sample"]]]
  ABP.signal <- parameter.results[["normalised.ABP.signal"]]
  
  CBFV.time <- time.instants -
               time.instants[parameter.results[["min.CBFV.sample"]]]
  CBFV.signal <- parameter.results[["normalised.CBFV.signal"]]
  
  xmin <- min(c(ABP.time, CBFV.time))
  xmax <- max(c(ABP.time, CBFV.time))
  rx <- xmax - xmin
  dx <- rx / 100
  ymin <- min(c(ABP.signal, CBFV.signal))
  ymax <- max(c(ABP.signal, CBFV.signal))
  ry <- ymax - ymin
  dy <- ry / 100
  
  # Plots CBFV
  dcbfv <- data.frame(
    Time = CBFV.time,
    Signal = CBFV.signal,
    Segment = "Normalised CBFV"
  )
  p3 <- ggplot(
    data = dcbfv,
    mapping = aes(x = Time, y = Signal, colour = Segment)
  )
  d <- dcbfv[1:(parameter.results[["transient.CBFV.samples"]][1]), ]
  p3 <- p3 + geom_line(data = d, linetype = "dashed")
  r <- tail(parameter.results[["transient.CBFV.samples"]], 1)
  d <- dcbfv[r:nrow(dcbfv), ]
  p3 <- p3 + geom_line(data = d, linetype = "dashed")
  
  # Plots ABP
  dabp <- data.frame(
    Time = ABP.time,
    Signal = ABP.signal,
    Segment = "Normalised ABP"
  )
  d <- dabp[1:(parameter.results[["transient.ABP.samples"]][1]), ]
  p3 <- p3 + geom_line(data = d, linetype = "dashed")
  r <- tail(parameter.results[["transient.ABP.samples"]], 1)
  d <- dabp[r:nrow(dabp), ]
  p3 <- p3 + geom_line(data = d, linetype = "dashed")
  
  # Plots ABP transient segment
  d <- dabp[parameter.results[["transient.ABP.samples"]], ]
  p3 <- p3 + geom_line(data = d, linetype = "11")
  d[["Signal"]] <- parameter.results[["transient.ABP.line"]]
  d[["Segment"]] <- "Transient Line"
  p3 <- p3 + geom_line(data = d)
  
  # Plots CBFV transient segment
  d <- dcbfv[parameter.results[["transient.CBFV.samples"]], ]
  p3 <- p3 + geom_line(data = d, linetype = "11")
  d[["Signal"]] <- parameter.results[["transient.CBFV.line"]]
  d[["Segment"]] <- "Transient Line"
  p3 <- p3 + geom_line(data = d)
  
  # Plots CBFV steady segment
  d <- dcbfv[parameter.results[["steady.CBFV.samples"]], ]
  d[["Signal"]] <- parameter.results[["steady.CBFV.line"]]
  d[["Segment"]] <- "Steady Line"
  p3 <- p3 + geom_line(data = d, linetype = "dashed")
  
  # Plots and annotates Delta Tau
  s2 <- parameter.results[["transient.CBFV.samples"]]
  d2 <- dcbfv[s2, ]
  y <- min(d2[["Signal"]], na.rm = TRUE)
  tini2 <- CBFV.time[s2[1]]
  tfin2 <- CBFV.time[tail(s2, 1)]
  tini1 <- ABP.time > tini2 - time.tol
  tfin1 <- ABP.time < tfin2 + time.tol
  s1 <- which(tini1 & tfin1)
  d1 <- dabp[s1, ]
  y <- min(c(d1[["Signal"]], y), na.rm = TRUE)
  
  d2[["Signal"]] <- y - 2 * dy
  d2[["Segment"]] <- "Transient Line"
  p3 <- p3 + geom_line(data = d2)
  m <- round(nrow(d2) / 2)
  d <- d2[m, ]
  d[["Signal"]] <- d[["Signal"]] - dy
  s <- paste(
    "Delta*tau",
    "==",
    sprintf("%.1f", parameter.results[["Delta.tau"]])
  )
  d <- cbind(d, Text = s)
  p3 <- p3 + geom_text(
    data = d,
    mapping = aes(label = Text),
    size = 3,
    hjust = 0.4,
    vjust = 1,
    parse = TRUE
  )
  
  # Annotates Ks
  d2 <- dcbfv[parameter.results[["steady.CBFV.samples"]], ]
  i <- d2[["Signal"]] < parameter.results[["Ks"]]
  d2[["Signal"]] <- parameter.results[["Ks"]]
  d2[["Segment"]] <- "Steady Line"
  p3 <- p3 + geom_line(data = d2)
  u <- 2 * dy
  vjust <- 0
  if(sum(i) - sum(!i) < 0)
  {
    u <- -u
    vjust <- 1 - vjust
  }
  m <- round(nrow(d2) / 2)
  d <- d2[m, ]
  d[["Signal"]] <- d[["Signal"]] + u
  s <- paste("K[s]", "==", sprintf("%.4f", parameter.results[["Ks"]]))
  d <- cbind(d, Text = s)
  p3 <- p3 + geom_text(
    data = d,
    mapping = aes(label = Text),
    size = 3,
    hjust = 0.5,
    vjust = vjust,
    parse = TRUE
  )
  
  # Plots and annotates Phi
  if(!is.null(ABP.signal))
  {
    d1 <- dabp[parameter.results[["transient.ABP.samples"]], ]
    d1[["Signal"]] <- parameter.results[["transient.ABP.line"]]
    d2 <- dcbfv[parameter.results[["transient.CBFV.samples"]], ]
    d2[["Signal"]] <- parameter.results[["transient.CBFV.line"]]
    m <- round(min(nrow(d1), nrow(d2)) * 0.8)
    #d <- rbind(d1[m, ], d2[m, ])
    #d[["Segment"]] <- "Angle"
    #p3 <- p3 + geom_point(data = d)
    r <- d1[["Time"]][m]
    tt <- seq(-90, 90, length.out = 100)
    xx <- r * cos(tt * pi / 180)
    yy <- (sin(tt * pi / 180) + 1) / 2
    yy <- sin(tt * pi / 180)
    #d <- data.frame(Time = xx, Signal = yy, Segment = "Angle")
    #p3 <- p3 + geom_path(data = d, linetype = "dotted")
    .dist <- function(x, y) sqrt((x - xx)^2 + (y - yy)^2)
    .tmp.fun <- function(x, y) min(.dist(x, y))
    s1 <- mapply(.tmp.fun, d1[["Time"]], d1[["Signal"]])
    s1 <- which.min(s1)
    s2 <- mapply(.tmp.fun, d2[["Time"]], d2[["Signal"]])
    s2 <- which.min(s2)
    t1 <- ABP.time[parameter.results[["transient.ABP.samples"]][s1]]
    t2 <- CBFV.time[parameter.results[["transient.CBFV.samples"]][s2]]
    y1 <- parameter.results[["transient.ABP.line"]][s1]
    y2 <- parameter.results[["transient.CBFV.line"]][s2]
    d <- data.frame(Time = c(t1, t2), Signal = c(y1, y2), Segment = "BLA")
    #p3 <- p3 + geom_point(data = d)
    i <- yy < max(y1, y2)
    j <- yy > min(y1, y2)
    if(sum(i & j) > 0)
    {      
      d <- data.frame(
        Time = xx[i & j],
        Signal = yy[i & j],
        Segment = "Angle"
      )
      p3 <- p3 + geom_path(data = d, linetype = "11")
    }
    
    i <- yy < max(y1, y2) & yy < 1
    j <- yy > min(y1, y2) & yy > 0
    if(sum(i & j) > 0)
    {
      xx <- xx[i & j]
      yy <- yy[i & j]
      d <- data.frame(Time = xx, Signal = yy, Segment = "Angle")
      p3 <- p3 + geom_path(data = d)
    }
    m <- round(nrow(d) / 2)
    d <- d[m, ]
    d[["Time"]] <- d[["Time"]] + dx
    d[["Segment"]] <- "Angle"
    s <- paste(
      "varphi",
      "==",
      sprintf("%.2f*degree", parameter.results[["Phi"]])
    )
    d <- cbind(d, Text = s)
    p3 <- p3 + geom_text(
      data = d,
      mapping = aes(label = Text),
      size = 3,
      hjust = 0,
      vjust = 0.5,
      parse = TRUE
    )
  }
  
  p3 <- p3 + xlab("Time") + ylab("Normalised Signals")
  cmatch2 <- c(
    "Normalised ABP" = abp.palette[4],
    "Normalised CBFV" = cbfv.palette[4],
    "Transient Line" = ann.palette[2],
    "Steady Line" = ann.palette[3],
    "Angle" = ann.palette[4]
  )
  p3 <- p3 + scale_colour_manual(
    values = cmatch2,
    breaks = names(cmatch2)
  )
  p3 <- p3 + theme(
    legend.justification = c(0, 0),
    legend.background = element_rect(fill = alpha('grey90', 0.0)),
    legend.position = c(0, 0),
    legend.key.size =  unit(0.4, "cm"),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 6)
  )
  
  #
  # Joins plots
  #
  
  main <- NULL
  if(!is.null(parameter.results[["mfARI"]]))
    main <- sprintf("mfARI = %.1f", parameter.results[["mfARI"]])
  else
    if(!is.null(index.estimation.function))
    {
      mfari <- index.estimation.function(
        Ks = parameter.results[["Ks"]],
        Delta.tau = parameter.results[["Delta.tau"]],
        Phi = parameter.results[["Phi"]]
      )
      main <- sprintf("estimated mfARI = %.1f", mfari)
    }
  
  if(!is.null(id))
    if(is.null(main))
      main <- id
    else
      main <- paste(id, main, sep = ", ")
  
  p <- arrangeGrob(
    arrangeGrob(p1, p2, nrow = 2),
    p3,
    nrow = 2,
    main = main
  )
  
  p
}
get.plots.mfARI.parameters.fixed.coord <- function(
      time.instants,
      ABP.signal,
      CBFV.signal,
      parameter.results,
      time.until.min,
      time.after.min,
      min.normalised.intensity,
      max.normalised.intensity,
      index.estimation.function = NULL,
      abp.palette = brewer.pal(n = 9, name = "Reds"),
      cbfv.palette = brewer.pal(n = 9, name = "Blues"),
      ann.palette = brewer.pal(n = 9, name = "Greens")[seq(3, 9, 2)],
      time.tol = min(diff(time.instants)) / 100,
      id = NULL,
      ...
      )
{
  #
  # Plot 1 (original signals)
  #
  
  abp.ymin <- min(ABP.signal, na.rm = TRUE)
  abp.ymax <- max(ABP.signal, na.rm = TRUE)
  abp.ry <- abp.ymax - abp.ymin
  abp.dy <- abp.ry / 100
  
  cbfv.ymin <- min(CBFV.signal, na.rm = TRUE)
  cbfv.ymax <- max(CBFV.signal, na.rm = TRUE)
  cbfv.ry <- cbfv.ymax - cbfv.ymin
  cbfv.dy <- cbfv.ry / 100
  
  rx <- tail(time.instants, 1) - time.instants[1]
  dx <- rx / 100
  
  # Plots ABP
  dabp <- data.frame(
    Time = time.instants,
    Signal = ABP.signal,
    Segment = "ABP Signal"
  )
  p1 <- ggplot(
    data = dabp,
    mapping = aes(x = Time, y = Signal, colour = Segment)
  )
  p1 <- p1 + geom_line(linetype = "dashed")
  
  # Plots CBFV
  dcbfv <- data.frame(
    Time = time.instants,
    Signal = CBFV.signal,
    Segment = "CBFV Signal"
  )
  p2 <- ggplot(
    data = dabp,
    mapping = aes(x = Time, y = Signal, colour = Segment)
  )
  p2 <- p2 + geom_line(data = dcbfv, linetype = "dashed")
  
  # Plots ABP baseline
  d <- dabp[parameter.results[["ABP.baseline.samples"]], ]
  d[["Segment"]] <- "Baseline"
  p1 <- p1 + geom_point(data = d)
  d[["Signal"]] <- parameter.results[["ABP.baseline.value"]]
  p1 <- p1 + geom_line(data = d)
  
  # Plots CBFV baseline
  d <- dcbfv[parameter.results[["CBFV.baseline.samples"]], ]
  d[["Segment"]] <- "Baseline"
  p2 <- p2 + geom_point(data = d)
  d[["Signal"]] <- parameter.results[["CBFV.baseline.value"]]
  p2 <- p2 + geom_line(data = d)
    
  # Plots min ABP window
  d <- dabp[parameter.results[["min.ABP.samples"]], ]
  d[["Segment"]] <- "Min. Search Window"
  p1 <- p1 + geom_point(data = d)
  d[["Signal"]] <- parameter.results[["min.ABP.value"]]
  p1 <- p1 + geom_line(data = d, linetype = "dashed")
  
  # Plots min CBFV window
  d <- dcbfv[parameter.results[["min.CBFV.samples"]], ]
  d[["Segment"]] <- "Min. Search Window"
  p2 <- p2 + geom_point(data = d)
  
  # Adds min ABP time annotation
  d <- dabp[parameter.results[["min.ABP.sample"]], ]
  d[["Signal"]] <- d[["Signal"]] - 2 * abp.dy
  d[["Segment"]] <- "Min. Search Window"
  d <- cbind(d, Text = sprintf("%.1f", d[["Time"]]))
  p1 <- p1 + geom_text(
    data = d,
    mapping = aes(label = Text),
    size = 3,
    hjust = 0.5,
    vjust = 1
  )
  
  # Adds min CBFV time annotation
  d <- dcbfv[parameter.results[["min.CBFV.sample"]], ]
  d[["Signal"]] <- d[["Signal"]] - 2 * cbfv.dy
  d[["Segment"]] <- "Min. Search Window"
  d <- cbind(d, Text = sprintf("%.1f", d[["Time"]]))
  p2 <- p2 + geom_text(
    data = d,
    mapping = aes(label = Text),
    size = 3,
    hjust = 0.5,
    vjust = 1
  )
  
  # Adds min ABP drop annotation
  d <- dabp[parameter.results[["min.ABP.samples"]][1], ]
  d[["Segment"]] <- "Min. Search Window"
  d[["Signal"]] <- parameter.results[["min.ABP.value"]] - 4 * cbfv.dy
  d <- cbind(
    d,
    Text = sprintf("%.1f%% drop", parameter.results[["min.ABP.drop.pc"]])
  )
  p1 <- p1 + geom_text(
    data = d,
    mapping = aes(label = Text),
    size = 2,
    hjust = 0.5,
    vjust = 1
  )
  
  p1 <- p1 + xlab("Time") + ylab("ABP")
  p2 <- p2 + xlab("Time") + ylab("CBFV")
  cmatch1 <- c(
    "ABP Signal" = abp.palette[4],
    "CBFV Signal" = cbfv.palette[4],
    "Baseline" = ann.palette[2],
    "Min. Search Window" = ann.palette[3]
  )
  p1 <- p1 + scale_colour_manual(values = cmatch1, breaks = names(cmatch1))
  p2 <- p2 + scale_colour_manual(values = cmatch1, breaks = names(cmatch1))
  p1 <- p1 + theme(
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = alpha('grey90', 0.0)),
    legend.position = c(1, 1),
    legend.key.size =  unit(0.3, "cm"),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 6)
  )
  p2 <- p2 + theme(
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = alpha('grey90', 0.0)),
    legend.position = c(1, 1),
    legend.key.size =  unit(0.4, "cm"),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 6)
  )
  
  #
  # Plot 3 (normalised signals)
  #
  
  ABP.time <- time.instants -
              time.instants[parameter.results[["min.ABP.sample"]]]
  ABP.signal <- parameter.results[["normalised.ABP.signal"]]
  
  CBFV.time <- time.instants -
               time.instants[parameter.results[["min.CBFV.sample"]]]
  CBFV.signal <- parameter.results[["normalised.CBFV.signal"]]
  
  time.ini <- 0 - time.until.min - time.tol
  time.fin <- 0 + time.after.min + time.tol
  
  rx <- time.fin - time.ini
  dx <- rx / 100
  ry <- max.normalised.intensity - min.normalised.intensity
  dy <- ry / 100
  
  # Plots CBFV
  dcbfv <- data.frame(
    Time = CBFV.time,
    Signal = CBFV.signal,
    Segment = "Normalised CBFV"
  )
  p3 <- ggplot(
    data = dcbfv,
    mapping = aes(x = Time, y = Signal, colour = Segment)
  )
  d <- dcbfv[1:(parameter.results[["transient.CBFV.samples"]][1]), ]
  p3 <- p3 + geom_line(data = d, linetype = "dashed")
  r <- tail(parameter.results[["transient.CBFV.samples"]], 1):nrow(dcbfv)
  d <- dcbfv[r, ]
  p3 <- p3 + geom_line(data = d, linetype = "dashed")
  
  # Plots ABP, if corresponds
  dabp <- data.frame(
    Time = ABP.time,
    Signal = ABP.signal,
    Segment = "Normalised ABP"
  )
  d <- dabp[1:(parameter.results[["transient.ABP.samples"]][1]), ]
  p3 <- p3 + geom_line(data = d, linetype = "dashed")
  d <-dabp[tail(parameter.results[["transient.ABP.samples"]], 1):nrow(dabp), ]
  p3 <- p3 + geom_line(data = d, linetype = "dashed")
  
  # Plots ABP transient segment
  d <- dabp[parameter.results[["transient.ABP.samples"]], ]
  p3 <- p3 + geom_line(data = d, linetype = "11")
  d[["Signal"]] <- parameter.results[["transient.ABP.line"]]
  d[["Segment"]] <- "Transient Line"
  p3 <- p3 + geom_line(data = d)
  
  # Plots CBFV transient segment
  d <- dcbfv[parameter.results[["transient.CBFV.samples"]], ]
  p3 <- p3 + geom_line(data = d, linetype = "11")
  d[["Signal"]] <- parameter.results[["transient.CBFV.line"]]
  d[["Segment"]] <- "Transient Line"
  p3 <- p3 + geom_line(data = d)
  
  # Plots CBFV steady segment
  d <- dcbfv[parameter.results[["steady.CBFV.samples"]], ]
  d[["Signal"]] <- parameter.results[["steady.CBFV.line"]]
  d[["Segment"]] <- "Steady Line"
  p3 <- p3 + geom_line(data = d, linetype = "dashed")
  
  # Plots and annotates Delta Tau
  s2 <- parameter.results[["transient.CBFV.samples"]]
  d2 <- dcbfv[s2, ]
  y2 <- min(d2[["Signal"]], na.rm = TRUE)
  tini2 <- CBFV.time[s2[1]]
  tfin2 <- CBFV.time[tail(s2, 1)]
  tini1 <- ABP.time > tini2 - time.tol
  tfin1 <- ABP.time < tfin2 + time.tol
  s1 <- which(tini1 & tfin1)
  d1 <- dabp[s1, ]
  y1 <- min(d1[["Signal"]], na.rm = TRUE)
  y <- min(y1, y2)
  d2[["Signal"]] <- y - 2 * dy
  d2[["Segment"]] <- "Transient Line"
  p3 <- p3 + geom_line(data = d2)
  m <- round(nrow(d2) / 2)
  d <- d2[m, ]
  d[["Signal"]] <- d[["Signal"]] - dy
  s <- paste(
    "Delta*tau",
    "==",
    sprintf("%.1f", parameter.results[["Delta.tau"]])
  )
  d <- cbind(d, Text = s)
  p3 <- p3 + geom_text(
    data = d,
    mapping = aes(label = Text),
    size = 3,
    hjust = 0.4,
    vjust = 1,
    parse = TRUE
  )
  
  # Annotates Ks
  d2 <- dcbfv[parameter.results[["steady.CBFV.samples"]], ]
  i <- d2[["Signal"]] < parameter.results[["Ks"]]
  d2[["Signal"]] <- parameter.results[["Ks"]]
  d2[["Segment"]] <- "Steady Line"
  p3 <- p3 + geom_line(data = d2)
  u <- 2 * dy
  vjust <- 0
  if(sum(i) - sum(!i) < 0)
  {
    u <- -u
    vjust <- 1 - vjust
  }
  m <- round(nrow(d2) / 2)
  d <- d2[m, ]
  d[["Signal"]] <- d[["Signal"]] + u
  s <- paste("K[s]", "==", sprintf("%.4f", parameter.results[["Ks"]]))
  d <- cbind(d, Text = s)
  p3 <- p3 + geom_text(
    data = d,
    mapping = aes(label = Text),
    size = 3,
    hjust = 0.5,
    vjust = vjust,
    parse = TRUE
  )
  
  # Plots and annotates Phi
  d1 <- dabp[parameter.results[["transient.ABP.samples"]], ]
  d1[["Signal"]] <- parameter.results[["transient.ABP.line"]]
  d2 <- dcbfv[parameter.results[["transient.CBFV.samples"]], ]
  d2[["Signal"]] <- parameter.results[["transient.CBFV.line"]]
  m <- round(min(nrow(d1), nrow(d2)) * 0.8)
  #d <- rbind(d1[m, ], d2[m, ])
  #d[["Segment"]] <- "Angle"
  #p3 <- p3 + geom_point(data = d)
  r <- d1[["Time"]][m]
  tt <- seq(-90, 90, length.out = 100)
  xx <- r * cos(tt * pi / 180)
  yy <- (sin(tt * pi / 180) + 1) / 2
  yy <- sin(tt * pi / 180)
  #d <- data.frame(Time = xx, Signal = yy, Segment = "Angle")
  #p3 <- p3 + geom_path(data = d, linetype = "dotted")
  .dist <- function(x, y) sqrt((x - xx)^2 + (y - yy)^2)
  .tmp.fun <- function(x, y) min(.dist(x, y))
  s1 <- mapply(.tmp.fun, d1[["Time"]], d1[["Signal"]])
  s1 <- which.min(s1)
  s2 <- mapply(.tmp.fun, d2[["Time"]], d2[["Signal"]])
  s2 <- which.min(s2)
  t1 <- ABP.time[parameter.results[["transient.ABP.samples"]][s1]]
  t2 <- CBFV.time[parameter.results[["transient.CBFV.samples"]][s2]]
  y1 <- parameter.results[["transient.ABP.line"]][s1]
  y2 <- parameter.results[["transient.CBFV.line"]][s2]
  #d <- data.frame(Time = c(t1, t2), Signal = c(y1, y2), Segment = "BLA")
  #p3 <- p3 + geom_point(data = d)
  i <- yy < max(y1, y2)
  j <- yy > min(y1, y2)
  d <- data.frame(
    Time = xx[i & j],
    Signal = yy[i & j],
    Segment = "Angle"
  )
  p3 <- p3 + geom_path(data = d, linetype = "11")
  
  i <- yy < max(y1, y2) & yy < 1
  j <- yy > min(y1, y2) & yy > 0
  if(sum(i & j) > 0)
  {
    xx <- xx[i & j]
    yy <- yy[i & j]
    d <- data.frame(Time = xx, Signal = yy, Segment = "Angle")
    p3 <- p3 + geom_path(data = d)
  }
  m <- round(nrow(d) / 2)
  d <- d[m, ]
  d[["Time"]] <- d[["Time"]] + dx
  s <- paste(
    "varphi",
    "==",
    sprintf("%.2f*degree", parameter.results[["Phi"]])
  )
  d <- cbind(d, Text = s)
  p3 <- p3 + geom_text(
    data = d,
    mapping = aes(label = Text),
    size = 3,
    hjust = 0,
    vjust = 0.5,
    parse = TRUE
  )
  
  p3 <- p3 + xlab("Time") + ylab("Normalised Signals")
  cmatch2 <- c(
    "Normalised ABP" = abp.palette[4],
    "Normalised CBFV" = cbfv.palette[4],
    "Transient Line" = ann.palette[2],
    "Steady Line" = ann.palette[3],
    "Angle" = ann.palette[4]
  )
  p3 <- p3 + scale_colour_manual(
    values = cmatch2,
    breaks = names(cmatch2)
  )
  p3 <- p3 + theme(
    legend.justification = c(0, 0),
    legend.background = element_rect(fill = alpha('grey90', 0.0)),
    legend.position = c(0, 0),
    legend.key.size =  unit(0.4, "cm"),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 6)
  )
  p3 <- p3 +  coord_cartesian(
    xlim = c(time.ini, time.fin),
    ylim = c(min.normalised.intensity, max.normalised.intensity)
  )
  
  #
  # Joins plots
  #
  
  main <- NULL
  if(!is.null(parameter.results[["mfARI"]]))
    main <- sprintf("mfARI = %.1f", parameter.results[["mfARI"]])
  else
    if(!is.null(index.estimation.function))
    {
      mfari <- index.estimation.function(
        Ks = parameter.results[["Ks"]],
        Delta.tau = parameter.results[["Delta.tau"]],
        Phi = parameter.results[["Phi"]]
      )
      main <- sprintf("estimated mfARI = %.1f", mfari)
    }
  
  if(!is.null(id))
    if(is.null(main))
      main <- id
    else
      main <- paste(id, main, sep = ", ")
  
  p <- arrangeGrob(
    arrangeGrob(p1, p2, nrow = 2),
    p3,
    nrow = 2,
    main = main
  )
  
  p
}


#
# Estimates the MSE for specified duration of the transient CBFV signal
# In this version, Ks is the value of the normalised CBFV at the end
# of the transient period.
#
# # Would be better to limit the CBFV angle here?
# # This would affect the slope and straight line,
# # affecting the transient MSE...
#
#
estimate.mfARI.CBFV.parameters.type.2 <- function(
      time.instants,
      normalised.CBFV.signal,
      min.CBFV.sample,
      min.CBFV.time.instant,
      transient.CBFV.duration,
      steady.CBFV.duration,
      time.tol = min(diff(time.instants)) / 100,
      ...
      )
{
  ans <- list()
  
  ans[["Delta.tau"]] <- transient.CBFV.duration
  ans[["tau"]] <- min.CBFV.time.instant + transient.CBFV.duration
  ans[["transient.CBFV.samples"]] <- which(
    time.instants >= min.CBFV.time.instant - time.tol &
    time.instants <= ans[["tau"]] + time.tol
  )
  ans[["transient.CBFV.time.instants"]] <- 
    time.instants[ans[["transient.CBFV.samples"]]]
  ans[["transient.normalised.CBFV.signal"]] <-
    normalised.CBFV.signal[ans[["transient.CBFV.samples"]]]
  ans[["transient.CBFV.slope"]] <- 
    mean(ans[["transient.normalised.CBFV.signal"]]) /
    mean(ans[["transient.CBFV.time.instants"]] - min.CBFV.time.instant)
  ans[["transient.CBFV.line"]] <- 
    ans[["transient.CBFV.slope"]] *
    (time.instants[ans[["transient.CBFV.samples"]]] -
     min.CBFV.time.instant)
  if(length(ans[["transient.normalised.CBFV.signal"]]) > 0)
    ans[["transient.CBFV.MSE"]] <- get.MSE(
      sim = ans[["transient.CBFV.line"]],
      obs = ans[["transient.normalised.CBFV.signal"]]
    )
  else
    ans[["transient.CBFV.MSE"]] <- Inf
  
  ans[["Ks"]] <- tail(ans[["transient.normalised.CBFV.signal"]], 1)
  ans[["nominal.steady.CBFV.duration"]] <- steady.CBFV.duration
  ans[["steady.CBFV.samples"]] <- which(
    time.instants >= ans[["tau"]] - time.tol &
    time.instants <= ans[["tau"]] + steady.CBFV.duration + time.tol
  )
  ans[["steady.CBFV.time.instants"]] <-
    time.instants[ans[["steady.CBFV.samples"]]]
  steady.CBFV.length <- length(ans[["steady.CBFV.samples"]])
  ans[["steady.CBFV.duration"]] <-
    ans[["steady.CBFV.time.instants"]][length(ans[["steady.CBFV.time.instants"]])] -
    ans[["tau"]]
  ans[["steady.normalised.CBFV.signal"]] <-
    normalised.CBFV.signal[ans[["steady.CBFV.samples"]]]
  ans[["steady.CBFV.line"]] <- rep(ans[["Ks"]], steady.CBFV.length)
  if(steady.CBFV.length > 0)
    ans[["steady.CBFV.MSE"]] <- get.MSE(
      sim = ans[["steady.CBFV.line"]],
      obs = ans[["steady.normalised.CBFV.signal"]]
    )
  else
    ans[["steady.CBFV.MSE"]] <- Inf
  
  ans[["CBFV.response.duration"]] <-
    ans[["Delta.tau"]] +
    ans[["steady.CBFV.duration"]]
  if(is.finite(ans[["transient.CBFV.MSE"]]) &&
     is.finite(ans[["steady.CBFV.MSE"]]))
    ans[["CBFV.response.MSE"]] <-
      (ans[["Delta.tau"]] / ans[["CBFV.response.duration"]]) *
      ans[["transient.CBFV.MSE"]] +
      (ans[["steady.CBFV.duration"]] / ans[["CBFV.response.duration"]]) *
      ans[["steady.CBFV.MSE"]]
  else
    ans[["CBFV.response.MSE"]] <- Inf
  
  return(ans)
}


#
# Estimates the parameters and MSE for each possible duration of the
# transient CBFV signal.
#
# # Here, if the signal is too short to cover the specified maximum
# # transient CBFV duration, this maximum is reduced (keeping the
# # specified duration of the steady CBFV signal).
#
# # For equal MSE values, it prefers the one with lower transient CBFV
# # duration.
# 
search.mfARI.CBFV.parameters <- function(
      time.instants,
      normalised.CBFV.signal,
      min.CBFV.sample,
      min.CBFV.time.instant,
      min.transient.CBFV.duration,
      max.transient.CBFV.duration,
      steady.CBFV.duration,
      time.tol = min(diff(time.instants)) / 100,
      estimation.function = estimate.mfARI.CBFV.parameters.type.2,
      keep.search.results = FALSE,
      add.search.plots = FALSE,
      id = NULL,
      ...
      )
{
  min.delta.tau <- min.CBFV.time.instant + min.transient.CBFV.duration
  max.delta.tau <- min.CBFV.time.instant + max.transient.CBFV.duration
  last.time <- max.delta.tau + steady.CBFV.duration
  last.instant <-time.instants[length(time.instants)]
  if(last.time > last.instant + time.tol)
  {
    last.time <- last.instant
    max.delta.tau <- last.time - steady.CBFV.duration
    max.transient.CBFV.duration <- max.delta.tau - min.CBFV.time.instant
    if(is.null(id))
      msg <- "Warning: signal is too short for specified parameters;"
    else
      msg <- sprintf(
        "Warning: signal is too short for specified parameters (%s);",
        id
      ) 
    msg <- paste(
      msg,
      "maximum transient CBFV signal duration was reduced to")
    msg <- paste(
      msg,
      sprintf('%.1f seconds', max.transient.CBFV.duration)
    )
    warning(msg)
  }
  i <- which(
    time.instants >= min.delta.tau - time.tol &
    time.instants <= max.delta.tau + time.tol
  )
  
  ans <- list()
  ans[["Delta.tau.values"]] <- time.instants[i] - min.CBFV.time.instant
  
  plots <- list()
  results <- list()
  best.result <- list(CBFV.response.MSE = Inf)
  
  for(delta.tau in ans[["Delta.tau.values"]])
  {
    current.estimation <- estimation.function(
      time.instants = time.instants,
      normalised.CBFV.signal = normalised.CBFV.signal,
      min.CBFV.sample = min.CBFV.sample,
      min.CBFV.time.instant = min.CBFV.time.instant,
      transient.CBFV.duration = delta.tau,
      steady.CBFV.duration = steady.CBFV.duration,
      time.tol = time.tol,
      ...
    )
    
    if(current.estimation[["CBFV.response.MSE"]] <
       best.result[["CBFV.response.MSE"]])
      best.result <- current.estimation
    
    str.delta.tau <- as.character(delta.tau)
    if(keep.search.results)
    {
      lce <- list(current.estimation)
      names(lce) <- str.delta.tau
      results <- c(results, lce)
    }
    
    if(add.search.plots)
    {
      p <- get.plot.CBFV.parameters.search(
        time.instants,
        normalised.CBFV.signal,
        min.CBFV.sample,
        min.CBFV.time.instant,
        current.estimation,
        id = id,
        ...
      )
      lp <- list(p)
      names(lp) <- str.delta.tau
      plots <- c(plots, lp)
    }
  }
  
  if(keep.search.results)
    ans[["search.results"]] <- results
  
  if(add.search.plots)
    ans[["search.plots"]] <- plots
  
  ans <- c(ans, best.result)
  
  return(ans)
}


normalise.ABP.signal <- function(
      time.instants,
      ABP.signal,
      sample.release,
      sampling.time,
      baseline.initial.time = time.instants[1],
      baseline.final.time = time.instants[sample.release],
      min.ABP.max.delta.time = 5 * 0.8,
      time.tol = sampling.time / 100
      )
{
  ans <- list()
  valid <- !is.na(ABP.signal)
  time.release <- time.instants[sample.release]
  
  # Adds baseline data to answer
  if(baseline.final.time < baseline.initial.time)
    stop("final time for the ABP baseline cannot be ",
         "before its initial time")
  if(baseline.final.time > time.release)
    stop("final time for the ABP baseline cannot be ",
         "after the release of the thigh cuffs")
  
	i <- which(valid & time.instants >= baseline.initial.time - time.tol)
  if(length(i) == 0)
    stop("no time instant for the beginning of the ABP baseline could be determined")
	i <- i[1]
  ans[["ABP.baseline.initial.time"]] <- time.instants[i]
  ans[["ABP.baseline.initial.sample"]] <- i
  
	i <- which(valid & time.instants <= baseline.final.time + time.tol)
  if(length(i) == 0)
    stop("no time instant for the end of the ABP baseline could be determined")
	i <- tail(i, 1)
  ans[["ABP.baseline.final.time"]] <- time.instants[i]
  ans[["ABP.baseline.final.sample"]] <- i
  
	ans[["ABP.baseline.samples"]] <-
    ans[["ABP.baseline.initial.sample"]]:ans[["ABP.baseline.final.sample"]]
	ans[["ABP.baseline.duration"]] <-
    ans[["ABP.baseline.final.time"]] - ans[["ABP.baseline.initial.time"]]
  ans[["ABP.baseline.value"]] <- mean(
    ABP.signal[ans[["ABP.baseline.samples"]]],
    na.rm = TRUE
  )
  
  # Sets min ABP window
  ans[["min.ABP.max.delta.time"]] <- min.ABP.max.delta.time
  i <- time.instants > time.release + time.tol
  j <- time.instants < time.release + ans[["min.ABP.max.delta.time"]] +
       time.tol
  ans[["min.ABP.samples"]] <- which(valid & i & j)
  if(length(ans[["min.ABP.samples"]]) == 0)
    stop("no candidates for min ABP")
    
  # Gets minimum ABP
  ans[["min.ABP.window"]] <- ABP.signal[ans[["min.ABP.samples"]]]
  min.sample.in.window <- which.min(ans[["min.ABP.window"]])
  min.ABP.sample <- min.sample.in.window + ans[["min.ABP.samples"]][1] - 1
  ans[["min.ABP.time.instant"]] <- time.instants[min.ABP.sample]
  ans[["min.ABP.sample"]] <- min.ABP.sample
  ans[["min.ABP.value"]] <- ABP.signal[min.ABP.sample]
  
  # Gets min ABP type
  min.info <- findpeaks(
    -ans[["min.ABP.window"]],
    zero = "-",
    sortstr = TRUE
  )
  ans[["min.ABP.type"]] <- "minimum value in window"
  if(!is.null(min.info) && min.info[1, 2] == min.sample.in.window)
    ans[["min.ABP.type"]] <- "local minimum"
  
  # Measures min ABP drop
  ans[["min.ABP.drop"]] <-
    ans[["ABP.baseline.value"]] - ans[["min.ABP.value"]]
  ans[["min.ABP.drop.pc"]] <-
    ans[["min.ABP.drop"]] / ans[["ABP.baseline.value"]]
  ans[["min.ABP.drop.pc"]] <- round(ans[["min.ABP.drop.pc"]] * 100, 2)
  
  # Normalises the signal
  ans[["normalised.ABP.signal"]] <- normalise.signal(
    signal = ABP.signal,
    signal.baseline.value = ans[["ABP.baseline.value"]],
    signal.min.value = ans[["min.ABP.value"]]
  )
  
  invisible(ans)
}


normalise.CBFV.signal <- function(
      time.instants,
      CBFV.signal,
      sample.release,
      sampling.time,
      baseline.initial.time = time.instants[1],
      baseline.final.time = time.instants[sample.release],
      min.CBFV.max.delta.time = 8 * 0.8,
      time.tol = sampling.time / 100
      )
{
  ans <- list()
  valid <- !is.na(CBFV.signal)
  time.release <- time.instants[sample.release]
  
  # Adds baseline data to answer
  if(baseline.final.time < baseline.initial.time)
    stop("final time for the CBFV baseline cannot be before its initial time")
  if(baseline.final.time > time.release)
    stop("final time for the CBFV baseline cannot be after the release of the thigh cuffs")
  
	i <- which(valid & time.instants >= baseline.initial.time - time.tol)
  if(length(i) == 0)
    stop("no time instant for the beginning of the CBFV baseline could be determined")
	i <- i[1]
  ans[["CBFV.baseline.initial.time"]] <- time.instants[i]
  ans[["CBFV.baseline.initial.sample"]] <- i
  
	i <- which(valid & time.instants <= baseline.final.time + time.tol)
  if(length(i) == 0)
    stop("no time instant for the end of the CBFV baseline could be determined")
	i <- tail(i, 1)
  ans[["CBFV.baseline.final.time"]] <- time.instants[i]
  ans[["CBFV.baseline.final.sample"]] <- i
  
	ans[["CBFV.baseline.samples"]] <- ans[["CBFV.baseline.initial.sample"]]:ans[["CBFV.baseline.final.sample"]]
	ans[["CBFV.baseline.duration"]] <- ans[["CBFV.baseline.final.time"]] - ans[["CBFV.baseline.initial.time"]]
  
  # Gets baseline value
  ans[["CBFV.baseline.value"]] <- mean(CBFV.signal[ans[["CBFV.baseline.samples"]]], na.rm = TRUE)
  
  # Sets min CBFV window
  ans[["min.CBFV.max.delta.time"]] <- min.CBFV.max.delta.time
  i <- time.instants > time.release + time.tol
  j <- time.instants < time.release + ans[["min.CBFV.max.delta.time"]] +
       time.tol
  ans[["min.CBFV.samples"]] <- which(valid & i & j)
  if(length(ans[["min.CBFV.samples"]]) == 0)
    stop("no candidates for min CBFV")
  
  # Gets minimum CBFV
  ans[["min.CBFV.window"]] <- CBFV.signal[ans[["min.CBFV.samples"]]]
  min.sample.in.window <- which.min(ans[["min.CBFV.window"]])
  min.CBFV.sample <- min.sample.in.window + ans[["min.CBFV.samples"]][1] - 1
  ans[["min.CBFV.time.instant"]] <- time.instants[min.CBFV.sample]
  ans[["min.CBFV.sample"]] <- min.CBFV.sample
  ans[["min.CBFV.value"]] <- CBFV.signal[min.CBFV.sample]
  
  # Gets min CBFV type
  min.info <- findpeaks(
    -ans[["min.CBFV.window"]],
    zero = "-",
    sortstr = TRUE
  )
  ans[["min.CBFV.type"]] <- "minimum value in window"
  if(!is.null(min.info) && min.info[1, 2] == min.sample.in.window)
    ans[["min.CBFV.type"]] <- "local minimum"
  
  # Normalises the signal
  ans[["normalised.CBFV.signal"]] <- normalise.signal(
    signal = CBFV.signal,
    signal.baseline.value = ans[["CBFV.baseline.value"]],
    signal.min.value = ans[["min.CBFV.value"]]
  )
  
  invisible(ans)
}


get.mfARI.parameters <- function(
      time.instants,
      ABP.signal,
      CBFV.signal,
      sampling.time,
      time.release,
      baseline.initial.time = time.instants[1],
      baseline.final.time = time.release,
      min.ABP.max.delta.time = 5 * 0.8,
      transient.ABP.duration = 6,
      min.CBFV.max.delta.time = 8 * 0.8, 
      min.transient.CBFV.duration = 1.5,
      max.transient.CBFV.duration = 10,
      steady.CBFV.duration = 6,
      min.Ks = 0.0,
      max.Ks = 1.022095,
      min.ABP.angle = 0.0,
      max.ABP.angle = 90.0,
      min.CBFV.angle = 0.0,
      max.CBFV.angle = 90.0,
      min.Phi = 0.0,
      max.Phi = 35.238932,
      keep.details = TRUE,
      ABP.rounding.digits = 2,
      CBFV.rounding.digits = 2,
      Ks.rounding.digits = 4,
      Phi.rounding.digits = 2,
      time.tol = sampling.time / 100,
      ...
      )
{
  # Simple validations
  if(transient.ABP.duration < sampling.time)
    stop("duration of the transient ABP signal is too short")
  if(steady.CBFV.duration < sampling.time)
    stop("duration of the steady CBFV signal is too short")
  if(max.transient.CBFV.duration < min.transient.CBFV.duration)
    stop("maximum duration of the transient CBFV signal must be equal or higher than the specified minimum duration")
  if(max.Ks < min.Ks)
    stop("maximum value for Ks must be equal or higher than the specified minimum value")
  if(max.ABP.angle < min.ABP.angle)
    stop("maximum value for the ABP recovery angle must be equal or higher than the specified minimum value")
  if(max.CBFV.angle < min.CBFV.angle)
    stop("maximum value for the CBFV recovery angle must be equal or higher than the specified minimum value")
  if(max.Phi < min.Phi)
    stop("maximum value for the difference between ABP and CBFV recovery angles must be equal or higher than the specified minimum value")
    
  # Validates the lengths of the signals
  if(length(ABP.signal) != length(CBFV.signal))
    stop("ABP and CBFV signals must be of the same length")
  if(length(ABP.signal) != length(time.instants))
    stop("number of time instants must coincide with the signals")
  
  # Validates sampling time
  if(sampling.time < 0.1 - time.tol)
    stop("sampling time must be equal or higher than 0.1")
  
  # Defines the detailed answer list
  dans <- list()
  
  # Copies the original signals
	dans[["time.instants"]] <- round(time.instants, 1)
	dans[["ABP.signal"]] <- round(ABP.signal, ABP.rounding.digits)
	dans[["CBFV.signal"]] <- round(CBFV.signal, CBFV.rounding.digits)
  
  # Sets frequency
	dans[["sampling.time"]] <- round(sampling.time, 1)
	dans[["frequency"]] <- 1 / sampling.time
  
  # Sets release sample
	dans[["time.release"]] <- time.release
  i <- which(are.tolerably.equal(time.instants, time.release, time.tol))
  if(length(i) != 1)
    stop("a unique time instant for the thigh-cuff release could not be determined")
  dans[["sample.release"]] <- i
  if(!is.finite(ABP.signal[i]))
    stop("invalid ABP signal value at the time of thigh-cuff release")
  if(!is.finite(CBFV.signal[i]))
    stop("invalid CBFV signal value at the time of thigh-cuff release")
  
  # Normalises ABP
  nabp <- normalise.ABP.signal(
    time.instants = dans[["time.instants"]],
    ABP.signal = dans[["ABP.signal"]],
    sample.release = dans[["sample.release"]],
    sampling.time = dans[["sampling.time"]],
    baseline.initial.time = baseline.initial.time,
    baseline.final.time = baseline.final.time,
    min.ABP.max.delta.time = min.ABP.max.delta.time,
    time.tol = time.tol
  )
  dans <- c(dans, nabp)
  
  # Normalises CBFV
  ncbfv <- normalise.CBFV.signal(
    time.instants = dans[["time.instants"]],
    CBFV.signal = dans[["CBFV.signal"]],
    sample.release = dans[["sample.release"]],
    sampling.time = dans[["sampling.time"]],
    baseline.initial.time = baseline.initial.time,
    baseline.final.time = baseline.final.time,
    min.CBFV.max.delta.time = min.CBFV.max.delta.time,
    time.tol = time.tol
  )
  dans <- c(dans, ncbfv)
  
  if(any(nabp[["ABP.baseline.samples"]] != ncbfv[["CBFV.baseline.samples"]]))
    warning("baseline samples are different for ABP and CBFV")
  
  # Search for best Ks and delta-tau
  search.results <- search.mfARI.CBFV.parameters(
    time.instants = time.instants,
    normalised.CBFV.signal = dans[["normalised.CBFV.signal"]],
    min.CBFV.sample = dans[["min.CBFV.sample"]],
    min.CBFV.time.instant = dans[["min.CBFV.time.instant"]],
    min.transient.CBFV.duration = min.transient.CBFV.duration,
    max.transient.CBFV.duration = max.transient.CBFV.duration,
    steady.CBFV.duration = steady.CBFV.duration,
    time.tol = time.tol,
    ...)
  dans <- c(dans, search.results)
  
  dans[["Delta.tau"]] <- round(dans[["Delta.tau"]], 1)
  
  # Bounds Ks parameter
  if(dans[["Ks"]] < min.Ks)
    dans[["Ks"]] <- min.Ks
  if(dans[["Ks"]] > max.Ks)
    dans[["Ks"]] <- max.Ks
  dans[["Ks"]] <- round(dans[["Ks"]], Ks.rounding.digits)
    
  # Gets transient ABP representation
  dans[["nominal.transient.ABP.duration"]] <- transient.ABP.duration
  nominal.transient.ABP.end.time <- dans[["min.ABP.time.instant"]] + transient.ABP.duration
  dans[["transient.ABP.samples"]] <- which(time.instants >= dans[["min.ABP.time.instant"]] & time.instants <= nominal.transient.ABP.end.time)
  dans[["transient.ABP.time.instants"]] <- time.instants[dans[["transient.ABP.samples"]]]
  dans[["transient.ABP.duration"]] <- dans[["transient.ABP.time.instants"]][length(dans[["transient.ABP.time.instants"]])] - dans[["min.ABP.time.instant"]]
  dans[["transient.normalised.ABP.signal"]] <- dans[["normalised.ABP.signal"]][dans[["transient.ABP.samples"]]]
  dans[["transient.ABP.slope"]] <- mean(dans[["transient.normalised.ABP.signal"]]) / mean(dans[["transient.ABP.time.instants"]] - dans[["min.ABP.time.instant"]])
  dans[["transient.ABP.line"]] <- dans[["transient.ABP.slope"]] * (time.instants[dans[["transient.ABP.samples"]]] - dans[["min.ABP.time.instant"]])
  
  # Gets transient ABP angle
  dans[["transient.ABP.angle"]] <- atan(dans[["transient.ABP.slope"]]) * 180 / pi
  dans[["bounded.transient.ABP.angle"]] <- dans[["transient.ABP.angle"]]
  if(dans[["bounded.transient.ABP.angle"]] < min.ABP.angle)
    dans[["bounded.transient.ABP.angle"]] <- min.ABP.angle
  if(dans[["bounded.transient.ABP.angle"]] > max.ABP.angle)
    dans[["bounded.transient.ABP.angle"]] <- max.ABP.angle
  
  # Gets transient CBFV angle
  dans[["transient.CBFV.angle"]] <- atan(dans[["transient.CBFV.slope"]]) * 180 / pi
  dans[["bounded.transient.CBFV.angle"]] <- dans[["transient.CBFV.angle"]]
  if(dans[["bounded.transient.CBFV.angle"]] < min.CBFV.angle)
    dans[["bounded.transient.CBFV.angle"]] <- min.CBFV.angle
  if(dans[["bounded.transient.CBFV.angle"]] > max.CBFV.angle)
    dans[["bounded.transient.CBFV.angle"]] <- max.CBFV.angle
  
  # Gets Phi parameter
  dans[["transient.angles.difference"]] <- dans[["transient.CBFV.angle"]] - dans[["transient.ABP.angle"]]
  dans[["bounded.transient.angles.difference"]] <- dans[["bounded.transient.CBFV.angle"]] - dans[["bounded.transient.ABP.angle"]]
  dans[["Phi"]] <- dans[["bounded.transient.angles.difference"]]
  if(dans[["Phi"]] < min.Phi)
    dans[["Phi"]] <- min.Phi
  if(dans[["Phi"]] > max.Phi)
    dans[["Phi"]] <- max.Phi
  dans[["Phi"]] <- round(dans[["Phi"]], Phi.rounding.digits)
  
  if(!keep.details)
  {
    ans <- list()
    
    ans[["ABP.baseline.samples"]] <- dans[["ABP.baseline.samples"]]
    ans[["ABP.baseline.value"]] <- dans[["ABP.baseline.value"]]
    
    ans[["min.ABP.samples"]] <- dans[["min.ABP.samples"]]
    ans[["min.ABP.sample"]] <- dans[["min.ABP.sample"]]
    ans[["min.ABP.value"]] <- dans[["min.ABP.value"]]
    ans[["min.ABP.type"]] <- dans[["min.ABP.type"]]
    ans[["min.ABP.drop.pc"]] <- dans[["min.ABP.drop.pc"]]
    
    ans[["CBFV.baseline.samples"]] <- dans[["CBFV.baseline.samples"]]
    ans[["CBFV.baseline.value"]] <- dans[["CBFV.baseline.value"]]
    
    ans[["min.CBFV.samples"]] <- dans[["min.CBFV.samples"]]
    ans[["min.CBFV.sample"]] <- dans[["min.CBFV.sample"]]
    ans[["min.CBFV.value"]] <- dans[["min.CBFV.value"]]
    ans[["min.CBFV.type"]] <- dans[["min.CBFV.type"]]
    
    ans[["normalised.ABP.signal"]] <- dans[["normalised.ABP.signal"]]
    ans[["normalised.CBFV.signal"]] <- dans[["normalised.CBFV.signal"]]
    
    ans[["transient.ABP.samples"]] <- dans[["transient.ABP.samples"]]
    ans[["transient.ABP.slope"]] <- dans[["transient.ABP.slope"]]
    ans[["transient.ABP.line"]] <- dans[["transient.ABP.line"]]
    
    ans[["transient.CBFV.samples"]] <- dans[["transient.CBFV.samples"]]
    ans[["transient.CBFV.slope"]] <- dans[["transient.CBFV.slope"]]
    ans[["transient.CBFV.line"]] <- dans[["transient.CBFV.line"]]
    ans[["steady.CBFV.samples"]] <- dans[["steady.CBFV.samples"]]
    ans[["steady.CBFV.line"]] <- dans[["steady.CBFV.line"]]
    ans[["steady.CBFV.duration"]] <- dans[["steady.CBFV.duration"]]
    
    ans[["Delta.tau"]] <- dans[["Delta.tau"]]
    ans[["Ks"]] <- dans[["Ks"]]
    ans[["Phi"]] <- dans[["Phi"]]
  }
  else
    ans <- dans
  
  invisible(ans)
}


get.unbounded.mfARI.parameters.for.AT.decimal <- function()
{
  sampling.time <- 0.1
  time.until.release <- 10
  time.after.release <- 20
  smooth.step.stimulus <- FALSE
  filter.order <- 2
  cutoff.frequency <- 0.20
  left.stabilisation.time <- ifelse(smooth.step.stimulus, 30, 0)
  time.rounding.digits <-  1
  stabilisation.time <- 1
  rounding.digits <- 6
  
  min.ABP.max.delta.time <- 5 * 0.8
  transient.ABP.duration <- 6
  min.CBFV.max.delta.time <- 8 * 0.8
  min.transient.CBFV.duration <- 0.1
  max.transient.CBFV.duration <- 20 - 6 - 0.1
  steady.CBFV.duration <- 6
  min.Ks <- -Inf
  max.Ks <- Inf
  min.ABP.angle <- -Inf
  max.ABP.angle <- Inf
  min.CBFV.angle <- -Inf
  max.CBFV.angle <- Inf
  min.Phi <- -Inf
  max.Phi <- Inf
  
  params <- get.AT.decimal.templates.parameters(rounding.digits = rounding.digits)
  curves <- get.AT.decimal.templates(sampling.time = sampling.time,
                                     time.until.release = time.until.release,
                                     time.after.release = time.after.release,
                                     smooth.step.stimulus = smooth.step.stimulus,
                                     filter.order = filter.order,
                                     cutoff.frequency = cutoff.frequency,
                                     left.stabilisation.time = left.stabilisation.time,
                                     time.rounding.digits =  time.rounding.digits,
                                     stabilisation.time = stabilisation.time,
                                     rounding.digits = rounding.digits)
  
  .tmp.fun <- function(i) get.mfARI.parameters(
    time.instants = curves[[i]][["time.instants"]],
    ABP.signal = curves[[i]][["ABP.normalised"]],
    CBFV.signal = curves[[i]][["CBFV.step.response"]],
    sampling.time = curves[[i]][["sampling.time"]],
    time.release = curves[[i]][["time.release"]],
    baseline.initial.time = curves[[i]][["time.instants"]][1],
    baseline.final.time = curves[[i]][["time.release"]],
    min.ABP.max.delta.time = min.ABP.max.delta.time,
    transient.ABP.duration = transient.ABP.duration,
    min.CBFV.max.delta.time = min.CBFV.max.delta.time,
    min.transient.CBFV.duration = min.transient.CBFV.duration,
    max.transient.CBFV.duration = max.transient.CBFV.duration,
    steady.CBFV.duration = steady.CBFV.duration,
    min.Ks = min.Ks,
    max.Ks = max.Ks,
    min.ABP.angle = min.ABP.angle,
    max.ABP.angle = max.ABP.angle,
    min.CBFV.angle = min.CBFV.angle,
    max.CBFV.angle = max.CBFV.angle,
    min.Phi = min.Phi,
    max.Phi = max.Phi,
    rounding.digits = rounding.digits,
    keep.details = FALSE
  )
  mfARI.curves <- lapply(1:length(curves), .tmp.fun)
  
  ATARI <- params[["ARI"]]
  Ks <- sapply(mfARI.curves, function(c) c[["Ks"]])
  Delta.tau <- sapply(mfARI.curves, function(c) c[["Delta.tau"]])
  Phi <- sapply(mfARI.curves, function(c) c[["Phi"]])
  
  data.frame(ATARI, Ks, Delta.tau, Phi)
}


get.bounds.of.mfARI.parameters.for.AT.decimal <- function()
{
  table <- get.unbounded.mfARI.parameters.for.AT.decimal()
  table[["Delta.tau"]][1] <- max(table[["Delta.tau"]][-1])
  
  mins <- c(min(table[["Ks"]]), min(table[["Delta.tau"]]), min(table[["Phi"]]))
  maxs <- c(max(table[["Ks"]]), max(table[["Delta.tau"]]), max(table[["Phi"]]))
  
  ans <- data.frame(matrix(c(mins, maxs), nrow = 3))
  rownames(ans) <- c("Ks", "Delta.tau", "Phi")
  colnames(ans) <- c("Min", "Max")
  
  ans
}


get.bounded.mfARI.parameters.for.AT.decimal <- function()
{
  sampling.time <- 0.1
  time.until.release <- 10
  time.after.release <- 20
  smooth.step.stimulus <- FALSE
  filter.order <- 2
  cutoff.frequency <- 0.20
  left.stabilisation.time <- ifelse(smooth.step.stimulus, 30, 0)
  time.rounding.digits <-  1
  stabilisation.time <- 1
  rounding.digits <- 6
  
  min.ABP.max.delta.time <- 5 * 0.8
  transient.ABP.duration <- 6
  min.CBFV.max.delta.time <- 8 * 0.8
  min.transient.CBFV.duration <-  1.5
  max.transient.CBFV.duration <- 10.0
  steady.CBFV.duration <- 6
  min.Ks <- 0.0
  max.Ks <- 1.022095
  min.ABP.angle <- 0
  max.ABP.angle <- 90
  min.CBFV.angle <- 0
  max.CBFV.angle <- 90
  min.Phi <- 0.0
  max.Phi <- 35.238932
  
  params <- get.AT.decimal.templates.parameters(rounding.digits = rounding.digits)
  curves <- get.AT.decimal.templates(sampling.time = sampling.time,
                                     time.until.release = time.until.release,
                                     time.after.release = time.after.release,
                                     smooth.step.stimulus = smooth.step.stimulus,
                                     filter.order = filter.order,
                                     cutoff.frequency = cutoff.frequency,
                                     left.stabilisation.time = left.stabilisation.time,
                                     time.rounding.digits =  time.rounding.digits,
                                     stabilisation.time = stabilisation.time,
                                     rounding.digits = rounding.digits)
  
  .tmp.fun <- function(i) get.mfARI.parameters(
    time.instants = curves[[i]][["time.instants"]],
    ABP.signal = curves[[i]][["ABP.normalised"]],
    CBFV.signal = curves[[i]][["CBFV.step.response"]],
    sampling.time = curves[[i]][["sampling.time"]],
    time.release = curves[[i]][["time.release"]],
    baseline.initial.time = curves[[i]][["time.instants"]][1],
    baseline.final.time = curves[[i]][["time.release"]],
    min.ABP.max.delta.time = min.ABP.max.delta.time,
    transient.ABP.duration = transient.ABP.duration,
    min.CBFV.max.delta.time = min.CBFV.max.delta.time,
    min.transient.CBFV.duration = min.transient.CBFV.duration,
    max.transient.CBFV.duration = max.transient.CBFV.duration,
    steady.CBFV.duration = steady.CBFV.duration,
    min.Ks = min.Ks,
    max.Ks = max.Ks,
    min.ABP.angle = min.ABP.angle,
    max.ABP.angle = max.ABP.angle,
    min.CBFV.angle = min.CBFV.angle,
    max.CBFV.angle = max.CBFV.angle,
    min.Phi = min.Phi,
    max.Phi = max.Phi,
    rounding.digits = rounding.digits,
    keep.details = FALSE
  )
    
  n <- length(curves)
  mfARI.curves <- lapply(1:n, .tmp.fun)
  
  ATARI <- params[["ARI"]]
  Ks <- sapply(mfARI.curves, function(c) c[["Ks"]])
  Delta.tau <- sapply(mfARI.curves, function(c) c[["Delta.tau"]])
  Delta.tau[1] <- max.transient.CBFV.duration
  Phi <- sapply(mfARI.curves, function(c) c[["Phi"]])
  
  data.frame(ATARI, Ks, Delta.tau, Phi)
}


get.ATARI.mfARI.linear.relationship <- function()
{
  params <<- get.bounded.mfARI.parameters.for.AT.decimal()
  model <- lm(ATARI ~ Ks + Delta.tau + Phi, data = params)
  print(summary(model))
  
  predictions <- stats::predict(model, newdata = params, interval = "confidence")
  colnames(predictions) <- c("mfARI", "lwr", "upr")
  params <- cbind(params, predictions)
  
  p1 <- ggplot(params, aes(x = ATARI, y = Ks))
  p1 <- p1 + geom_line() + geom_point()
  p2 <- ggplot(params, aes(x = ATARI, y = Delta.tau))
  p2 <- p2 + geom_line() + geom_point()
  p3 <- ggplot(params, aes(x = ATARI, y = Phi))
  p3 <- p3 + geom_line() + geom_point()
  
  i <- complete.cases(params)
  print(params[!i])
  print(params)
  
  g <- ggplot(data = params, aes(x=ATARI, y = mfARI))
  g <- g + geom_point()
  g <- g + stat_smooth(method = "lm", colour = "red")
  g <- g + xlim(0, 9) + ylim(0, 9)
  p <- arrangeGrob(p1, p2, p3, g)
  #capture.output(print(summary(m), prmsd=TRUE, digits=1))
  
  list(model, p)
}


get.ATARI.mfARI.linear.relationship.no.Phi <- function()
{
  params <<- get.bounded.mfARI.parameters.for.AT.decimal()
  model <- lm(ATARI ~ Ks + Delta.tau, data = params)
  print(summary(model))
  
  predictions <- stats::predict(model, newdata = params, interval = "confidence")
  colnames(predictions) <- c("mfARI", "lwr", "upr")
  params <- cbind(params, predictions)
  
  p1 <- ggplot(params, aes(x = ATARI, y = Ks))
  p1 <- p1 + geom_line() + geom_point()
  p2 <- ggplot(params, aes(x = ATARI, y = Delta.tau))
  p2 <- p2 + geom_line() + geom_point()
  p3 <- ggplot(params, aes(x = ATARI, y = Phi))
  p3 <- p3 + geom_line() + geom_point()
  
  g <- ggplot(data = params, aes(x=ATARI, y = mfARI))
  g <- g + geom_point()
  g <- g + stat_smooth(method = "lm", colour = "red")
  g <- g + xlim(0, 9) + ylim(0, 9)
  p <- arrangeGrob(p1, p2, p3, g)
  #capture.output(print(summary(m), prmsd=TRUE, digits=1))
  
  list(model, p)
}

