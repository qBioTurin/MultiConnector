#' Plot maximum-discrimination functions
#'
#' Computes the time-dependent discriminant weights associated with the fitted
#' projection directions. Measures are ordered by the total absolute area under
#' their discriminant functions.
#'
#' @param CONNECTORDataClustered A [CONNECTORDataClustered-class] object.
#' @param absvalue Logical; if `TRUE`, plot absolute weights. If `FALSE`, retain
#'   their signs.
#' @return
#' MaximumDiscriminationFunction returns a list containing:
#' \itemize{
#'   \item \code{DiscrFunctionsPlot}: A ggplot object showing overlapping discriminant functions for each measure in a grid.
#'   \item \code{Separated}: A ggplot object showing separated discriminant functions, ordered by their total absolute area.
#'   \item \code{measure_areas}: Total absolute coefficient area per measure; this is not a percentage of clustering importance.
#'   \item \code{LocalSeparationPlot}: A ggplot comparing the local separation index across measures on a common vertical scale.
#'   \item \code{local_separation}: A data frame with \code{measureID}, \code{Time}, \code{BetweenVariance}, \code{WithinVariance}, and \code{LocalSeparation}.
#'   \item \code{separation_summary}: Per-measure \code{MeanSeparation} and \code{Area}, integrated over the common interval \code{TimeStart} to \code{TimeEnd}, sorted by decreasing mean separation.
#'   \item \code{SeparationSummaryPlot}: A horizontal bar chart of mean local separation over the common time interval.
#'   \item \code{SeparationOverview}: A patchwork combining the local separation and summary plots.
#'   \item \code{separation_intervals}: Background-band cells with \code{TimeStart}, \code{TimeEnd}, and the leading \code{measureID}; \code{NA} denotes ties or unavailable comparisons.
#' }
#' @details
#' The local index is \eqn{J_m(t) = \sum_k \pi_k
#' (\mu_{km}(t) - \bar\mu_m(t))^2 / v_m(t)}, where cluster means and
#' proportions come from the fitted model and
#' \eqn{v_m(t) = s_m(t)\Gamma s_m(t)^T + \sigma_m}.
#' It measures marginal separation relative to within-cluster variation, not
#' additional information conditional on other measures. It is independent of
#' \code{absvalue} and is not a probability or an importance percentage.
#' The summary uses trapezoidal integration on each measure's time grid within
#' the intersection of all time ranges, with linear interpolation at its
#' endpoints. Mean separation is area divided by interval duration. If the
#' common interval has no positive duration, summary scores are \code{NA} and
#' a warning is issued. Nonpositive within-cluster variances produce \code{NA}
#' indices and a warning; affected summary scores are also \code{NA}.
#' Plot colors identify the same measures across all panels. Discriminant
#' directions are distinguished by line type. Shading under weights extends to
#' zero and does not represent uncertainty. Background bands in the local
#' separation plot compare linearly interpolated indices at the midpoints of
#' 400 equal cells over the common interval. They approximate which measure has
#' the largest local index; ties within relative tolerance \code{1e-8} (with
#' scale at least one) and missing comparisons are grey. No extrapolation or
#' confidence intervals are used. Bands are omitted for a single measure or
#' an empty common interval.
#'
#' @examples
#' \dontrun{
#' discrimination <- MaximumDiscriminationFunction(clustered_data)
#' discrimination$DiscrFunctionsPlot
#' discrimination$measure_areas
#' discrimination$LocalSeparationPlot
#' discrimination$local_separation
#' discrimination$separation_summary
#' discrimination$SeparationOverview
#' discrimination$SeparationSummaryPlot
#' }
#'
#' @seealso [DiscriminantPlot()], [selectCluster()]
#'
#' @author Cordero Francesca, Pernice Simone, Sirovich Roberta
#'
#' @references
#' Gareth M. James and Catherine A. Sugar, (2003). Clustering for Sparsely Sampled Functional Data. Journal of the American Statistical Association.
#'
#'
#'
#' @import ggplot2
#' @importFrom Matrix bdiag
#' @importFrom dplyr select filter
#' @export
#'
setGeneric("MaximumDiscriminationFunction", function(CONNECTORDataClustered, absvalue = TRUE) {
  standardGeneric("MaximumDiscriminationFunction")
})

#' @rdname MaximumDiscriminationFunction
setMethod("MaximumDiscriminationFunction", signature(CONNECTORDataClustered = "CONNECTORDataClustered"), function(CONNECTORDataClustered, absvalue = TRUE) {
  if (!inherits(CONNECTORDataClustered, "CONNECTORDataClustered")) {
    stop("Input must be of class 'CONNECTORDataClustered'. Current class: ", class(CONNECTORDataClustered))
  }

  parameters <- CONNECTORDataClustered@CfitandParameters$cfit$parameters
  DiscriminantResults <- list()
  FullS <- CONNECTORDataClustered@KData$FullS
  sigma <- parameters$sigma
  M <- getMeasures(CONNECTORDataClustered)

  for (j in M) {
    A <- FullS[[j]]
    if (j == M[[1]]) {
      Si <- A
    } else {
      Si <- bdiag(Si, A)
    } # Questo if è necessario poiché bdiag ha bisogno di una matrice iniziale su cui attaccarsi (ovvero quella della prima misura)
  }

  S <- as.matrix(Si)

  nt <- nrow(S)
  Gamma <- parameters$Gamma

 sigma_rows <- unlist(lapply(M, function(m) {
  rep(unname(sigma[as.character(m)]), nrow(FullS[[m]]))
}), use.names = FALSE)

  Sigma <- S %*% Gamma %*% t(S) + diag(sigma_rows, nrow = nrow(S))
  discrim <- as.data.frame(solve(Sigma, S %*% parameters$Lambda))

  #Sigma <- S %*% Gamma %*% t(S) + sigma * diag(nt)
  #Lambda <- parameters$Lambda
  #discrim <- as.data.frame(solve(Sigma) %*% S %*% Lambda)

  colnames(discrim) <- paste0("DiscrFunc", 1:ncol(discrim))


  if (absvalue) {
    discrim <- abs(discrim)
  }
  n <- ncol(discrim)
  DiscrList <- lapply(1:n, function(x) {
    data.frame(
      Time = unlist(CONNECTORDataClustered@KData$TimeGrids),
      DiscrFunc = discrim[, x],
      DiscrNumber = paste0("DiscrFunc", x)
    )
  })

  q <- lapply(M, function(i) {
    rep(i, length(CONNECTORDataClustered@KData$TimeGrids[[i]]))
  })

  measureID <- unlist(q)

  separation <- .local_separation_index(
    S, parameters, sigma_rows,
    unlist(CONNECTORDataClustered@KData$TimeGrids[M], use.names = FALSE),
    measureID
  )

  # Aggiungi measureID a tutti gli elementi di DiscrList (non solo al primo)
  for (i in 1:length(DiscrList)) {
    DiscrList[[i]]$measureID <- measureID
  }
  DiscrFrame <- do.call("rbind", DiscrList)

  # Reorder measureID based on the sum of areas under the curves
  measure_areas <- DiscrFrame %>%
    group_by(measureID, DiscrNumber) %>%
    arrange(Time, .by_group = TRUE) %>%
    summarise(Area = sum(diff(Time) * (abs(DiscrFunc[-1]) + abs(DiscrFunc[-length(DiscrFunc)])) / 2), .groups = "drop") %>%
    group_by(measureID) %>%
    summarise(TotalArea = sum(Area), .groups = "drop") %>%
    arrange(desc(TotalArea))

  DiscrFrame$measureID <- factor(DiscrFrame$measureID, levels = measure_areas$measureID)

  DiscriminantFunctions <- .discrimination_plots(
    DiscrFrame, measure_areas, separation, absvalue, M
  )

  return(DiscriminantFunctions)
})

# Compute marginal separation without forming another time-by-time covariance.
.local_separation_index <- function(S, parameters, sigma_rows, times, measureID) {
  alpha <- as.matrix(parameters$alpha)
  pi <- as.numeric(parameters$pi)
  if (length(pi) != nrow(alpha) || any(!is.finite(pi)) ||
      any(pi < 0) || sum(pi) <= 0) {
    stop("Cluster proportions must be finite, nonnegative, and match alpha.")
  }
  pi <- pi / sum(pi)
  if (length(times) != nrow(S) || length(measureID) != nrow(S) ||
      length(sigma_rows) != nrow(S) || any(!is.finite(times))) {
    stop("Time grids, measures, and residual variances must match the spline rows.")
  }
  # The shared mean cancels; centering alpha avoids subtracting large means.
  centered_alpha <- sweep(alpha, 2, as.vector(crossprod(pi, alpha)))
  centered_means <- S %*% parameters$Lambda %*% t(centered_alpha)
  between <- as.vector(centered_means^2 %*% pi)
  within <- rowSums((S %*% parameters$Gamma) * S) + sigma_rows
  invalid <- !is.finite(within) | within <= 0 | !is.finite(between)
  index <- rep(NA_real_, length(within))
  index[!invalid] <- between[!invalid] / within[!invalid]
  if (any(invalid)) {
    warning("Nonpositive or nonfinite within-cluster variance or nonfinite between-cluster variance: local separation is NA.")
  }
  values <- data.frame(measureID = as.character(measureID), Time = times,
                       BetweenVariance = between, WithinVariance = within,
                       LocalSeparation = index)
  measures <- unique(values$measureID)
  ranges <- lapply(measures, function(m) range(times[measureID == m]))
  start <- max(vapply(ranges, `[`, numeric(1), 1L))
  end <- min(vapply(ranges, `[`, numeric(1), 2L))
  if (end <= start) {
    warning("Time grids have no common interval of positive duration: separation summary is NA.")
  }
  summary <- do.call(rbind, lapply(measures, function(m) {
    z <- values[values$measureID == m, ]
    z <- z[order(z$Time), ]
    area <- NA_real_
    if (end > start && all(is.finite(z$LocalSeparation))) {
      grid <- sort(unique(c(start, z$Time[z$Time > start & z$Time < end], end)))
      y <- stats::approx(z$Time, z$LocalSeparation, xout = grid)$y
      area <- sum(diff(grid) * (head(y, -1L) + tail(y, -1L)) / 2)
    }
    data.frame(measureID = m, TimeStart = start, TimeEnd = end,
               Area = area, MeanSeparation = if (end > start) area / (end - start) else NA_real_)
  }))
  summary <- summary[order(summary$MeanSeparation, decreasing = TRUE, na.last = TRUE), ]
  rownames(summary) <- NULL
  list(values = values, summary = summary)
}

# Background bands are descriptive comparisons, never uncertainty intervals.
.separation_bands <- function(values, bins = 400L) {
  measures <- unique(as.character(values$measureID))
  empty <- data.frame(TimeStart = numeric(), TimeEnd = numeric(),
                      measureID = character())
  if (length(measures) < 2L) return(empty)
  groups <- lapply(measures, function(m) values[values$measureID == m, ])
  start <- max(vapply(groups, function(z) min(z$Time), numeric(1)))
  end <- min(vapply(groups, function(z) max(z$Time), numeric(1)))
  if (!is.finite(start) || !is.finite(end) || end <= start) return(empty)
  edges <- seq(start, end, length.out = bins + 1L)
  mid <- (head(edges, -1L) + tail(edges, -1L)) / 2
  scores <- vapply(groups, function(z) {
    if (sum(is.finite(z$LocalSeparation)) < 2L) return(rep(NA_real_, bins))
    stats::approx(z$Time, z$LocalSeparation, xout = mid, na.rm = FALSE)$y
  }, numeric(bins))
  winner <- apply(scores, 1, function(v) {
    if (any(!is.finite(v))) return(NA_character_)
    best <- max(v)
    tied <- abs(v - best) <= 1e-8 * max(1, abs(best))
    if (sum(tied) != 1L) return(NA_character_)
    measures[which.max(v)]
  })
  data.frame(TimeStart = head(edges, -1L), TimeEnd = tail(edges, -1L),
             measureID = winner)
}

.discrimination_plots <- function(discr, areas, separation, absvalue, measures) {
  # Fixed mapping across every panel; sorting the rankings never changes colors.
  palette <- stats::setNames(grDevices::hcl.colors(length(measures), "Dark 3"), measures)
  style <- theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          strip.text = element_text(face = "bold"),
          plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(colour = "grey35"),
          plot.caption = element_text(colour = "grey40", hjust = 0),
          legend.position = "bottom")
  weight_label <- if (absvalue) "Absolute discriminant weight" else "Signed discriminant weight"
  direction_labels <- stats::setNames(
    paste("Direction", seq_along(unique(discr$DiscrNumber))), unique(discr$DiscrNumber))
  weights <- ggplot(discr, aes(Time, DiscrFunc, colour = measureID,
                               linetype = DiscrNumber, group = DiscrNumber)) +
    geom_ribbon(aes(ymin = pmin(0, DiscrFunc), ymax = pmax(0, DiscrFunc),
                    fill = measureID), alpha = .10, colour = NA, show.legend = FALSE) +
    geom_hline(yintercept = 0, colour = "grey65", linewidth = .35) +
    geom_line(linewidth = .85) +
    facet_wrap(~measureID) +
    scale_colour_manual(values = palette, guide = "none") +
    scale_fill_manual(values = palette, guide = "none") +
    scale_linetype_discrete(labels = direction_labels) +
    labs(title = "How the model weights each part of a curve",
         subtitle = "Compare directions within each measure; larger weights do not imply greater importance.",
         x = "Time", y = weight_label, linetype = NULL) + style
  separate <- weights + facet_grid(measureID ~ DiscrNumber,
                                   labeller = labeller(DiscrNumber = direction_labels)) +
    labs(title = "Discriminant weights, one direction at a time") +
    guides(linetype = "none")

  values <- separation$values
  bands <- .separation_bands(values)
  local <- ggplot(values, aes(Time, LocalSeparation, colour = measureID)) +
    geom_rect(data = bands, aes(xmin = TimeStart, xmax = TimeEnd,
                               ymin = -Inf, ymax = Inf, fill = measureID),
              inherit.aes = FALSE, alpha = .10, colour = NA, show.legend = FALSE) +
    geom_line(linewidth = 1, na.rm = TRUE) +
    scale_colour_manual(values = palette) +
    scale_fill_manual(values = palette, na.value = "grey80", guide = "none") +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, .06))) +
    labs(title = "When does each measure separate the clusters?",
         subtitle = "Higher J(t) means greater separation relative to within-cluster variation.",
         x = "Time", y = "Local separation J(t)", colour = "Measure",
         caption = if (nrow(bands)) paste(
           "Background color: measure with the highest J(t), within the shared time range.",
           "Approximate intervals; grey = tie or unavailable comparison. No confidence bands.", sep = "\n") else
           "Local separation describes each measure on its own; it is not a percentage of importance.") + style

  summary <- separation$summary
  summary$measureID <- factor(summary$measureID, levels = rev(summary$measureID))
  summary$Label <- ifelse(is.finite(summary$MeanSeparation),
                         formatC(summary$MeanSeparation, digits = 3, format = "fg"),
                         "Unavailable")
  ranking <- ggplot(summary, aes(x = MeanSeparation, y = measureID, fill = measureID)) +
    geom_col(width = .6, na.rm = TRUE) +
    geom_text(aes(x = ifelse(is.finite(MeanSeparation), MeanSeparation, 0), label = Label),
              hjust = -.15, size = 3.5) +
    scale_fill_manual(values = palette, guide = "none") +
    scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, .25))) +
    labs(title = "Which measure separates the clusters most on average?",
         subtitle = if (all(is.finite(summary$MeanSeparation)))
           sprintf("Average J(t) over the same interval: %.3g to %.3g.",
                   summary$TimeStart[1], summary$TimeEnd[1]) else
           "Unavailable scores indicate insufficient overlap or undefined local indices.",
         x = "Mean local separation", y = NULL,
         caption = "Marginal separation, not additional information beyond other measures.") + style
  overview <- patchwork::wrap_plots(local, ranking, ncol = 1, heights = c(2, 1))
  list(DiscrFunctionsPlot = weights, Separated = separate, measure_areas = areas,
       LocalSeparationPlot = local, local_separation = values,
       separation_summary = separation$summary, SeparationSummaryPlot = ranking,
       SeparationOverview = overview, separation_intervals = bands)
}
