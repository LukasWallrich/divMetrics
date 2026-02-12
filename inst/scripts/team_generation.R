# Internal helpers for pkgdown articles and simulations

# Draw Dirichlet weights without extra dependencies.
dirichlet_weights <- function(k, concentration = 1) {
  if (!is.numeric(k) || length(k) != 1 || k < 1) stop("`k` must be a positive integer.")
  if (!is.numeric(concentration) || length(concentration) != 1 || concentration <= 0) {
    stop("`concentration` must be a single positive number.")
  }
  w <- stats::rgamma(k, shape = concentration, rate = 1)
  w / sum(w)
}

#' Generate age-based teams for vignette simulations (internal)
#'
#' This generator is designed to produce plausible team compositions that
#' decouple (a) how wide a range of ages a team covers from (b) how evenly the
#' team is distributed across that range. This helps illustrate divergences
#' between CEI (coverage + evenness), Blau (bin-based variety), and
#' variance-based measures (SD/CV).
#'
#' Mechanisms:
#' - "single_cohort": one age stratum (very homogeneous).
#' - "career_ladder": adjacent strata with hierarchical (uneven) staffing.
#' - "balanced_roles": multiple strata spread across a wider span, fairly even.
#' - "compact_balanced": multiple strata but within a narrow span (high Blau but low coverage).
#' - "tokenism": one dominant stratum plus 1–2 "token" members far away.
#'
#' @param size Team size (integer >= 2).
#' @param n_teams Number of teams to generate (integer >= 1).
#' @param bounds Numeric length-2 vector of theoretical min and max age.
#' @param bin_width Width of age strata (years). Used only for generation (not
#'   for CEI computation).
#' @param profile_probs Named numeric vector of probabilities for profiles.
#' @param p_two_tokens For tokenism teams, probability of having two tokens
#'   (rather than one).
#'
#' @return A tibble with columns `team_id`, `age`, `team_size`,
#'   `diversity_profile`, and `diversity_prop` (coverage target on [0,1]).
#'
#' @keywords internal
#' @noRd
generate_age_teams <- function(size,
                               n_teams,
                               bounds = c(20, 70),
                               bin_width = 5,
                               profile_probs = c(
                                 single_cohort = 0.15,
                                 career_ladder = 0.25,
                                 balanced_roles = 0.20,
                                 compact_balanced = 0.25,
                                 tokenism = 0.15
                               ),
                               p_two_tokens = 0.35) {
  if (!is.numeric(size) || length(size) != 1 || size < 2) stop("`size` must be an integer >= 2.")
  size <- as.integer(size)
  if (!is.numeric(n_teams) || length(n_teams) != 1 || n_teams < 1) stop("`n_teams` must be an integer >= 1.")
  n_teams <- as.integer(n_teams)
  if (!is.numeric(bounds) || length(bounds) != 2 || bounds[1] >= bounds[2]) stop("`bounds` must be c(min, max) with min < max.")
  if (!is.numeric(bin_width) || length(bin_width) != 1 || bin_width <= 0) stop("`bin_width` must be a single positive number.")

  min_age <- bounds[1]
  max_age <- bounds[2]
  total_span <- max_age - min_age
  sd_within <- bin_width / 3
  breaks <- seq(from = min_age, to = max_age, by = bin_width)
  if (utils::tail(breaks, 1) < max_age) breaks <- c(breaks, max_age)
  n_bins <- length(breaks) - 1
  bin_midpoints <- utils::head(breaks, -1) + diff(breaks) / 2

  profile_probs <- profile_probs / sum(profile_probs)
  if (is.null(names(profile_probs)) || any(names(profile_probs) == "")) {
    stop("`profile_probs` must be a named vector.")
  }
  allowed_profiles <- c("single_cohort", "career_ladder", "balanced_roles", "compact_balanced", "tokenism")
  if (!all(names(profile_probs) %in% allowed_profiles)) {
    stop("`profile_probs` names must be in: ", paste(allowed_profiles, collapse = ", "))
  }

  clamp_round <- function(x) {
    round(pmin(pmax(x, min_age), max_age))
  }

  sample_from_bins <- function(bin_idx, n) {
    mu <- bin_midpoints[bin_idx]
    clamp_round(stats::rnorm(n, mean = mu, sd = sd_within))
  }

  ensure_all_present <- function(counts, size, k) {
    # Ensure each of k bins contributes at least 1 member (when feasible).
    if (k > size) stop("Cannot ensure all bins present when k > size.")
    counts <- as.integer(counts)
    if (sum(counts) != size) counts <- counts + (size - sum(counts))
    zero <- which(counts == 0)
    if (length(zero) == 0) return(counts)
    donors <- order(counts, decreasing = TRUE)
    for (z in zero) {
      donor <- donors[which(counts[donors] > 1)][1]
      if (is.na(donor)) break
      counts[donor] <- counts[donor] - 1L
      counts[z] <- counts[z] + 1L
    }
    counts
  }

  teams <- vector("list", n_teams)

  for (i in seq_len(n_teams)) {
    profile <- sample(names(profile_probs), size = 1, prob = profile_probs)

    if (profile == "single_cohort") {
      k <- 1L
      bin <- sample.int(n_bins, 1)
      ages <- sample_from_bins(bin, size)

    } else if (profile == "career_ladder") {
      # Adjacent strata (e.g., a functional team with a seniority pyramid).
      k <- sample(2:min(4L, size), 1, prob = c(0.55, 0.30, 0.15)[seq_len(min(3L, min(4L, size) - 1L))])
      start_bin <- sample.int(n_bins - k + 1L, 1)
      bins <- start_bin:(start_bin + k - 1L)
      # Skewed allocation: more people at one end of the ladder.
      tilt <- sample(c("younger", "older"), 1)
      base <- 0.65
      weights <- if (tilt == "younger") base^(0:(k - 1L)) else base^((k - 1L):0)
      weights <- weights / sum(weights)
      counts <- as.integer(stats::rmultinom(1, size, prob = weights))
      counts <- ensure_all_present(counts, size = size, k = k)
      ages <- unlist(Map(sample_from_bins, bins, counts), use.names = FALSE)

    } else if (profile == "balanced_roles") {
      # Multiple roles/career stages with deliberate coverage (fairly even).
      k <- sample(3:min(5L, size), 1, prob = c(0.55, 0.30, 0.15)[seq_len(min(3L, min(5L, size) - 2L))])
      # Pick k distinct bins spread across a wide span.
      # This yields high coverage for a given number of occupied bins.
      low <- sample.int(max(1L, n_bins - (k - 1L) * 2L), 1)
      high <- sample.int(n_bins, 1)
      if (high <= low) high <- min(n_bins, low + (k - 1L) * 2L)
      target <- round(seq(low, high, length.out = k))
      target <- pmin(pmax(target, 1L), n_bins)
      bins <- sort(unique(target))
      while (length(bins) < k) {
        bins <- sort(unique(c(bins, sample.int(n_bins, 1))))
      }
      bins <- bins[seq_len(k)]
      weights <- dirichlet_weights(k, concentration = 6)
      counts <- as.integer(stats::rmultinom(1, size, prob = weights))
      counts <- ensure_all_present(counts, size = size, k = k)
      ages <- unlist(Map(sample_from_bins, bins, counts), use.names = FALSE)

    } else if (profile == "compact_balanced") {
      # Multiple strata but within a narrow span (e.g., teams built from a
      # compressed talent pipeline). This tends to yield high Blau but low
      # coverage, which is useful for illustrating divergences.
      k <- sample(3:min(5L, size), 1, prob = c(0.55, 0.30, 0.15)[seq_len(min(3L, min(5L, size) - 2L))])
      # Choose adjacent bins (narrow span).
      start_bin <- sample.int(n_bins - k + 1L, 1)
      bins <- start_bin:(start_bin + k - 1L)
      weights <- dirichlet_weights(k, concentration = 10)
      counts <- as.integer(stats::rmultinom(1, size, prob = weights))
      counts <- ensure_all_present(counts, size = size, k = k)
      ages <- unlist(Map(sample_from_bins, bins, counts), use.names = FALSE)

    } else if (profile == "tokenism") {
      # Dominant cohort + 1–2 far-away tokens (large SD/CV, low evenness).
      k <- if (size >= 5 && stats::runif(1) < p_two_tokens) 3L else 2L
      primary_bin <- sample.int(n_bins, 1)
      if (k == 2L) {
        token_bin <- if (primary_bin <= n_bins / 2) n_bins else 1L
        bins <- c(primary_bin, token_bin)
        counts <- c(size - 1L, 1L)
      } else {
        bins <- c(primary_bin, 1L, n_bins)
        counts <- c(size - 2L, 1L, 1L)
      }
      ages <- unlist(Map(sample_from_bins, bins, counts), use.names = FALSE)

    } else {
      stop("Unhandled profile: ", profile)
    }

    ages <- clamp_round(ages)
    # Randomize order so patterns are not artefacts of construction.
    ages <- sample(ages)

    diversity_prop <- (max(ages) - min(ages)) / total_span

    teams[[i]] <- tibble::tibble(
      team_id = paste0("Team_", size, "_", i),
      age = ages,
      team_size = size,
      diversity_profile = profile,
      diversity_prop = diversity_prop
    )
  }

  dplyr::bind_rows(teams)
}
