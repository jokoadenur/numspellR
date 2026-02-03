#' Detect Numeric Persistence and Spell Patterns
#'
#' @description
#' `numspellr()` is a universal numeric persistence detector designed to
#' identify prolonged stability ("spells") and rigidity patterns in numeric
#' data. The function automatically detects numeric variables from various
#' data structures (numeric vectors or data frames) and computes persistence
#' metrics commonly used in rigidity and stickiness analysis.
#'
#' The output includes interpretable measures such as average spell length,
#' stability ratio, elasticity, and adjustment frequency, accompanied by
#' plain-language interpretations suitable for policy and applied analysis.
#'
#' @param data
#' A numeric vector or a data.frame/tibble containing one or more numeric
#' variables. Non-numeric columns are ignored automatically.
#'
#' @param lang
#' Language for persistence status labels and interpretation text.
#' Must be either `"indonesia"` or `"english"`.
#'
#' @param min_spell
#' Minimum length (in periods) required for a sequence to be considered a
#' persistence spell. Default is `2`.
#'
#' @param tol
#' Optional numeric tolerance threshold for detecting stability.
#' If `NULL` (default), tolerance is determined automatically based on
#' variability of first differences.
#'
#' @details
#' The function identifies persistence spells as consecutive periods where
#' absolute changes fall below a tolerance threshold. Metrics computed include:
#'
#' \itemize{
#'   \item \strong{Average spell length} — average duration of stable periods.
#'   \item \strong{Median spell length} — median duration of stability.
#'   \item \strong{Maximum spell length} — longest observed stable period.
#'   \item \strong{Stability ratio} — proportion of observations belonging to spells.
#'   \item \strong{Elasticity index} — average relative magnitude of changes.
#'   \item \strong{Adjustment frequency} — share of periods with meaningful changes.
#'   \item \strong{Spell concentration} — degree to which stability is dominated by
#'   a few long spells.
#' }
#'
#' Persistence status is classified into qualitative categories ranging from
#' highly flexible to highly rigid, with labels adapted to the selected language.
#'
#' @return
#' A data.frame with one row per numeric variable containing:
#'
#' \describe{
#'   \item{structure}{Detected data structure type.}
#'   \item{id}{Variable identifier.}
#'   \item{variable}{Variable name.}
#'   \item{avg_spell}{Average persistence spell length.}
#'   \item{median_spell}{Median persistence spell length.}
#'   \item{max_spell}{Maximum persistence spell length.}
#'   \item{stability_ratio}{Share of observations in stable spells.}
#'   \item{elasticity_index}{Average relative change magnitude.}
#'   \item{adjustment_frequency}{Frequency of meaningful adjustments.}
#'   \item{spell_concentration}{Concentration index of persistence spells.}
#'   \item{persistence_status}{Qualitative persistence category.}
#'   \item{interpretation}{Plain-language interpretation suitable for policy analysis.}
#' }
#'
#' @examples
#' # Example with numeric vector
#' x <- c(10, 10, 10, 11, 11, 11, 11, 12)
#' numspellr(x, lang = "english")
#'
#' # Example with data frame
#' df <- data.frame(
#'   time = 1:8,
#'   value = c(5, 5, 5, 5, 6, 6, 6, 7)
#' )
#' numspellr(df, lang = "indonesia")
#'
#' @references
#' Caballero, R. J., & Engel, E. M. R. A. (1993).
#' Microeconomic adjustment hazards and aggregate dynamics.
#' \emph{Quarterly Journal of Economics}, 108(2), 359–383.
#'
#' Nakamura, E., & Steinsson, J. (2008).
#' Five facts about prices: A reevaluation of menu cost models.
#' \emph{Quarterly Journal of Economics}, 123(4), 1415–1464.
#'
#' @author
#' Joko Nursiyono
#'
#' @seealso
#' \code{\link{diff}}, \code{\link{rle}}
#'
#' @export
numspellr <- function(data,
                      lang = c("indonesia", "english"),
                      min_spell = 2,
                      tol = NULL) {

  lang <- match.arg(lang)

  ##########################################################
  # 1. COERCE INPUT
  ##########################################################

  if (is.numeric(data)) {
    df <- data.frame(x = data)
    structure_type <- ifelse(lang == "indonesia",
                             "vektor_numerik",
                             "numeric_vector")
  } else if (is.data.frame(data)) {
    df <- data
    structure_type <- ifelse(lang == "indonesia",
                             "seri_numerik",
                             "numeric_series")
  } else {
    stop("Struktur data tidak dikenali / Data structure not supported")
  }

  num_vars <- names(df)[sapply(df, is.numeric)]
  if (length(num_vars) == 0)
    stop("Tidak ada variabel numerik yang terdeteksi")

  ##########################################################
  # 2. CORE ENGINE
  ##########################################################

  spell_engine <- function(x) {

    x <- as.numeric(x)
    x <- x[!is.na(x)]
    n <- length(x)
    if (n < min_spell + 1) return(NULL)

    dx <- diff(x)

    tol_use <- if (is.null(tol)) {
      max(sd(dx, na.rm = TRUE), .Machine$double.eps)
    } else tol

    stable <- abs(dx) <= tol_use

    r <- rle(stable)
    spells <- r$lengths[r$values] + 1
    spells <- spells[spells >= min_spell]
    if (length(spells) == 0) return(NULL)

    ##########################################################
    # 3. METRICS
    ##########################################################

    avg_spell <- mean(spells)
    median_spell <- median(spells)
    max_spell <- max(spells)

    stability_ratio <- sum(spells) / n
    adjustment_frequency <- sum(!stable) / n
    elasticity_index <- mean(abs(dx / x[-length(x)]), na.rm = TRUE)
    spell_concentration <- sum(spells^2) / (sum(spells)^2)

    ##########################################################
    # 4. PERSISTENCE STATUS (BILINGUAL)
    ##########################################################

    if (lang == "indonesia") {
      persistence_status <- cut(
        stability_ratio,
        c(-Inf, .2, .4, .6, .8, Inf),
        labels = c(
          "Sangat Fleksibel",
          "Relatif Fleksibel",
          "Sedang",
          "Kaku",
          "Sangat Kaku"
        )
      )
    } else {
      persistence_status <- cut(
        stability_ratio,
        c(-Inf, .2, .4, .6, .8, Inf),
        labels = c(
          "Very Low Persistence",
          "Low Persistence",
          "Moderate Persistence",
          "High Persistence",
          "Very High Persistence"
        )
      )
    }

    ##########################################################
    # 5. INTERPRETATION (PLAIN LANGUAGE)
    ##########################################################

    if (lang == "indonesia") {

      interp_spell <- paste0(
        "Rata-rata nilai bertahan sekitar ",
        round(avg_spell, 1),
        " periode sebelum mengalami perubahan. "
      )

      interp_max <- if (max_spell >= 10) {
        paste0("Terdapat fase panjang hingga ",
               max_spell,
               " periode tanpa perubahan berarti. ")
      } else ""

      interp_freq <- if (adjustment_frequency < 0.05) {
        "Perubahan sangat jarang terjadi. "
      } else if (adjustment_frequency < 0.2) {
        "Perubahan terjadi namun relatif jarang. "
      } else {
        "Perubahan cukup sering terjadi. "
      }

      interp_elastic <- if (elasticity_index < 0.05) {
        "Jika berubah, besarnya perubahan sangat kecil. "
      } else if (elasticity_index < 0.5) {
        "Perubahan terjadi secara bertahap. "
      } else {
        "Perubahan cenderung besar dan terasa. "
      }

      interp_conc <- if (spell_concentration > 0.2) {
        "Periode stagnan terkonsentrasi pada beberapa fase panjang. "
      } else {
        "Periode stagnan tersebar pada banyak fase pendek. "
      }

      interp_policy <- if (stability_ratio > 0.8 &&
                           elasticity_index < 0.05) {
        "Dari sisi kebijakan, variabel ini tergolong sangat kaku atau stagnan. Intervensi kecil kemungkinan besar tidak efektif; diperlukan kebijakan yang kuat dan struktural."
      } else if (adjustment_frequency > 0.25 &&
                 elasticity_index > 0.5) {
        "Variabel ini cukup responsif. Perubahan kebijakan relatif cepat tercermin pada nilainya."
      } else {
        "Variabel menunjukkan penyesuaian terbatas dan cenderung lambat merespons perubahan."
      }

      interpretation <- paste(
        interp_spell,
        interp_max,
        interp_freq,
        interp_elastic,
        interp_conc,
        interp_policy
      )

    } else {

      interpretation <- paste0(
        "The series shows numeric persistence. ",
        "Values remain unchanged for about ",
        round(avg_spell, 1),
        " periods on average. ",
        ifelse(adjustment_frequency < 0.1,
               "Adjustments are infrequent. ",
               "Adjustments occur periodically. "),
        ifelse(elasticity_index < 0.05,
               "When changes occur, their magnitude is small. ",
               "Changes tend to be sizable. "),
        ifelse(stability_ratio > 0.8,
               "From a policy perspective, this indicates strong rigidity and slow response.",
               "The variable shows moderate adaptability to change.")
      )
    }

    ##########################################################
    # 6. OUTPUT
    ##########################################################

    data.frame(
      structure = structure_type,
      avg_spell = avg_spell,
      median_spell = median_spell,
      max_spell = max_spell,
      stability_ratio = stability_ratio,
      elasticity_index = elasticity_index,
      adjustment_frequency = adjustment_frequency,
      spell_concentration = spell_concentration,
      persistence_status = as.character(persistence_status),
      interpretation = interpretation,
      stringsAsFactors = FALSE
    )
  }

  ##########################################################
  # 7. APPLY TO ALL NUMERIC VARIABLES
  ##########################################################

  res <- lapply(num_vars, function(v) {
    out <- spell_engine(df[[v]])
    if (!is.null(out)) {
      out$id <- v
      out$variable <- v
      out
    }
  })

  res <- do.call(rbind, res)
  rownames(res) <- NULL
  res
}
