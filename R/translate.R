#' Convert an R script or expression to SAS code
#'
#' High-level entry point. Pass either a character string of R code or a
#' file path and get back equivalent SAS syntax as a character string.
#'
#' @param x Character string containing R code, or a path to an .R file when
#'   \code{file = TRUE}.
#' @param file Logical. If TRUE, \code{x} is treated as a file path.
#' @param output_file Optional path to write the SAS output to.
#' @param verbose Logical. Print conversion notes to the console.
#'
#' @return A character string containing SAS code.
#' @export
#'
#' @examples
#' r_code <- "
#' df <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
#' fit <- lm(y ~ x, data = df)
#' summary(fit)
#' "
#' cat(r2sas(r_code))
r2sas <- function(x, file = FALSE, output_file = NULL, verbose = FALSE) {
  if (file) {
    if (!file.exists(x)) stop("File not found: ", x)
    x <- paste(readLines(x), collapse = "\n")
  }

  lines <- strsplit(x, "\n")[[1]]
  sas_lines <- character(0)
  sas_lines <- c(sas_lines, "/* Converted from R to SAS by r2sas package */",
                 "/* Review and adjust as needed - auto-translation is approximate */", "")

  i <- 1
  while (i <= length(lines)) {
    line <- stringr::str_trim(lines[[i]])

    # skip empty or comment lines
    if (nchar(line) == 0) {
      sas_lines <- c(sas_lines, "")
      i <- i + 1
      next
    }
    if (startsWith(line, "#")) {
      sas_lines <- c(sas_lines, paste0("/* ", substring(line, 2), " */"))
      i <- i + 1
      next
    }

    converted <- .convert_line(line, verbose = verbose)
    sas_lines <- c(sas_lines, converted)
    i <- i + 1
  }

  result <- paste(sas_lines, collapse = "\n")
  if (!is.null(output_file)) writeLines(result, output_file)
  result
}

#' Convert a single R expression string to SAS
#'
#' @param expr A single-line R expression as a character string.
#' @param verbose Logical. Print conversion notes.
#' @return Character string of SAS code.
#' @export
#'
#' @examples
#' r2sas_expr("lm(y ~ x, data = df)")
#' r2sas_expr("mean(x, na.rm = TRUE)")
r2sas_expr <- function(expr, verbose = FALSE) {
  .convert_line(stringr::str_trim(expr), verbose = verbose)
}

# ---- internal line dispatcher ----

.convert_line <- function(line, verbose = FALSE) {
  # data.frame() creation
  if (stringr::str_detect(line, "\\bdata\\.frame\\s*\\(")) {
    return(.convert_dataframe(line, verbose))
  }
  # dplyr pipe chains
  if (stringr::str_detect(line, "%>%|\\|>")) {
    return(.convert_dplyr(line, verbose))
  }
  # lm / glm models
  if (stringr::str_detect(line, "\\blm\\s*\\(|\\bglm\\s*\\(")) {
    return(.convert_model(line, verbose))
  }
  # t.test
  if (stringr::str_detect(line, "\\bt\\.test\\s*\\(")) {
    return(.convert_ttest(line, verbose))
  }
  # chisq.test
  if (stringr::str_detect(line, "\\bchisq\\.test\\s*\\(")) {
    return(.convert_chisq(line, verbose))
  }
  # aov / anova
  if (stringr::str_detect(line, "\\baov\\s*\\(|\\banova\\s*\\(")) {
    return(.convert_anova(line, verbose))
  }
  # summary() on a dataset or model
  if (stringr::str_detect(line, "\\bsummary\\s*\\(")) {
    return(.convert_summary(line, verbose))
  }
  # table()
  if (stringr::str_detect(line, "\\btable\\s*\\(")) {
    return(.convert_table(line, verbose))
  }
  # read.csv / read_csv / readRDS / load
  if (stringr::str_detect(line, "\\bread\\.csv\\s*\\(|\\bread_csv\\s*\\(|\\breadRDS\\s*\\(|\\bload\\s*\\(|\\bread\\.table\\s*\\(")) {
    return(.convert_read(line, verbose))
  }
  # write.csv / write_csv
  if (stringr::str_detect(line, "\\bwrite\\.csv\\s*\\(|\\bwrite_csv\\s*\\(")) {
    return(.convert_write(line, verbose))
  }
  # ggplot
  if (stringr::str_detect(line, "\\bggplot\\s*\\(|\\bgeom_")) {
    return(.convert_ggplot(line, verbose))
  }
  # print / cat / message
  if (stringr::str_detect(line, "^\\s*(print|cat|message)\\s*\\(")) {
    return(.convert_print(line, verbose))
  }
  # for loop
  if (stringr::str_detect(line, "^\\s*for\\s*\\(")) {
    return(.convert_for(line, verbose))
  }
  # if / else
  if (stringr::str_detect(line, "^\\s*if\\s*\\(|^\\s*\\}\\s*else")) {
    return(.convert_if(line, verbose))
  }
  # assignment with <- or = (generic variable/arithmetic)
  if (stringr::str_detect(line, "<-|=")) {
    return(.convert_assignment(line, verbose))
  }
  # fallback: comment out unknown line
  if (verbose) message("[r2sas] No rule matched: ", line)
  paste0("/* TODO: ", line, " */")
}
