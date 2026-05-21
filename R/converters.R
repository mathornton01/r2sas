#' Internal converters for specific R constructs
#'
#' These functions handle specific patterns and return SAS code strings.
#' Not exported; called by .convert_line() dispatcher.
#' @keywords internal
#' @name converters
NULL

# ---- data.frame ----

.convert_dataframe <- function(line, verbose = FALSE) {
  # Extract assignment target if present
  target <- stringr::str_match(line, "^\\s*(\\w+)\\s*<?-")[, 2]
  dsname <- if (!is.na(target)) target else "work_data"
  if (verbose) message("[r2sas] data.frame -> DATA step for: ", dsname)

  c(
    paste0("/* R: ", line, " */"),
    paste0("DATA ", toupper(dsname), ";"),
    "  /* Manually transcribe column vectors as INPUT/DATALINES or set from another dataset */",
    "  /* Example: INPUT x y; DATALINES; 1 4 / 2 5 / 3 6; */",
    "RUN;",
    ""
  )
}

# ---- dplyr ----

.convert_dplyr <- function(line, verbose = FALSE) {
  if (verbose) message("[r2sas] dplyr pipe: ", line)
  sas <- c(paste0("/* R (dplyr): ", line, " */"))

  if (stringr::str_detect(line, "\\bfilter\\s*\\(")) {
    cond <- .extract_args(line, "filter")
    if (is.na(cond)) cond <- stringr::str_match(line, "filter\\s*\\(([^)]+)\\)")[, 2]
    cond_sas <- .r_to_sas_cond(cond)
    sas <- c(sas, "DATA filtered;", paste0("  SET source_data;"),
             paste0("  WHERE ", cond_sas, ";"), "RUN;", "")
  } else if (stringr::str_detect(line, "\\bselect\\s*\\(")) {
    vars <- stringr::str_match(line, "select\\s*\\(([^)]+)\\)")[, 2]
    sas <- c(sas, "DATA selected;",
             paste0("  SET source_data (KEEP=", gsub(",\\s*", " ", vars), ");"),
             "RUN;", "")
  } else if (stringr::str_detect(line, "\\bgroup_by\\s*\\(.*summarise|\\bgroup_by\\s*\\(.*summarize")) {
    by_vars <- stringr::str_match(line, "group_by\\s*\\(([^)]+)\\)")[, 2]
    sas <- c(sas, "PROC MEANS DATA=source_data NOPRINT;",
             paste0("  CLASS ", by_vars, ";"),
             "  VAR numeric_vars;",
             "  OUTPUT OUT=summary_data MEAN= N= SUM= / AUTONAME;",
             "RUN;", "")
  } else if (stringr::str_detect(line, "\\bmutate\\s*\\(")) {
    sas <- c(sas, "DATA mutated;", "  SET source_data;",
             "  /* Add computed columns here */",
             "RUN;", "")
  } else if (stringr::str_detect(line, "\\barrange\\s*\\(")) {
    by_vars <- stringr::str_match(line, "arrange\\s*\\(([^)]+)\\)")[, 2]
    desc <- stringr::str_detect(line, "desc\\s*\\(")
    order <- if (desc) "DESCENDING" else ""
    sas <- c(sas, paste0("PROC SORT DATA=source_data OUT=sorted_data;"),
             paste0("  BY ", order, " ", gsub("desc\\(|\\)", "", by_vars), ";"),
             "RUN;", "")
  } else if (stringr::str_detect(line, "\\bleft_join\\s*\\(|\\binner_join\\s*\\(|\\bfull_join\\s*\\(")) {
    jtype <- if (stringr::str_detect(line, "left_join")) "LEFT=a"
              else if (stringr::str_detect(line, "inner_join")) ""
              else "LEFT=a RIGHT=b"
    sas <- c(sas, "DATA merged;",
             paste0("  MERGE left_ds (IN=a) right_ds (IN=b);"),
             "  BY key_variable;",
             if (jtype != "") paste0("  IF ", jtype, ";") else "  IF a OR b;",
             "RUN;", "")
  } else {
    sas <- c(sas, "/* TODO: translate this dplyr chain manually */", "")
  }
  sas
}

# ---- models ----

#' Convert R model calls to SAS PROC steps
#'
#' @param line Character string with lm() or glm() call
#' @param verbose Logical
#' @return Character vector of SAS lines
#' @export
r2sas_model <- function(line, verbose = FALSE) {
  paste(.convert_model(line, verbose), collapse = "\n")
}

.convert_model <- function(line, verbose = FALSE) {
  if (verbose) message("[r2sas] model: ", line)
  is_glm <- stringr::str_detect(line, "\\bglm\\s*\\(")
  family <- stringr::str_match(line, "family\\s*=\\s*(\\w+)")[, 2]
  formula_m <- stringr::str_match(line, "(lm|glm)\\s*\\(([^,]+),")
  formula_str <- if (!is.na(formula_m[, 3])) stringr::str_trim(formula_m[, 3]) else "y ~ x"
  parts <- strsplit(formula_str, "~")[[1]]
  dep <- stringr::str_trim(parts[1])
  indep <- if (length(parts) > 1) stringr::str_trim(parts[2]) else "x"
  indep_sas <- gsub("\\+", " ", indep)
  data_m <- stringr::str_match(line, "data\\s*=\\s*(\\w+)")
  dsname <- if (!is.na(data_m[, 2])) data_m[, 2] else "mydata"

  proc_name <- if (!is_glm || is.na(family) || family == "gaussian") {
    "REG"
  } else if (family == "binomial") {
    "LOGISTIC"
  } else if (family == "poisson") {
    "GENMOD"
  } else {
    "GENMOD"
  }

  sas <- c(paste0("/* R: ", line, " */"),
           paste0("PROC ", proc_name, " DATA=", dsname, ";"),
           paste0("  MODEL ", dep, " = ", indep_sas, ";"),
           "RUN;", "")

  if (proc_name == "GENMOD") {
    dist <- switch(family,
                   poisson = "POISSON",
                   gamma   = "GAMMA",
                   "NORMAL")
    sas[3] <- paste0("  MODEL ", dep, " = ", indep_sas, " / DIST=", dist, ";")
  }
  sas
}

# ---- t.test ----

.convert_ttest <- function(line, verbose = FALSE) {
  if (verbose) message("[r2sas] t.test: ", line)
  args <- .extract_args(line, "t\\.test")
  if (is.na(args)) args <- stringr::str_match(line, "t\\.test\\s*\\(([^)]+)\\)")[, 2]
  mu_m <- stringr::str_match(args, "mu\\s*=\\s*([\\d.]+)")
  mu <- if (!is.na(mu_m[, 2])) mu_m[, 2] else ""
  # two-sample: has a second positional arg that is NOT a named param (mu=, var.equal=, etc.)
  two_sample <- !stringr::str_detect(args, "^\\s*[\\w.]+\\s*,\\s*mu\\s*=") &&
    !stringr::str_detect(args, "^\\s*[\\w.]+\\s*$") &&
    stringr::str_detect(args, "^\\s*[\\w.]+\\s*,\\s*[a-zA-Z][\\w.]*\\s*(,|$)")

  if (two_sample) {
    c(paste0("/* R: ", line, " */"),
      "PROC TTEST DATA=mydata;",
      "  CLASS group_var;",
      "  VAR numeric_var;",
      "RUN;", "")
  } else {
    h0_val <- if (nchar(mu) > 0) mu else "0"
    c(paste0("/* R: ", line, " */"),
      paste0("PROC TTEST DATA=mydata H0=", h0_val, ";"),
      "  VAR numeric_var;",
      "RUN;", "")
  }
}

# ---- chisq.test ----

.convert_chisq <- function(line, verbose = FALSE) {
  if (verbose) message("[r2sas] chisq.test: ", line)
  c(paste0("/* R: ", line, " */"),
    "PROC FREQ DATA=mydata;",
    "  TABLES row_var * col_var / CHISQ;",
    "RUN;", "")
}

# ---- aov / anova ----

.convert_anova <- function(line, verbose = FALSE) {
  if (verbose) message("[r2sas] anova/aov: ", line)
  formula_m <- stringr::str_match(line, "(aov|anova)\\s*\\(([^,]+)")
  formula_str <- if (!is.na(formula_m[, 3])) stringr::str_trim(formula_m[, 3]) else "y ~ group"
  parts <- strsplit(formula_str, "~")[[1]]
  dep <- stringr::str_trim(parts[1])
  indep <- if (length(parts) > 1) stringr::str_trim(parts[2]) else "group"
  c(paste0("/* R: ", line, " */"),
    "PROC ANOVA DATA=mydata;",
    paste0("  CLASS ", indep, ";"),
    paste0("  MODEL ", dep, " = ", indep, ";"),
    "  MEANS / TUKEY;",
    "RUN;", "")
}

# ---- summary ----

.convert_summary <- function(line, verbose = FALSE) {
  if (verbose) message("[r2sas] summary: ", line)
  c(paste0("/* R: ", line, " */"),
    "PROC MEANS DATA=mydata N MEAN STD MIN MAX;",
    "  VAR _NUMERIC_;",
    "RUN;", "")
}

# ---- table ----

.convert_table <- function(line, verbose = FALSE) {
  if (verbose) message("[r2sas] table: ", line)
  args <- .extract_args(line, "table")
  if (is.na(args)) args <- stringr::str_match(line, "table\\s*\\(([^)]+)\\)")[, 2]
  # Strip df$col to just col; remove whitespace
  vars <- if (!is.na(args)) {
    v <- gsub("\\w+\\$", "", args)   # drop dataset prefix (df$col -> col)
    gsub("\\s", "", v)               # remove spaces
  } else "var1 var2"
  var_list <- strsplit(vars, ",")[[1]]
  if (length(var_list) == 1) {
    c(paste0("/* R: ", line, " */"),
      "PROC FREQ DATA=mydata;",
      paste0("  TABLES ", stringr::str_trim(var_list[1]), ";"),
      "RUN;", "")
  } else {
    c(paste0("/* R: ", line, " */"),
      "PROC FREQ DATA=mydata;",
      paste0("  TABLES ", paste(sapply(var_list, stringr::str_trim), collapse = " * "), ";"),
      "RUN;", "")
  }
}

# ---- file I/O ----

.convert_read <- function(line, verbose = FALSE) {
  if (verbose) message("[r2sas] read: ", line)
  file_m <- stringr::str_match(line, "\"([^\"]+)\"")
  fname <- if (!is.na(file_m[, 2])) file_m[, 2] else "/path/to/file.csv"
  target <- stringr::str_match(line, "^\\s*(\\w+)\\s*<?-")[, 2]
  dsname <- if (!is.na(target)) toupper(target) else "IMPORTED"

  if (stringr::str_detect(line, "\\.sas7bdat|\\.xpt|\\.csv|\\.txt")) {
    c(paste0("/* R: ", line, " */"),
      paste0("PROC IMPORT DATAFILE=\"", fname, "\""),
      paste0("  OUT=", dsname),
      "  DBMS=CSV REPLACE;",
      "  GETNAMES=YES;",
      "RUN;", "")
  } else {
    c(paste0("/* R: ", line, " */"),
      paste0("/* Use PROC IMPORT or a LIBNAME statement to load the data */"),
      paste0("/* LIBNAME mylib '/path/to/'; SET mylib.dataset; */"), "")
  }
}

.convert_write <- function(line, verbose = FALSE) {
  if (verbose) message("[r2sas] write: ", line)
  file_m <- stringr::str_match(line, "\"([^\"]+)\"")
  fname <- if (!is.na(file_m[, 2])) file_m[, 2] else "/path/to/output.csv"
  c(paste0("/* R: ", line, " */"),
    paste0("PROC EXPORT DATA=mydata"),
    paste0("  OUTFILE=\"", fname, "\""),
    "  DBMS=CSV REPLACE;",
    "RUN;", "")
}

# ---- ggplot ----

#' Convert ggplot2 R code to SAS SGPLOT stub
#'
#' @param line Character string with ggplot call
#' @param verbose Logical
#' @return Character vector of SAS lines
#' @export
r2sas_plot <- function(line, verbose = FALSE) {
  paste(.convert_ggplot(line, verbose), collapse = "\n")
}

.convert_ggplot <- function(line, verbose = FALSE) {
  if (verbose) message("[r2sas] ggplot: ", line)
  sas <- c(paste0("/* R (ggplot2): ", line, " */"),
           "PROC SGPLOT DATA=mydata;")
  if (stringr::str_detect(line, "geom_point|geom_jitter")) {
    x <- stringr::str_match(line, "aes\\s*\\(.*?x\\s*=\\s*(\\w+)")[, 2]
    y <- stringr::str_match(line, "aes\\s*\\(.*?y\\s*=\\s*(\\w+)")[, 2]
    x <- if (is.na(x)) "x_var" else x
    y <- if (is.na(y)) "y_var" else y
    sas <- c(sas, paste0("  SCATTER X=", x, " Y=", y, ";"))
  } else if (stringr::str_detect(line, "geom_line")) {
    sas <- c(sas, "  SERIES X=x_var Y=y_var;")
  } else if (stringr::str_detect(line, "geom_bar|geom_col")) {
    sas <- c(sas, "  VBAR category_var;")
  } else if (stringr::str_detect(line, "geom_histogram")) {
    sas <- c(sas, "  HISTOGRAM numeric_var;")
  } else if (stringr::str_detect(line, "geom_boxplot")) {
    sas <- c(sas, "  VBOX numeric_var / CATEGORY=group_var;")
  } else {
    sas <- c(sas, "  /* TODO: choose appropriate SGPLOT statement */")
  }
  c(sas, "RUN;", "")
}

# ---- print / cat ----

.convert_print <- function(line, verbose = FALSE) {
  inner <- stringr::str_match(line, "(print|cat|message)\\s*\\((.+)\\)")[, 3]
  if (is.na(inner)) inner <- "\"output\""
  c(paste0("/* R: ", line, " */"),
    paste0("%PUT ", inner, ";"), "")
}

# ---- for loop ----

.convert_for <- function(line, verbose = FALSE) {
  # Use balanced paren extraction to handle seq(a, b) inside for(i in seq(...))
  for_inner <- .extract_args(line, "for")
  if (!is.na(for_inner)) {
    in_m <- stringr::str_match(for_inner, "^\\s*(\\w+)\\s+in\\s+(.+)$")
    var <- if (!is.na(in_m[, 2])) in_m[, 2] else "i"
    seq_str <- if (!is.na(in_m[, 3])) stringr::str_trim(in_m[, 3]) else "1:10"
  } else {
    var <- "i"
    seq_str <- "1:10"
  }
  seq_sas <- gsub("seq\\((\\d+),\\s*(\\d+)[^)]*\\)", "\\1 to \\2", seq_str)
  seq_sas <- gsub("(\\d+):(\\d+)", "\\1 to \\2", seq_sas)
  c(paste0("/* R: ", line, " */"),
    paste0("%DO ", var, " = ", seq_sas, ";"),
    "  /* loop body */",
    paste0("%END;"), "")
}

# ---- if / else ----

.convert_if <- function(line, verbose = FALSE) {
  if (stringr::str_detect(line, "^\\s*\\}\\s*else")) {
    return(c("ELSE DO;", "  /* else body */", "END;", ""))
  }
  cond <- stringr::str_match(line, "if\\s*\\((.+)\\)")[, 2]
  cond_sas <- if (!is.na(cond)) .r_to_sas_cond(cond) else "condition"
  c(paste0("/* R: ", line, " */"),
    paste0("IF ", cond_sas, " THEN DO;"),
    "  /* if body */",
    "END;", "")
}

# ---- generic assignment ----

.convert_assignment <- function(line, verbose = FALSE) {
  # Simple arithmetic / variable assignment
  sas_line <- line
  sas_line <- stringr::str_replace_all(sas_line, "<-", "=")
  sas_line <- .r_to_sas_ops(sas_line)
  # strip quotes around object names that aren't strings
  paste0(sas_line, ";")
}

# ---- balanced paren extractor ----

.extract_args <- function(text, func_name) {
  pat <- paste0(func_name, "\\s*\\(")
  m <- regexpr(pat, text)
  if (m == -1L) return(NA_character_)
  open_pos <- m + attr(m, "match.length") - 1L
  chars <- strsplit(text, "")[[1]]
  depth <- 1L
  i <- open_pos + 1L
  while (i <= length(chars) && depth > 0L) {
    if (chars[i] == "(") depth <- depth + 1L
    else if (chars[i] == ")") depth <- depth - 1L
    i <- i + 1L
  }
  if (depth == 0L) substr(text, open_pos + 1L, i - 2L) else NA_character_
}

# ---- condition / operator helpers ----

.r_to_sas_cond <- function(cond) {
  # Handle !is.na() before is.na() to avoid partial substitution
  cond <- gsub("!is\\.na\\(([^)]+)\\)", "^MISSING(\\1)", cond)
  cond <- gsub("is\\.na\\(([^)]+)\\)", "MISSING(\\1)", cond)
  cond <- stringr::str_replace_all(cond, "==", "=")
  cond <- stringr::str_replace_all(cond, "!=", "^=")
  cond <- stringr::str_replace_all(cond, "&&", "AND")
  cond <- stringr::str_replace_all(cond, "\\|\\|", "OR")
  cond <- stringr::str_replace_all(cond, "\\$", ".")
  cond
}

.r_to_sas_ops <- function(expr) {
  expr <- stringr::str_replace_all(expr, "<-", "=")
  expr <- stringr::str_replace_all(expr, "\\$", ".")
  expr <- stringr::str_replace_all(expr, "\\bTRUE\\b", "1")
  expr <- stringr::str_replace_all(expr, "\\bFALSE\\b", "0")
  expr <- stringr::str_replace_all(expr, "\\bNA\\b", ".")
  expr <- stringr::str_replace_all(expr, "\\bNULL\\b", ".")
  expr <- stringr::str_replace_all(expr, "\\bc\\(([^)]+)\\)", "(\\1)")
  expr <- stringr::str_replace_all(expr, "\\bpaste0?\\(([^)]+)\\)", "CATS(\\1)")
  expr <- stringr::str_replace_all(expr, "\\bnchar\\(", "LENGTHN(")
  expr <- stringr::str_replace_all(expr, "\\bsubstr\\(", "SUBSTR(")
  expr <- stringr::str_replace_all(expr, "\\btoupper\\(", "UPCASE(")
  expr <- stringr::str_replace_all(expr, "\\btolower\\(", "LOWCASE(")
  expr <- stringr::str_replace_all(expr, "\\babs\\(", "ABS(")
  expr <- stringr::str_replace_all(expr, "\\bsqrt\\(", "SQRT(")
  expr <- stringr::str_replace_all(expr, "\\bexp\\(", "EXP(")
  expr <- stringr::str_replace_all(expr, "\\blog\\(", "LOG(")
  expr <- stringr::str_replace_all(expr, "\\bround\\(", "ROUND(")
  expr <- stringr::str_replace_all(expr, "\\bfloor\\(", "FLOOR(")
  expr <- stringr::str_replace_all(expr, "\\bceiling\\(", "CEIL(")
  expr <- stringr::str_replace_all(expr, "\\bmax\\(", "MAX(")
  expr <- stringr::str_replace_all(expr, "\\bmin\\(", "MIN(")
  expr <- stringr::str_replace_all(expr, "\\bsum\\(", "SUM(")
  expr <- stringr::str_replace_all(expr, "\\bmean\\(", "MEAN(")
  expr
}
