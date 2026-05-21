#' Convert an R data frame operation to a SAS DATA step
#'
#' @param df_name Name of the data frame (character)
#' @param operations Named list of operations. Currently supports
#'   \code{filter}, \code{select}, \code{rename}, \code{mutate}.
#' @return Character string of SAS DATA step code.
#' @export
#'
#' @examples
#' r2sas_dataframe("patients",
#'   list(filter = "age > 18", select = c("id", "age", "sex")))
r2sas_dataframe <- function(df_name, operations = list()) {
  out_name <- paste0(df_name, "_sas")
  lines <- c(
    paste0("DATA ", toupper(out_name), ";"),
    paste0("  SET ", toupper(df_name), ";")
  )
  if (!is.null(operations$filter)) {
    cond <- .r_to_sas_cond(operations$filter)
    lines <- c(lines, paste0("  WHERE ", cond, ";"))
  }
  if (!is.null(operations$select)) {
    vars <- paste(operations$select, collapse = " ")
    lines <- c(lines, paste0("  KEEP ", vars, ";"))
  }
  if (!is.null(operations$rename)) {
    renames <- paste(names(operations$rename), operations$rename,
                     sep = "=", collapse = " ")
    lines <- c(lines, paste0("  RENAME ", renames, ";"))
  }
  if (!is.null(operations$mutate)) {
    for (nm in names(operations$mutate)) {
      expr <- .r_to_sas_ops(operations$mutate[[nm]])
      lines <- c(lines, paste0("  ", nm, " = ", expr, ";"))
    }
  }
  lines <- c(lines, "RUN;")
  paste(lines, collapse = "\n")
}

#' Build a SAS PROC step as a string
#'
#' @param proc Name of the PROC (e.g. "MEANS", "FREQ", "REG")
#' @param data Dataset name
#' @param statements Named list of additional statements (e.g. list(VAR="_NUMERIC_"))
#' @param options Character vector of proc-level options
#' @return Character string of the PROC step
#' @export
#'
#' @examples
#' sas_proc("MEANS", "mydata", list(VAR = "_NUMERIC_"), c("N", "MEAN", "STD"))
sas_proc <- function(proc, data, statements = list(), options = character(0)) {
  opts <- if (length(options)) paste(options, collapse = " ") else ""
  header <- paste0("PROC ", toupper(proc), " DATA=", data,
                   if (nchar(opts)) paste0(" ", opts) else "", ";")
  body <- unlist(lapply(names(statements), function(nm) {
    paste0("  ", toupper(nm), " ", statements[[nm]], ";")
  }))
  paste(c(header, body, "RUN;"), collapse = "\n")
}

#' Build a SAS DATA step as a string
#'
#' @param out_name Output dataset name
#' @param in_name Input dataset name
#' @param stmts Character vector of DATA step statements (without trailing semicolons)
#' @return Character string of the DATA step
#' @export
#'
#' @examples
#' sas_datastep("work.out", "work.in", c("WHERE age > 18", "bmi = weight / (height**2)"))
sas_datastep <- function(out_name, in_name, stmts = character(0)) {
  body <- paste0("  ", stmts, ";")
  paste(c(paste0("DATA ", out_name, ";"),
          paste0("  SET ", in_name, ";"),
          body,
          "RUN;"), collapse = "\n")
}

#' Convert an R dplyr pipeline string to SAS
#'
#' @param pipeline Character string with dplyr pipeline
#' @param verbose Logical
#' @return Character string of SAS code
#' @export
#'
#' @examples
#' r2sas_dplyr("df %>% filter(age > 18) %>% select(id, age)")
r2sas_dplyr <- function(pipeline, verbose = FALSE) {
  paste(.convert_dplyr(pipeline, verbose), collapse = "\n")
}

#' Convert an R file to a SAS script file
#'
#' @param r_path Path to an .R source file
#' @param sas_path Path for the output .sas file (optional; defaults to same
#'   name with .sas extension)
#' @param verbose Logical
#' @return Invisibly returns the path to the written .sas file
#' @export
r2sas_file <- function(r_path, sas_path = NULL, verbose = TRUE) {
  if (is.null(sas_path)) {
    sas_path <- sub("\\.R$", ".sas", r_path, ignore.case = TRUE)
    if (sas_path == r_path) sas_path <- paste0(r_path, ".sas")
  }
  sas_code <- r2sas(r_path, file = TRUE, verbose = verbose)
  writeLines(sas_code, sas_path)
  if (verbose) message("Written to: ", sas_path)
  invisible(sas_path)
}

#' Generate SAS IF/THEN assignment blocks from a conditions table
#'
#' Inspired by MSToolkit's \code{convertToSASCode()} pattern: given a
#' data frame (or 3-column matrix) of condition/variable/value rows, emit
#' SAS \code{IF condition THEN variable = value;} statements.
#' This is independently reimplemented under MIT (MSToolkit is GPL).
#'
#' @param conditions A data frame or matrix with at least 3 columns:
#'   \itemize{
#'     \item Column 1 (or named \code{condition}): R/SAS condition expression
#'     \item Column 2 (or named \code{variable}): SAS variable name to assign
#'     \item Column 3 (or named \code{value}): value to assign
#'   }
#' @param convert_cond Logical. If TRUE (default), run R->SAS condition conversion
#'   on column 1 before emitting (e.g., \code{==} -> \code{=}, \code{&&} -> \code{AND}).
#' @param else_missing Logical. If TRUE (default), emit \code{ELSE variable = .;}
#'   after the last IF block for each unique variable.
#' @return A single character string of SAS code.
#' @export
#'
#' @examples
#' conds <- data.frame(
#'   condition = c("age >= 18 && age < 65", "age >= 65"),
#'   variable  = c("age_group", "age_group"),
#'   value     = c('"adult"', '"senior"'),
#'   stringsAsFactors = FALSE
#' )
#' cat(r2sas_conditions(conds))
r2sas_conditions <- function(conditions, convert_cond = TRUE, else_missing = TRUE) {
  if (is.matrix(conditions)) conditions <- as.data.frame(conditions, stringsAsFactors = FALSE)
  # Normalise column names
  nms <- names(conditions)
  cond_col  <- if ("condition" %in% nms) "condition" else nms[1]
  var_col   <- if ("variable"  %in% nms) "variable"  else nms[2]
  value_col <- if ("value"     %in% nms) "value"     else nms[3]

  lines <- character(0)
  for (i in seq_len(nrow(conditions))) {
    cond  <- as.character(conditions[i, cond_col])
    vname <- as.character(conditions[i, var_col])
    val   <- as.character(conditions[i, value_col])
    if (convert_cond) cond <- .r_to_sas_cond(cond)
    lines <- c(lines, paste0("  IF ", cond, " THEN ", vname, " = ", val, ";"))
  }

  if (else_missing) {
    vars <- unique(as.character(conditions[[var_col]]))
    for (v in vars) {
      lines <- c(lines, paste0("  ELSE ", v, " = .;"))
    }
  }

  paste(c("DATA _NULL_;", "  SET mydata;", lines, "RUN;"), collapse = "\n")
}

#' Print a quick-reference table of common R -> SAS equivalents
#'
#' @return Invisibly returns a data frame of equivalents.
#' @export
r2sas_cheatsheet <- function() {
  tbl <- data.frame(
    R = c(
      "lm(y ~ x, data=df)",
      "glm(y ~ x, family=binomial, data=df)",
      "t.test(x, y)",
      "chisq.test(table(a, b))",
      "aov(y ~ group, data=df)",
      "summary(df)",
      "table(df$a, df$b)",
      "read.csv('file.csv')",
      "write.csv(df, 'file.csv')",
      "ggplot(df, aes(x,y)) + geom_point()",
      "filter(df, x > 5)",
      "select(df, a, b)",
      "mutate(df, z = x + y)",
      "group_by(df, g) %>% summarise(m=mean(x))",
      "arrange(df, desc(x))",
      "left_join(a, b, by='id')",
      "mean(x, na.rm=TRUE)",
      "is.na(x)",
      "paste0(a, b)",
      "nchar(s)"
    ),
    SAS = c(
      "PROC REG DATA=df; MODEL y = x; RUN;",
      "PROC LOGISTIC DATA=df; MODEL y = x; RUN;",
      "PROC TTEST DATA=df; CLASS grp; VAR x; RUN;",
      "PROC FREQ DATA=df; TABLES a*b / CHISQ; RUN;",
      "PROC ANOVA DATA=df; CLASS group; MODEL y=group; RUN;",
      "PROC MEANS DATA=df N MEAN STD MIN MAX; VAR _NUMERIC_; RUN;",
      "PROC FREQ DATA=df; TABLES a*b; RUN;",
      "PROC IMPORT DATAFILE='file.csv' OUT=df DBMS=CSV; GETNAMES=YES; RUN;",
      "PROC EXPORT DATA=df OUTFILE='file.csv' DBMS=CSV; RUN;",
      "PROC SGPLOT DATA=df; SCATTER X=x Y=y; RUN;",
      "DATA out; SET df; WHERE x > 5; RUN;",
      "DATA out; SET df (KEEP=a b); RUN;",
      "DATA out; SET df; z = x + y; RUN;",
      "PROC MEANS DATA=df; CLASS g; VAR x; OUTPUT OUT=s MEAN=m; RUN;",
      "PROC SORT DATA=df OUT=out; BY DESCENDING x; RUN;",
      "DATA out; MERGE a (IN=la) b (IN=lb); BY id; IF la; RUN;",
      "MEAN(x) (in DATA step with NMISS handling)",
      "MISSING(x)",
      "CATS(a, b)",
      "LENGTHN(s)"
    ),
    stringsAsFactors = FALSE
  )
  print(tbl, right = FALSE)
  invisible(tbl)
}
