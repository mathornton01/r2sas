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
#' Generate SAS INFILE/INPUT import code from an R data.frame
#'
#' Given an R data.frame and a SAS dataset name, generates SAS code to import
#' the data. If \code{output_dir} is provided, writes the data to a CSV file
#' and emits a PROC IMPORT block pointing to it. Otherwise, emits a
#' DATALINES-based DATA step with column metadata.
#'
#' Independently reimplemented under MIT; inspired by the MattKelliher-Gibson/r2sas
#' (GPL) and foreign::write.foreign (GPL) concepts.
#'
#' @param df A data.frame to generate import code for.
#' @param dataset_name Character. SAS dataset name to use.
#' @param output_dir Optional character. If provided, the CSV is written here
#'   and a PROC IMPORT step is generated.
#' @return A single character string of SAS code.
#' @export
#'
#' @examples
#' df <- data.frame(id = 1:3, name = c("Alice", "Bob", "Carol"),
#'                  score = c(95.1, 87.3, 92.0))
#' cat(r2sas_import(df, "mydata"))
r2sas_import <- function(df, dataset_name, output_dir = NULL) {
  if (!is.data.frame(df)) stop("df must be a data.frame")
  ds <- toupper(dataset_name)

  # Classify columns
  is_char <- vapply(df, function(col) {
    is.character(col) || is.factor(col)
  }, logical(1))

  col_names <- names(df)

  if (!is.null(output_dir)) {
    # Write CSV and generate PROC IMPORT
    csv_file <- file.path(output_dir, paste0(tolower(dataset_name), ".csv"))
    utils::write.csv(df, csv_file, row.names = FALSE)
    lines <- c(
      paste0("/* Generated by r2sas_import for dataset: ", ds, " */"),
      paste0("PROC IMPORT DATAFILE=\"", csv_file, "\""),
      paste0("  OUT=", ds),
      "  DBMS=CSV REPLACE;",
      "  GETNAMES=YES;",
      "RUN;",
      "",
      paste0("/* Column types: ",
             paste(col_names, ifelse(is_char, "character", "numeric"), sep = "=", collapse = ", "),
             " */")
    )
  } else {
    # Generate DATALINES-based DATA step with INPUT statement
    input_parts <- vapply(seq_along(col_names), function(i) {
      if (is_char[i]) paste0(col_names[i], " $") else col_names[i]
    }, character(1))
    input_stmt <- paste("  INPUT", paste(input_parts, collapse = " "), ";")

    label_stmts <- character(0)
    for (nm in col_names) {
      label_stmts <- c(label_stmts, paste0("  LABEL ", nm, " = \"", nm, "\";"))
    }

    # Build a few DATALINES rows from actual data (up to 5 rows)
    n_rows <- min(nrow(df), 5L)
    datalines <- character(n_rows)
    for (r in seq_len(n_rows)) {
      row_vals <- vapply(seq_along(col_names), function(i) {
        val <- as.character(df[r, i])
        if (is.na(val)) val <- "."
        # Quote character values if they contain spaces
        if (is_char[i] && grepl(" ", val)) val <- paste0("\"", val, "\"")
        val
      }, character(1))
      datalines[r] <- paste(row_vals, collapse = " ")
    }

    lines <- c(
      paste0("/* Generated by r2sas_import for dataset: ", ds, " */"),
      paste0("DATA ", ds, ";"),
      "  INFILE DATALINES DLM=' ' MISSOVER;",
      input_stmt,
      label_stmts,
      "  DATALINES;",
      datalines,
      ";",
      "RUN;"
    )
  }

  paste(lines, collapse = "\n")
}

#' Generate a SAS PROC FORMAT VALUE statement from a named vector or factor
#'
#' Given a named vector (name=label pairs) or an R factor, generates a
#' SAS PROC FORMAT block with a VALUE statement.
#'
#' Inspired by the sassy/fmtr package (CC0 / public domain). This implementation
#' is original and released under MIT.
#'
#' @param x A named character vector where names are format keys and values are
#'   labels, or an R factor (levels become keys, labels become values).
#' @param fmt_name Character. The SAS format name to use (default: "myfmt").
#' @return A single character string of SAS PROC FORMAT code.
#' @export
#'
#' @examples
#' r2sas_proc_format(c("1" = "Male", "2" = "Female"), "gender")
#' r2sas_proc_format(factor(c("Low", "Med", "High")), "severity")
r2sas_proc_format <- function(x, fmt_name = "myfmt") {
  if (is.factor(x)) {
    lvls <- levels(x)
    keys <- as.character(seq_along(lvls))
    labels <- lvls
  } else {
    if (is.null(names(x))) stop("x must be a named vector or factor")
    keys <- names(x)
    labels <- as.character(x)
  }

  # Determine if keys are all numeric
  all_numeric_keys <- all(grepl("^-?[0-9]+(\\.[0-9]+)?$", keys))

  # Build VALUE pairs
  pairs <- vapply(seq_along(keys), function(i) {
    k <- keys[i]
    lbl <- labels[i]
    if (all_numeric_keys) {
      paste0(k, "='", lbl, "'")
    } else {
      paste0("'", k, "'='", lbl, "'")
    }
  }, character(1))

  value_body <- paste("  VALUE", fmt_name, paste(pairs, collapse = " "), ";")

  paste(c("PROC FORMAT;", value_body, "RUN;"), collapse = "\n")
}

#' Generate SAS PROC TRANSPOSE code from parameters
#'
#' Builds a PROC TRANSPOSE step from dataset name, BY variables, VAR variable,
#' and an optional ID variable.
#'
#' Inspired by the sassy ecosystem's proc_transpose (CC0 / public domain). This
#' implementation is original and released under MIT.
#'
#' @param df_name Character. The input SAS dataset name.
#' @param by_vars Character vector. Variables for the BY statement.
#' @param var Character. The variable to transpose (VAR statement).
#' @param id Character or NULL. Optional variable for the ID statement (column
#'   names in the output).
#' @return A single character string of SAS PROC TRANSPOSE code.
#' @export
#'
#' @examples
#' r2sas_transpose("mydata", c("id", "group"), "score", id = "timepoint")
r2sas_transpose <- function(df_name, by_vars, var, id = NULL) {
  ds <- toupper(df_name)
  out_ds <- paste0(ds, "_T")

  lines <- c(
    paste0("PROC TRANSPOSE DATA=", ds, " OUT=", out_ds, ";"),
    paste0("  BY ", paste(by_vars, collapse = " "), ";"),
    paste0("  VAR ", var, ";")
  )

  if (!is.null(id)) {
    lines <- c(lines, paste0("  ID ", id, ";"))
  }

  lines <- c(lines, "RUN;")
  paste(lines, collapse = "\n")
}

#' Generate a SAS DATA step MERGE for combining two datasets
#'
#' Builds a SAS DATA step MERGE block for two datasets with a BY key and
#' optional join type (left, right, inner, full).
#'
#' Independently implemented under MIT.
#'
#' @param left Character. Name of the left dataset.
#' @param right Character. Name of the right dataset.
#' @param by Character vector. Variable(s) to merge on.
#' @param type Character. Join type: "left" (default), "right", "inner", or "full".
#' @return A single character string of SAS DATA step MERGE code.
#' @export
#'
#' @examples
#' r2sas_merge("patients", "labs", by = "patient_id", type = "left")
#' r2sas_merge("a", "b", by = c("site", "id"), type = "inner")
r2sas_merge <- function(left, right, by, type = "left") {
  type <- tolower(type)
  if (!type %in% c("left", "right", "inner", "full")) {
    stop("type must be one of: left, right, inner, full")
  }

  out_ds <- paste0(toupper(left), "_", toupper(right))
  by_stmt <- paste0("  BY ", paste(by, collapse = " "), ";")

  merge_stmt <- paste0(
    "  MERGE ", toupper(left), " (IN=_left_) ",
    toupper(right), " (IN=_right_);"
  )

  keep_stmt <- switch(type,
    left  = "  IF _left_;",
    right = "  IF _right_;",
    inner = "  IF _left_ AND _right_;",
    full  = "  IF _left_ OR _right_;"
  )

  lines <- c(
    paste0("DATA ", out_ds, ";"),
    merge_stmt,
    by_stmt,
    keep_stmt,
    "RUN;"
  )

  paste(lines, collapse = "\n")
}

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
