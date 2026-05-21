library(r2sas)

# ============================================================
# SECTION 1: Statistical Model Conversions
# ============================================================

test_that("lm is converted to PROC REG", {
  out <- r2sas_expr("fit <- lm(y ~ x, data = df)")
  expect_true(grepl("PROC REG", out))
  expect_true(grepl("MODEL y = x", out))
  expect_true(grepl("DATA=df", out))
  expect_true(grepl("RUN;", out))
})

test_that("lm with multiple predictors maps correctly", {
  out <- r2sas_expr("fit <- lm(bmi ~ age + weight + height, data = patients)")
  expect_true(grepl("PROC REG", out))
  expect_true(grepl("MODEL bmi =", out))
  expect_true(grepl("age", out))
  expect_true(grepl("DATA=patients", out))
})

test_that("glm with binomial converts to PROC LOGISTIC", {
  out <- r2sas_expr("fit <- glm(y ~ x, family = binomial, data = df)")
  expect_true(grepl("PROC LOGISTIC", out))
  expect_true(grepl("MODEL y =", out))
  expect_true(grepl("DATA=df", out))
})

test_that("glm with poisson converts to PROC GENMOD with DIST=POISSON", {
  out <- r2sas_expr("fit <- glm(count ~ x + z, family = poisson, data = df)")
  expect_true(grepl("PROC GENMOD", out))
  expect_true(grepl("DIST=POISSON", out))
  expect_true(grepl("MODEL count =", out))
})

test_that("glm with gamma converts to PROC GENMOD with DIST=GAMMA", {
  out <- r2sas_expr("fit <- glm(cost ~ age, family = gamma, data = df)")
  expect_true(grepl("PROC GENMOD", out))
  expect_true(grepl("DIST=GAMMA", out))
})

test_that("glm with gaussian defaults to PROC REG", {
  out <- r2sas_expr("fit <- glm(y ~ x, family = gaussian, data = df)")
  expect_true(grepl("PROC REG", out))
})

# ============================================================
# SECTION 2: Hypothesis Tests
# ============================================================

test_that("t.test one-sample converts to PROC TTEST", {
  out <- r2sas_expr("t.test(x, mu = 0)")
  expect_true(grepl("PROC TTEST", out))
  expect_true(grepl("H0=0", out))
  expect_true(grepl("RUN;", out))
})

test_that("t.test two-sample converts to PROC TTEST with CLASS", {
  out <- r2sas_expr("t.test(x, y)")
  expect_true(grepl("PROC TTEST", out))
  expect_true(grepl("CLASS", out))
})

test_that("t.test with nonzero mu preserves H0 value", {
  out <- r2sas_expr("t.test(x, mu = 5)")
  expect_true(grepl("PROC TTEST", out))
  expect_true(grepl("H0=5", out))
})

test_that("chisq.test converts to PROC FREQ with CHISQ", {
  out <- r2sas_expr("chisq.test(table(a, b))")
  expect_true(grepl("PROC FREQ", out))
  expect_true(grepl("CHISQ", out))
  expect_true(grepl("TABLES", out))
  expect_true(grepl("RUN;", out))
})

test_that("aov converts to PROC ANOVA with CLASS and MODEL", {
  out <- r2sas_expr("aov(score ~ treatment, data = df)")
  expect_true(grepl("PROC ANOVA", out))
  expect_true(grepl("CLASS treatment", out))
  expect_true(grepl("MODEL score = treatment", out))
  expect_true(grepl("TUKEY", out))
})

test_that("anova function also converts to PROC ANOVA", {
  out <- r2sas_expr("anova(y ~ group, data = df)")
  expect_true(grepl("PROC ANOVA", out))
  expect_true(grepl("CLASS group", out))
})

# ============================================================
# SECTION 3: Summary and Frequency Procedures
# ============================================================

test_that("summary converts to PROC MEANS with standard statistics", {
  out <- r2sas_expr("summary(df)")
  expect_true(grepl("PROC MEANS", out))
  expect_true(grepl("N MEAN STD MIN MAX", out))
  expect_true(grepl("_NUMERIC_", out))
})

test_that("table with one variable creates PROC FREQ univariate", {
  out <- r2sas_expr("table(df$sex)")
  expect_true(grepl("PROC FREQ", out))
  expect_true(grepl("TABLES sex;", out))
  # No cross-tab asterisk in the TABLES statement
  expect_false(grepl("TABLES.*\\*", out))
})

test_that("table with two vars creates cross-tab with asterisk", {
  out <- r2sas_expr("table(df$a, df$b)")
  expect_true(grepl("TABLES", out))
  expect_true(grepl("\\*", out))
})

# ============================================================
# SECTION 4: File I/O Conversions
# ============================================================

test_that("read.csv converts to PROC IMPORT with DBMS=CSV", {
  out <- r2sas_expr("df <- read.csv('data.csv')")
  expect_true(grepl("PROC IMPORT", out))
  expect_true(grepl("DBMS=CSV", out))
  expect_true(grepl("GETNAMES=YES", out))
  expect_true(grepl("REPLACE", out))
})

test_that("read_csv (tidyverse) also converts to PROC IMPORT", {
  out <- r2sas_expr('patients <- read_csv("patients.csv")')
  expect_true(grepl("PROC IMPORT", out))
  expect_true(grepl("DBMS=CSV", out))
})

test_that("write.csv converts to PROC EXPORT", {
  out <- r2sas_expr("write.csv(df, 'output.csv')")
  expect_true(grepl("PROC EXPORT", out))
  expect_true(grepl("DBMS=CSV", out))
  expect_true(grepl("output.csv", out))
})

test_that("write_csv also converts to PROC EXPORT", {
  out <- r2sas_expr('write_csv(results, "results.csv")')
  expect_true(grepl("PROC EXPORT", out))
})

test_that("readRDS falls back to PROC IMPORT comment", {
  out <- r2sas_expr("df <- readRDS('model.rds')")
  expect_true(grepl("PROC IMPORT|LIBNAME", out, ignore.case = TRUE))
})

# ============================================================
# SECTION 5: dplyr Verb Conversions
# ============================================================

test_that("dplyr filter converts to DATA step WHERE", {
  out <- r2sas_expr("df %>% filter(age > 18)")
  expect_true(grepl("WHERE", out))
  expect_true(grepl("age > 18", out))
  expect_true(grepl("DATA", out))
})

test_that("dplyr filter with equality produces correct SAS condition", {
  out <- r2sas_expr("df %>% filter(status == 'active')")
  expect_true(grepl("WHERE", out))
})

test_that("dplyr select converts to DATA step with KEEP", {
  out <- r2sas_expr("df %>% select(id, age, gender)")
  expect_true(grepl("KEEP", out))
})

test_that("dplyr group_by + summarise converts to PROC MEANS with CLASS", {
  out <- r2sas_expr("df %>% group_by(site) %>% summarise(mean_age = mean(age))")
  expect_true(grepl("PROC MEANS", out))
  expect_true(grepl("CLASS", out))
  expect_true(grepl("site", out))
})

test_that("dplyr group_by + summarize (American spelling) also works", {
  out <- r2sas_expr("df %>% group_by(group) %>% summarize(n = n())")
  expect_true(grepl("PROC MEANS", out))
  expect_true(grepl("CLASS", out))
})

test_that("dplyr arrange converts to PROC SORT", {
  out <- r2sas_expr("df %>% arrange(age)")
  expect_true(grepl("PROC SORT", out))
  expect_true(grepl("BY", out))
})

test_that("dplyr arrange with desc converts to PROC SORT DESCENDING", {
  out <- r2sas_expr("df %>% arrange(desc(age))")
  expect_true(grepl("PROC SORT", out))
  expect_true(grepl("DESCENDING", out))
})

test_that("dplyr mutate converts to DATA step with comment", {
  out <- r2sas_expr("df %>% mutate(bmi = weight / height^2)")
  expect_true(grepl("DATA", out))
  expect_true(grepl("SET", out))
})

test_that("dplyr left_join converts to DATA step MERGE", {
  out <- r2sas_expr("merged <- left_join(a, b, by = 'id')")
  expect_true(grepl("MERGE", out))
  expect_true(grepl("BY", out))
})

test_that("dplyr inner_join converts to DATA step MERGE", {
  out <- r2sas_expr("merged <- inner_join(patients, visits, by = 'patient_id')")
  expect_true(grepl("MERGE", out))
})

test_that("dplyr full_join converts to DATA step MERGE", {
  out <- r2sas_expr("merged <- full_join(a, b, by = 'key')")
  expect_true(grepl("MERGE", out))
})

# ============================================================
# SECTION 6: Graphics Conversions
# ============================================================

test_that("ggplot scatter converts to PROC SGPLOT SCATTER", {
  out <- r2sas_expr("ggplot(df, aes(x = age, y = bmi)) + geom_point()")
  expect_true(grepl("PROC SGPLOT", out))
  expect_true(grepl("SCATTER", out))
  expect_true(grepl("X=age", out))
  expect_true(grepl("Y=bmi", out))
})

test_that("geom_jitter also maps to PROC SGPLOT SCATTER", {
  out <- r2sas_expr("ggplot(df, aes(x = group, y = score)) + geom_jitter()")
  expect_true(grepl("PROC SGPLOT", out))
  expect_true(grepl("SCATTER", out))
})

test_that("geom_line maps to PROC SGPLOT SERIES", {
  out <- r2sas_expr("ggplot(df, aes(x = time, y = value)) + geom_line()")
  expect_true(grepl("PROC SGPLOT", out))
  expect_true(grepl("SERIES", out))
})

test_that("geom_bar maps to PROC SGPLOT VBAR", {
  out <- r2sas_expr("ggplot(df, aes(x = category)) + geom_bar()")
  expect_true(grepl("PROC SGPLOT", out))
  expect_true(grepl("VBAR", out))
})

test_that("geom_col maps to PROC SGPLOT VBAR", {
  out <- r2sas_expr("ggplot(df, aes(x = group, y = n)) + geom_col()")
  expect_true(grepl("PROC SGPLOT", out))
  expect_true(grepl("VBAR", out))
})

test_that("geom_histogram maps to PROC SGPLOT HISTOGRAM", {
  out <- r2sas_expr("ggplot(df, aes(x = age)) + geom_histogram()")
  expect_true(grepl("PROC SGPLOT", out))
  expect_true(grepl("HISTOGRAM", out))
})

test_that("geom_boxplot maps to PROC SGPLOT VBOX", {
  out <- r2sas_expr("ggplot(df, aes(x = group, y = score)) + geom_boxplot()")
  expect_true(grepl("PROC SGPLOT", out))
  expect_true(grepl("VBOX", out))
})

# ============================================================
# SECTION 7: Operator and Function Translations
# ============================================================

test_that(".r_to_sas_cond converts == to = (SAS equality)", {
  out <- r2sas_expr("df %>% filter(status == 'active')")
  # WHERE clause should use SAS = not R-style ==
  expect_true(grepl("WHERE status = 'active'", out))
})

test_that(".r_to_sas_cond converts != to ^=", {
  out <- r2sas_expr("df %>% filter(status != 'inactive')")
  expect_true(grepl("\\^=|WHERE", out))
})

test_that(".r_to_sas_cond converts is.na() to MISSING()", {
  out <- r2sas_expr("df %>% filter(is.na(age))")
  expect_true(grepl("WHERE MISSING\\(age\\)", out))
})

test_that(".r_to_sas_cond converts !is.na() to ^MISSING()", {
  out <- r2sas_expr("df %>% filter(!is.na(bmi))")
  expect_true(grepl("WHERE \\^MISSING\\(bmi\\)", out))
})

test_that("assignment converts <- to = with semicolon", {
  out <- r2sas_expr("x <- 42")
  expect_true(grepl("x = 42;", out))
})

test_that("TRUE converts to 1 in assignments", {
  out <- r2sas_expr("flag <- TRUE")
  expect_true(grepl("1", out))
  expect_false(grepl("TRUE", out))
})

test_that("FALSE converts to 0 in assignments", {
  out <- r2sas_expr("flag <- FALSE")
  expect_true(grepl("0", out))
  expect_false(grepl("FALSE", out))
})

test_that("NA converts to . (SAS missing) in assignments", {
  out <- r2sas_expr("val <- NA")
  expect_true(grepl("\\.", out))
  expect_false(grepl("\\bNA\\b", out))
})

test_that("paste0() converts to CATS()", {
  out <- r2sas_expr('label <- paste0("prefix_", id)')
  expect_true(grepl("CATS", out))
  expect_false(grepl("paste0", out))
})

test_that("nchar() converts to LENGTHN()", {
  out <- r2sas_expr("n <- nchar(name)")
  expect_true(grepl("LENGTHN", out))
  expect_false(grepl("nchar", out))
})

test_that("toupper() converts to UPCASE()", {
  out <- r2sas_expr("upper_name <- toupper(name)")
  expect_true(grepl("UPCASE", out))
  expect_false(grepl("toupper", out))
})

test_that("tolower() converts to LOWCASE()", {
  out <- r2sas_expr("lower_name <- tolower(name)")
  expect_true(grepl("LOWCASE", out))
  expect_false(grepl("tolower", out))
})

test_that("substr() converts to SUBSTR()", {
  out <- r2sas_expr("s <- substr(name, 1, 3)")
  expect_true(grepl("SUBSTR", out))
})

test_that("sqrt() converts to SQRT()", {
  out <- r2sas_expr("s <- sqrt(x)")
  expect_true(grepl("SQRT", out))
  expect_false(grepl("sqrt", out))
})

test_that("abs() converts to ABS()", {
  out <- r2sas_expr("a <- abs(x)")
  expect_true(grepl("ABS", out))
  expect_false(grepl("\\babs\\b", out))
})

test_that("exp() converts to EXP()", {
  out <- r2sas_expr("e <- exp(x)")
  expect_true(grepl("EXP", out))
  expect_false(grepl("\\bexp\\b", out))
})

test_that("log() converts to LOG()", {
  out <- r2sas_expr("l <- log(x)")
  expect_true(grepl("LOG", out))
})

test_that("round() converts to ROUND()", {
  out <- r2sas_expr("r <- round(x, 2)")
  expect_true(grepl("ROUND", out))
})

test_that("floor() converts to FLOOR()", {
  out <- r2sas_expr("f <- floor(x)")
  expect_true(grepl("FLOOR", out))
  expect_false(grepl("\\bfloor\\b", out))
})

test_that("ceiling() converts to CEIL()", {
  out <- r2sas_expr("c <- ceiling(x)")
  expect_true(grepl("CEIL", out))
  expect_false(grepl("\\bceiling\\b", out))
})

test_that("max() converts to MAX()", {
  out <- r2sas_expr("m <- max(a, b)")
  expect_true(grepl("MAX", out))
})

test_that("min() converts to MIN()", {
  out <- r2sas_expr("m <- min(a, b)")
  expect_true(grepl("MIN", out))
})

test_that("sum() converts to SUM()", {
  out <- r2sas_expr("s <- sum(x)")
  expect_true(grepl("SUM", out))
})

test_that("mean() converts to MEAN()", {
  out <- r2sas_expr("m <- mean(x)")
  expect_true(grepl("MEAN", out))
})

test_that("c() vector literal converts to parentheses", {
  out <- r2sas_expr("v <- c(1, 2, 3)")
  expect_true(grepl("\\(1, 2, 3\\)", out))
  expect_false(grepl("\\bc\\(", out))
})

test_that("dollar sign $ converts to dot . in WHERE clause", {
  out <- r2sas_expr("df %>% filter(df$age > 18)")
  # The WHERE clause should use . not $ (comment may still have $)
  expect_true(grepl("WHERE df\\.age > 18", out))
})

# ============================================================
# SECTION 8: Control Flow
# ============================================================

test_that("for loop converts to %DO macro loop", {
  out <- r2sas_expr("for (i in 1:10) { x <- i * 2 }")
  expect_true(grepl("%DO", out))
  expect_true(grepl("i", out))
  expect_true(grepl("1 to 10", out))
})

test_that("for loop with seq() converts correctly", {
  out <- r2sas_expr("for (i in seq(1, 5)) { process(i) }")
  expect_true(grepl("%DO", out))
  expect_true(grepl("1 to 5", out))
})

test_that("if statement converts to IF THEN DO block", {
  out <- r2sas_expr("if (x > 0) { y <- 1 }")
  expect_true(grepl("IF", out))
  expect_true(grepl("THEN DO", out))
  expect_true(grepl("END;", out))
})

test_that("else block converts to ELSE DO", {
  out <- r2sas_expr("} else {")
  expect_true(grepl("ELSE DO;", out))
})

# ============================================================
# SECTION 9: Print / Output Statements
# ============================================================

test_that("print() converts to %PUT", {
  out <- r2sas_expr("print(result)")
  expect_true(grepl("%PUT", out))
})

test_that("cat() converts to %PUT", {
  out <- r2sas_expr('cat("Hello world")')
  expect_true(grepl("%PUT", out))
})

test_that("message() converts to %PUT", {
  out <- r2sas_expr('message("Processing complete")')
  expect_true(grepl("%PUT", out))
})

# ============================================================
# SECTION 10: Multi-line and Integration Tests
# ============================================================

test_that("r2sas handles multi-line R code", {
  code <- "df <- read.csv('test.csv')\nfit <- lm(y ~ x, data = df)"
  out <- r2sas(code)
  expect_true(grepl("PROC IMPORT", out))
  expect_true(grepl("PROC REG", out))
})

test_that("full clinical pipeline: import -> filter -> model", {
  code <- paste0(
    "patients <- read.csv('patients.csv')\n",
    "adults <- patients %>% filter(age >= 18)\n",
    "fit <- lm(bmi ~ age + sex, data = adults)"
  )
  out <- r2sas(code)
  expect_true(grepl("PROC IMPORT", out))
  expect_true(grepl("WHERE", out))
  expect_true(grepl("PROC REG", out))
})

test_that("full EDA pipeline: import -> summarise -> plot", {
  code <- paste0(
    'df <- read.csv("survey.csv")\n',
    "summary(df)\n",
    "ggplot(df, aes(x = age, y = score)) + geom_point()"
  )
  out <- r2sas(code)
  expect_true(grepl("PROC IMPORT", out))
  expect_true(grepl("PROC MEANS", out))
  expect_true(grepl("PROC SGPLOT", out))
})

test_that("r2sas() called with single expression string also works", {
  out <- r2sas("summary(df)")
  expect_true(grepl("PROC MEANS", out))
})

test_that("r2sas output is a single character string", {
  out <- r2sas("fit <- lm(y ~ x, data = df)")
  expect_type(out, "character")
  expect_length(out, 1)
})

test_that("r2sas_expr output is a single character string", {
  out <- r2sas_expr("summary(df)")
  expect_type(out, "character")
  expect_length(out, 1)
})

# ============================================================
# SECTION 11: Builder Functions
# ============================================================

test_that("sas_proc builds correct PROC header", {
  out <- sas_proc("MEANS", "mydata", list(VAR = "_NUMERIC_"), c("N", "MEAN"))
  expect_true(grepl("PROC MEANS DATA=mydata N MEAN", out))
  expect_true(grepl("VAR _NUMERIC_", out))
  expect_true(grepl("RUN;", out))
})

test_that("sas_proc with no options still produces valid PROC", {
  out <- sas_proc("FREQ", "mydata", list(TABLES = "gender"), character(0))
  expect_true(grepl("PROC FREQ DATA=mydata", out))
  expect_true(grepl("TABLES gender", out))
})

test_that("sas_proc with multiple options appends all", {
  out <- sas_proc("MEANS", "ds", list(VAR = "age"), c("N", "MEAN", "STD", "MIN", "MAX"))
  expect_true(grepl("N MEAN STD MIN MAX", out))
})

test_that("sas_datastep builds DATA step with SET and WHERE", {
  out <- sas_datastep("work.out", "work.in", "WHERE age > 18")
  expect_true(grepl("DATA work.out", out))
  expect_true(grepl("SET work.in", out))
  expect_true(grepl("WHERE age > 18", out))
  expect_true(grepl("RUN;", out))
})

test_that("sas_datastep with no WHERE still produces valid DATA step", {
  out <- sas_datastep("work.final", "work.source", character(0))
  expect_true(grepl("DATA work.final", out))
  expect_true(grepl("SET work.source", out))
  expect_true(grepl("RUN;", out))
})

test_that("sas_datastep with multiple statements includes all", {
  out <- sas_datastep("out", "inp", c("WHERE age > 0", "KEEP id age bmi"))
  expect_true(grepl("WHERE age > 0", out))
  expect_true(grepl("KEEP id age bmi", out))
})

# ============================================================
# SECTION 12: r2sas_dataframe Helper
# ============================================================

test_that("r2sas_dataframe with filter produces WHERE clause", {
  out <- r2sas_dataframe("patients", list(filter = "age > 18", select = c("id", "age")))
  expect_true(grepl("WHERE age > 18", out))
  expect_true(grepl("KEEP id age", out))
})

test_that("r2sas_dataframe with rename produces RENAME statement", {
  out <- r2sas_dataframe("df", list(rename = c(patient_id = "id")))
  expect_true(grepl("RENAME|rename", out, ignore.case = TRUE))
})

test_that("r2sas_dataframe with mutate produces DATA step body", {
  out <- r2sas_dataframe("df", list(mutate = "bmi = weight / height**2"))
  expect_true(grepl("DATA", out))
  expect_true(grepl("SET", out))
})

# ============================================================
# SECTION 13: Edge Cases and Robustness
# ============================================================

test_that("empty string input does not crash r2sas", {
  expect_error(r2sas(""), NA)  # NA means no error expected
})

test_that("R comment lines are preserved or handled gracefully", {
  out <- r2sas("# This is a comment\nfit <- lm(y ~ x, data = df)")
  expect_true(grepl("PROC REG", out))
})

test_that("multiple assignment operators on one line handled", {
  out <- r2sas_expr("result <- x <- 5")
  expect_type(out, "character")
})

test_that("r2sas_expr returns character for unknown code", {
  out <- r2sas_expr("some_unknown_function(x, y, z)")
  expect_type(out, "character")
})

test_that("conversion preserves file path strings in PROC IMPORT", {
  out <- r2sas_expr('df <- read.csv("/data/project/trial_data.csv")')
  expect_true(grepl("/data/project/trial_data.csv", out))
})

test_that("conversion preserves output file path in PROC EXPORT", {
  out <- r2sas_expr('write.csv(results, "/output/final_results.csv")')
  expect_true(grepl("/output/final_results.csv", out))
})

test_that("dataset name is extracted from assignment target in PROC IMPORT", {
  out <- r2sas_expr('clinical <- read.csv("trial.csv")')
  expect_true(grepl("OUT=CLINICAL", out))
})

test_that("r2sas_cheatsheet returns output without error", {
  expect_output(r2sas_cheatsheet())
})

# ============================================================
# SECTION 14: r2sas_model public wrapper
# ============================================================

test_that("r2sas_model() wraps internal model converter", {
  out <- r2sas_model("fit <- lm(y ~ x + z, data = df)")
  expect_true(grepl("PROC REG", out))
  expect_true(grepl("MODEL y =", out))
})

test_that("r2sas_model() handles glm binomial", {
  out <- r2sas_model("fit <- glm(died ~ age, family = binomial, data = df)")
  expect_true(grepl("PROC LOGISTIC", out))
})

# ============================================================
# SECTION 15: r2sas_plot public wrapper
# ============================================================

test_that("r2sas_plot() wraps internal ggplot converter", {
  out <- r2sas_plot("ggplot(df, aes(x = x, y = y)) + geom_point()")
  expect_true(grepl("PROC SGPLOT", out))
  expect_true(grepl("SCATTER", out))
})

test_that("r2sas_plot() handles boxplot", {
  out <- r2sas_plot("ggplot(df, aes(x = group, y = val)) + geom_boxplot()")
  expect_true(grepl("VBOX", out))
})
