library(r2sas)

test_that("lm is converted to PROC REG", {
  out <- r2sas_expr("fit <- lm(y ~ x, data = df)")
  expect_true(grepl("PROC REG", out))
  expect_true(grepl("MODEL y = x", out))
})

test_that("glm with binomial converts to PROC LOGISTIC", {
  out <- r2sas_expr("fit <- glm(y ~ x, family = binomial, data = df)")
  expect_true(grepl("PROC LOGISTIC", out))
})

test_that("t.test converts to PROC TTEST", {
  out <- r2sas_expr("t.test(x, y)")
  expect_true(grepl("PROC TTEST", out))
})

test_that("chisq.test converts to PROC FREQ with CHISQ", {
  out <- r2sas_expr("chisq.test(table(a, b))")
  expect_true(grepl("PROC FREQ", out))
  expect_true(grepl("CHISQ", out))
})

test_that("summary converts to PROC MEANS", {
  out <- r2sas_expr("summary(df)")
  expect_true(grepl("PROC MEANS", out))
})

test_that("table with two vars creates cross-tab", {
  out <- r2sas_expr("table(df$a, df$b)")
  expect_true(grepl("TABLES", out))
  expect_true(grepl("\\*", out))
})

test_that("read.csv converts to PROC IMPORT", {
  out <- r2sas_expr("df <- read.csv('data.csv')")
  expect_true(grepl("PROC IMPORT", out))
  expect_true(grepl("DBMS=CSV", out))
})

test_that("write.csv converts to PROC EXPORT", {
  out <- r2sas_expr("write.csv(df, 'out.csv')")
  expect_true(grepl("PROC EXPORT", out))
})

test_that("ggplot scatter converts to PROC SGPLOT SCATTER", {
  out <- r2sas_expr("ggplot(df, aes(x = age, y = bmi)) + geom_point()")
  expect_true(grepl("PROC SGPLOT", out))
  expect_true(grepl("SCATTER", out))
})

test_that("dplyr filter converts to DATA step WHERE", {
  out <- r2sas_expr("df %>% filter(age > 18)")
  expect_true(grepl("WHERE", out))
})

test_that("r2sas handles multi-line R code", {
  code <- "df <- read.csv('test.csv')\nfit <- lm(y ~ x, data = df)"
  out <- r2sas(code)
  expect_true(grepl("PROC IMPORT", out))
  expect_true(grepl("PROC REG", out))
})

test_that("sas_proc builds correct PROC header", {
  out <- sas_proc("MEANS", "mydata", list(VAR = "_NUMERIC_"), c("N", "MEAN"))
  expect_true(grepl("PROC MEANS DATA=mydata N MEAN", out))
  expect_true(grepl("VAR _NUMERIC_", out))
})

test_that("sas_datastep builds DATA step", {
  out <- sas_datastep("work.out", "work.in", "WHERE age > 18")
  expect_true(grepl("DATA work.out", out))
  expect_true(grepl("SET work.in", out))
  expect_true(grepl("WHERE age > 18", out))
})

test_that("r2sas_dataframe with filter and select", {
  out <- r2sas_dataframe("patients", list(filter = "age > 18", select = c("id", "age")))
  expect_true(grepl("WHERE age > 18", out))
  expect_true(grepl("KEEP id age", out))
})
