context("attitude correlation matrix")


test_that("attitude correlation matrix", {

  options("shoegazer.test"=as.POSIXct("1985-10-27 19:30:00"))
  
  correlation.matrix <- cor(attitude)

  expect_output_file(
    stargazer(correlation.matrix, type="latex"),
    file = "tests/testthat/correlation.matrix.tex"
    #,update=TRUE
  )  
  
  
  expect_output_file(
    stargazer(correlation.matrix, type="html"),
    file = "tests/testthat/correlation.matrix.html"
    #,update=TRUE
  )  
  
})
