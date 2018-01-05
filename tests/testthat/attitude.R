context("attitude")


test_that("attitude data frame", {

  options("shoegazer.test"=as.POSIXct("1985-10-27 19:30:00"))
  
  
  expect_output_file(
    stargazer(attitude),
    file = "tests/testthat/attitude.df.tex"
  #  ,update=TRUE
  )  

  expect_output_file(
    stargazer(attitude, type = "html"),
    file = "tests/testthat/attitude.df.html"
  #  ,update=TRUE
  )  
  
    
})
