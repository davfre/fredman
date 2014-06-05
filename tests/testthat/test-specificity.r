context("specificity")

m1 <- matrix(data=c(0,0,0,0),nrow=2,ncol=2)
m2 <- matrix(data=c(0,1,0,0),nrow=2,ncol=2)

test_that("negative or all-zero numerical input returns NA", {

  #all zero
  expect_that(shannonEntropy(c(0,0), equals(NA)))

  #negative number
  expect_that(shannonEntropy(c(0,-1,1,0)), equals(NA))
    
})

test_that("non-numeric input fails", {
  expect_that(shannonEntropy(c(0,"a")), throws_error())  
})

test_that("simple numerical input gives expected entropy in bits", {
  expect_that(shannonEntropy(c(1,1)), equals(1))
  expect_that(shannonEntropy(c(1,1,1,1)), equals(2))    
  expect_that(shannonEntropy(c(0,1)), equals(0))      
})

test_that("simple numerical input gives expected specificity", {
  expect_that(temporalSpecificity(c(1,1)), equals(0))
  expect_that(temporalSpecificity(c(1,1,1,1)), equals(0))    
  expect_that(temporalSpecificity(c(0,1)), equals(1))      
})

test_that("simple numerical input gives expected categoricalSpecificity", {
  expect_that(categoricalSpecificity(c(0,1)), equals(c(Inf,1)))
  expect_that(temporalSpecificity(c(1,1)), equals(c(1,1)))    
  expect_that(categoricalSpecificity(c(0,1e-3,1e3)), equals(categoricalSpecificity(c(0,1,1e6))))      
})
