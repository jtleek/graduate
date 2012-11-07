source("../generateRmdSummary.R")

testCases <- function(case)
{
  if (case == 1)
  {
    xVals <- 1:100
    yVals <- rnorm(100) + xVals
    
    model <- lm(yVals ~ xVals)
    
    generateRmdSummary(model,0.05,"out1.Rmd")
  }
  else if (case == 2)
  {
    xVals <- rnorm(100)
    yVals <- rnorm(100)
    
    model <- lm(yVals ~ xVals)
    
    generateRmdSummary(model,0.05,"out2.Rmd")
  }
}