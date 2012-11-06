xVals <- 1:100
yVals <- rnorm(100) + xVals

xVals <- rnorm(100)
yVals <- rnorm(100)

model <- lm(yVals ~ xVals)
plot(model)

# ?plot.lm

getLmSummary <- function(model)
{
  get
  variableVec <- attr(attr(model$terms,"factors"),"dimnames")[[1]]
  
  save(model, file="model.RData")
  #write.table(model$model, file="model.txt")
  
  introduction <- paste0("Linear model summary\n=====================\n\n",
                         "A linear regression was performed on the data. ",
                         "The analysis and summary is supplied on this page. ",
                         "The original data can be found through a link at the bottom.\n\n")
  
  presentation <- paste0("## Presentation of the data\n\n",
                         "Here are some graphs representing the original data\n",
                         )
  
  setupChunk <- paste0("```{r setup}\nopts_chunk$set(fig.width=5, fig.height=5)\n",
                       "load(\"model.RData\")\n",
                       "```\n")
  
}

generateRmdSummary <- function(model)
{
  if (class(model) == "lm")
  {
    getLmSummary(model)
  }
  else
  {
    stop("Object of class ", class(model), " invalid")
  }
}