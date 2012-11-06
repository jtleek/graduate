xVals <- 1:100
yVals <- rnorm(100) + xVals

xVals <- rnorm(100)
yVals <- rnorm(100)

model <- lm(yVals ~ xVals)
plot(model)

# ?plot.lm

writeHeader <- function(string, headerLevel, hashOnly = FALSE)
{
  if (length(string) != 1)
  {
    stop("invalid string vector length: ", length(string))
  }
  if (length(headerLevel) != 1)
  {
    stop("invalid integer vector length: ", length(headerLevel))
  }
  if (class(string) != "character")
  {
    stop("invalid string object class: ", class(string))
  }
  if (class(headerLevel) != "numeric" && class(headerLevel) != "integer" )
  {
    stop("invalid header class: ", class(headerLevel))
  }
  if (headerLevel < 1 || headerLevel > 7)
  {
    stop("invalid header level: ", headerLevel)
  }
  
  headerLevel <- round(headerLevel)
  
  if (hashOnly || headerLevel > 2) 
  {
    frontHashes <- paste(rep("#", headerLevel), collapse="")
    header <- paste0(frontHashes, "\t", string)
    return(header)
  }
  else
  {
    delimitChar <- "=" 
    if (headerLevel == 2)
    {
      delimitChar <- "-"
    }
    nChar <- nchar(string)
    bottomDelimiter <- paste(rep(delimitChar, nChar), collapse="")
    header <- paste0(string, "\n", bottomDelimiter)
    return(header)
  }
}

writeCodeChunk <- function(codeVec, chunkName = "")
{
  if (class(codeVec) != "character")
  {
    stop("invalid code class: ", class(codeVec))
  }
  if (class(chunkName) != "character")
  {
    stop("invalid chunk name class: ", class(chunkName))
  }
  if (length(chunkName) != 1)
  {
    stop("invalid chunk name length: ", length(chunkName))
  }
  chunk <- "```{r"
  if (chunkName != "")
  {
    chunk <- paste(chunk, chunkName)
  }
  chunk <- paste0(chunk, "}\n")
  for (i in 1:length(codeVec))
  {
    chunk <- paste0(chunk, codeVec[i])
    chunk <- paste0(chunk, "\n")
  }
  chunk <- paste0(chunk,"```")
  return(chunk)
}

generateLmSummary <- function(model)
{
  
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
    generateLmSummary(model)
  }
  else
  {
    stop("Object of class ", class(model), " invalid")
  }
}