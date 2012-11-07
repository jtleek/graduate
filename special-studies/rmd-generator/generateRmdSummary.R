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

writeCodeInline <- function(string, rCode = TRUE)
{
  if (rCode)
  {
    inline <- paste0("`r ",string,"`")
    return(inline)
  }
  else
  {
    inline <- paste0("`",string,"`")
    return(inline)
  }
}

getPValue <- function(model)
{
  if (class(model) == "lm")
  {
    f <- summary(model)$fstatistic
    pValue <- pf(f[1], f[2], f[3], lower=FALSE)
    return(pValue)
  }
  else
  {
    stop("object of class ", class(model), " is invalid")
  }
}

getTitle <- function(model, rDataFile)
{
  if (class(model) == "lm")
  {
    title <- writeHeader("Analysis of a Linear Model",1)
    codeVec <- paste0("load(\"",rDataFile,"\")")
    codeSetup <- writeCodeChunk(codeVec, "importData")
    section <- paste(title,codeSetup,sep = "\n\n")
    return(section)
  }
  else
  {
    stop("object of class ", class(model), " is invalid")
  }
}

getIntroduction <- function(model)
{
  if (class(model) == "lm")
  {
    intro <- writeHeader("Introduction", 2)
    
    lmInline <- writeCodeInline("lm",FALSE)
    callChunk <- writeCodeChunk("model$call")
    
    text <- paste0("A linear regression analysis was performed in R, using the function ",
                    lmInline,
                    ". The data was fitted to the following model")
    
    section <- paste(intro, text, callChunk, sep = "\n\n")
    return(section)
  }
  else
  {
    stop("object of class ", class(model), " is invalid")
  }
}

getGraphs <- function(model)
{
  if (class(model) == "lm")
  {
    intro <- writeHeader("Graphs", 2)
    text <- "The following explanatory graphs will be presented unashamedly without commentary."
    plotChunk <- writeCodeChunk("plot(model)")
    
    section <- paste(intro, text, plotChunk, sep = "\n\n")
  }
  else
  {
    stop("object of class ", class(model), " is invalid")
  }
}

getAnalysis <- function(model)
{
  if (class(model) == "lm")
  {
    intro <- writeHeader("Data Analysis", 2)
    text1 <- "Next we show the analysis of the data."
    chunk1 <- writeCodeChunk("summary(model)")
    text2 <- "Notice that the p-value is "
    codeVec <- c("f <- summary(model)$fstatistic",
                 "pValue <- pf(f[1], f[2], f[3], lower=FALSE)",
                 "pValue")
    chunk2 <- writeCodeChunk(codeVec)
    
    section <- paste(intro, text1, chunk1, text2, chunk2, sep = "\n\n")
    return(section)
  }
  else
  {
    stop("object of class ", class(model), " is invalid")
  }
}

getConclusions <- function(model, significantAt)
{
  if (class(model) == "lm")
  {
    intro <- writeHeader("Conclusions", 2)
    
    pValue <- getPValue(model)
    if (pValue < significantAt)
    {
      text <- paste0("The results are statistically significant because of the p-value ",
                     writeCodeInline("pValue"), ".")
      section <- paste(intro, text, sep = "\n\n")
      return(section)
    }
    else
    {
      text <- paste0("The results are not statistically significant because of the p-value ",
                     writeCodeInline("pValue"), ".")
      section <- paste(intro, text, sep = "\n\n")
      return(section)
    }
  }
  else
  {
    stop("object of class ", class(model), " is invalid")
  }
}

getAppendix <- function(files)
{
  title <- writeHeader("Appendix", 2)
  
  string <- ""
  for (i in 1:length(files))
  {
    string <- paste0(string, "+","\t","[",files[i],"]","(",files[i],")","\n")
  }
  
  section <- paste(title, string, sep = "\n\n")
  return(section)
}

generateLmSummary <- function(model, significantAt, file)
{
  fileName <- "model"
  rDataFile <- paste0(fileName, ".RData")
  csvFile <- paste0(fileName, ".csv")
  save(model, file=rDataFile)
  write.csv(model$model, file=csvFile)
  
  title <- getTitle(model, rDataFile)
  intro <- getIntroduction(model)
  graphs <- getGraphs(model)
  analysis <- getAnalysis(model)
  conclusions <- getConclusions(model, significantAt)
  appendix <- getAppendix(c(rDataFile, csvFile))
  
  rmdFile <- paste(title, intro, graphs, analysis, conclusions, appendix, sep="\n\n")
  
  sink(file)
  cat(rmdFile)
  sink()
}

generateRmdSummary <- function(model, significantAt, file = "out.Rmd")
{
  if (class(model) == "lm")
  {
    generateLmSummary(model, significantAt, file)
  }
  else
  {
    stop("object of class ", class(model), " is invalid")
  }
}