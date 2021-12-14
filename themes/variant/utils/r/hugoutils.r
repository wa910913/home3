hugocmd <- function(m,arg,autoend=F,end=F){
  if (end){
    cat(paste("{{<",paste0("/",m),">}}",sep=" "))
  }
  else{
  cmdstr = paste("{{<",m,arg, ">}}",sep=" ")
  cat(cmdstr)
  
  if (autoend){
       cat(paste("{{<",paste0("/",m),">}}",sep=" "))
  }
  }
  return (invisible())
}

help2<-function(topic){
  helpfile <- utils:::.getHelpFile(help(topic))
  outfile <- tempfile(fileext = ".html")
  tools:::Rd2HTML(helpfile, out =outfile)
  
  rawHTML <- paste(readLines(outfile), collapse="\n")
  knitr::asis_output(htmltools::htmlPreserve(rawHTML))

}
