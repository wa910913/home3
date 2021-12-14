
# hugocmd <- function(m,arg,autoend=F,end=F){
#   if (end){
#     cat(paste("{{<",paste0("/",m),">}}",sep=" "))
#   }
#   else{
#   cmdstr = paste("{{<",m,arg, ">}}",sep=" ")
#   cat(cmdstr)
  
#   if (autoend){
#        cat(paste("{{<",paste0("/",m),">}}",sep=" "))
#   }
#   }
#   return (invisible())
# }
##---------------------------
library(knitr)
debugp<-function(...)
{
  write(paste0("debug",unlist(...)),file=stderr())
}
wprint<- function(cmd,body){
  paste0("{{","< ",cmd,">}}\n",body,"\n{{","< /",cmd," >","}}" )
}
one_string = function(x, ...) paste(x, ..., collapse = '\n')
splitLines<-function(x){
    if (length(grep("\n", x)) == 0L) 
        return(x)
    x = gsub("\n$", "\n\n", x)
    x[x == ""] = "\n"
    unlist(strsplit(x, "\n"))
}

# 收集類別
collectSourceClass<-function(options){
 rst=""   
 if(!is.null(options$class.source) && nzchar(options$class.source)   ){
      rst = options$class.source
 }else{
     rst=".code-source"
 }
 if(!is.null(options$attr.source) && nzchar(options$attr.source) ){   
   attr.class = regmatches(options$attr.source, regexec("class=(.*)[ |,|$]?", options$attr.source))
   if(length(attr.class)>0){
       rst=paste0(rst,".",attr.class[[1]][2])
   }
 }
 return(rst)
}

collectOutputClass<-function(options){
 rst=""   
 if(!is.null(options$class.output) && nzchar(options$class.output)   ){
      rst = options$class.output
 }else{
     rst=".code-out"
 }
 if(!is.null(options$attr.output) && nzchar(options$attr.output) ){   
   attr.class = regmatches(options$attr.output, regexec("class=(.*)[ |,|$]?", options$attr.output))
   if(length(attr.class)>0){
       rst=paste0(rst,".",attr.class[[1]][2])
   }
 }
 return(rst)
}
# 重新整理chunk 類別
parseSourceHead <-function(x,options)
{
  
  y = splitLines(x)
  #要找出 ```{r開頭、```r、```.r一直到結尾
  headidx = which(grepl("```\\{?[.]?r.*$",y)) 
  
  if(length(headidx)==0)
    return(x)
  thislang=tolower(options$engine)
  #print(paste0("找到",y[headidx]))
  
  print(y[headidx])
  sa<-glue::glue("```{thislang}?lang={thislang}")
  for(i in 1:length(headidx)){
    exclass= collectSourceClass(options)
    if(exclass!="")
    sa<-glue::glue("{sa}?preclass={exclass}")
    y[headidx[i]]<-sa
  }
  
  rst<-one_string(y)
  
   if (!is.null(options$quiz) && options$quiz %in% c("head","body")) {
     if(options$quiz=="body") {
       rst<-paste0("\n{{<",glue::glue(" quizbody id=\"{options$label}\" "),">}}\n",rst,"\n{{< /quizbody >}}\n")
     }else if (options$quiz=="head"){
       rst<-paste0("\n{{<", glue::glue(" quizhead id=\"{options$label}\" "),">}}\n",rst,"\n{{< /quizhead >}}\n")
     }
   }

  
  return(rst)
}

parseOutputHead <-function(x,options)
{
  y = splitLines(x)
  headidx = which(grepl("```.*$",y)) # ```
  if(length(headidx)==0)
    return(x)
  print(y[headidx])
  thislang=tolower(options$engine)
  sa<-glue::glue("```{thislang}?lang={thislang}")
  for(i in 1:1){
    exclass= collectOutputClass(options)
    if(exclass!="")
    sa<-glue::glue("{sa}?preclass={exclass}")
    y[headidx[i]]<-sa
  }
  #print(y)  
  rst<-one_string(y)
   if (!is.null(options$quiz) && options$quiz %in% c("head","body")) {
     if(options$quiz=="head") {
       rst<-paste0("\n{{<",glue::glue(" quizbody id=\"{options$label}\" "),">}}\n",rst,"\n{{< /quizbody >}}\n")
     }else if (options$quiz=="body"){
       rst<-paste0("\n{{<",glue::glue(" quizhead id=\"{options$label}\" "),">}}\n",rst,"\n{{< /quizhead >}}\n")
     }
   }
  return(rst)
}


# 主程式
ohook_source <- knitr::knit_hooks$get('source') 
knitr::knit_hooks$set(source = function(x, options) {
  if (isTRUE(options$hugomd)) {  
    y<-ohook_source(x, options)
    y<-parseSourceHead(y,options)
    #y<-wprint("markdown", y)
    return(y)
  } else {
    return(ohook_source(x, options))
  }
})

ohook_output <- knitr::knit_hooks$get('output') 
knitr::knit_hooks$set(output = function(x, options) {
  if (isTRUE(options$hugomd) & !isTRUE(options$collapse)) {
    y<-ohook_output(x, options)
    y<-parseOutputHead(y,options)
    return(y)
  } else {
    return(ohook_output(x, options))
  }
})

# 當collapse=T的時候,要清掉output chunk open tag
ohook_chunk <- knitr::knit_hooks$get('chunk') 
knitr::knit_hooks$set(chunk = function(x, options) {  
  y<-ohook_chunk(x, options)
  
  if (isTRUE(options$hugomd) & isTRUE(options$collapse) ) {
    r<-"\n([`]{3,})\n+\\1\\?lang=R[^\n]*?\n"
    y<-gsub(r,"\n",y)
  }
  return(y)  
})
# 開關設定
# knitr::opts_chunk$set(collapse=F)
#knitr::opts_chunk$set(hugomd=T)
 
# define a method for objects of the class data.frame
# 只是在外面加一個 bstable macro
knit_print.data.frame = function(x, ...) {
  res = paste(c('{{< bstable table_class="w-50 m-auto">}}', kable(x), '{{< /bstable >}}'),collapse = '\n')
  asis_output(res)
}
# register the method
registerS3method("knit_print", "data.frame", knit_print.data.frame)