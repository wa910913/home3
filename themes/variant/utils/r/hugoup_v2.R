

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
debugp <- function(...)
{
  write(paste0("debug", unlist(...)), file = stderr())
}
wprint <- function(cmd, body) {
  paste0("{{", "< ", cmd, ">}}\n", body, "\n{{", "< /", cmd, " >", "}}")
}

`%n%` = function(x, y) if (is.null(x)) y else x

one_string = function(x, ...)
  paste(x, ..., collapse = '\n')
splitLines <- function(x) {
  if (length(grep("\n", x)) == 0L)
    return(x)
  x = gsub("\n$", "\n\n", x)
  x[x == ""] = "\n"
  unlist(strsplit(x, "\n"))
}
adddot<-function(x){
gsub("[.]*",".",x)
}
dedot<-function(x){
gsub("[.]*","",x)
}
# 收集類別
#block.pre.class = ".ms-5.w-75"
block.pre.class = ""
# 所有class都加點
as.markdownClass = function(x) {
  if (length(x) > 0){
     y<-unlist(strsplit(x, '\\s+|[.]'))
     y<-y[sapply(y,function(m){nchar(m)>0})]
    gsub('^[.]*', '.', y)
  }
}

# outTYpe: source,output,error,warning
parseBlockClass <- function(options, outType) {
  aClass = paste0("class.", outType)
  aAttr = paste0("attr.", outType)
  rst = options[[aClass]]
  if (length(options[[aAttr]]) > 0) {
    item = regmatches(options[[aAttr]], regexec("class=(.*)[ |,|$]?", options[[aAttr]]))
    if (length(item) > 0) {
      rst = paste0(rst, " ", item[[1]][2])
    }
  }
  #rst = paste( block.pre.class, rst ) # 到目前為止都是空格分開
  rst = paste(rst, outType)
  
  rst = paste0(as.markdownClass(rst), collapse = "")
  
  return(rst)
}
 
# 重新整理chunk 類別

parseHead <- function(x, options, blockType)
{
  y = splitLines(x)
  headidx = which(grepl("```.*$", y)) # ```
  if (length(headidx) == 0)
    return(x)
  thislang = tolower(options$engine)
  sa <- glue::glue("```{thislang}?lang={thislang}")
  for (i in 1:1) {
    #codeClass= collectOutputClass(options)
    codeClass = parseBlockClass(options, blockType)
    if (codeClass != "")
      sa <- glue::glue("{sa}?class={codeClass}")
  }
   if (length(options[["block.pre.class"]]) > 0)
     sa <- glue::glue('{sa}?preclass={options[["block.pre.class"]]}')

  y[headidx[i]] <- sa
  rst <- one_string(y)
  return(rst)
}

parseSourceHead <- function(x, options)
{
  # y = splitLines(x)
  # #要找出 ```{r開頭、```r、```.r一直到結尾
  # headidx = which(grepl("```\\{?[.]?r.*$",y))
  
  # if(length(headidx)==0)
  #   return(x)
  # thislang=tolower(options$engine)
  # #print(paste0("找到",y[headidx]))
  
  # print(y[headidx])
  # sa<-glue::glue("```{thislang}?lang={thislang}")
  # for(i in 1:length(headidx)){
  #   #codeClass= collectSourceClass(options)
  #   codeClass = parseBlockClass(options,"source")
  #   if(codeClass!="")
  #   sa<-glue::glue("{sa}?preclass={codeClass}")
  #   y[headidx[i]]<-sa
  # }
  
  # rst<-one_string(y)
  rst <- parseHead(x, options, "source")
  if (!is.null(options$quiz) &&
      options$quiz %in% c("head", "body")) {
    if (options$quiz == "body") {
      rst <-
        paste0(
          "\n{{<",
          glue::glue(" quizbody id=\"{options$label}\" "),
          ">}}\n",
          rst,
          "\n{{< /quizbody >}}\n"
        )
    } else if (options$quiz == "head") {
      rst <-
        paste0(
          "\n{{<",
          glue::glue(" quizhead id=\"{options$label}\" "),
          ">}}\n",
          rst,
          "\n{{< /quizhead >}}\n"
        )
    }
  }
  
  
  return(rst)
}

parseOutputHead <- function(x, options)
{
  # y = splitLines(x)
  # headidx = which(grepl("```.*$",y)) # ```
  # if(length(headidx)==0)
  #   return(x)
  # print(y[headidx])
  # thislang=tolower(options$engine)
  # sa<-glue::glue("```{thislang}?lang={thislang}")
  # for(i in 1:1){
  #   #codeClass= collectOutputClass(options)
  #   codeClass = parseBlockClass(options,"output")
  #   if(codeClass!="")
  #   sa<-glue::glue("{sa}?preclass={codeClass}")
  #   y[headidx[i]]<-sa
  # }
  # #print(y)
  # rst<-one_string(y)
  rst <- parseHead(x, options, "output")
  if (!is.null(options$quiz) &&
      options$quiz %in% c("head", "body")) {
    if (options$quiz == "head") {
      rst <-
        paste0(
          "\n{{<",
          glue::glue(" quizbody id=\"{options$label}\" "),
          ">}}\n",
          rst,
          "\n{{< /quizbody >}}\n"
        )
    } else if (options$quiz == "body") {
      rst <-
        paste0(
          "\n{{<",
          glue::glue(" quizhead id=\"{options$label}\" "),
          ">}}\n",
          rst,
          "\n{{< /quizhead >}}\n"
        )
    }
  }
  return(rst)
}


# 主程式
ohook_source <- knitr::knit_hooks$get('source')
knitr::knit_hooks$set(
  source = function(x, options) {
    y <- ohook_source(x, options)
    if (isTRUE(options$hugomd)) {
      y <- parseSourceHead(y, options)
    }
    return(y)
  }
)

ohook_output <- knitr::knit_hooks$get('output')
knitr::knit_hooks$set(
  output = function(x, options) {
    y <- ohook_output(x, options)
    if (isTRUE(options$hugomd) & !isTRUE(options$collapse)) {
      y <- parseOutputHead(y, options)
    }
    return(y)
  }
)



ohook_warning <- knitr::knit_hooks$get('warning')
knitr::knit_hooks$set(
  warning = function(x, options) {
    y <- ohook_warning(x, options)
    if (isTRUE(options$hugomd)) {
      y <- parseHead(y, options, "warning")
    }
    return(y)
  }
)


ohook_error <- knitr::knit_hooks$get('error')
knitr::knit_hooks$set(
  error = function(x, options) {
    y <- ohook_error(x, options)
    if (isTRUE(options$hugomd)) {
      y <- parseHead(y, options, "error")
    }
    return(y)
  }
)

# 當collapse=T的時候,要清掉output chunk open tag
ohook_chunk <- knitr::knit_hooks$get('chunk')
knitr::knit_hooks$set(
  chunk = function(x, options) {
    y <- ohook_chunk(x, options)
    
    if (isTRUE(options$hugomd)) { # 只有這個狀態才處理
      if (isTRUE(options$collapse)) { 
        # 輸出輸出都同一個BLOCK,把凡是```\n```....?lang=R...\n開頭的都殺掉(就是區塊結尾+區塊開頭)
        r <- "\n([`]{3,})\n+\\1.*?\\?lang=R[^\n]*?\n" 
        y <- gsub(r, "\n", y)
      }
      else{
        
        if(grepl("\\{\\{<[\\s]*([\\w]*).*>\\}\\}([\\s\\S]*?)\\{\\{<[\\s]*\\/\\1[\\s]*>\\}\\}",y,perl=T)){
          m<-gregexec("\\{\\{<[\\s]*([\\w]*).*>\\}\\}([\\s\\S]*?)\\{\\{<[\\s]*\\/\\1[\\s]*>\\}\\}",y,perl=T)
          ms<-m[[1]] #
          ml<-attr(ms,"match.length")
          size<-dim(ms)
          for(ncol in size[2]:1){
            start<-ms[1,ncol]
            end<-ms[1,ncol]+ml[1,ncol]-1
            ss<-substr(y,start,end)
            ss<-gsub("\n([`]{3,})[\n]+\\1r?\\?.*?\n","\n",ss,perl=T) #有```開頭的都去掉
            #ss<-gsub("[`]{3,}).*?\n","\n",ss,perl=T) #有```開頭的都去掉
            y<-paste0(substr(y,1,start-1),ss,substr(y,end+1,nchar(y))) #貼回去
          }
        }  

      }
    }
    return(y)
  }
)

.upload.url = function(x) {
  opts_knit$get('upload.fun')(x)
} 

ohook_plot <- knitr::knit_hooks$get('plot')
knitr::knit_hooks$set(
  plot = function(x, options) {
    if (options$fig.show!="asis" || isFALSE(options$hugomd)) {
      y <- ohook_plot(x, options)
      return(y)
    }
    base = opts_knit$get('base.url') %n% ''
    cap = paste0("figure ",options$fig.cap )
    #block.pre.class= options$block.pre.class %n% ''
    plot.class=options$plot.class %n% ''
    #alt = .img.cap(options, alt = TRUE)  
    #figclass= parseBlockClass(options)
    
    #res = sprintf('![%s](%s%s)', cap, base, .upload.url(x))   
    #print(options$block.pre.class)
    paste0("{{< ",
      glue::glue(
      'figure src="{x}" class=" {plot.class} "
      caption="{cap}"  attrlink=""' 
      ),
      ">}}")   
  }
)

# 開關設定
# knitr::opts_chunk$set(collapse=F)
#knitr::opts_chunk$set(hugomd=T)

# define a method for objects of the class data.frame
# 只是在外面加一個 bstable macro
knit_print.data.frame = function(x, ...) {
  res = paste(c(
    '{{< bstable table_class="w-50 m-auto">}}',
    kable(x),
    '{{< /bstable >}}'
  ),
  collapse = '\n')
  asis_output(res)
}
# register the method
registerS3method("knit_print", "data.frame", knit_print.data.frame)

