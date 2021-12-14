# 經驗報告
# 執行的時候,工作目錄在專案根目錄,.profile也有讀入

relativePath<- function(apath,root){
  apath = normalizePath(apath,winslash="/",mustWork =F) # 反斜線都變斜線
  root = normalizePath(root,winslash="/",mustWork =F)
  if(substr(root,nchar(root),nchar(root))!="/"  ) # 最後一個字,一定是要分隔符號
   root=paste0(root,"/")
    #root=substr(root,1,nchar(root)-1)
  #root=paste0(root,"[/]?")  
  return(gsub(root,"",apath))
}

knitr_files_dir <- function(file) {
  paste(xfun::sans_ext(file), "_files", sep = "")
}

tryCatch({
  # knitr::opts_knit$set(base.dir = normalizePath("static/", mustWork = TRUE),
  #                      base.url = "/")
  
#knitr::knit_hooks$set(knitr::hooks_markdown())
#knitr::knit_hooks$set(knitr::hooks_html()) # 會變成HTML輸出
knitr::render_markdown(strict = FALSE, fence_char = "`") # 大致上等同knitr::knit_hooks$set(knitr::hooks_markdown())
knitr::opts_knit$set(rmarkdown.pandoc.to="html")
source("themes/variant/utils/r/hugoutils.r",environment(),encoding="UTF-8")
source("themes/variant/utils/r/hugoup.r",environment(),encoding="UTF-8")

  cachepath = normalizePath("cache/", mustWork = F)
  if(!file.exists(cachepath)){
    dir.create(cachepath)
  }
  knitr::opts_chunk$set(
    cache.path = normalizePath("cache/", mustWork = TRUE),
    #collapse = F,
    comment  = "#>"
  )
  
  #infile <- commandArgs(TRUE)
  #infile <-"content\\r\\engine\\knitr自訂輸出.Rmarkdown"
  # infile<-"content\\r\\engine\\純測試.Rmarkdown"
  outfile<-""
  if (!tools::file_ext(infile) %in% c("Rmarkdown","Rmd") ){
   stop("不是Rmarkdown,也不是RMD")
   }
  #infile<-"content\\rlang\\4_3_type_matrix.Rmarkdown"

  if (grepl("\\Rmd$", infile)) {
    outfile  <- sub("\\.Rmd$", ".md", infile)
  } else if (grepl("\\Rmarkdown$", infile)) {
    outfile  <- sub("\\.Rmarkdown$", ".markdown", infile)
  } else {
    stop("不會到這裡")
  }
  # backupfile<-paste0(outfile,"~")
  # if (file.exists(backupfile))
  #  unlink(backupfile)
  # if(file.exists(outfile)) 
  #  file.rename(outfile,backupfile) 
  indir=relativePath(dirname(infile),"content") #不用了吧,這個是用來去掉content的目錄
  base_name <- tools::file_path_sans_ext(basename(infile))
  knitr_files_root<-paste(base_name,"_files",sep="")
  print(sprintf("輸入檔案%s",infile))  
  
  knitr::opts_knit$set(root.dir = dirname(infile))
  knitr::opts_knit$set(base.dir = dirname(infile))
  knitr::opts_chunk$set(
    #fig.path = glue::glue("{base_name}_files/figure-html/")
    fig.path = glue::glue("{knitr_files_root}/figure-html/")
  )
  
  # knitr::opts_chunk$set(dev="png")
  # res= knitr::knit(input = infile,
  #              output = outfile,
  #              encoding = "UTF-8")  #,env=environment())

  #input <- readLines(infile,encoding="UTF-8")
  input<-xfun::read_utf8(infile)
  mdtxt <- knit(text = input,envir=new.env(),quiet=F)
  md<-splitLines(mdtxt)
  # Get dependencies from knitr
  deps <- knit_meta()
  deps<-deps[lapply(deps,function(item) {inherits(item,"html_dependency")})==T]
  if (length(deps)) {
    m = rmarkdown:::html_dependencies_as_string(deps, file.path(dirname(infile),knitr_files_root), dirname(infile))
    if (length(i <- grep("^---\\s*$", md)) >= 2) {
      md = append(md, m, i[2])
    }
    else warning("找不到YAML ", 
                 "無法寫出HTML dependencies")
  }  
  
  mdtxt<-one_string(md) 
  writeLines(mdtxt, outfile,useBytes = T)


  print(paste0("輸出:",outfile))             
},
error=function(cond) {
  print( paste0("錯誤:",conditionMessage(cond) ))
},
warning=function(cond) {
  print(warning(cond))
  
},
finally={
  print("done")
})
 