function parseParameter(params) {
  var keypairs = params.split('?');
  var obj = {};
  if (keypairs.length >= 2) {
    for (let i = 1; i < keypairs.length; i++) {
      let keypair = keypairs[i].split('=');
      obj[keypair[0]] = keypair[1];
    }
  }
  return obj;
};

function insertAfter(newNode, referenceNode) {
  return referenceNode.parentNode.insertBefore(newNode, referenceNode.nextSibling);
}
//----------begin ---- 掛上stata 產生的文件 
function mountToa(ds){
  let toa = document.querySelector(".toa")
  let ul=null;
  if (toa.childElementCount==0){
   ul=toa.appendChild(document.createElement("ul"));
  }else{
    ul=toa.querySelector("ul");
  }
  let li=ul.appendChild(document.createElement("li"));
  let item = li.appendChild(document.createElement("a"));
  item.href="#"+ds.posixId;
  item.innerText=ds.posixTitle;
  
}

/*
 v1: 尋找短碼產生然後在這個PRE之後,顯示HTML或是圖形。
 
*/
function stataPosix2() {
  
  let nodes = document.querySelectorAll("[data-role-posix]");
  if (nodes.length ==0 ) return;

  for (let i = 0; i < nodes.length; i++) {
    let posixnode = nodes[i];
    let dataset = posixnode.dataset;
    let parent = posixnode.closest("pre");
    let next = parent.nextSibling;
    let post = null;
    //在pre 之後,找下一個節點,如果沒有類別post,就是第一個,否則加入節點在後面
    if (next &&  next.classList && next.classList.contains("post")){
      post= next;
    }else{
      post = insertAfter(document.createElement("div"),parent);
      post.classList.add("post"); //第一個
    } 
    
    let link = posixnode.href || posixnode.src;
    let fileExt = link.split('.').pop();  //副檔名
    if (fileExt == "html") {   
      let label = post.appendChild(document.createElement("label"));
      label.innerText = dataset.posixTitle;
      let e = document.createElement("div");
      e.id="attach_"+i;
      e.classList.add("outTable");
      if (dataset.posixClass) e.classList.add(dataset.posixClass);
      dataset["posixId"]=e.id;
      post.appendChild(e);
      
      mountToa(dataset);
      fetch(link) //讀入,並顯示html
        .then(r => r.text())
        .then(t => {   
          e.innerHTML = t;
        })
    } else if(["png","jpg","gif"].includes(fileExt)){ //找到的是圖形檔案
      let e=document.createElement("img");
      e.id="attach_"+i;
      if (dataset.posixClass) e.classList.add(dataset.posixClass);
      dataset["posixId"]=e.id;
      post.appendChild(e);
      e.src=link;
      mountToa(dataset)
    } else{
      let e=document.createElement("a");
      post.appendChild(e);
      e.href=link;
      e.id="attach_"+i;
      if (dataset.posixClass)  e.classList.add(dataset.posixClass);
      dataset["posixId"]=e.id;
      e.innerText=posixnode.dataset.posixTitle + "(" + posixnode.innerText+")";
      mountToa(dataset);
    }
  }
}
function stataPosix() {
  
  let nodes = document.querySelectorAll("[data-role-posix]");
  if (nodes.length ==0 ) return;

  for (let i = 0; i < nodes.length; i++) {
    let posixnode = nodes[i];
    let dataset = posixnode.dataset;
      mountToa(dataset);
  }
}

//----------end--掛上stata 產生的文件----------- 

function htmlUnescape(str){
    //re = /(<!--html_preserve-->)((\s|\S)*?)(?:^\1|<!--\/html_preserve-->)/g;
    //re = /(&lt;!--html_preserve--&gt;)((\s|\S)*?)(?:^\1|&lt;!--\/html_preserve--&gt;)/g
    //上面兩個都要
    re = /((&lt;|<)!--html_preserve--(&gt;|>))((\s|\S)*?)(?:^\1|(&lt;|<)!--\/html_preserve--(&gt;|>))/g; 
    str=str.replace(re,(m)=>unescapeHtmlChunk(m))
    return( str);
    
}

function unescapeHtmlChunk(str){
  var map = {amp: '&', lt: '<', gt: '>', quot: '"', '#039': "'"}
str = str.replaceAll(/&([^;]+);/g, (m, c) => map[c])
str=str.replace(/\s+/g, ' ')
//console.log(str)
return( str);
}

function htmlEscape(str) {
  var map = {
    '&': '&amp;',
    '<': '&lt;',
    '>': '&gt;',
    '"': '&quot;',
    "'": '&#039;'
  };
  
  return str.replace(/[&<>"']/g, function(m) { return map[m]; });
}
// v1 pair  
function pair(headidx,tailidx){
  return ({"head":headidx,"tail":tailidx})
}
// v1 pair  
function debugtoken(str,pairs){
  pairs.forEach(element => {
    console.log(str.substr(element.head,element.tail-element.head+1))
  });
}
// v1 pair  
function inpair(pairs,nidx){
  let rst = false;
  for(let idx=0;idx<pairs.length;idx++){
    if(nidx>=pairs[idx].head && nidx<=pairs[idx].tail){
      rst=true
      break;
    }
  }
  return rst;
}
// v1 pair  
function hugo_unescape(str){
  //str = codeblock.innerHTML;
  let htmlstr=htmlUnescape(str)
  let area = document.createElement("div");
  area.style.display = 'none'
  area.innerHTML=htmlstr;
  //area=temparea.querySelector("code") 
  // 1) 這裡利用outerHTML和innerHTML找出頭尾標籤的開始和結束位置。
  // 2) 然後利用這些位置，區分這個區域的字串為是否要處理成HTML
  let npblock = Array.from(area.querySelectorAll("[data-nopre]"))
  let pairs = [];
  oheadidx=0
  npblock.forEach(element => {
    oheadidx = area.innerHTML.indexOf(element.outerHTML,oheadidx)
    olen= element.outerHTML.length;
    
    iheadidx= element.outerHTML.indexOf(element.innerHTML)
    ilen = element.innerHTML.length;
    if (iheadidx>0) //沒有子節點
      pairs.push( pair(oheadidx,oheadidx+iheadidx-1))
    pairs.push( pair(oheadidx+iheadidx+ilen,oheadidx+olen-1))
    oheadidx=oheadidx+iheadidx
    iheadidx=oheadidx+iheadidx+ilen
  });
  pairs.sort((a,b)=>a.head-b.head)
  ustr = area.innerHTML;
  
  var map = {
    '&': '&amp;',
    '<': '&lt;',
    '>': '&gt;',
    '"': '&quot;',
    "'": '&#039;'
  };

  let parts=[]
  let istrue=[]
  let ptr=0; 
  let ptail=0
  for(let i=0;i<pairs.length;i++){
    parts.push(ustr.slice(ptr,pairs[i].head))    
    istrue.push(true)
    parts.push(ustr.slice(pairs[i].head,pairs[i].tail+1))
    istrue.push(false)
    ptr=pairs[i].tail+1
  }
  if(ptr<ustr.length){
    parts.push(ustr.slice(ptr,ustr.length))
    istrue.push(true)
  }
  for(let i=0;i<parts.length;i++){
    if (istrue[i]){
      parts[i]= parts[i].replace(/(&(?!(amp;|gt;|lt;|quot;|#039))|[<>"'])/g,function(m) { return map[m]; })
    }
  }
  rst=parts.join("")
//    first.replace(/(&(?!amp)|>)/g,function(m) { return map[m]; })

 area.remove()
 return(rst)
 
}

document.addEventListener('DOMContentLoaded', (event) => {
  //這個函數的功能有2
  // 1) 處理pre 區塊屬性,利用分解問號達成
  // 2) 打光
  //var list = document.querySelectorAll("pre code");
  
  var list = document.querySelectorAll("pre:not(.chroma) code");
  for (let i = 0; i < list.length; i++) {
    codenode = list[i]
    let parent = codenode.closest("pre");
    let mm= /(.*?)(\?.*?=.*)/ 
    if(!parent || !mm.test(codenode.className))
      continue;
    
    
    matches=Array.from(codenode.className.match(mm))
    let obj = parseParameter(matches[2]);
    
    codenode.className = obj["lang"]?"language-"+obj["lang"]: matches[1];
    if ( obj["preclass"]) {
        obj["preclass"].split(".").forEach(ee =>{
           if (ee.length>0){
            parent.classList.add(ee);
           }
        }) 
    }  
    
    if (obj["class"]) {
      obj["class"].split(".").forEach(ee =>{
        if (ee.length>0){
         //parent.classList.add(ee);    //原先放在PRE
         codenode.classList.add(ee)
        }
     }) 
    }
  }

/*  
prechunks=Array.from(document.querySelectorAll("pre code:not(.language-html)"));
  prechunks= prechunks.filter(e=> !e.classList.contains("language-.html"))
  */  
  prechunks=Array.from(document.querySelectorAll("pre:not(.no-hugo-post)"));
  prechunks.forEach(apre=>{
    {
      precode = apre.querySelector("code")
      //s=hugo_unescape(precode.innerHTML) //v1 pair 
      s=htmlUnescape(precode.innerHTML)
      s=s.replace(RegExp("\\n?hugocmd.*?\\n\#(>|&gt;)","gm"),"") //殺掉指令hugocmd,有需要這個嗎?stata不需要,因為hugocmd不會出現
      
      precode.innerHTML=s
    }    
  });
  
  
  document.querySelectorAll('pre:not(.chroma) code').forEach((el) => {
    hljs.highlightBlock(el);
  });

  var tooltipTriggerList = [].slice.call(document.querySelectorAll('[data-bs-toggle="tooltip"]'))
  var tooltipList = tooltipTriggerList.map(function (e) {
    return new bootstrap.Tooltip(e)
  })
  
  //附件的位置加入TOC
  stataPosix();
});
