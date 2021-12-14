
    // Selecting the iframe element
    var iframes = document.getElementsByClassName("Result");
	[].forEach.call(iframes,
	  function(frame){
        frame.onload = function(){
		    var body = frame.contentWindow.document.querySelector('body');
		    //body.style.fontSize = '2rem';
        frame.style.height = (20+frame.contentWindow.document.body.scrollHeight) + 'px';
	    }
	  }
	)


function findAncestor (el, cls) {
    while ((el = el.parentElement) && !el.classList.contains(cls));
    return el;
}

function execArea(e,basedir){
  //var p = e.parentNode;
  //var p=findAncestor(e,"demoContainer");
  let p=e.closest(".demoContainer")
  areanode=p.getElementsByTagName("textarea")[0]
  //重新載入到iframe
  fm=p.getElementsByTagName("iframe")[0]
  fm.srcdoc=areanode.value;

}

/*
本來用這個來解決載入不同子目錄的時候,可以有不一樣的base,可是好像不行
function execArea(e,basedir){
  //var p = e.parentNode;
  //var p=findAncestor(e,"demoContainer");
  

  let p=e.closest(".demoContainer")
  areanode=p.getElementsByTagName("textarea")[0]
  //重新載入到iframe
  fm=p.getElementsByTagName("iframe")[0]
  fm.srcdoc= areanode.value;
  fm.addEventListener("load", (evt) => {
    if (basedir!=""){
      var bt = fm.contentDocument.createElement("base");
      bt.setAttribute("href",basedir);
      bt.setAttribute("href",basedir);
      fm.contentDocument.getElementsByTagName("head")[0].appendChild(bt);  
     }
  }, { once: true })
   

}
*/
function switchEdit(e){
  //var p = e.parentNode;
  var p=findAncestor(e,"demoContainer");
  if (!p) return;
  nodes=p.getElementsByClassName("switchable");
	[].forEach.call(nodes,
	  function(item){
        item.classList.toggle("switchEdit");
	  }
	)  

}

function expandsw(e,aid){
	//var p=findAncestor(e,"demoContainer");
	target  = document.getElementById(aid);
	
	status= e.innerText;
	if (status=="展開"){
		e.innerText="收縮"
		target.classList.toggle("full")
	}else{
		e.innerText="展開"
		target.classList.toggle("full")
		
	}		
}

  
function openTab(evt, tabName) {
  // Declare all variables
  var i, tabcontent, tablinks;

  // Get all elements with class="tabcontent" and hide them
  tabcontent = document.getElementsByClassName("tab3content");
  for (i = 0; i < tabcontent.length; i++) {
    //tabcontent[i].style.display = "none";
	tabcontent[i].classList.remove("active");
  }
/*
  // Get all elements with class="tablinks" and remove the class "active"
  tablinks = document.getElementsByClassName("tablinks");
  for (i = 0; i < tablinks.length; i++) {
    tablinks[i].className = tablinks[i].className.replace(" active", "");
  }
*/
  // Show the current tab, and add an "active" class to the button that opened the tab
  targettab = document.getElementById(tabName);
  //targettab.style.display = "block";
  targettab.classList.toggle("active");
  //evt.currentTarget.className += " active";
}  