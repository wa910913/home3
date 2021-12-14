document.addEventListener('DOMContentLoaded', qnoteInit);
var qnoteEdit=undefined;
function qnoteInit(event){
	loadStoredNotes();	
	//document.querySelector("#qnote-clear").addEventListener("click", cleanNotes)
	//document.querySelector("#qnote-add-item").addEventListener("click", addNote)
	qnoteEdit = document.querySelector("#qnote-edit");
}

function qnSaveGoogle(){
  qnSaveLocal();
}
function qnSaveLocal(){
	let txt= localStorage.getItem(webkey());
	var textToSaveAsBlob = new Blob([txt], {
	  type: "text/plain"
	}); 
	var url = URL.createObjectURL(textToSaveAsBlob);
	download(url,"全部速記.txt")	 
	URL.revokeObjectURL(url);
}
 
function qnLoadGoogle(e){
	
	
	if (e.files.length==0) return;
    
	
	let reader = new FileReader();
	reader.onload = function(){
	  let text = reader.result;
      localStorage.setItem(webkey(), text);
	  Array.from( document.querySelectorAll(".qnote-item")).forEach((ele)=>ele.remove());
	  loadStoredNotes();
	  e.value=null;
	};
	reader.readAsText(e.files[0]);	
}
const download = (path, filename) => {
    const anchor = document.createElement('a');
    anchor.href = path;
    anchor.download = filename;
    document.body.appendChild(anchor);
    anchor.click();
    document.body.removeChild(anchor);
}; 
/*
使用者選擇的NODE
*/
function getSelectedNode()
{

    if (document.selection)
        return document.selection.createRange().parentElement();
    else
    {
        var selection = window.getSelection();
        if (selection.rangeCount > 0)
            return selection.getRangeAt(0).startContainer.parentNode;
    }
 
}
/*
 3個一起
 */
function isElementInViewport (el) {

    // Special bonus for those using jQuery
    if (typeof jQuery === "function" && el instanceof jQuery) {
        el = el[0];
    }

    var rect = el.getBoundingClientRect();

    return (
        rect.top >= 0 &&
        rect.left >= 0 &&
        rect.bottom <= (window.innerHeight || document.documentElement.clientHeight) && /* or $(window).height() */
        rect.right <= (window.innerWidth || document.documentElement.clientWidth) /* or $(window).width() */
    );
}
function getTopEle(){
	let body = document.querySelector("#body-inner");
	let list = body.querySelectorAll("[id]");
	let rst=null;
	for(let i=0;i<list.length;i++){
		if (list[i].id!="body-inner" && isElementInViewport(list[i])){
             rst=list[i];
			 break;
		}
	}
	return rst;
}
function tryGetId(){
	let rst=getSelectedNode();
	if ((rst && ["","body-inner"].includes(rst.id) )|| !rst) {
		rst=getTopEle();
	}
	  
	while (rst && (rst.id==null || rst.id=="") ){
	  rst= rst.previousSibling==null?rst.parentElement:rst.previousSibling;
		  
	}
	if(rst && rst.id!="")
	  return rst.id;
	else
	  return "";	
}
/*
刪除一個
 */
function deleteQnoteItem(ele){
	let p=ele;
	while(p && !p.classList.contains("qnote-item") ) 
	  p=p.parentElement;
	let idx= parseInt(p.getAttribute("data-qnote-item-no"));  
	var storedNotes= new Array();
	storedNotes = JSON.parse(localStorage.getItem(webkey()));
	storedNotes.splice(idx,1);
	localStorage.setItem(webkey(), JSON.stringify(storedNotes));  
	Array.from( document.querySelectorAll(".qnote-item")).forEach((ele)=>ele.remove());  
    loadStoredNotes();
}

/*
// $("textarea").on("change keyup input",function(){
// 	if($(this).val().length>0)$("#crteStkyBtn").removeClass("ntActv");
// 	else if($(this).val().length>100)
// 	this.value=this.value.substring(0,max);
// 	else $("#crteStkyBtn").addClass("ntActv")});
}
*/
function cleanNotes(){
	if (window.confirm("刪除全部速記?")) {
		//localStorage.clear();
		localStorage.removeItem(webkey())
		Array.from( document.querySelectorAll(".qnote-item")).forEach((ele)=>ele.remove());
		loadStoredNotes();
			
	  }
}
function webkey(){
	return escape(location.host);
}
function getStoredNotes(){
	var storedNotes= new Array();
	let txt= localStorage.getItem(webkey());
	if (txt && txt!="")
	  storedNotes = JSON.parse(txt);
	return storedNotes;
}

function loadStoredNotes(){
	var pastNotes= getStoredNotes()
	pastNotes.forEach((value,idx) => {
		if(value!="") 
		displayQnoteItem(JSON.parse(value),idx)
	});

}


 
function addNote(){
	let eTxt = document.querySelector(".txtBox");
	let eTitle  = document.querySelector(".txtitle");
     var usrInput =eTxt.value;// $('.txtBox').val();
	  var title = eTitle.value;//$('.txtitle').val();
	  var href = new URL(window.location.href).pathname+"#"+tryGetId();
	if(usrInput.length > 0){
		addtoSticky({title: title, text:usrInput,href:href});
	}
	eTxt.value="";
	eTitle.value="";
	qnoteEdit.classList.remove("show");
}


function addtoSticky(note){
	if(note != null){
  	    var current = getStoredNotes();//JSON.parse(localStorage.getItem("notes"));
		let len = current.push(JSON.stringify(note));
		localStorage.setItem(webkey(), JSON.stringify(current));
	    displayQnoteItem(note,len-1);
	}	
}
/*
function displayQnoteItem(note){
  if(note == null) return;
  $('ul.qnote-list').append('<li class="qnote-box"><h3 class="m-0 p-0">' + note.title + '</h3>'+note.text+'</li>');
} 
*/

function displayQnoteItem(note, idx){
	var tmp1= `
    <div  data-qnote-item-no=${idx} class="qnote-item list-group-item list-group-item-action py-3 lh-tight" >
      <div class="d-flex flex-row w-100 align-items-end justify-content-between">
        <strong class="mb-1 note_title">${note.title}</strong>
        <small>
		<button onclick="deleteQnoteItem(this)" type="button" class="btn btn-primary btn-inline">D</button>
		<button  type="button" class="btn btn-primary btn-inline">E</button>	
		<a href="${note.href}" class="btn btn-primary btn-inline">G</a>	
		</small>
      </div>
      <div class="col-10 mb-1 small note_text">${note.text}</div>
    </div>	
	`;
	if(note == null) return;
	
	$('div#qnote-list').append(tmp1);
  } 
	