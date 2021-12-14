

var searchResults = document.getElementById("search-results");
var searchPane = document.getElementById("search-pane");
var searchInput = document.getElementById("search-query");
var searchbutton = document.getElementById("search-button");
// the length of the excerpts
var contextDive = 40;

var timerUserInput = false;

function getIdxbase(){
    var getUrl = window.location;
    var baseUrl = getUrl .protocol + "//" + getUrl.host + "/" + getUrl.pathname.split('/')[1];
    return(baseUrl)    
}
//let idxfile = getIdxbase()+"/index.json"
if (searchInput || searchbutton){
searchInput.addEventListener("input", function()
{
    // 使用者離開鍵盤的時候,仍然綁定timerUserInput表示仍然在時間內,因此清掉重設.
    if (timerUserInput) { clearTimeout(timerUserInput); }
    timerUserInput = setTimeout(
        function()
        {
            search(searchInput.value.trim());
        },
        500
    );
});
searchbutton.addEventListener("click",function()
{
    search(searchInput.value.trim());
});
}
const _hanmatch = new RegExp("\\p{Script=Han}","u")
function ishan(something){
 return _hanmatch.test(something);
}
function search(searchQuery)
{
    // clear previous search results
    while (searchResults.firstChild)
    {
        searchResults.removeChild(searchResults.firstChild);
    }

    // ignore empty and short search queries
    if (searchQuery.length === 0 || (searchQuery.length < 3 && !ishan(searchQuery)))
    {
        searchPane.style.display = "none";
        
        return;
    }

    
    searchPane.style.display = "block";

    // load your index file
    getJSON(idxfile, function (contents)
    {
        var results = [];
        let regex = new RegExp(searchQuery, "i");
        // iterate through posts and collect the ones with matches
        contents.forEach(function(post)
        {
            // here you can also search in tags, categories
            // or whatever you put into the index.json layout
            if (post.title.match(regex) || post.content.match(regex))
            {
                results.push(post);
            }
        });

        if (results.length > 0)
        {
            searchResults.appendChild(
                htmlToElement("<div><b>Found: ".concat(results.length, "</b></div>"))
            );

            // populate search results block with excerpts around the matched search query
            results.forEach(function (value, key)
            {
 

                let div = "".concat("<li><a href='", value.uri, "'>")
                    .concat(value.title," (",value.description," )")
                    .concat("</li>");
                searchResults.appendChild(htmlToElement(div));

 
            });
        }
        else
        {
            searchResults.appendChild(
                htmlToElement("<div><b>Nothing found</b></div>")
            );
        }
    });
}

function getJSON(url, fn)
{
    let xhr = new XMLHttpRequest();
    xhr.open("GET", url);
    xhr.onload = function ()
    {
        if (xhr.status === 200)
        {
            fn(JSON.parse(xhr.responseText));
        }
        else
        {
            console.error(
                "Some error processing ".concat(url, ": ", xhr.status)
                );
        }
    };
    xhr.onerror = function ()
    {
        console.error("Connection error: ".concat(xhr.status));
    };
    xhr.send();
}

// it is faster (more convenient)
// to generate an element from the raw HTML code
function htmlToElement(html)
{
    let template = document.createElement("template");
    html = html.trim();
    template.innerHTML = html;
    return template.content.firstChild;
}

//});