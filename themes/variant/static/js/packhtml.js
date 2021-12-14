

const getBlobFromUrl = (myImageUrl) => {
  return new Promise((resolve, reject) => {
      let request = new XMLHttpRequest();
      request.open('GET', myImageUrl, true);
      request.responseType = 'blob';
      request.onload = () => {
          resolve(request.response);
      };
      request.onerror = reject;
      request.send();
  })
}

const getDataFromBlob = (myBlob) => {
  return new Promise((resolve, reject) => {
      let reader = new FileReader();
      reader.onload = () => {
          resolve(reader.result);
      };
      reader.onerror = reject;
      reader.readAsDataURL(myBlob);
  })
}

const convertUrlToImageData = async (myImageUrl) => {
  try {
      let myBlob = await getBlobFromUrl(myImageUrl);
      let myImageData = await getDataFromBlob(myBlob);
      return myImageData;
  } catch (err) {
      console.log(err);
      return null;
  }
}



function ff(e){
  return(e.href.match(/\.doc$|\.docx$|\.xlsx$|\.xls$|\.png$|\.jpg$|\.html$/i))  
}

function basename (path) {
  return path.substring(path.lastIndexOf('/') + 1)
}

async function packhref(ele) 
{
  let fname = basename(ele.href);
  txt= await convertUrlToImageData(ele.href);
  ele.download = fname;
  ele.href = txt;;
  

}
async function packthis(e) 
{
  let tmp = document.querySelector("#body-inner");
  let links = tmp.querySelectorAll("a");
  links= Array.apply(null, links);
  let these=links.filter(ff);
  these.forEach(e=> packhref(e))

  let imgsrc = tmp.querySelectorAll("img");
  imgsrc = Array.apply(null, imgsrc);
  imgsrc.forEach(async ele=>  {
    let fname = basename(ele.src);
    txt= await convertUrlToImageData(ele.src);
    ele.download = fname;
    ele.src = txt;;
  
  })

}
