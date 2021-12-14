# variant-learn Theme

Based on `Hugo Learn Theme`, i customized my own theme.

目前是這樣用
```
git submodule add http://u18docker:3000/linchao/myhugotheme.git themes 
```

## 紀錄

- add safeURL to editurl, in layout/partial/head.html


### 刪除hug-learn.js
 
hugo-learn.js裡面主要處理feather-light,這樣做的話整個site的網頁都要處理,但是有些網頁,我不要他處理。
- 所以,我把它去掉,但是去掉的同時,裡面有個ready函數,調用了jquery.sticky()作用是保持頂板不動
- 為了解決上面的問題,我參考[捲動](https://stackoverflow.com/questions/2731496/css-100-height-and-then-scroll-div-not-page)
  - 頂板保持不動,而其他div 可以捲動。
- 放棄上面的作法,直接搬到`variant.js`,同時在themes的子頁面`footer.html`更改對應的hugo-learn.js為variant.js。

- 在footer.html中註解掉modernizr.js,這個用到htmo5shiv.js 我覺得根本不用了。但是檔案都留著,
