cd doc_en/
sed -i -- 's/<head>/<head><link rel="stylesheet" href="..\/styles.css">/g' *.html
sed -i -- 's/<body lang="en">/<body lang="en"><div class="topnav" id="myTopnav"><\/div><div class="main">/g' *.html
sed -i -- 's/<body lang="ru">/<body lang="ru"><div class="topnav" id="myTopnav"><\/div><div class="main">/g' *.html
sed -i -- 's/<\/body>/<\/div><script type="text\/javascript" src="..\/accordion.js"><\/script><\/body>/g' *.html
sed -i -- 's/.png.png/.png/g' *.html
cd ../doc_ru/
sed -i -- 's/<head>/<head><link rel="stylesheet" href="..\/styles.css">/g' *.html
sed -i -- 's/<body lang="en">/<body lang="en"><div class="topnav" id="myTopnav"><\/div><div class="main">/g' *.html
sed -i -- 's/<body lang="ru">/<body lang="ru"><div class="topnav" id="myTopnav"><\/div><div class="main">/g' *.html
sed -i -- 's/<\/body>/<\/div><script type="text\/javascript" src="..\/accordion.js"><\/script><\/body>/g' *.html
sed -i -- 's/.png.png/.png/g' *.html
cd ..
