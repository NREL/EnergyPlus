var cmn = document.getElementById("myTopnav");
if(cmn)	cmn.innerHTML = '<a href="../index.html">Главная</a>'+
	'<div class="dropdown"><button class="dropbtn">MathGL[RU]</button>'+
	'<div class="dropdown-content">'+
		'<a href="../doc_ru/Main.html">сайт</a>'+
		'<a href="../doc_ru/Overview.html">обзор</a>'+
		'<a href="../doc_ru/Examples.html">обучение</a>'+
		'<a href="../doc_ru/General-concepts.html">концепция</a>'+
		'<a href="../doc_ru/MathGL-core.html">ядро</a>'+
		'<a href="../doc_ru/Widget-classes.html">виджеты</a>'+
		'<a href="../doc_ru/Data-processing.html">массивы данных</a>'+
		'<a href="../doc_ru/MGL-scripts.html">скрипты</a>'+
		'<a href="../doc_ru/All-samples.html">примеры</a>'+
		'<a href="../doc_ru/Symbols-and-hot_002dkeys.html">приложения</a>'+
	'</div></div>'+
	'<div class="dropdown"><button class="dropbtn">MathGL[EN]</button>'+
	'<div class="dropdown-content">'+
		'<a href="../doc_en/Main.html">website</a>'+
		'<a href="../doc_en/Overview.html">overview</a>'+
		'<a href="../doc_en/Examples.html">examples</a>'+
		'<a href="../doc_en/General-concepts.html">concepts</a>'+
		'<a href="../doc_en/MathGL-core.html">core</a>'+
		'<a href="../doc_en/Widget-classes.html">widgets</a>'+
		'<a href="../doc_en/Data-processing.html">data arrays</a>'+
		'<a href="../doc_en/MGL-scripts.html">scripts</a>'+
		'<a href="../doc_en/All-samples.html">all samples</a>'+
		'<a href="../doc_en/Symbols-and-hot_002dkeys.html">appendix</a>'+
	'</div></div>'+
	'<div class="dropdown"><button class="dropbtn">Games</button>'+
	'<div class="dropdown-content">'+
		'<a href="../games/tetris.html">Tetris</a>'+
		'<a href="../games/pentix.html">Pentix</a>'+
		'<a href="../games/columns.html">Columns</a>'+
		'<a href="../games/colorex.html">Colorex</a>'+
		'<a href="../games/shiftix.html">Shiftix</a>'+
		'<a href="../games/hextris.html">Hextris</a>'+
		'<a href="../games/jumps.html">Jumps</a>'+
	'</div></div>'+
	'<a href="../games/mk61.html">PocketMK</a>'+
	'<a href="../json/json.html">MathGL+JS</a>'+
	'<a href="../json/mgl_cgi.html">MGL.CGI</a>'+
	'<a rel="license" href="http://creativecommons.org/licenses/by-nc/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nc/4.0/88x31.png" /></a>'+
	'<a class="icon" onclick="showExtMenu()">&#9776;</a>';

		
function showExtMenu()
{
	var x = document.getElementById("myTopnav");
	if (x.className === "topnav")	x.className += " responsive";
	else	x.className = "topnav";
	var y = document.getElementById("mainTopnav");
	if (y.className === "topnav")	y.className += " responsive";
	else	y.className = "topnav";
}

var acc = document.getElementsByClassName("accordion");
for (var i = 0; i < acc.length; i++) {
    acc[i].addEventListener("click", function() {
        /* Toggle between adding and removing the "active" class,
        to highlight the button that controls the panel */
        this.classList.toggle("active");

        /* Toggle between hiding and showing the active panel */
        var panel = this.nextElementSibling;
        if (panel.style.display === "block") {
            panel.style.display = "none";
        } else {
            panel.style.display = "block";
        }
    });
} 

/* Loop through all dropdown buttons to toggle between hiding and showing its dropdown content - This allows the user to have multiple dropdowns without any conflict */
var dropdown = document.getElementsByClassName("dropdown-btn");
for (var i = 0; i < dropdown.length; i++)
{
	dropdown[i].addEventListener("click", function() {
	this.classList.toggle("active");
	var dropdownContent = this.nextElementSibling;
	if (dropdownContent.style.display === "block")
		dropdownContent.style.display = "none";
	else	dropdownContent.style.display = "block";
	});
}
