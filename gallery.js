let box = document.createElement('div');
box.style = 'display:none; position:fixed; top:0px; left:0px; background-color:rgba(0,0,0,0.7); width:100%; height:100%';
let img = document.createElement('img');
img.style = 'display:block; margin:auto; max-width:100%; max-height:100%';
box.appendChild(img);
box.addEventListener('click', function(e) { hide(); });
img.addEventListener('click', function(e) { move(+1); e.stopPropagation(); });

let imgs = null;
let active = null;

function move(n) { show(active + n); }
function hide() { box.style.display = "none"; }

function show(i) {
	if (i >= imgs.length || i < 0) {
		hide();
	} else {
		box.style.display = "block";
		img.src = "";
		img.src = imgs[i].parentNode.href;
		active = i;
	}
}

window.addEventListener('load', function (e) {
	imgs = document.querySelectorAll('img.th');
	for (let i = 0; i < imgs.length; i++) {
		imgs[i].addEventListener('click', function (e) {
			show(i);
			e.preventDefault();
		});
	}
	document.body.appendChild(box);
});

document.addEventListener('keydown', function (e) {
	if (box.style.display === 'block') {
		let k = {/* <- -> */ 37:-1, 39:+1, /* PgUp, PgDn */ 33:-1, 34:+1};
		if (e.keyCode in k) { move(k[e.keyCode]); e.preventDefault(); }
		else { hide(); }
	}
});
