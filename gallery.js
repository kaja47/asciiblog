let box = document.createElement('div');
box.style = 'display:none;position:fixed;top:0px;left:0px;background-color:rgba(0,0,0,0.7);width:100%;height:100%';
let img = document.createElement('img');
img.style = 'display:block;margin:auto;max-width:100%;max-height:100%';
let x = document.createElement('div');
x.style = 'position:fixed;top:0px;right:0px;font-size:1.5em;padding:0.5em;background:rgba(255,255,255,0.7);cursor:pointer';
x.textContent = "❌";
let label = document.createElement('div');
label.style='position:fixed;bottom:0px;left:0px;font-size:1em;padding:0.5em;background:rgba(255,255,255,0.7)';
box.appendChild(img);
box.appendChild(x);
box.appendChild(label);
box.addEventListener('click', function(e) { hide(); });
img.addEventListener('click', function(e) { move(1); e.stopPropagation(); });

let imgs = [], active = null;

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
		let t = imgs[i].parentElement.parentElement;
		label.style.display = (t.textContent ? 'block' : 'none');
		t = t.cloneNode(true);
		t.removeChild(t.firstChild); // remove img tag itself
		t.setAttribute('class', '');
		label.textContent = '';
		label.appendChild(t);
	}
}

window.addEventListener('load', function (e) {
	let is = document.querySelectorAll('img.thz');
	for (let i = 0; i < is.length; i++) {
		if (is[i].parentNode.href.match(/.*(jpg|png|gif)/i)) {
			imgs.push(is[i]);
		}
	}
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
		let k = {/* <- -> */ 37:-1, 39:1, /* PgUp, PgDn */ 33:-1, 34:1};
		if (e.keyCode in k) { move(k[e.keyCode]); e.preventDefault(); }
		else { hide(); }
	}
});
