// s/zoom/Z/
// s/box/B/
// s/corner/C/
// s/image/I/
// s/links/K/
// s/show/S/
// s/active/A/
// s/label/L/
// s/document.createElement/E/
// s/document/D/
// s/.appendChild/[P]/
// s/.style/[T]/
// s/width:100%;/'+W+'/
// s/height:100%;/'+H+'/
// s/position:fixed;top:0;/'+F+'/
// s/+''//
// s/''+//
// prep let D=document,P="appendChild",E=t=>D.createElement(t),W='width:100%;',H='height:100%;',F='position:fixed;top:0;',T='style';

let box    = document.createElement('div'),
		image  = document.createElement('img'),
		corner = document.createElement('div'),
		label  = document.createElement('span');
		zoom   = document.createElement('a'),
		links  = [], // a tags with images inside
		active = 0;

box.style    = 'display:none;position:fixed;top:0;left:0;background-color:rgba(0,0,0,0.7);width:100%;height:100%;';
image.style  = 'display:block;margin:auto;max-width:100%;max-height:100%;';
corner.style = 'position:fixed;top:0;right:0;font-size:1.2em;padding:.5em;background:#aaa';

corner.appendChild(label);

zoom.textContent = "[+]";
zoom.onclick = e => e.stopPropagation();
corner.appendChild(zoom);

corner.appendChild(document.createTextNode(" ❌"));

box.appendChild(image);
box.appendChild(corner);
box.onclick = e => show(-1, e);
image.onclick = e => show(active+1, e);


function show(i, e) {
	if (links[i]) {
		image.src = zoom.href = links[i].href;
		active = i;
		let t = links[i].parentNode.cloneNode(true);
		t.removeChild(t.firstChild); // remove img tag itself
		label.innerHTML = t.innerHTML;
	}
	box.style.display = (links[i] ? "block" : "none");
	e.preventDefault();
	e.stopPropagation();
}

window.onload = x => {
	links = Array.from(document.querySelectorAll('img.thz'), n => n.parentNode);
	links.forEach((n, i) => { n.onclick = e => show(i, e); });

//	for (let n of document.querySelectorAll('img.thz')) {
//		let a = n.parentNode;
//		//if (a.href.match(/.*(jpg|png|gif)/i)) {
//			links.push(a);
//		//}
//	}
//	links.forEach((n, i) => {
//		n.onclick = e => show(i, e);
//	});
	document.body.appendChild(box);
};

document.onkeydown = e => {
	if (box.style.display === 'block') {
		let k = {/* <- -> */ 37:-1, 39:1, /* PgUp, PgDn */ 33:-1, 34:1}[e.keyCode];
		show(k ? (active+k) : -1, e);
	}
};
