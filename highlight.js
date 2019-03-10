document.querySelectorAll("pre[data-hl]").forEach(p => {
  if (!p.append) return;
  let t = p.innerText,  // source text
      h = p.dataset.hl, // hightlight description
      j = 0;            // position in source (unhighlighted) text
  p.innerHTML = "";
  for (i = 0; i < h.length; i+=3) {
    let f = x => h.charCodeAt(i+x)-40,
        d = f(0), // delta
        l = f(1), // length
        c = f(2), // color
        el = document.createElement('span');

    el.style.color = ['#F92672', '#63f800', '#66D9EF', '#AE81FF', '#E6DB74', '#7E8E91'][c];
    el.textContent = t.substr(j+d, l);
    p.append(t.substr(j, d), el);
    j += d + l;
  }
  p.append(t.substr(j));
})
