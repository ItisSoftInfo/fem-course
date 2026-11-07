// "Your own matrices" on the matrix pages: the viewer chooses the rows and columns of each matrix, types the
// numbers and sees the result, or why the operation is not defined (user request, 2026-11-07).
// The page holds <section class="calc" data-ops="mul" data-names="A,B">; fem_site.py writes it.
(function (root) {
  "use strict";
  const MAX = 24; // the largest matrix in the course: the 24 by 24 stiffness matrix of video 217
  const TINY = 1e-12;

  const OPS = {
    add: { label: "A + B", two: true },
    sub: { label: "A − B", two: true },
    mul: { label: "A × B", two: true },
    transpose: { label: "transpose of A", two: false },
    det: { label: "determinant of A", two: false },
    inv: { label: "inverse of A", two: false },
    props: { label: "properties of A", two: false },
    eig: { label: "eigenvalues of A", two: false },
    solve: { label: "solve A x = B", two: true },
    gs: { label: "Gauss-Seidel for A x = B", two: true },
    chol: { label: "Cholesky factor of A", two: false },
  };

  const size = (m) => `${m.length} by ${m[0].length}`;
  const square = (m) => m.length === m[0].length;
  const zeros = (r, c) => Array.from({ length: r }, () => new Array(c).fill(0));
  const copy = (m) => m.map((row) => row.slice());
  const scale = (m) => Math.max(1, ...m.flat().map(Math.abs));
  const symmetric = (m) => square(m) && m.every((row, i) => row.every((v, j) => Math.abs(v - m[j][i]) <= 1e-9 * scale(m)));

  function transpose(a) {
    return a[0].map((_, j) => a.map((row) => row[j]));
  }
  function mul(a, b) {
    const c = zeros(a.length, b[0].length);
    for (let i = 0; i < a.length; i++)
      for (let j = 0; j < b[0].length; j++)
        for (let k = 0; k < b.length; k++) c[i][j] += a[i][k] * b[k][j];
    return c;
  }
  // Gaussian elimination with row swaps: the determinant, or the solution of a x = b for every column of b
  function eliminate(a, b) {
    const n = a.length, m = copy(a), r = b ? copy(b) : null;
    let det = 1;
    const tol = TINY * scale(a) * n;
    for (let k = 0; k < n; k++) {
      let p = k;
      for (let i = k + 1; i < n; i++) if (Math.abs(m[i][k]) > Math.abs(m[p][k])) p = i;
      if (Math.abs(m[p][k]) <= tol) return { det: 0, singular: true };
      if (p !== k) { [m[p], m[k]] = [m[k], m[p]]; if (r) [r[p], r[k]] = [r[k], r[p]]; det = -det; }
      det *= m[k][k];
      for (let i = k + 1; i < n; i++) {
        const f = m[i][k] / m[k][k];
        if (f === 0) continue;
        for (let j = k; j < n; j++) m[i][j] -= f * m[k][j];
        if (r) for (let j = 0; j < r[0].length; j++) r[i][j] -= f * r[k][j];
      }
    }
    if (!r) return { det };
    const x = zeros(n, r[0].length);
    for (let j = 0; j < r[0].length; j++)
      for (let i = n - 1; i >= 0; i--) {
        let s = r[i][j];
        for (let k = i + 1; k < n; k++) s -= m[i][k] * x[k][j];
        x[i][j] = s / m[i][i];
      }
    return { det, x };
  }
  const identity = (n) => Array.from({ length: n }, (_, i) => Array.from({ length: n }, (_, j) => (i === j ? 1 : 0)));
  function cholesky(a) {
    const n = a.length, l = zeros(n, n);
    for (let i = 0; i < n; i++)
      for (let j = 0; j <= i; j++) {
        let s = a[i][j];
        for (let k = 0; k < j; k++) s -= l[i][k] * l[j][k];
        if (i === j) {
          if (s <= TINY * scale(a)) return null;
          l[i][i] = Math.sqrt(s);
        } else l[i][j] = s / l[j][j];
      }
    return l;
  }
  // Jacobi rotations: the eigenvalues of a symmetric matrix, smallest first
  function eigen(a) {
    const n = a.length, m = copy(a);
    for (let sweep = 0; sweep < 100; sweep++) {
      let off = 0;
      for (let i = 0; i < n; i++) for (let j = i + 1; j < n; j++) off += m[i][j] * m[i][j];
      if (off <= 1e-24 * scale(a) * scale(a)) break;
      for (let p = 0; p < n; p++)
        for (let q = p + 1; q < n; q++) {
          if (Math.abs(m[p][q]) < 1e-300) continue;
          const th = (m[q][q] - m[p][p]) / (2 * m[p][q]);
          const t = Math.sign(th || 1) / (Math.abs(th) + Math.sqrt(th * th + 1));
          const c = 1 / Math.sqrt(t * t + 1), s = t * c;
          for (let k = 0; k < n; k++) {
            const kp = m[k][p], kq = m[k][q];
            m[k][p] = c * kp - s * kq; m[k][q] = s * kp + c * kq;
          }
          for (let k = 0; k < n; k++) {
            const pk = m[p][k], qk = m[q][k];
            m[p][k] = c * pk - s * qk; m[q][k] = s * pk + c * qk;
          }
        }
    }
    return m.map((row, i) => row[i]).sort((x, y) => x - y);
  }
  function gaussSeidel(a, b) {
    const n = a.length, x = zeros(n, 1);
    for (let it = 1; it <= 200; it++) {
      let change = 0;
      for (let i = 0; i < n; i++) {
        let s = b[i][0];
        for (let j = 0; j < n; j++) if (j !== i) s -= a[i][j] * x[j][0];
        const v = s / a[i][i];
        change = Math.max(change, Math.abs(v - x[i][0]));
        x[i][0] = v;
      }
      if (!Number.isFinite(change)) return { sweeps: it, x: null };
      if (change <= 1e-10 * Math.max(1, ...x.map((r) => Math.abs(r[0])))) return { sweeps: it, x };
    }
    return { sweeps: 200, x: null };
  }

  // One operation: a matrix, a number, a list of lines, or the reason it is not defined
  function run(op, a, b, names) {
    const [A, B] = names;
    const need = (cond, why) => { if (!cond) throw new Error(why); };
    try {
      switch (op) {
        case "add":
        case "sub":
          need(a.length === b.length && a[0].length === b[0].length,
            `Not defined: ${A} is ${size(a)} and ${B} is ${size(b)}. ${op === "add" ? "Adding" : "Subtracting"} needs two matrices of the same size.`);
          return { matrix: a.map((row, i) => row.map((v, j) => (op === "add" ? v + b[i][j] : v - b[i][j]))) };
        case "mul":
          need(a[0].length === b.length,
            `Not defined: ${A} has ${a[0].length} column${a[0].length > 1 ? "s" : ""} but ${B} has ${b.length} row${b.length > 1 ? "s" : ""}. ${A} × ${B} needs as many columns in ${A} as rows in ${B}.`);
          return { matrix: mul(a, b), note: `${A} × ${B} is ${a.length} by ${b[0].length}.` };
        case "transpose":
          return { matrix: transpose(a), note: `The transpose is ${a[0].length} by ${a.length}${symmetric(a) ? ", the same as " + A + ": " + A + " is symmetric" : ""}.` };
        case "det":
          need(square(a), `Not defined: ${A} is ${size(a)}. Only a square matrix has a determinant.`);
          return { number: eliminate(a).det, label: `determinant of ${A}` };
        case "inv": {
          need(square(a), `Not defined: ${A} is ${size(a)}. Only a square matrix can have an inverse.`);
          const e = eliminate(a, identity(a.length));
          need(!e.singular, `No inverse: the determinant of ${A} is zero, so ${A} is singular.`);
          return { matrix: e.x };
        }
        case "props": {
          need(square(a), `Not defined: ${A} is ${size(a)}. These properties are for square matrices.`);
          const e = eliminate(a), sym = symmetric(a);
          const pd = sym && !e.singular && cholesky(a) !== null;
          return { lines: [
            `symmetric: ${sym ? "yes" : "no"}`,
            `determinant: ${fmt(e.det)}`,
            `singular: ${e.singular ? "yes (no inverse)" : "no"}`,
            `positive definite: ${sym ? (pd ? "yes" : "no") : "not checked (only for symmetric matrices)"}`,
          ] };
        }
        case "eig":
          need(square(a), `Not defined: ${A} is ${size(a)}. Only a square matrix has eigenvalues.`);
          need(symmetric(a), `${A} is not symmetric. This page finds the eigenvalues of symmetric matrices, like stiffness and mass matrices; make ${A} symmetric first.`);
          return { matrix: eigen(a).map((v) => [v]), note: "The eigenvalues, smallest first." };
        case "solve": {
          need(square(a), `Not defined: ${A} is ${size(a)}. To solve ${A} x = ${B}, ${A} must be square.`);
          need(b.length === a.length, `Not defined: ${A} has ${a.length} rows but ${B} has ${b.length}. ${B} needs one row for each row of ${A}.`);
          const e = eliminate(a, b);
          need(!e.singular, `No unique solution: ${A} is singular (its determinant is zero), as for a model without enough supports.`);
          return { matrix: e.x, note: "x, one column for each column of " + B + "." };
        }
        case "gs": {
          need(square(a), `Not defined: ${A} is ${size(a)}. To solve ${A} x = ${B}, ${A} must be square.`);
          need(b.length === a.length && b[0].length === 1, `Not defined: ${B} must be one column with ${a.length} rows, one for each row of ${A}.`);
          need(a.every((row, i) => row[i] !== 0), `Gauss-Seidel cannot start: a number on the diagonal of ${A} is zero.`);
          const g = gaussSeidel(a, b);
          need(g.x !== null, `Gauss-Seidel did not settle in ${g.sweeps} sweeps. It is safe when ${A} is symmetric and positive definite, or when each diagonal number is larger than the rest of its row.`);
          return { matrix: g.x, note: `x after ${g.sweeps} sweep${g.sweeps > 1 ? "s" : ""}.` };
        }
        case "chol": {
          need(square(a), `Not defined: ${A} is ${size(a)}. Cholesky needs a square matrix.`);
          need(symmetric(a), `Not possible: ${A} is not symmetric. Cholesky needs a symmetric matrix.`);
          const l = cholesky(a);
          need(l !== null, `Not possible: ${A} is not positive definite (a pivot is zero or negative), as for a model without enough supports.`);
          return { matrix: l, note: `L, with L × L transposed = ${A}.` };
        }
      }
    } catch (err) {
      return { error: err.message };
    }
    return { error: "Unknown operation." };
  }

  function fmt(v) {
    if (Math.abs(v) < 1e-10 * Math.max(1, Math.abs(v))) v = 0;
    if (Math.abs(v) < 5e-13) return "0";
    const t = Math.abs(v) >= 1e7 || Math.abs(v) < 1e-4 ? v.toExponential(4).replace(/\.?0+e/, "e") : String(Number(v.toPrecision(6)));
    return t.replace("-", "−");
  }

  // a starting matrix that is our own, square ones well conditioned and symmetric
  function start(r, c, seed) {
    return Array.from({ length: r }, (_, i) => Array.from({ length: c }, (_, j) => {
      if (r === c) return i === j ? 4 + ((i + seed) % 3) : (Math.abs(i - j) === 1 ? -1 : 0);
      return ((2 * i + 3 * j + seed) % 7) - 2;
    }));
  }

  function build(sec) {
    const doc = sec.ownerDocument;
    const ops = sec.dataset.ops.split(",");
    const names = (sec.dataset.names || "A,B").split(",");
    const sizes = (sec.dataset.sizes || "3,3,3,3").split(",").map(Number);
    const el = (tag, cls, text) => { const e = doc.createElement(tag); if (cls) e.className = cls; if (text !== undefined) e.textContent = text; return e; };
    const label = (op) => OPS[op].label.replace(/\bA\b/g, names[0]).replace(/\bB\b/g, names[1]);
    let op = ops[0];
    const mats = [start(sizes[0], sizes[1], 0), start(sizes[2], sizes[3], 1)];

    sec.append(el("h2", "", "Your own matrices"));
    sec.append(el("p", "lead", `Choose the rows and columns of each matrix, type the numbers, and the result follows. Any size from 1 to ${MAX}.`));
    const bar = el("div", "calc-bar");
    sec.append(bar);
    if (ops.length > 1) {
      const lab = el("label", "", "Operation ");
      const sel = el("select");
      for (const o of ops) { const opt = el("option", "", label(o)); opt.value = o; sel.append(opt); }
      sel.addEventListener("change", () => { op = sel.value; draw(); });
      lab.append(sel);
      bar.append(lab);
    }
    const grids = el("div", "calc-grids");
    const out = el("div", "calc-out");
    sec.append(grids, out);

    function sizeBox(k, dim) {
      const inp = el("input");
      inp.type = "number"; inp.min = 1; inp.max = MAX; inp.step = 1; inp.inputMode = "numeric";
      inp.value = dim === 0 ? mats[k].length : mats[k][0].length;
      inp.setAttribute("aria-label", `${names[k]} ${dim === 0 ? "rows" : "columns"}`);
      inp.addEventListener("change", () => {
        const n = Math.min(MAX, Math.max(1, Math.round(Number(inp.value)) || 1));
        inp.value = n;
        const m = mats[k], r = dim === 0 ? n : m.length, c = dim === 1 ? n : m[0].length;
        mats[k] = Array.from({ length: r }, (_, i) => Array.from({ length: c }, (_, j) => (i < m.length && j < m[0].length ? m[i][j] : 0)));
        draw();
      });
      return inp;
    }
    function grid(k) {
      const box = el("div", "calc-mat");
      const head = el("div", "calc-size");
      head.append(el("b", "", names[k] + " "), sizeBox(k, 0), el("span", "", " rows by "), sizeBox(k, 1), el("span", "", " columns"));
      const wrap = el("div", "calc-wrap"), table = el("table");
      mats[k].forEach((row, i) => {
        const tr = el("tr");
        row.forEach((v, j) => {
          const td = el("td"), inp = el("input");
          inp.value = fmt(v).replace("−", "-");
          inp.inputMode = "decimal"; inp.autocomplete = "off";
          inp.setAttribute("aria-label", `${names[k]} row ${i + 1}, column ${j + 1}`);
          inp.addEventListener("input", () => {
            const t = inp.value.trim().replace(",", ".");
            const x = Number(t);
            const ok = t !== "" && Number.isFinite(x);
            inp.classList.toggle("bad", !ok);
            if (ok) { mats[k][i][j] = x; result(); }
          });
          td.append(inp); tr.append(td);
        });
        table.append(tr);
      });
      wrap.append(table);
      box.append(head, wrap);
      return box;
    }
    function result() {
      out.textContent = "";
      const r = run(op, mats[0], mats[1], names);
      out.append(el("h3", "", label(op)));
      if (r.error) { out.append(el("p", "calc-why", r.error)); return; }
      if (r.number !== undefined) out.append(el("p", "calc-num", fmt(r.number)));
      if (r.lines) { const ul = el("ul"); r.lines.forEach((t) => ul.append(el("li", "", t))); out.append(ul); }
      if (r.matrix) {
        const wrap = el("div", "calc-wrap"), table = el("table", "calc-res");
        r.matrix.forEach((row) => { const tr = el("tr"); row.forEach((v) => tr.append(el("td", "", fmt(v)))); table.append(tr); });
        wrap.append(table); out.append(wrap);
      }
      if (r.note) out.append(el("p", "calc-note", r.note));
    }
    function draw() {
      grids.textContent = "";
      grids.append(grid(0));
      if (OPS[op].two) grids.append(grid(1));
      result();
    }
    draw();
  }

  const api = { run, fmt, MAX };
  if (typeof module !== "undefined" && module.exports) module.exports = api;
  else root.addEventListener("DOMContentLoaded", () => {
    root.document.querySelectorAll("section.calc[data-ops]").forEach(build);
  });
})(typeof window !== "undefined" ? window : globalThis);
