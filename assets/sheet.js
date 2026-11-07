// Exercise pages: a small spreadsheet engine , number formats, solving and the page itself.
(function (root) {
  "use strict";

  class XlError extends Error {
    constructor(code) { super(code); this.code = code; }
  }
  const NUM = () => new XlError("#NUM!"), DIV = () => new XlError("#DIV/0!"), NA = () => new XlError("#N/A"), VAL = () => new XlError("#VALUE!");

  // ---- parser ----
  function tokenize(s) {
    const out = [];
    let i = 0;
    while (i < s.length) {
      const ch = s[i];
      if (/\s/.test(ch)) { i++; continue; }
      if (ch === '"') {
        let j = i + 1, t = "";
        for (;;) {
          if (j >= s.length) throw new Error("open string: " + s);
          if (s[j] === '"') {
            if (s[j + 1] === '"') { t += '"'; j += 2; continue; }
            break;
          }
          t += s[j++];
        }
        out.push({ k: "str", v: t });
        i = j + 1;
        continue;
      }
      let m = /^\d+(\.\d*)?([eE][+-]?\d+)?|^\.\d+([eE][+-]?\d+)?/.exec(s.slice(i));
      if (m) { out.push({ k: "num", v: parseFloat(m[0]) }); i += m[0].length; continue; }
      m = /^\$?[A-Z]{1,3}\$?\d+(?![A-Za-z0-9_.(])/.exec(s.slice(i));
      if (m) { out.push({ k: "ref", v: m[0].replace(/\$/g, "") }); i += m[0].length; continue; }
      m = /^[A-Za-z_][A-Za-z0-9_.]*/.exec(s.slice(i));
      if (m) { out.push({ k: "id", v: m[0].replace(/^_xlfn\.|^_xlws\./, "").toUpperCase() }); i += m[0].length; continue; }
      m = /^(<=|>=|<>|[-+*/^&=<>(),:%{};])/.exec(s.slice(i));
      if (m) { out.push({ k: "op", v: m[0] }); i += m[0].length; continue; }
      throw new Error("cannot read: " + s.slice(i));
    }
    return out;
  }

  // The usual spreadsheet order: comparison, &, + -, * /, ^, unary minus, %, range
  function parse(src) {
    const t = tokenize(src.replace(/^=/, ""));
    let p = 0;
    const isOp = (v) => t[p] && t[p].k === "op" && t[p].v === v;
    function binary(next, ops) {
      return function () {
        let a = next();
        while (t[p] && t[p].k === "op" && ops.includes(t[p].v)) {
          const op = t[p++].v;
          a = { n: "bin", op, a, b: next() };
        }
        return a;
      };
    }
    function unary() {
      if (isOp("-") || isOp("+")) {
        const op = t[p++].v;
        const a = unary();
        return op === "-" ? { n: "neg", a } : a;
      }
      return postfix();
    }
    function postfix() {
      let a = atom();
      while (isOp("%")) { p++; a = { n: "pct", a }; }
      return a;
    }
    function atom() {
      const x = t[p++];
      if (!x) throw new Error("formula ends early: " + src);
      if (x.k === "num") return { n: "num", v: x.v };
      if (x.k === "str") return { n: "str", v: x.v };
      if (x.k === "ref") {
        if (isOp(":")) {
          p++;
          const y = t[p++];
          if (!y || y.k !== "ref") throw new Error("bad range: " + src);
          return { n: "range", a: x.v, b: y.v };
        }
        return { n: "ref", v: x.v };
      }
      if (x.k === "id") {
        if (isOp("(")) {
          p++;
          const args = [];
          if (!isOp(")")) {
            for (;;) {
              args.push(compare());
              if (isOp(",")) { p++; continue; }
              break;
            }
          }
          if (!isOp(")")) throw new Error("missing ): " + src);
          p++;
          return { n: "call", f: x.v, args };
        }
        if (x.v === "TRUE") return { n: "bool", v: true };
        if (x.v === "FALSE") return { n: "bool", v: false };
        if (x.v.startsWith("_XLPM.")) return { n: "name", v: x.v };
        throw new Error("unknown name " + x.v);
      }
      if (x.k === "op" && x.v === "{") {
        const rows = [[]];
        for (;;) {
          {
            const e = compare();
            rows[rows.length - 1].push(e.n === "neg" ? -e.a.v : e.v);
          }
          if (isOp(",")) { p++; continue; }
          if (isOp(";")) { p++; rows.push([]); continue; }
          break;
        }
        if (!isOp("}")) throw new Error("missing }: " + src);
        p++;
        return { n: "arr", rows };
      }
      if (x.k === "op" && x.v === "(") {
        const a = compare();
        if (!isOp(")")) throw new Error("missing ): " + src);
        p++;
        return a;
      }
      throw new Error("unexpected " + x.v + " in " + src);
    }
    const power = binary(unary, ["^"]);
    const mul = binary(power, ["*", "/"]);
    const add = binary(mul, ["+", "-"]);
    const cat = binary(add, ["&"]);
    const compare = binary(cat, ["=", "<", ">", "<=", ">=", "<>"]);
    const tree = compare();
    if (p !== t.length) throw new Error("left over in " + src);
    return tree;
  }

  // ---- values ----
  function colNum(c) {
    let n = 0;
    for (const ch of c) n = n * 26 + ch.charCodeAt(0) - 64;
    return n;
  }
  function colName(n) {
    let s = "";
    while (n > 0) { const r = (n - 1) % 26; s = String.fromCharCode(65 + r) + s; n = (n - 1 - r) / 26; }
    return s;
  }
  function split(ref) {
    const m = /^([A-Z]+)(\d+)$/.exec(ref);
    return [colNum(m[1]), parseInt(m[2], 10)];
  }
  // an array of values, rows of columns
  class Mat {
    constructor(rows) { this.rows = rows; this.h = rows.length; this.w = rows[0].length; }
    at(i, j) {
      const r = this.h === 1 ? 0 : i, c = this.w === 1 ? 0 : j;
      if (r >= this.h || c >= this.w) throw NA();
      return this.rows[r][c];
    }
    flat() { return [].concat(...this.rows); }
  }
  const asMat = (v) => (v instanceof Mat ? v : new Mat([[v]]));
  function numbers(m) {
    return m.rows.map((row) => row.map((v) => {
      if (typeof v !== "number") throw VAL();
      return v;
    }));
  }
  function inverse(a) {
    const n = a.length;
    if (!n || a[0].length !== n) throw VAL();
    const m = a.map((row, i) => row.concat(row.map((_, j) => (i === j ? 1 : 0))));
    for (let c = 0; c < n; c++) {
      let p = c;
      for (let r = c + 1; r < n; r++) if (Math.abs(m[r][c]) > Math.abs(m[p][c])) p = r;
      if (m[p][c] === 0) throw NUM();
      [m[c], m[p]] = [m[p], m[c]];
      const d = m[c][c];
      for (let j = 0; j < 2 * n; j++) m[c][j] /= d;
      for (let r = 0; r < n; r++) {
        if (r === c || m[r][c] === 0) continue;
        const f = m[r][c];
        for (let j = 0; j < 2 * n; j++) m[r][j] -= f * m[c][j];
      }
    }
    return m.map((row) => row.slice(n));
  }
  function determinant(a) {
    const n = a.length;
    if (!n || a[0].length !== n) throw VAL();
    const m = a.map((row) => row.slice());
    let det = 1;
    for (let c = 0; c < n; c++) {
      let p = c;
      for (let r = c + 1; r < n; r++) if (Math.abs(m[r][c]) > Math.abs(m[p][c])) p = r;
      if (m[p][c] === 0) return 0;
      if (p !== c) { [m[c], m[p]] = [m[p], m[c]]; det = -det; }
      det *= m[c][c];
      for (let r = c + 1; r < n; r++) {
        const f = m[r][c] / m[c][c];
        for (let j = c; j < n; j++) m[r][j] -= f * m[c][j];
      }
    }
    return det;
  }
  function roundTo(x, d, how) {
    const p = Math.pow(10, Math.trunc(d)), y = Number((Math.abs(x) * p).toPrecision(15));
    return Math.sign(x) * how(y) / p;
  }
  const ONE = {
    SQRT: (x) => { if (x < 0) throw NUM(); return Math.sqrt(x); },
    LOG10: (x) => { if (x <= 0) throw NUM(); return Math.log10(x); },
    LN: (x) => { if (x <= 0) throw NUM(); return Math.log(x); },
    EXP: (x) => checkNum(Math.exp(x)),
    SIN: Math.sin, COS: Math.cos, TAN: Math.tan, ATAN: Math.atan,
    ACOS: (x) => { if (x < -1 || x > 1) throw NUM(); return Math.acos(x); },
    ABS: Math.abs, SINH: (x) => checkNum(Math.sinh(x)), COSH: (x) => checkNum(Math.cosh(x)), TANH: Math.tanh,
    RADIANS: (x) => x * Math.PI / 180, DEGREES: (x) => x * 180 / Math.PI,
    INT: Math.floor, SIGN: Math.sign,
    FACT: (x) => {
      if (x < 0) throw NUM();
      let r = 1;
      for (let i = 2; i <= Math.trunc(x); i++) r *= i;
      return checkNum(r);
    },
  };
  const TWO = {
    ROUND: (x, d) => roundTo(x, d, Math.round),
    ROUNDUP: (x, d) => roundTo(x, d, Math.ceil),
    ATAN2: (x, y) => { if (x === 0 && y === 0) throw DIV(); return Math.atan2(y, x); },
    MOD: (x, y) => { if (y === 0) throw DIV(); return x - y * Math.floor(x / y); },
    LOG: (x, b) => { if (x <= 0 || b <= 0) throw NUM(); return Math.log(x) / Math.log(b); },
    CEILING: (x, y) => { if (y === 0) return 0; return Number((Math.ceil(Number((x / y).toPrecision(15))) * y).toPrecision(15)); },
  };
  function toNum(v) {
    if (v instanceof Mat) v = v.rows[0][0];
    if (v === null || v === undefined || v === "") {
      if (v === "") throw VAL();
      return 0;
    }
    if (typeof v === "number") return v;
    if (typeof v === "boolean") return v ? 1 : 0;
    const x = Number(v);
    if (v.trim && v.trim() !== "" && !Number.isNaN(x)) return x;
    throw VAL();
  }
  function toText(v) {
    if (v === null || v === undefined) return "";
    if (typeof v === "boolean") return v ? "TRUE" : "FALSE";
    if (typeof v === "number") return String(parseFloat(v.toPrecision(15)));
    return String(v);
  }
  function checkNum(x) {
    if (!Number.isFinite(x)) throw NUM();
    return x;
  }

  // inverse of the standard normal distribution (Wichura, algorithm AS 241)
  function poly(c, r) {
    let s = 0;
    for (let i = c.length - 1; i >= 0; i--) s = s * r + c[i];
    return s;
  }
  const AS241 = {
    a: [3.387132872796366608, 133.14166789178437745, 1971.5909503065514427, 13731.693765509461125, 45921.953931549871457, 67265.770927008700853, 33430.575583588128105, 2509.0809287301226727],
    b: [1, 42.313330701600911252, 687.1870074920579083, 5394.1960214247511077, 21213.794301586595867, 39307.89580009271061, 28729.085735721942674, 5226.495278852854561],
    c: [1.42343711074968357734, 4.6303378461565452959, 5.7694972214606914055, 3.64784832476320460504, 1.27045825245236838258, 0.24178072517745061177, 0.0227238449892691845833, 7.7454501427834140764e-4],
    d: [1, 2.05319162663775882187, 1.6763848301838038494, 0.68976733498510000455, 0.14810397642748007459, 0.0151986665636164571966, 5.475938084995344946e-4, 1.05075007164441684324e-9],
    e: [6.6579046435011037772, 5.4637849111641143699, 1.7848265399172913358, 0.29656057182850489123, 0.026532189526576123093, 0.0012426609473880784386, 2.71155556874348757815e-5, 2.01033751145516034820e-7],
    f: [1, 0.59983322206558879373, 0.13692988092273580531, 0.0148753612908506148525, 7.868691311456132591e-4, 1.8463183175100546818e-5, 1.4215117583164458887e-7, 2.04426310338993978564e-15],
  };
  function normSInv(p) {
    if (!(p > 0 && p < 1)) throw NUM();
    const q = p - 0.5;
    if (Math.abs(q) <= 0.425) {
      const r = 0.180625 - q * q;
      return q * poly(AS241.a, r) / poly(AS241.b, r);
    }
    let r = Math.sqrt(-Math.log(q < 0 ? p : 1 - p)), x;
    if (r <= 5) { r -= 1.6; x = poly(AS241.c, r) / poly(AS241.d, r); }
    else { r -= 5; x = poly(AS241.e, r) / poly(AS241.f, r); }
    return q < 0 ? -x : x;
  }

  // ---- one sheet ----
  class Sheet {
    // cells: { "C11": {v: 3.2} or {f: "=C1+1"} }
    constructor(cells) {
      this.cells = {};
      for (const k of Object.keys(cells)) {
        const c = cells[k];
        this.cells[k] = { v: c.v === undefined ? null : c.v, tree: c.f ? parse(c.f) : null, of: c.of, arr: Boolean(c.a) };
      }
      this.memo = {};
      this.busy = {};
      this.lets = [];
    }
    reset() { this.memo = {}; this.busy = {}; }
    set(ref, v) { this.cells[ref].v = v; this.cells[ref].tree = null; this.reset(); }
    // the value of a cell, or an XlError object as a value
    value(ref) {
      if (ref in this.memo) return this.memo[ref];
      const c = this.cells[ref];
      if (!c) return null;
      if (c.of) return this.spilled(ref, c.of);
      if (c.arr) return this.spilled(ref, ref);
      if (!c.tree) return c.v;
      if (this.busy[ref]) return (this.memo[ref] = new XlError("#REF!"));
      this.busy[ref] = true;
      let out;
      try { out = this.ev(c.tree); } catch (e) {
        if (e instanceof XlError) out = e; else throw e;
      }
      if (out instanceof Mat) out = out.rows[0][0];
      if (out === null) out = 0;
      this.busy[ref] = false;
      return (this.memo[ref] = out);
    }
    // one cell of an array formula: the whole array is worked out once
    spilled(ref, owner) {
      const key = "=" + owner;
      if (!(key in this.memo)) {
        if (this.busy[key]) return (this.memo[ref] = new XlError("#REF!"));
        this.busy[key] = true;
        let out;
        try { out = asMat(this.ev(this.cells[owner].tree)); } catch (e) {
          if (e instanceof XlError) out = e; else throw e;
        }
        this.busy[key] = false;
        this.memo[key] = out;
      }
      const m = this.memo[key];
      if (m instanceof XlError) return (this.memo[ref] = m);
      const [c0, r0] = split(owner), [c1, r1] = split(ref);
      let v;
      try { v = m.at(r1 - r0, c1 - c0); } catch (e) {
        if (e instanceof XlError) v = e; else throw e;
      }
      return (this.memo[ref] = v === null ? 0 : v);
    }
    get(ref) {
      const v = this.value(ref);
      if (v instanceof XlError) throw v;
      return v;
    }
    rangeRefs(a, b) {
      const [c1, r1] = split(a), [c2, r2] = split(b), out = [];
      for (let r = Math.min(r1, r2); r <= Math.max(r1, r2); r++) {
        for (let c = Math.min(c1, c2); c <= Math.max(c1, c2); c++) out.push(colName(c) + r);
      }
      return out;
    }
    // numbers of the arguments as SUM reads them: text and blanks in ranges are skipped
    nums(args) {
      const out = [];
      for (const a of args) {
        if (a.n === "range") {
          for (const r of this.rangeRefs(a.a, a.b)) {
            const v = this.get(r);
            if (typeof v === "number") out.push(v);
          }
        } else if (a.n === "ref") {
          const v = this.get(a.v);
          if (typeof v === "number") out.push(v);
        } else {
          const v = this.ev(a);
          if (v instanceof Mat) { for (const x of v.flat()) if (typeof x === "number") out.push(x); }
          else out.push(toNum(v));
        }
      }
      return out;
    }
    // a range as an array of its values
    block(a, b) {
      const [c1, r1] = split(a), [c2, r2] = split(b), rows = [];
      for (let r = Math.min(r1, r2); r <= Math.max(r1, r2); r++) {
        const row = [];
        for (let c = Math.min(c1, c2); c <= Math.max(c1, c2); c++) row.push(this.get(colName(c) + r));
        rows.push(row);
      }
      return new Mat(rows);
    }
    // a function of one or two values, cell by cell over arrays
    map(a, b, fn) {
      if (!(a instanceof Mat) && !(b instanceof Mat)) return fn(a, b);
      const A = asMat(a), B = asMat(b), h = Math.max(A.h, B.h), w = Math.max(A.w, B.w), rows = [];
      for (let i = 0; i < h; i++) {
        const row = [];
        for (let j = 0; j < w; j++) {
          try { row.push(fn(A.at(i, j), B.at(i, j))); } catch (e) {
            if (e instanceof XlError) row.push(e); else throw e;
          }
        }
        rows.push(row);
      }
      return new Mat(rows);
    }
    ev(n) {
      switch (n.n) {
        case "num": case "str": case "bool": return n.v;
        case "ref": return this.get(n.v);
        case "neg": return this.map(this.ev(n.a), 0, (x) => -toNum(x));
        case "pct": return this.map(this.ev(n.a), 0, (x) => toNum(x) / 100);
        case "range": return this.block(n.a, n.b);
        case "arr": return new Mat(n.rows);
        case "name": {
          for (let i = this.lets.length - 1; i >= 0; i--) if (n.v in this.lets[i]) return this.lets[i][n.v];
          throw new XlError("#NAME?");
        }
        case "bin": return this.bin(n);
        case "call": return this.call(n.f, n.args);
      }
      throw new Error("node " + n.n);
    }
    bin(n) {
      return this.map(this.ev(n.a), this.ev(n.b), (a, b) => this.op(n, a, b));
    }
    op(n, a, b) {
      switch (n.op) {
        case "+": return toNum(a) + toNum(b);
        case "-": return toNum(a) - toNum(b);
        case "*": return toNum(a) * toNum(b);
        case "/": {
          const d = toNum(b);
          if (d === 0) throw DIV();
          return toNum(a) / d;
        }
        case "^": {
          const x = toNum(a), y = toNum(b);
          if (x === 0 && y === 0) throw NUM();
          if (x === 0 && y < 0) throw DIV();
          return checkNum(Math.pow(x, y));
        }
        case "&": return toText(a) + toText(b);
        default: return this.cmp(n.op, a, b);
      }
    }
    cmp(op, a, b) {
      const rank = (v) => (typeof v === "number" || v === null ? 0 : typeof v === "string" ? 1 : 2);
      let x = a === null ? (typeof b === "string" ? "" : typeof b === "boolean" ? false : 0) : a;
      let y = b === null ? (typeof a === "string" ? "" : typeof a === "boolean" ? false : 0) : b;
      let c;
      if (rank(x) !== rank(y)) c = rank(x) - rank(y);
      else {
        if (typeof x === "string") { x = x.toLowerCase(); y = y.toLowerCase(); }
        // numbers are compared to 15 significant digits
        if (typeof x === "number") { x = Number(x.toPrecision(15)); y = Number(y.toPrecision(15)); }
        c = x < y ? -1 : x > y ? 1 : 0;
      }
      return { "=": c === 0, "<>": c !== 0, "<": c < 0, ">": c > 0, "<=": c <= 0, ">=": c >= 0 }[op];
    }
    criteria(crit) {
      const m = typeof crit === "string" ? /^(<=|>=|<>|=|<|>)(.*)$/.exec(crit) : null;
      const op = m ? m[1] : "=";
      let want = m ? m[2] : crit;
      if (typeof want === "string" && want.trim() !== "" && !Number.isNaN(Number(want))) want = Number(want);
      return (v) => {
        if (v === null) return op === "<>";
        if (typeof want === "number" && typeof v !== "number") return op === "<>";
        if (typeof want === "string" && typeof v !== "string") return op === "<>";
        return this.cmp(op, v, want);
      };
    }
    call(f, a) {
      const one = () => toNum(this.ev(a[0]));
      const vals = (arg) => (arg.n === "range" ? this.rangeRefs(arg.a, arg.b).map((r) => this.get(r)) : asMat(this.ev(arg)).flat());
      if (f in ONE) return this.map(this.ev(a[0]), 0, (x) => ONE[f](toNum(x)));
      if (f in TWO) return this.map(this.ev(a[0]), a.length > 1 ? this.ev(a[1]) : (f === "LOG" ? 10 : 0), (x, y) => TWO[f](toNum(x), toNum(y)));
      switch (f) {
        case "PI": return Math.PI;
        case "ISEVEN": return Math.trunc(one()) % 2 === 0;
        case "ISNUMBER": {
          try { return typeof this.ev(a[0]) === "number"; } catch (e) {
            if (e instanceof XlError) return false;
            throw e;
          }
        }
        case "IF": {
          const c = this.ev(a[0]);
          if (c instanceof Mat) {
            const yes = a.length > 1 ? this.ev(a[1]) : true, no = a.length > 2 ? this.ev(a[2]) : false;
            return this.map(this.map(c, yes, (x, y) => [x, y]), no, (p, z) => (toNum(p[0]) ? p[1] : z));
          }
          const yes = typeof c === "string" ? (() => { throw VAL(); })() : Boolean(c);
          if (yes) return a.length > 1 ? this.ev(a[1]) : true;
          return a.length > 2 ? this.ev(a[2]) : false;
        }
        case "AND": case "OR": {
          let all = true, any = false;
          for (const x of a) {
            const list = asMat(this.ev(x)).flat().filter((v) => v !== null && typeof v !== "string");
            for (const v of list) { if (toNum(v)) any = true; else all = false; }
          }
          return f === "AND" ? all : any;
        }
        case "LET": {
          const scope = {};
          this.lets.push(scope);
          try {
            for (let i = 0; i + 1 < a.length; i += 2) scope[a[i].v] = this.ev(a[i + 1]);
            return this.ev(a[a.length - 1]);
          } finally { this.lets.pop(); }
        }
        case "SUMPRODUCT": {
          const ms = a.map((x) => asMat(this.ev(x)));
          for (const m of ms) if (m.h !== ms[0].h || m.w !== ms[0].w) throw VAL();
          let s = 0;
          for (let i = 0; i < ms[0].h; i++) {
            for (let j = 0; j < ms[0].w; j++) {
              let p = 1;
              for (const m of ms) { const v = m.rows[i][j]; p *= typeof v === "number" ? v : 0; }
              s += p;
            }
          }
          return s;
        }
        case "SUMSQ": return this.nums(a).reduce((s, x) => s + x * x, 0);
        case "MMULT": {
          const A = numbers(asMat(this.ev(a[0]))), B = numbers(asMat(this.ev(a[1])));
          if (A[0].length !== B.length) throw VAL();
          return new Mat(A.map((row) => B[0].map((_, j) => row.reduce((s, x, k) => s + x * B[k][j], 0))));
        }
        case "MINVERSE": return new Mat(inverse(numbers(asMat(this.ev(a[0])))));
        case "MDETERM": return determinant(numbers(asMat(this.ev(a[0]))));
        case "TRANSPOSE": {
          const m = asMat(this.ev(a[0]));
          return new Mat(m.rows[0].map((_, j) => m.rows.map((row) => row[j])));
        }
        case "MUNIT": {
          const n = Math.trunc(one());
          if (n < 1) throw VAL();
          return new Mat(Array.from({ length: n }, (_, i) => Array.from({ length: n }, (_, j) => (i === j ? 1 : 0))));
        }
        case "SUMIFS": {
          const sums = vals(a[0]);
          const tests = [];
          for (let i = 1; i + 1 < a.length; i += 2) tests.push([vals(a[i]), this.criteria(this.ev(a[i + 1]))]);
          let s = 0;
          sums.forEach((v, i) => { if (typeof v === "number" && tests.every(([k, t]) => t(k[i]))) s += v; });
          return s;
        }
        case "VALUE": {
          const v = this.ev(a[0]);
          if (typeof v === "number") return v;
          const x = Number(String(v).trim());
          if (String(v).trim() === "" || Number.isNaN(x)) throw VAL();
          return x;
        }
        case "TEXT": return format(toNum(this.ev(a[0])), String(this.ev(a[1]))).replace("−", "-");
        case "SUM": return this.nums(a).reduce((s, x) => s + x, 0);
        case "COUNT": {
          let n = 0;
          for (const x of a) {
            if (x.n === "range") {
              for (const r of this.rangeRefs(x.a, x.b)) if (typeof this.value(r) === "number") n++;
            } else if (x.n === "ref") {
              if (typeof this.value(x.v) === "number") n++;
            } else {
              try { if (typeof this.ev(x) === "number") n++; } catch (e) { if (!(e instanceof XlError)) throw e; }
            }
          }
          return n;
        }
        case "MAX": { const v = this.nums(a); return v.length ? Math.max(...v) : 0; }
        case "MIN": { const v = this.nums(a); return v.length ? Math.min(...v) : 0; }
        case "AVERAGE": {
          const v = this.nums(a);
          if (!v.length) throw DIV();
          return v.reduce((s, x) => s + x, 0) / v.length;
        }
        case "STDEV.S": {
          const v = this.nums(a);
          if (v.length < 2) throw DIV();
          const m = v.reduce((s, x) => s + x, 0) / v.length;
          return Math.sqrt(v.reduce((s, x) => s + (x - m) ** 2, 0) / (v.length - 1));
        }
        case "SKEW": {
          const v = this.nums(a), n = v.length;
          if (n < 3) throw DIV();
          const m = v.reduce((s, x) => s + x, 0) / n;
          const s = Math.sqrt(v.reduce((t, x) => t + (x - m) ** 2, 0) / (n - 1));
          if (s === 0) throw DIV();
          return n / ((n - 1) * (n - 2)) * v.reduce((t, x) => t + ((x - m) / s) ** 3, 0);
        }
        case "SMALL": {
          const v = this.nums([a[0]]).sort((x, y) => x - y), k = Math.ceil(toNum(this.ev(a[1])));
          if (k < 1 || k > v.length) throw NUM();
          return v[k - 1];
        }
        case "RANK.EQ": {
          const x = toNum(this.ev(a[0])), v = this.nums([a[1]]);
          const asc = a.length > 2 && toNum(this.ev(a[2])) !== 0;
          if (!v.includes(x)) throw NA();
          return 1 + v.filter((y) => (asc ? y < x : y > x)).length;
        }
        case "COMBIN": {
          const n = Math.trunc(one()), k = Math.trunc(toNum(this.ev(a[1])));
          if (n < 0 || k < 0 || k > n) throw NUM();
          let r = 1;
          for (let i = 1; i <= k; i++) r = r * (n - k + i) / i;
          return Math.round(r);
        }
        case "COUNTIF": {
          const crit = this.ev(a[1]);
          if (crit instanceof Mat) return this.map(crit, 0, (c) => vals(a[0]).filter(this.criteria(c)).length);
          return vals(a[0]).filter(this.criteria(crit)).length;
        }
        case "SUMIF": {
          const test = this.criteria(this.ev(a[1]));
          const keys = this.rangeRefs(a[0].a, a[0].b);
          const sums = a.length > 2 ? this.rangeRefs(a[2].a, a[2].b) : keys;
          let s = 0;
          keys.forEach((r, i) => {
            if (test(this.get(r))) { const v = this.get(sums[i]); if (typeof v === "number") s += v; }
          });
          return s;
        }
        case "SLOPE": case "INTERCEPT": {
          const ys = vals(a[0]), xs = vals(a[1]), px = [], py = [];
          ys.forEach((y, i) => { if (typeof y === "number" && typeof xs[i] === "number") { py.push(y); px.push(xs[i]); } });
          const n = px.length;
          if (n < 2) throw DIV();
          const mx = px.reduce((s, x) => s + x, 0) / n, my = py.reduce((s, x) => s + x, 0) / n;
          let sxy = 0, sxx = 0;
          px.forEach((x, i) => { sxy += (x - mx) * (py[i] - my); sxx += (x - mx) ** 2; });
          if (sxx === 0) throw DIV();
          const m = sxy / sxx;
          return f === "SLOPE" ? m : my - m * mx;
        }
        case "NORM.S.INV": return normSInv(one());
        case "MATCH": {
          const want = this.ev(a[0]), list = vals(a[1]), mode = a.length > 2 ? toNum(this.ev(a[2])) : 1;
          if (mode === 0) {
            const i = list.findIndex((v) => v !== null && typeof v === typeof want && this.cmp("=", v, want));
            if (i < 0) throw NA();
            return i + 1;
          }
          let best = -1;
          list.forEach((v, i) => {
            if (v !== null && typeof v === typeof want && (mode > 0 ? this.cmp("<=", v, want) : this.cmp(">=", v, want))) best = i;
          });
          if (best < 0) throw NA();
          return best + 1;
        }
        case "INDEX": {
          const rv = this.ev(a[1]), cv = a.length > 2 ? this.ev(a[2]) : null;
          if (a[0].n !== "range" || rv instanceof Mat || cv instanceof Mat || toNum(rv) === 0 || (cv !== null && toNum(cv) === 0)) {
            const m = asMat(this.ev(a[0]));
            return this.map(rv, cv === null ? (m.h === 1 ? null : 1) : cv, (r0, c0) => {
              let row = toNum(r0), col = c0 === null ? 1 : toNum(c0);
              if (a.length === 2 && m.h === 1) { col = row; row = 1; }
              if (row < 0 || col < 0 || row > m.h || col > m.w) throw new XlError("#REF!");
              if (row === 0 && col === 0) return m;
              if (row === 0) return new Mat(m.rows.map((x) => [x[col - 1]]));
              if (col === 0) return new Mat([m.rows[row - 1]]);
              return m.rows[row - 1][col - 1];
            });
          }
          const [c1, r1] = split(a[0].a), [c2, r2] = split(a[0].b);
          const h = Math.abs(r2 - r1) + 1, w = Math.abs(c2 - c1) + 1;
          let row = toNum(this.ev(a[1])), col = a.length > 2 ? toNum(this.ev(a[2])) : 1;
          if (a.length === 2 && h === 1) { col = row; row = 1; }
          if (row < 1 || col < 1 || row > h || col > w) throw new XlError("#REF!");
          return this.get(colName(Math.min(c1, c2) + col - 1) + (Math.min(r1, r2) + row - 1));
        }
        case "HLOOKUP": {
          const want = this.ev(a[0]);
          const [c1, r1] = split(a[1].a), [c2, r2] = split(a[1].b);
          const row = Math.trunc(toNum(this.ev(a[2]))), exact = a.length > 3 && !toNum(this.ev(a[3]));
          if (row < 1 || row > Math.abs(r2 - r1) + 1) throw new XlError("#REF!");
          let hit = -1;
          for (let c = Math.min(c1, c2); c <= Math.max(c1, c2); c++) {
            const v = this.get(colName(c) + Math.min(r1, r2));
            if (v === null || typeof v !== typeof want) continue;
            if (exact ? this.cmp("=", v, want) : this.cmp("<=", v, want)) hit = c;
            if (exact && hit > 0) break;
          }
          if (hit < 0) throw NA();
          return this.get(colName(hit) + (Math.min(r1, r2) + row - 1));
        }
      }
      throw new Error("function not available: " + f);
    }
    // Solve: change one cell until another reaches the target, starting from the present value (secant, then halving a bracket)
    solve(target, goal, change) {
      const f = (x) => {
        this.set(change, x);
        const v = this.value(target);
        return typeof v === "number" && Number.isFinite(v) ? v - goal : NaN;
      };
      const start = toNum(this.cells[change].v);
      const scale = Math.max(Math.abs(goal), 1);
      let x0 = start, f0 = f(x0);
      let lo = null, hi = null;
      if (Number.isFinite(f0) && Math.abs(f0) < 1e-11 * scale) return true;
      let x1 = start === 0 ? 0.01 : start * 1.01, f1 = f(x1);
      for (let i = 0; i < 200; i++) {
        if (!Number.isFinite(f1)) { x1 = (x0 + x1) / 2; f1 = f(x1); continue; }
        if (Math.abs(f1) < 1e-11 * scale) return true;
        if (Number.isFinite(f0) && f0 * f1 < 0) { lo = [x0, f0]; hi = [x1, f1]; break; }
        const d = f1 - f0;
        let x2 = Number.isFinite(f0) && d !== 0 ? x1 - f1 * (x1 - x0) / d : x1 * 1.1;
        if (!Number.isFinite(x2)) x2 = x1 * 1.1;
        if (Math.abs(x2 - x1) > 4 * Math.abs(x1) + 1) x2 = x1 + Math.sign(x2 - x1) * (Math.abs(x1) + 1);
        x0 = x1; f0 = f1; x1 = x2; f1 = f(x1);
      }
      if (lo) {
        for (let i = 0; i < 200; i++) {
          const m = (lo[0] + hi[0]) / 2, fm = f(m);
          if (Number.isFinite(fm) && Math.abs(fm) < 1e-11 * scale) return true;
          if (!Number.isFinite(fm) || fm * lo[1] > 0) lo = [m, fm];
          else hi = [m, fm];
          if (Math.abs(hi[0] - lo[0]) < 1e-14 * Math.max(Math.abs(m), 1e-9)) { f(m); return Math.abs(fm) < 1e-6 * scale; }
        }
      }
      this.set(change, start);
      return false;
    }
  }

  // ---- number formats ----
  function fixed(x, d) {
    // decimals rounded: half away from zero on the shortest decimal form
    const s = Math.abs(x).toPrecision(15);
    const n = Number(s);
    let out = n.toFixed(d);
    const up = (n + Math.pow(10, -d) * 1e-9).toFixed(d);
    if (up !== out) out = up;
    return out;
  }
  function general(x) {
    if (x === 0) return "0";
    const a = Math.abs(x);
    if (a >= 1e11 || a < 1e-4) return x.toExponential(4).replace(/e([+-])(\d)$/, "E$10$2").replace("e", "E");
    return String(parseFloat(x.toPrecision(10)));
  }
  function digits(x, zeros, hashes) {
    // zeros decimals always, hashes more when they are not zero
    let t = fixed(x, zeros + hashes);
    if (hashes && t.includes(".")) {
      const keep = t.indexOf(".") + 1 + zeros;
      let end = t.length;
      while (end > keep && t[end - 1] === "0") end--;
      t = t.slice(0, end);
      if (t.endsWith(".")) t = t.slice(0, -1);
    }
    return t;
  }
  function fraction(x) {
    const whole = Math.floor(x), part = x - whole;
    let best = [0, 1], err = part;
    for (let d = 1; d <= 9; d++) {
      const n = Math.round(part * d);
      if (Math.abs(part - n / d) < err - 1e-12) { best = [n, d]; err = Math.abs(part - n / d); }
    }
    if (best[0] === best[1]) return String(whole + 1);
    if (best[0] === 0) return String(whole);
    return (whole ? whole + " " : "") + best[0] + "/" + best[1];
  }
  function section(v, fmt) {
    // the part of a number format that applies to v, and whether it writes its own sign
    const parts = fmt.split(";");
    const cond = (p) => /^\[([<>]=?|=)(-?[\d.]+)\]/.exec(p);
    if (parts.some(cond)) {
      for (let k = 0; k < parts.length; k++) {
        const m = cond(parts[k]);
        if (!m) continue;
        const x = Number(m[2]);
        const ok = { "<": v < x, ">": v > x, "<=": v <= x, ">=": v >= x, "=": v === x }[m[1]];
        if (ok) return [parts[k].replace(cond(parts[k])[0], ""), k > 0 || /^\\-/.test(parts[k].replace(m[0], ""))];
      }
      const rest = parts.filter((p) => !cond(p));
      return [rest.length ? rest[rest.length - 1] : parts[parts.length - 1].replace(/^\[[^\]]*\]/, ""), false];
    }
    if (v < 0 && parts.length > 1) return [parts[1], true];
    if (v === 0 && parts.length > 2) return [parts[2], false];
    return [parts[0], false];
  }
  function format(v, fmt) {
    if (v === null || v === undefined) return "";
    if (v instanceof XlError) return v.code;
    if (typeof v === "boolean") return v ? "TRUE" : "FALSE";
    if (typeof v === "string") return v;
    if (!Number.isFinite(v)) return "#NUM!";
    const [raw, own] = section(v, fmt || "General");
    let sign = v < 0 && !own ? "−" : "";
    const lit = (raw.match(/^\\(.)/) || [])[1] || "";
    const f = raw.replace(/\\./g, "");
    const pre = lit === "-" ? "−" : lit;
    if (f === "General") return sign + pre + general(Math.abs(v));
    if (/\?\/\?/.test(f)) return sign + pre + fraction(Math.abs(v));
    const a = /%/.test(f) ? Math.abs(v) * 100 : Math.abs(v);
    const dec = /\.([0#]*)/.exec(f.replace(/E.*$/, ""));
    const zeros = dec ? (dec[1].match(/0/g) || []).length : 0, hashes = dec ? (dec[1].match(/#/g) || []).length : 0;
    if (/E\+/.test(f)) {
      let e = a === 0 ? 0 : Math.floor(Math.log10(a));
      let m = a === 0 ? 0 : a / Math.pow(10, e);
      if (Number(fixed(m, zeros + hashes)) >= 10) { e += 1; m /= 10; }
      const width = (f.match(/E\+(0+)/) || [, "0"])[1].length;
      const text = sign + pre + digits(m, zeros, hashes) + "E" + (e < 0 ? "-" : "+") + String(Math.abs(e)).padStart(width, "0");
      return text;
    }
    let t = digits(a, zeros, hashes);
    if (/#,##/.test(f)) {
      const [x, y] = t.split(".");
      t = x.replace(/\B(?=(\d{3})+(?!\d))/g, ",") + (y !== undefined ? "." + y : "");
    }
    if (Number(t.replace(/,/g, "")) === 0 && !lit) sign = "";
    return sign + pre + t + (/%/.test(f) ? "%" : "");
  }

  // ---- the page ----
  function start(doc) {
    const data = JSON.parse(doc.getElementById("sheet-data").textContent);
    const sheet = new Sheet(data.cells);
    const outs = {}, inputs = {};
    doc.querySelectorAll("[data-out]").forEach((el) => { outs[el.dataset.out] = el; });
    doc.querySelectorAll("input[data-in]").forEach((el) => { inputs[el.dataset.in] = el; });
    const fmt = (r) => data.cells[r].n;

    function paint() {
      sheet.reset();
      for (const r of Object.keys(outs)) {
        const v = sheet.value(r);
        outs[r].textContent = format(v, fmt(r));
        outs[r].classList.toggle("err", v instanceof XlError);
      }
    }
    function showInput(r) {
      inputs[r].value = format(sheet.cells[r].v, fmt(r)).replace(/,/g, "").replace("−", "-");
    }
    for (const r of Object.keys(inputs)) {
      const el = inputs[r];
      showInput(r);
      el.addEventListener("input", () => {
        const t = el.value.trim().replace(",", ".");
        const x = Number(t);
        const ok = t !== "" && Number.isFinite(x);
        el.classList.toggle("bad", !ok);
        if (ok) { sheet.set(r, x); paint(); }
      });
      el.addEventListener("blur", () => {
        if (!el.classList.contains("bad")) showInput(r);
      });
    }
    doc.querySelectorAll("[data-solve]").forEach((b) => {
      const [target, goal, change] = b.dataset.solve.split("|");
      b.addEventListener("click", () => {
        const ok = sheet.solve(target, Number(goal), change);
        const note = b.parentElement.querySelector(".solve-note");
        if (note) note.textContent = ok ? "Solved." : "No answer from this starting value: try another one.";
        // the exact value stays in the sheet; the box shows it rounded
        showInput(change);
        paint();
      });
    });
    doc.querySelectorAll("[data-reset]").forEach((b) => {
      b.addEventListener("click", () => {
        for (const r of Object.keys(inputs)) {
          sheet.set(r, data.cells[r].v);
          inputs[r].classList.remove("bad");
          showInput(r);
        }
        doc.querySelectorAll(".solve-note").forEach((n) => { n.textContent = ""; });
        paint();
      });
    });
    const toggle = doc.getElementById("show-formulas");
    if (toggle) {
      toggle.addEventListener("change", () => doc.body.classList.toggle("formulas", toggle.checked));
    }
    paint();
  }

  const api = { Sheet, parse, format, XlError, start };
  if (typeof module !== "undefined" && module.exports) module.exports = api;
  else root.addEventListener("DOMContentLoaded", () => {
    if (root.document.getElementById("sheet-data")) start(root.document);
  });
})(typeof window !== "undefined" ? window : globalThis);
