/****************************************************************************/
/*****                                                                   ****/
/*****     from Syntax.hs                                                ****/
/*****                                                                   ****/
/****************************************************************************/


// variables are not strings any more: they are objects tagged "Var"
// environments are still objects

// Scope info

function LocalVar() {
    return {tag: "LocalVar"};
};
function GlobalVar(b, fp) {
    return {tag: "GlobalVar", longname: b, filepath: fp};
};
function AmbiguousVar() {
    return {tag: "AmbiguousVar"};
};
function OutOfScope() {
    return {tag: "OutOfScope"};
};
function InvalidNamespace() {
    return {tag: "InvalidNamespace"};
};
function dynVar(sco) {
    return !(sco.tag == "GlobalVar" && sco.longname || sco.tag == "InvalidNamespace");
};


// Constructors for Term, Pattern, Value

function Var(sco, x) { // scopechecked thing
    return {tag: "Var", scope: sco, name: x};
};
function Atom(a) { // Atom a
    return {tag: "Atom", atom: a};
};
function Lit(l) { // Lit l
    return {tag: "Lit", literal: l};
};
function Nil() { // Nil
    return {tag: "Nil"};
};
function Cell(x,y) { // Cell x y
    return {tag: "Cell", fst: x, snd: y};
};

// Term only
function Stringy(x) { // from String k ts u, we get ts tipped by u
    return {tag: "String", chunks: x};
};
function Strunk(p,c,t) { // one chunk from a String
    return {prefix: p, splice: c, tail: t};
};
function App(f, as) {       // App f as, where as is an array
    return {tag: "App", fun: f, args: as};
};
function Semi(l, r) { // Semi l r
    return {tag: "Semi", left: l, right: r};
};
function Prio(l, r) { // Semi l r
    return {tag: "Prio", left: l, right: r};
};
function Fun(hs, cs) { // Fun hs cs, both arrays
    return {tag: "Fun", handles: hs, clauses: cs};
};
function Match(p, t) { // Match p t
    return {tag: "Match", pat: p, term: t};
};
function Mask(a, t) { // Mask a t
    return {tag: "Mask", atom: a, term: t};
};

// PValue only
const PWild = {tag: "Wild"};

function PAs(x, p) {
    return {tag: "As", var: x, pat: p};
};

// PComputation, Computation
function Value(p) {
    return {tag: "Value", value: p};
};
function Request(a, ps, k) {
    return {tag: "Request", cmd: a, args: ps, cont: k};
};

// a thunk pattern is a string

// Lit
//function LitString(k,t) { return t; };
function LitNum(n,d) {
    if (n==0) { return {num: n, den: 1}; };
    if (d==0) { return null; };
    var a = n;
    var b = d;
    var r = 1;
    while (r != 0) {
        r = a % b;
        a = b;
        b = r;
    };
    return {num: n/a, den: d/a};
};
function LitEq(x,y) {
    if (stringy(x) && stringy(y)) { return (x == y); };
    if (boolean(x) && boolean(y)) { return (x == y); };
    if (stringy(x) || stringy(y)) { return false; };
    return (x.num == y.num && x.den == y.den);
};


/****************************************************************************/
/*****                                                                   ****/
/*****     from Semantics.hs                                             ****/
/*****                                                                   ****/
/****************************************************************************/

// Value only

function VPrim(p, hs) {
    return {tag: "VPrim", prim: p, handles: hs};
};

function VFun(fs     // null-terminated Cons-list of frames
              , rho  // environment object
              , hs   // null or array of arrays of strings
              , cs   // array of clauses
             ) {
    return {tag: "VFun", cont: fs, env: rho, handles: hs, clauses: cs};
};
function VThunk(c) {
    return {tag: "VThunk", thunk: c};
};

// what does a value, used as a function, handle in its arguments?
function fhandles(v) {
    if (v.tag == "VFun" || v.tag == "VPrim") { return v.handles; };
    return null;
};

function hasLength(x) {
    return (!(x === null || x === undefined || x.length === undefined));
};


// are there any handles in position i of handlers hss?
function anyhandles(i,hss) {
    if (hasLength(hss) && i < hss.length) {
        var hs = hss[i];
        return hasLength(hs) && hs.length > 0;
    };
    return false;
};

// is command c handled in position i by handlers hss?
function inhandles(c,i,hss) {
    if (hasLength(hss) && i < hss.length) {
        var hs = hss[i];
        if (hasLength(hs)) {
            for (i = 0; i < hs.length; i++) {
                if (c === hs[i]) { return true; };
            };
        };
    };
    return false;
};

// Clause
function Clause(ps,t) {
    return {pats: ps, term: t};
};


/* explicit environments */
function value2env(rho, v) {
    var stk = [v]; var i = 1;
    while (i > 0) {
        i--;
        v = stk[i];
        if (v.tag == "Cell") {
            if (v.fst.tag == "Atom") { rho[v.fst.atom] = v.snd; continue; };
            stk[i++] = v.fst;
            stk[i++] = v.snd;
        };
    };
    return;
};

function env2value(rho) {
    var v = Nil();
    for (x in rho) { v = Cell(Cell(Atom(x),rho[x]),v); };
    return v;
};


/* Matchers on success grow an environment, return a Boolean */

function stringy(x) { return (typeof(x)=="string"); };
function boolean(x) { return (typeof(x)=="boolean"); };

function cmatches(rho, qs, cs) {
    if (qs.length != cs.length) { return false; };
    var i;
    for (i = 0; i < qs.length; i++ ) {
        if (!cmatch(rho,qs[i],cs[i])) { return false; };
    };
    return true;
};

function cmatch(rho, q, c) {
    if (stringy(q)) { rho[q] = VThunk(c); return true; };
    if (q.tag == c.tag) {
        switch (q.tag) {
        case "Value" :
            return vmatch(rho,q.value,c.value);
        case "Request" :
            if (q.cmd == c.cmd && vmatches(rho,q.args,c.args)) {
                rho[q.cont] = VFun(c.cont,[],null,[Clause([Value("_return")],Var(LocalVar(),"_return"))]);
                return true;
            };
            return false;
        };
    }
    return null;
};

function vmatches(rho, ps, vs) {
    if (ps.length != vs.length) { return false; };
    var i;
    for (i = 0; i < ps.length; i++ ) {
        if (!vmatch(rho,ps[i],vs[i])) { return false; };
    };
    return true;
};

function vmatch(rho, p, v) {
    if (stringy(p)) { rho[p] = v; return true; };
    if (p.tag == "Wild") { return true; };
    if (p.tag == "As") { rho[p.var] = v; return (vmatch(rho,p.pat,v)); };
    if (p.tag == "String") {
        if (v.tag != "Lit" || !stringy(v.literal)) { return false; };
        return smatch(rho, p.chunks, v.literal);
    };
    if (p.tag == v.tag) {
        switch (p.tag) {
        case "Atom" :
            return (p.atom == v.atom);
        case "Nil" : return true;
        case "Cell" :
            if (vmatch(rho,p.fst,v.fst)) {
                return (vmatch(rho,p.snd,v.snd));
            };
            return false;
        case "Lit" :
            return LitEq(p.literal,v.literal);
        };
    };
    return false;
};

function smatch(rho, p, v) {
    var iv = 0;
    function skip (s) {
        var is = 0;
        while (is < s.length) {
            if (iv == v.length || v[iv] != s[is]) { return false; };
            iv++; is++;
        };
        return true;
    };
    function mfind(m) { // return the string demanded by the mode, fail is null
        switch (m.tag) {
        case "Head" :
            if (iv == v.length) { return null; }
            return (v.slice(iv, ++iv));
            break;
        case "Terminal" :
            var jv = v.length - m.delim.length;
            if (iv > jv) { return null; }
            return (v.slice(iv, iv=jv));
            break;
        case "Next" :
            var jv = v.indexOf(m.delim,iv);
            if (iv > jv) { return null; }
            return (v.slice(iv, iv=jv));
            break;
        };
        return null;
    };
    function tmatch(rho, p, m) {
        switch (p.tag) {
        case "As" :
            var s = tmatch(rho, p.pat, m);
            if (stringy(s)) { rho[p.var] = Lit(s) };
            return s;
        case "Cell" :
            var a = tmatch(rho, p.fst, {tag: "Head"});
            if (!stringy(a)) { return null; };
            var b = tmatch(rho, p.snd, m);
            if (!stringy(b)) { return null; };
            return a.concat(b);
        case "Nil" :
            return "";
        case "Atom" :
            return "";
        case "Lit" :
            return null;
        };
        var s = mfind(m);
        if (stringy(s) && vmatch(rho,p,Lit(s))) { return s; };
        return null;
    };
    while (!stringy(p)) {
        var m = {};
        if (!skip(p.prefix)) { return false; }
        if (stringy(p.tail)) { m = {tag: "Terminal", delim: p.tail}; }
        else { m = {tag: "Next", delim: p.tail.prefix}; };
        if (!stringy(tmatch(rho, p.splice, m))) { return false; }
        p = p.tail;
    };
    return (skip(p) && iv == v.length);
};

// Frame
function CellL(rho,t) {
    return {tag: 0, env: rho, snd: t};
};
function CellR(v,rho) {
    return {tag: 1, fst: v, env: rho};
};
function AppL(rho,ts) {
    return {tag: 2, env: rho, args: ts};
};
// AppR is a carefully engineered pun with Apply
function SemiL(rho,r) {
    return {tag: 4, env: rho, right: r};
};
function StringLR(v,rho,u) {
    return {tag: 5, done: v, env: rho, chunks: u};
};
function MatchR(p) {
    return {tag: 6, pat: p};
};
function PrioL(rho,r) {
    return {tag: 7, env: rho, right: r};
};
function Masking(a) {
    return {tag: 8, atom: a};
};
//Clauses is a carefully engineered pun with Call

function handleFrame(fr) {
    switch (fr.tag) {
    case 3 : // Apply
        return anyhandles(fr.now, fr.handles);
    case 7 : // PrioL
    case 8 : // Masking
    case 9 : // Call
        return true;
    };
    return false;
};

// State
// pop states, ie those which pop ctx until done
function Use(v) {
    return {tag: 0, val: v};
};
function Handle(c,i,as,k) {
    return {tag: 1, cmd: c, inx: i, args: as, cont: k};
};
function RequestAtomic(a,as) {
    return Handle(a,0,as,null);
};
function Complain(c,as) {
    return RequestAtomic(c,as);
};
// push states, ie those which grow ctx
function Eval(rho, t) {
    return {tag: 2, env: rho, term: t};
};
function Apply(f,cz,rho,i,hs,ts) { // carefully engineered pun with AppR
    return {tag: 3, fun: f, done: cz, env: rho, now: i, handles: hs, args: ts};
};
function Call(rho,cs,i,as) {
    return {tag: 9, env: rho, clauses: cs, finger: i, args: as};
};

function Abort() {
    return Complain("abort",[]);
};

// Primitives

// equality testing
function valHoHuh(x) {
    switch (x.tag) {
    case "VFun" :
    case "VPrim" :
    case "VThunk" :
        return true;
    default:
        return false;
    };
};

function valEqHuh(x, y) {
    var ls = [x]; var rs = [y]; var i = 1;
    while (i-- > 0) {
        x = ls[i]; y = rs[i];
        if (valHoHuh(x) || valHoHuh(y)) {return null;};
        if (x.tag == y.tag) {
            switch (x.tag) {
            case "Atom" :
                if (x.atom == y.atom) {continue;};
                break;
            case "Lit" :
                if (LitEq(x.literal, y.literal)) {continue;};
                break;
            case "Nil" :
                continue;
            case "Cell" :
                ls[i] = x.snd; rs[i] = y.snd; i++;
                ls[i] = x.fst; rs[i] = y.fst; i++;
                continue;
            };
        };
        return false;
    };
    return true;
};


// primStringConcat is needed by the interpreter, anyway
function primStringConcat(vs) {
    var ts = []; // final string
    var cs = vs.reverse(); // stack of arguments to go through

    while (hasLength(cs) && cs.length > 0) {
        var x = cs.pop();
        if (x.tag == "Value") { x = x.value; };

        switch (x.tag) {
        case "Lit":
            if (stringy(x.literal)) { ts += x.literal; continue; }
            else return Complain("Invalid_StringConcat_ArgType",[]);
        case "Cell":
            cs.push(x.snd,x.fst); // note push in the right order!
            continue;
        case "Atom":
            continue;
        case "Nil":
            continue;
        default:
            return Complain("Invalid_StringConcat_ArgType",[]);
        };
    };
    return Use(Lit(ts));
};

function prim(f, vs) {
    function primBooBin(nm,implem,cs) {
        if (hasLength(cs) && cs.length == 2) {
            if (cs[0].tag == "Value" && cs[1].tag == "Value" &&
                cs[0].value.tag == "Lit" && cs[1].value.tag == "Lit") {
                var x = cs[0].value.literal;
                var y = cs[1].value.literal;
                if (boolean(x) && boolean(y)) {
                    return Use(Lit(implem(x,y)));
                };
                return Complain("Invalid_" + nm + "_ArgType",[]);
            };
            return Complain("Invalid_" + nm + "_ArgRequest",[]);
        };
        return Complain("Invalid_" + nm + "_Arity",[]);
    };
    function primInfixAnd(cs) {
        return primBooBin("primInfixAnd"
                          , function(x,y){ return x && y;}
                          , cs
                         );
    };
    function primInfixOr(cs) {
        return primBooBin("primInfixOr"
                          , function(x,y){ return x || y;}
                          , cs
                         );
    };
    function primPrefixNot(cs) {
        if (hasLength(cs) && cs.length == 1) {
            if (cs[0].tag == "Value" &&
                cs[0].value.tag == "Lit") {
                var x = cs[0].value.literal;
                if (boolean(x)) {
                    return Use(Lit(!x));
                };
                return Complain("Invalid_primPrefixNot_ArgType",[]);
            };
            return Complain("Invalid_primPrefixNot_ArgRequest",[]);
        };
        return Complain("Invalid_primPrefixNot_Arity",[]);
    };
    function primInfixEquals(cs) {
        if (hasLength(cs) && cs.length == 2) {
            if (cs[0].tag == "Value" && cs[1].tag == "Value") {
                var b = valEqHuh(cs[0].value,cs[1].value);
                if (b === null) {
                    return Complain("higherOrderEqTest",[]);
                };
                return Use(Lit(b));
            };
            return Complain("Invalid_primInfixEquals_ArgRequest",[]);
        };
        return Complain("Invalid_primInfixEquals_Arity",[]);
    };
    function primInfixUnequal(cs) {
        if (hasLength(cs) && cs.length == 2) {
            if (cs[0].tag == "Value" && cs[1].tag == "Value") {
                var b = valEqHuh(cs[0].value,cs[1].value);
                if (b === null) {
                    return Complain("higherOrderEqTest",[]);
                };
                return Use(Lit(!b));
            };
            return Complain("Invalid_primInfixUnequal_ArgRequest",[]);
        };
        return Complain("Invalid_primInfixUnequal_Arity",[]);
    };
    function primNumBin(nm,implem,cs) {
        if (hasLength(cs) && cs.length == 2) {
            if (cs[0].tag == "Value" && cs[1].tag == "Value" &&
                cs[0].value.tag == "Lit" && cs[1].value.tag == "Lit") {
                var x = cs[0].value.literal;
                var y = cs[1].value.literal;
                if (!stringy(x) && !stringy(y) && !boolean(x) && !boolean(y)) {
                    return Use(Lit(implem(x,y)));
                };
                return Complain("Invalid_" + nm + "_ArgType",[]);
            };
            return Complain("Invalid_" + nm + "_ArgRequest",[]);
        };
        return Complain("Invalid_" + nm + "_Arity",[]);
    };
    function primInfixPlus(cs) {
        return primNumBin("primInfixPlus"
                          , function(x,y){ return LitNum(x.num*y.den + y.num*x.den,x.den*y.den);}
                          , cs
                         );
    };
    function primInfixTimes(cs) {
        return primNumBin("primInfixTimes"
                          , function(x,y){ return LitNum(x.num*y.num,x.den*y.den);}
                          , cs
                         );
    };
    function primInfixMinus(cs) {
        return primNumBin("primInfixMinus"
                          , function(x,y){ return LitNum(x.num*y.den - y.num*x.den,x.den*y.den);}
                          , cs
                         );
    };
    function primInfixOver(cs) {
        if (hasLength(cs) && cs.length == 2) {
            if (cs[0].tag == "Value" && cs[1].tag == "Value" &&
                cs[0].value.tag == "Lit" && cs[1].value.tag == "Lit") {
                var x = cs[0].value.literal;
                var y = cs[1].value.literal;
                if (!stringy(x) && !stringy(y) && !boolean(x) && !boolean(y)) {
                    if (y.num == 0) {
                        return Complain("divByZero",[]);
                    };
                    return Use(Lit(LitNum(x.num*y.den, x.den*y.num)));
                };
                return Complain("Invalid_primInfixOver_ArgType",[]);
            };
            return Complain("Invalid_primInfixOver_ArgRequest",[]);
        };
        return Complain("Invalid_primInfixOver_Arity",[]);
    };
    function primInfixLessEq(cs) {
        return primNumBin("primInfixLessEq"
                          , function(x,y){ return x.num*y.den <= y.num*x.den;}
                          , cs
                         );
    };
    function primInfixLess(cs) {
        return primNumBin("primInfixLessEq"
                          , function(x,y){ return x.num*y.den < y.num*x.den;}
                          , cs
                         );
    };
    function primInfixGreaterEq(cs) {
        return primNumBin("primInfixLessEq"
                          , function(x,y){ return x.num*y.den >= y.num*x.den;}
                          , cs
                         );
    };
    function primInfixGreater(cs) {
        return primNumBin("primInfixLessEq"
                          , function(x,y){ return x.num*y.den > y.num*x.den;}
                          , cs
                         );
    };
    function primNumToString(cs) {
        if (hasLength(cs) && cs.length == 1) {
            var x = cs[0];
            if (x.tag == "Value" &&
                x.value.tag == "Lit") {
                if (!stringy(x.value.literal) && !boolean(x.value.literal)) {
                    return Use(Lit(render(x)));
                };
                return Complain("Invalid_primNumToString_ArgType",[]);
            };
            return Complain("Invalid_primNumToString_ArgRequest",[]);
        };
        return Complain("Invalid_primNumToString_Arity",[]);
    };
    function primStringToNum(cs) {
        if (hasLength(cs) && cs.length == 1) {
            if (cs[0].tag == "Value" &&
                cs[0].value.tag == "Lit") {
                var x = cs[0].value.literal;
                if (stringy(x)) {
                    var p = /^(\d)+$/; // matches integers

                    if (p.test(x)) {
                        return Use(Lit(LitNum(parseInt(x), 1)));
                    };
                    var xs = x.split("/");
                    if (hasLength(xs) && xs.length == 2 &&
                        p.test(xs[0]) && p.test(xs[1])) {
                        return Use(Lit(LitNum(parseInt(xs[0]),
                                              parseInt(xs[1]))));
                    };
                    xs = x.split(".");
                    if (hasLength(xs) && xs.length == 2 &&
                        p.test(xs[0]) && p.test(xs[1])) {
                        let a = parseInt(xs[0]); // whole number
                        let b = parseInt(xs[1]); // num of rest
                        let c = Math.pow(10, xs[1].length); //denom of rest
                        return Use(Lit(LitNum(c * a + b, c)));
                    };
                };
                return Complain("Invalid_primStringToNum_ArgType",[]);
            };
            return Complain("Invalid_primStringToNum_ArgRequest",[]);
        };
        return Complain("Invalid_primStringToNum_Arity",[]);
    };

    switch (f) {
    case "primStringConcat":
        return primStringConcat(vs);
    case "primPrefixNot":
        return primPrefixNot(vs);
    case "primInfixAnd":
        return primInfixAnd(vs);
    case "primInfixOr":
        return primInfixOr(vs);
    case "primInfixEquals":
        return primInfixEquals(vs);
    case "primInfixUnequal":
        return primInfixUnequal(vs);
    case "primInfixPlus":
        return primInfixPlus(vs);
    case "primInfixMinus":
        return primInfixMinus(vs);
    case "primInfixTimes":
        return primInfixTimes(vs);
    case "primInfixOver":
        return primInfixOver(vs);
    case "primInfixLessEq":
        return primInfixLessEq(vs);
    case "primInfixLess":
        return primInfixLess(vs);
    case "primInfixGreaterEq":
        return primInfixGreaterEq(vs);
    case "primInfixGreater":
        return primInfixGreater(vs);
    case "primNumToString":
        return primNumToString(vs);
    case "primStringToNum":
        return primStringToNum(vs);
    default:
        return Complain("NoPrim",[]);
    };
};

// Lists made by null and
function Cons(h,t) {
    return {hd: h, tl: t};
};

//Continuations are null or
function Lox(l, k) {
    return {tag: false, lox: l, cont: k};
};
function Hox(f, l, k) {
    return {tag: true, hand: f, lox: l, cont: k};
};

function shonkier(glob,inputs,t) {
    var lox = null
    var hox = null;
    var fr = null;

    function done() { return (lox == null && hox == null); };
    function push(f) {
        if (handleFrame(f)) {
            hox = {hox: hox, lox: lox, hand: f};
            lox = null;
            return;
        };
        lox = {pop: lox, top: f};
    };
    function pop() {
        if (lox == null) {
            fr = hox.hand;
            lox = hox.lox;
            hox = hox.hox;
            return;
        };
        fr = lox.top; lox = lox.pop;
    };
    function cont(k) {
        while (k != null) {
            if (k.tag) {
                hox = {hox: hox, lox: lox, hand: k.hand};
                lox = null;
            };
            if (lox == null) {lox = k.lox} else {
                var l = k.lox;
                var m = null;
                var t = null;
                // nondestructively make m the reverse of l
                while (l != null) {
                    m = {top: l.top, pop: m};
                    l = l.pop;
                };
                // destructively reverse m onto lox
                while (m != null) {
                    t = m.pop;
                    m.pop = lox;
                    lox = m;
                    m = t;
                };
            };
            k = k.cont;
        };
    };

    function handleInputs(a, vs, k) {
        if (hasLength(vs) && vs.length == 1 && stringy(vs[0].literal)) {
            var f = vs[0].literal;
            // preprocess the array key depending on the effect
            switch (a) {
            case "POST":
                f = "POST_" + f;
                break;
            case "GET":
                f = "GET_" + f;
                break;
            }
            var v = inputs[f];
            if (v != undefined) {
                cont(k);
                return Use(Lit(v));
            } else { cont(k); return Complain("UnknownInput", vs); }
        } else { cont(k); return Complain("IncorrectInputRequest", vs); }
    }

    var state = Eval({},t);
    while (true) // returns when conditions in `if`s are true (ctx is null in a pop state)
    {
        if (state.tag == 0 && done()) { return Value(state.val); };
        if (state.tag == 1 && done()) {
            //unhandled request for the world; some the world can handle
            if (["POST", "GET", "meta"].indexOf(state.cmd) != -1) {
                state = handleInputs(state.cmd, state.args, state.cont);
                continue;
            }
            return Request(state.cmd,state.args,state.cont);
        }

        switch (state.tag) {
        case 0: // Use
            pop();
            switch (fr.tag) {
            case 0: // CellL
                push(CellR(state.val,fr.env));
                state = Eval(fr.env,fr.snd);
                continue;
            case 1: // CellR
                state = Use(Cell(fr.fst,state.val));
                continue;
            case 2: // AppL
                switch (state.val.tag) {
                case "Lit":
                    if (state.val.literal === false) {
                        state = Abort();
                        continue;
                    };
                    break;
                case "VFun":
                    cont(state.val.cont);
                case "Atom":
                case "VPrim":
                case "VThunk":
                    state = Apply(state.val,null,fr.env,0,fhandles(state.val),fr.args);
                    continue;
                };
                if (fr.args.length != 1) {
                    state = Complain("EnvironmentsAreNullary",[]);
                };
                var rho = Object.assign({}, fr.env);
                value2env(rho, state.val);
                state = Eval(rho, fr.args[0]);
                continue;
            case 3: // Apply
                state = Apply(fr.fun
                              ,Cons(Value(state.val),fr.done) // stash value
                              ,fr.env
                              ,fr.now+1 // move yer finger
                              ,fr.handles,fr.args);
                continue;
            case 4: // SemiL
                state = Eval(fr.env, fr.right);
                continue;
            case 5: // StringLR
                if (stringy(fr.chunks)){
                    state = primStringConcat([fr.done,state.val,Lit(fr.chunks)]);
                    continue;
                };
                push(StringLR(Cell(fr.done,Cell(state.val,Lit(fr.chunks.prefix))),fr.env,fr.chunks.tail));
                state = Eval(fr.env,fr.chunks.splice);
                continue;
            case 6: // MatchR
                var rho = {};
                if (vmatch(rho, fr.pat, state.val)) { state = Use(env2value(rho)); continue; };
                state = Abort();
                continue;
            case 7: // PrioL
            case 8: // Masking
            case 9: // Call
                continue;
            };
            state = Complain("BadFrame",[]);
            continue;
        case 1: // Handle
            if (hox == null) {
                state = Handle(state.cmd,state.inx,state.args,Lox(lox,state.cont));
                lox = null;
                continue;
            };
            var kox = lox;
            fr = hox.hand;
            lox = hox.lox;
            hox = hox.hox;
            if (fr.tag == 3 // Apply
                && inhandles(state.cmd,fr.now,fr.handles)
               ) {
                if (state.inx == 0) {
                    state = Apply(fr.fun
                                 ,Cons(Request(state.cmd,state.args,Lox(kox,state.cont)),fr.done)
                                 ,fr.env
                                 ,fr.now+1
                                 ,fr.handles
                                  ,fr.args); }
                else { state = Handle(state.cmd, state.inx-1, state.args, Hox(fr,kox,state.cont)); };
                continue;
            };
            if (fr.tag == 7 // PrioL
                && state.cmd == "abort") {
                if (state.inx == 0) { state = Eval(fr.env, fr.right); }
                else { state = Handle(state.cmd, state.inx-1, state.args, Hox(fr,kox,state.cont)); };
                continue;
            };
            if (fr.tag == 8 // Masking
                && state.cmd == fr.atom) {
                state = Handle(state.cmd, state.inx+1, state.args, Hox(fr,kox,state.cont));
                continue;
            };
            if (fr.tag == 9 // Call
                && state.cmd == "abort") {
                if (state.inx == 0) { state = fr; } // pun activated!
                else { state = Handle(state.cmd, state.inx-1, state.args, Hox(fr,kox,state.cont)); };
                continue;
            };
            state = Handle(state.cmd, state.inx, state.args, Hox(fr,kox,state.cont));
            continue;
        case 2: // Eval
            var t = state.term;
            if (stringy(t)) {
                state = Complain("antiqueVariable",[Lit(t)]);
                continue;
            };
            switch (t.tag) {
            case "Var":
                if (dynVar(t.scope)) {
                    var v = state.env[t.name];
                    if (v != undefined) {
                        state = Use(v);
                        continue;
                    };
                };
                if (t.scope.tag == "GlobalVar") {
                    var v = glob[t.name][t.scope.filepath];
                    if (v != undefined) {
                        state = Use(v);
                        continue;
                    };
                };
                state = Complain(t.scope.tag,[Lit(t.name)]);
                continue;
            case "Atom":
                state = Use(t);
                continue;
            case "Lit":
                state = Use(t);
                continue;
            case "Nil":
                state = Use(t);
                continue;
            case "Cell":
                push(CellL(state.env,t.snd));
                state = Eval(state.env,t.fst);
                continue;
            case "App":
                push(AppL(state.env,t.args));
                state = Eval(state.env,t.fun);
                continue;
            case "Semi":
                push(AppL(state.env,[t.right]));
                state = Eval(state.env,t.left);
                continue;
            case "Prio":
                push(PrioL(state.env,t.right));
                state = Eval(state.env,t.left);
                continue;
            case "Fun":
                state = Use(VFun(null,state.env,t.handles,t.clauses));
                continue;
            case "String":
                if (stringy(t.chunks)) {
                    state = Use(Lit(t.chunks));
                    continue;
                };
                push(StringLR(Lit(t.chunks.prefix),state.env,t.chunks.tail));
                state = Eval(state.env,t.chunks.splice);
                continue;
            case "Match":
                push(MatchR(t.pat));
                state = Eval(state.env,t.term);
                continue;
            case "Mask":
                if (t.atom == "abort" && hox != null
                    && (hox.hand.tag == 7     // PrioL
                        || hox.hand.tag == 9  // Call
                       )) {
                    var tmp = null;
                    while (lox != null) {
                        tmp = {hd: lox.top, tl: tmp};
                        lox = lox.pop;
                    };
                    lox = hox.lox;
                    hox = hox.hox;
                    while (tmp != null) {
                        lox = {pop: lox, top: tmp.hd};
                        tmp = tmp.tl;
                    };
                }
                else { push(Masking(t.atom)) };
                state = Eval(state.env,t.term);
                continue;
            };
            break;
        case 3: // Apply
            if (state.now < state.args.length) {
                push(state);
                state = Eval(state.env,state.args[state.now]);
                continue;
            };
            var args = [];
            var d = state.done;
            var i = state.now;
            while (i > 0) {
                i--;
                args[i] = d.hd;
                d = d.tl;
            };
            switch (state.fun.tag) {
            case "Atom":
                for (i = 0; i < args.length; i++) { args[i] = args[i].value; }
                state = RequestAtomic(state.fun.atom,args);
                continue;
            case "VPrim":
                state = prim(state.fun.prim, args);
                continue;
            case "VFun":
                // cont(state.fun.cont);
                state = Call(state.fun.env,state.fun.clauses,0,args);
                continue;
            case "VThunk":
                d = state.fun.thunk;
                switch (d.tag) {
                case "Value":
                    state = Use(d.value);
                    continue;
                case "Request":
                    state = Handle(d.cmd,0,d.args,d.cont);
                    continue;
                };
            };
            state = Complain("NotFuny",[]);
            continue;
        case 9: // Call
            var cs = state.clauses;
            var i = state.finger;
            while (i < cs.length) {
                var rho = Object.assign({}, state.env);
                if (cmatches(rho,cs[i].pats,state.args)) {
                    push(Call(state.env,state.clauses,i+1,state.args))
                    state = Eval(rho,cs[i].term);
                    break;
                };
                i++;
            };
            if (i == cs.length) { state = Abort(); };
            continue;
        };
    };
};

function renderList(a,b) {
    var xs = [];
    var i = 0;
    function output(str) {
        xs[i] = str;
        i++;
    };
    var hd = a;
    var tl = b;
    var space = false;
    while (tl != null) {
        if (space) { output(" "); }
        space = true;
        output(render(Value(hd)));
        switch (tl.tag) {
        case "Atom":
            output("|"); output(render(Value(tl))); tl = null; continue;
        case "Nil": tl = null; continue;
        case "Cell": hd = tl.fst; tl = tl.snd; continue;
        default: output("|"); output(render(Value(tl))); tl = null; continue;
        };
    };
    output("]");
    return xs.join('');
};

function render(v) {
    if (v.tag == "Request") { return v.cmd; };
    v = v.value;
    var xs = [];
    var i = 0;
    var stk = Cons(v,null);
    function output(str) {
        xs[i] = str;
        i++;
    };
    while (stk != null) {
        v = stk.hd;
        stk = stk.tl;
        switch (v.tag) {
        case "Atom":
            output("'".concat(v.atom)); continue;
        case "Nil" :
            output("[]"); continue;
        case "Cell": output("[");output(renderList(v.fst,v.snd)); continue;
        case "Lit":
            if (stringy(v.literal)) {
                output("\"");
                output(v.literal);
                output("\"");
                continue; };
            if (boolean(v.literal)) {
                if (v.literal) output("'1"); else output("'0");
                continue;
            };
            let n = v.literal.num; d = v.literal.den;
            if (d == 1) { output (n.toString()); continue; };
            if (d == 2) {
                output((Math.trunc(n/2)).toString());
                output(".5")
                continue;
            };
            if (d == 4) {
                output((Math.trunc(n/4)).toString());
                if (n % 4 == 1) { output(".25"); } else { output(".75"); };
                continue;
            };
            // otherwise
            output (n.toString());
            output("/");
            output(d.toString());
            continue;
        default: continue;
        };
    };
    return xs.join('');
};
