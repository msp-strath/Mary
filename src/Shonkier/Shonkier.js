/****************************************************************************/
/*****                                                                   ****/
/*****     from Syntax.hs                                                ****/
/*****                                                                   ****/
/****************************************************************************/


// variables are strings, environments are objects

// Constructors for Term, Pattern, Value

function Atom(a) { // Atom a
    return {tag: "Atom", atom: a};
};                 
function Lit(l) { // Lit l
    return {tag: "Lit", literal: l};
};               
function Cell(x,y) { // Cell x y
    return {tag: "Cell", fst: x, snd: y};
};         

// Term only
function App(f, as) {       // App f as, where as is an array
    return {tag: "App", fun: f, args: as};
};  
function Fun(hs, cs) { // Fun hs cs, both arrays
    return {tag: "Fun", handles: hs, clauses: cs};
};

// PValue only
const PWild = {tag: "Wild"};

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

// is command c handled in position i by handlers hs?
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

/* Matchers on success grow an environment, return a Boolean */

function stringy(x) { return (typeof(x)=="string"); };

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
                rho[q.cont] = c.cont; return true;
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
    if (p.tag == v.tag) {
        switch (p.tag) {
        case "Atom" :
            return (p.atom == v.atom);
        case "Cell" :
            if (vmatch(rho,p.fst,v.fst)) {
                return (vmatch(rho,p.snd,v.snd));
            };
            return false;
        case "Lit" :
            return LitEq(p.lit,v.lit);
        };
    };
    return false;
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
function AppR(f,cz,rho,i,hs,ts) { // carefully engineered pun with Apply
    return {tag: 3, fun: f, done: cz, env: rho, now: i, handles: hs, args: ts};
};

// State
// pop states, ie those which pop ctx until done
function Use(v) {
    return {tag: 0, val: v};
};
function Handle(c,as,k) {
    return {tag: 1, cmd: c, args: as, cont: k};
};
// push states, ie those which grow ctx
function Eval(rho, t) {
    return {tag: 2, env: rho, term: t};
};
function Apply(f,cz,rho,i,hs,ts) { // carefully engineered pun with AppR
    return {tag: 3, fun: f, done: cz, env: rho, now: i, handles: hs, args: ts};
};
function Call(rho,cs,as) {
    return {tag: 4, env: rho, clauses: cs, args: as};
};


// Continuations made by null and
function Cons(h,t) {
    return {hd: h, tl: t};
};

function shonkier(glob,t) {
    var ctx = null;
    var fr = null;
    function push(f) { ctx = {top: f, pop:ctx}; };
    function pop() { fr = ctx.top; ctx = ctx.pop; };
    var state = Eval({},t);
    while (state.tag > 1 || ctx != null) // done when ctx is null in a pop state
    {
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
                state = Apply(state.val,null,fr.env,0,fhandles(state.val),fr.args);
                continue;
            case 3: // AppR
                state = Apply(fr.fun
                              ,Cons(Value(state.val),fr.done) // stash value
                              ,fr.env
                              ,fr.now+1 // move yer finger
                              ,fr.handles,fr.args);
                continue;
            };
            state = Handle("BadFrame",[],null);
            continue;
        case 1: // Handle
            pop();
            if (fr.tag == 3
                && inhandles(state.cmd,fr.now,fr.handles)
               ) {
                state = Apply(fr.fun
                              ,Cons(Request(state.cmd,state.args,state.cont),fr.done)
                              ,fr.env
                              ,fr.now+1
                              ,fr.handles
                              ,fr.args);
                continue;
            };
            state = Handle(state.cmd, state.args, Cons(fr,state.cont));
            continue;
        case 2: // Eval
            var t = state.term;
            if (stringy(t)) {
                var v = state.env[t];
                if (v != undefined) {
                    state = Use(v);
                    continue;
                };
                v = glob[t];
                if (v != undefined) {
                    state = Use(v);
                    continue;
                };
                state = Handle("OutOfScope",[],null);
                continue;
            };
            switch (t.tag) {
            case "Lit":
                state = Use(t);
                continue;
            case "Atom":
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
            case "Fun":
                state = Use(VFun(null,state.env,t.handles,t.clauses));
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
            var d = state.done
            var i = state.now;
            while (i > 0) {
                i--;
                args[i] = d.hd;
                d = d.tl;
            };
            switch (state.fun.tag) {
            case "Atom":
                for (i = 0; i < args.length; i++) { args[i] = args[i].val; }
                state = Handle(state.fun.atom,args,null);
                continue;
            case "VFun":
                d = state.fun.cont;
                while (d != null) { push(d.hd); d = d.tl; }
                state = Call(state.fun.env,state.fun.clauses,args);
                continue;
            case "VThunk":
                d = state.fun.thunk;
                switch (d.tag) {
                case "Value":
                    state = Use(d.value);
                    continue;
                case "Request":
                    state = Handle(d.cmd,d.args,d.cont);
                    continue;
                };
            };
            state = Handle("NotFuny",[],null);
            continue;
        case 4: // Call
            var cs = state.clauses;
            var i = 0;
            while (i < cs.length) {
                var rho = Object.assign({}, state.env);
                if (cmatches(rho,cs[i].pats,args)) {
                    state = Eval(rho,cs[i].term);
                    break;
                };
                i++;
            };
            if (i == cs.length) { state = Handle("NoMatch",[],null); };
            continue;
        };
    };
    if (state.tag==0) { return Value(state.val); }
    if (state.tag==1) { return Request(state.cmd,state.args,state.cont); }
    return null;
};

function render(v) {
    if (v.tag == "Request") { return v.cmd; };
    v = v.value;
    var xs = [];
    var i = 0;
    var stk = Cons(v,null);
    while (stk != null) {
        v = stk.hd;
        stk = stk.tl;
        switch (v.tag) {
        case "Atom": xs[i] = "'".concat(v.atom); i++; continue;
        case "Cell": stk = Cons (v.fst, Cons(v.snd, stk)); continue;
        case "Lit":
            if (stringy(v.literal)) {
                xs[i] = v.literal; i++;
                continue;
            };
            xs[i] = v.literal.num.toString(); i++;
            if (v.literal.den == 1) { continue; };
            xs[i] = "/"; i++;
            xs[i] = v.literal.den.toString(); i++;
            continue;
        default: continue;
        };
    };
    return xs.join();
};
