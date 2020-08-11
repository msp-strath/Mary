/****************************************************************************/
/** machine                                                                **/
/****************************************************************************/

function shonkier(glob, inputs, state) {
    // inputs currently ignored; shouldn't be here anyway

    var x; // dogsbody
    var y; // dogsbody
    // setting up the machine stack
    var lox = Nil;  // list of local nonhandlers
    var hox = Nil;  // list of handlers with their local nonhandlers

    /*
    lox looks like Nil or [frame, lox]
    hox looks like Nil or [[handlerframe, lox], hox]
    */

    var fr; // frame just popped

    function done() {
        return (nilly(lox) && nilly(hox))
    };
    function push(f) {
        if (isHandler(f)) {
            hox = [[f, lox], hox];
            lox = [];
        } else {
            lox = [f,lox];
        };
    };

    function pop() {
        if (nilly(lox)) {
            fr   = hox[0][0];
            lox  = hox[0][1];
            hox  = hox[1];
            return;
        };
        fr  = lox[0];
        lox = lox[1];
    };

    /*
    a continuation, k looks like
    [lox, k'] where
    k' is either Nil or [[handlerframe, lox], k']
    */
    function cont(k) {
        lox = cat(k[0], lox);
        k = k[1];   // k is now a k'
        while (k.length > 1) {
            hox = [[k[0][0], lox], hox];
            lox = k[0][1];
            k = k[1];
        };
    };

    // main machine loop

    machine:
    while (!state.halting || !done()) {
        switch (state.tag) {
        case "use":
            pop();
            switch (fr[0]) {
            case "CellL": x = grab(fr[1], ["rho", "b"]);
                push(CellR(state.val, x.rho));
                state = eval(x.rho, x.b);
                continue;
            case "CellR": x = grab(fr[1], ["u", "rho"]);
                state = use(Cell(x.u, state.val));
                continue;
            case "AppL": x = grab(fr[1], ["rho", "as"]);
                if (state.val === false) { state = abort(); continue; };
                if (state.val === true || nilly(state.val)) {
                    if (celly(x.as)) {
                        state = eval(x.rho, x.as[0]);
                        continue;
                    };
                    state = complain("Unary", Nil);
                    continue;
                };
                if (atomic(state.val)) {
                    state = app(state.val, Nil, x.rho, handily(Nil,x.as));
                    continue;
                };
                switch (state.val[0]) {
                case "Prim": y = grab(state.val[1], ["p", "hss"]);
                    state = app(["Prim", [y.p, Nil]], Nil, x.rho, handily(y.hss,x.as));
                    continue;
                case "Fun": y = grab(state.val[1], ["k", "sig", "hss", "cs"]);
                    cont(y.k);
                    state = app(["Fun", [y.sig, [y.cs, Nil]]], Nil, x.rho, handily(y.hss, x.as));
                    continue;
                case "Thunk": y = grab(state.val[1], ["c"]);
                    if (celly(c) && c[0] === "Request") {
                        // ["Request", [[cmd, args], [kont, Nil]]]
                        state = handle(c[1][0][0], c[1][0][1], c[1][1][0]);
                        continue;
                    };
                    state = use(c);
                    continue;
                case "Cell":
                    if (celly(x.as)) {
                        state = eval([val2env(state.val), x.rho], x.as[0]);
                        continue;
                    };
                    state = complain("Unary", Nil);
                    continue;
                case "Env": y = grab(state.val[1], ["rho"]);
                    if (celly(x.as)) {
                        state = eval([y.rho, x.rho], x.as[0]);
                        continue;
                    };
                    state = complain("Unary", Nil);
                    continue;
                };
                continue;
            case "AppR": x = grab(fr[1], ["f","cz","hsrho","has"]);
                state = app(x.f, [state.val, x.cz], x.hsrho[1], x.has);
                continue;
            // "Clauses" dismounts when we use a value
            // "PrioL" dismounts when we use a value
            // "Masking" dismounts when we use a value
            };
            continue;
        case "eval":
            var t = state.trm;
            if (!celly(t)) {
                // t is an atom, nil, or a literal, and thus its own value
                state = use(t);
                continue;
            };
            // t is a cell whose head is a tag
            switch (t[0]) {
            case "Var": x = grab(t[1], ["v"]);
                if (atomic(x.v)) { t = x.v; } else { t = x.v[0]; };
                if (dynVar(x.v)) {
                    y = fetch(state.env, t);
                    if (y !== null) {
                        state = use(y);
                        continue;
                    };
                };
                if (celly(x.v) && celly(x.v[1]) && boolean(x.v[1][1])) {
                    y = glob[t][strung(x.v[1][0])];
                    if (y !== undefined) {
                        state = use(y);
                        continue;
                    };
                };
                state = complain("OutOfScope", [t, Nil]);
                continue;
            case "Cell": x = grab(t[1], ["a","b"]);
                push(CellL(state.env, x.b));
                state = eval(state.env, x.a);
                continue;
            case "App": x = {f: t[1][0], as: t[1][1]};
                push(AppL(state.env, x.as));
                state = eval(state.env, x.f);
                continue;
            case "Semi": x = grab(t[1], ["l", "r"]);
                push(AppL(state.env, [x.r, Nil]));
                state = eval(state.env, x.l);
                continue;
            case "Prio": x = grab(t[1], ["l", "r"]);
                push(PrioL(state.env, x.r));
                state = eval(state.env, x.l);
                continue;
            case "Mask": x = grab(t[1], ["a", "t"]);
                push(Masking(x.a));
                state = eval(state.env, x.t);
                continue;
            case "Fun":
                state = use(["Fun", [anIdCont, [state.env, [t[1][0], [t[1][1], []]]]]]);
                continue;
            };
            continue;
        case "app":
            if (celly(state.has)) {
                push(AppR(state.funy, state.cz, [state.has[0][0], state.env], state.has[1]));
                state = eval(state.env, state.has[0][1]);
                continue;
            };
            y = destrevapp(state.cz, Nil);
            if (atomic(state.funy)) {
                state = request(state.funy, y);
                continue;
            };
            switch (state.funy[0]) {
            case "Prim": x = grab(state.funy[1], ["p"]);
                state = shonkierPrimitive[x.p](y);
                continue;
            case "Fun": x = grab(state.funy[1], ["sig", "cs"]);
                state = call(x.sig, x.cs, y);
                continue;
            };
            continue;
        case "call":
            x = state.clauses;
            while (celly(x)) {
                y = x[0]; x = x[1];
                var sig = matches(y[0], state.args);
                if (sig === null) { continue; }
                if (celly(x)) { push(Clauses(state.env,x,state.args)); };
                state = eval([sig, state.env], rhs2Term(y[1]));
                continue machine;
            };
            state = abort();
            continue;
        case "handle":
            state.kont[0] = cat(state.kont[0],lox); lox = Nil;
            if (nilly(hox)) { break machine; }
            fr = hox[0][0]; lox = hox[0][1]; hox = hox[1];
            switch (fr[0]) {
            case "AppR": x = grab(fr[1],["f","cz","hsrho","has"]);
                if (atomIn(state.cmd,x.hsrho[0])) {
                    if (state.inx == 0) {
                        state = app(x.f,[["Request",[[state.cmd,state.args],[state.kont,Nil]]],x.cz],x.hsrho[1],x.has);
                        continue;
                    };
                    state.inx--;
                };
                break;
            case "Clauses": if (state.cmd != "abort") { break; };
                x = grab(fr[1],["rho", "cs", "as"]);
                state = call(x.rho,x.cs,x.as);
                continue;
            case "PrioL": if (state.cmd != "abort") { break; };
                x = grab(fr[1],["rho", "r"]);
                state = eval(x.rho, x.r);
                continue;
            case "Masking":
                if (state.cmd == fr[1][0]) { state.inx++; };
                break;
            };
            state.kont[1] = [[fr,state.kont[0]],state.kont[1]];
            state.kont[0] = Nil;
        };
    };

    return state;
};


/****************************************************************************/
/** machine states                                                         **/
/****************************************************************************/

function use(v) {
    return { halting: true,
             tag: "use",
             val: v
           };
};

function handle(c, i, as, k) {
    return { halting: true,
             tag: "handle",
             cmd: c,
             inx: i,
             args: as,
             kont: k
           };
};

function eval(rho, t) {
    return { halting: false,
             tag: "eval",
             env: rho,
             trm: t
           };
};

function app(f, cz, rho, has) {
    return { halting: false,
             tag: "app",
             funy: f,
             cz: cz,
             env: rho,
             has: has
           };
};

function call(rho, cs, as) {
    return { halting: false,
             tag: "call",
             env: rho,
             clauses: cs,
             args: as
           };
};

// derived states

function request(a, vs) { return handle(a, 0, vs, idCont()); };
function abort() { request("abort", Nil); };
var complain = request;


/****************************************************************************/
/** context frames                                                         **/
/****************************************************************************/

function CellL(rho, t) { return ["CellL", [rho, [t, Nil]]]; };

function CellR(v, rho) { return ["CellR", [v, [rho, Nil]]]; };

function AppL(rho, as) { return ["AppL", [rho, [as, Nil]]]; };

function AppR(f, cz, hsrho, has) { return ["AppR", [f, [cz, [hsrho, [has, Nil]]]]]; };

function Clauses(rho, cs, as) { return ["Clauses", [rho, [cs, [as, Nil]]]]; };

function PrioL(rho, r) { return ["PrioL", [rho, [r, Nil]]]; };

function isHandler(fr) {
    var x;
    switch (fr[0]) {
    case "AppR":
        x = grab(fr[1], ["f", "cz", "hsrho", "hsts"]);
        return celly(x.hsrho[0]);
    case "Clauses":
    case "PrioL":
        return true;
    };
    return false;
};

function maskAbortDrop(fr) {
    switch (fr[0]) {
    case "Clauses":
    case "PrioL":
        return true;
    };
    return false;
};


/****************************************************************************/
/** value constructors                                                     **/
/****************************************************************************/

var Nil = [];

function Cell(a, d) {
    return ["Cell", [a, [d, Nil]]];
};


/****************************************************************************/
/** continuations                                                          **/
/****************************************************************************/

function idCont() { return [Nil, Nil] };
var anIdCont = idCont();

/****************************************************************************/
/** value to environment                                                   **/
/****************************************************************************/

// I've eliminated the recursion from the following, in favour of an
// explicit stack. It could run a lot flatter if we indulged in fun with
// overwriting tail pointers.

function val2env(v) {
    var x;
    var flag = false;  // true if v is an env coming back
    var stk = [];
    var i = 0;
    while (i > 0 || !flag) {
        if (flag) {
            i--;
            if (stk[i].op) {
                v = [v, stk[i].en];
                continue;
            };
            x = stk[i].va;
            stk[i++] = {op: true, en: v};
            v = x; flag = false;
            continue;
        };
        if (celly(v) && v[0]==="Cell") {
            v = v[1];
            if (celly(v) && celly(v[1])) {
                x = v[0];
                v = v[1][0];
                if (atomic(x)) {
                    v = [x, v]; flag = true;
                    continue;
                };
                stk[i++] = {op: false, va: x};
                continue;
            };
        };
        v = Nil; flag = true;
        continue;
    };
    return v;
};


/****************************************************************************/
/** pattern matching                                                       **/
/****************************************************************************/

function matches(ps, cs) {
    var rho = Nil;
    var sig;
    while (celly(ps)) {
        if (!celly(cs)) { return null; };
        sig = match(ps[0], cs[0]);
        ps = ps[1]; cs = cs[1];
        if (sig === null) { return null; };
        if (celly(sig)) { rho = [sig, rho]; };
    };
    if (nilly(cs)) { return rho; }
    return null;
};

function match(p, c) {
    var rho = [];
    var ps = [p];
    var cs = [c];
    var i = 1;
    while (i-- > 0) {
        p = ps[i]; c = cs[i];
        if (nilly(p)) {
            if (nilly(c)) { continue; };
            return null;
        };
        if (atomic(p)) {
            if (p === c) { continue; };
            return null;
        };
        // literals?
        if (!celly(p)) { return null; }
        switch (p[0]) {
        case "Bind":
            if (compIsVal(c)) { rho = [[p[1][0], c], rho]; continue; };
            return null;
        case "Wild":
            if (compIsVal(c)) { continue; };
            return null;
        case "Cell":
            p = grab(p[1], ["a","d"]);
            if (celly(c) && c[0]==="Cell") {
                c = grab(c[1], ["a","d"]);
            } else {
                return null;
            };
            ps[i] = p.d; cs[i] = c.d; i++;
            ps[i] = p.a; cs[i] = c.a; i++;
            continue;
        case "Thunk":
            rho = [[p[1][0], ["Thunk", [c, Nil]]],rho];
            continue;
        case "Request": // ["Request", [[cmd,args], [kont, Nil]]]
            if (   !celly(c)
                   || c[0]!=="Request"
                   || !celly(c[1]) || !celly(c[1][0]) || !celly(c[1][1])
                   || p[1][0][0] !== c[1][0][0] // check cmds
               ) {
                return null;
            };
            // pattern need not bind the kont; if it does, let's have it
            if (celly(p[1][1])) {
                rho = [[p[1][1][0], ["Fun", [c[1][1][0], idClause]]]
                       , rho];
            };
            // not strictly left-to-right: problem?
            // now set up the args
            p = p[1][0][1]; c = c[1][0][1];
            while (celly(p)) { // similarly, should I try harder to push them backwards?
                if (!celly(c)) { return null; };
                ps[i] = p[0]; cs[i] = c[0]; i++;
                p = p[1]; c = c[1];
            };
            continue;
        };
        return null;
    };
    return rho;
};


/****************************************************************************/
/** utils                                                                  **/
/****************************************************************************/

function destrevapp(xz,ys) {
    while (celly(xz)) {
        var tmp = xz[1];
        xz[1]   = ys;
        ys      = xz;
        xz      = tmp;
    };
    return ys;
};

function cat(xs, ys) {
    if (!celly(ys)) { return xs; };
    var xz = Nil;
    while (celly(xs)) {
        xz = [xs[0], xz];
        xs = xs[1];
    };
    return destrevapp(xz, ys);
};

function grab(c, xs) {
    var i = 0;
    var v = {};
    while (i < xs.length) {
        v[xs[i]] = c[0];
        c = c[1];
        i++;
    };
    v.residue = c;
    return v;
};

function hasLength(x) {
    return (!(x === null || x === undefined || x.length === undefined));
};

function atomic(x) { return (typeof(x)=="string"); };
function boolean(x) { return (typeof(x)=="boolean"); };
function nilly(x) { return (hasLength(x) && x.length == 0); };
function celly(x) { return (typeof(x)=="object" && hasLength(x) && x.length > 1); };
function strung(x) {
    if (x.str === undefined) { return null; }
    return x.str;
};

function fetch(e, x) {
    var stk = [e];
    var i = 1;
    while (i > 0) {
        i--;
        if (!hasLength(stk[i]) || stk[i].length < 2) { continue; };
        var a = stk[i][0];
        var d = stk[i][1];
        if (a === x)    { return d; };
        if (atomic(a)) { continue; };
        stk[i] = d; i++;
        stk[i] = a; i++;
    };
    return null;
};

function handily(hss, as) {
    var haz = Nil;
    var h;
    while (celly(as)) {
        if (celly(hss)) { h = hss[0]; hss = hss[1]; } else { h = Nil; };
        haz = [[h,as[0]], haz];
        as = as[1];
    };
    return destrevapp(haz, Nil);
};

function rhs2Term(rs) {
    var tz = Nil;
    var r;
    while (celly(rs)) {
        r = rs[0]; rs = rs[1];
        if (celly(r[1])) {
            tz = [['App',[r[1][0], [r[0], []]]], tz];
        } else {
            tz = [r[0], tz];
        };
    };
    if (nilly(tz)) { return ['App',['abort',[]]]; };
    r = tz[0]; tz = tz[1];
    while (celly(tz)) {
        r = ['Prio', [tz[0], [r, []]]];
        tz = tz[1];
    };
    return r;
};

function compIsVal(c) {
    if (!celly(c)) { return true; }
    if (!atomic(c[0])) { return true; }
    switch (c[0]) {
    case "Request":
        return false;
    };
    return true;
};

function atomIn(a, t) {
    var ts = [t];
    var i = 1;
    while (i-- > 0) {
        t = ts[i];
        if (a === t) { return true; }
        if (celly(t)) {
            ts[i++] = t[1];
            ts[i++] = t[0];
        };
    };
    return false;
};

function dynVar(v) {
    return !(celly(v) && celly(v[1]) && boolean(v[1][1]) && v[1][1]);
};

// the following is used to turn a continuation into a function
var idClause = [Nil,[Nil,[[[[['Bind',['_return',Nil]],Nil],[[['Var',['_return',Nil]],Nil],Nil]],Nil],Nil]]];


/****************************************************************************/
/** shonkier primitives                                                    **/
/****************************************************************************/

var shonkierPrimitive =
    { primStringConcat: function (v) {
        var acc = "";
        var stk = [v];
        var i = 1;
        while (i-- > 0) {
            v = stk[i];
            if (celly(v)) {
                if (!atomic(v[0])) {
                    stk[i++] = v[1];
                    stk[i++] = v[0];
                    continue;
                };
                if (v[0]==="Cell") {
                    stk[i++] = v[1];
                    continue;
                };
                return complain("Invalid_StringConcat_ArgType", Nil);
            };
            if (nilly(v) || atomic(v)) { continue; };
            if (v.str !== undefined) {
                acc = acc.concat(v.str); // do some work!
                continue;
            };
            return complain("Invalid_StringConcat_ArgType", Nil);
        };
        return use({str: acc});
    }
    };




/****************************************************************************/
/** recursive partial uglyprinters, for diagnostic purposes                **/
/****************************************************************************/

function crapcdr(v) {
    if (nilly(v)) { return "]" };
    if (celly(v)) {
        var x = " ";
        x = x.concat(crapprint(v[0]));
        return x.concat(crapcdr(v[1]));
    };
    var x = "|";
    x = x.concat(crapprint(v));
    return x.concat("]");
};

function crapprint(v) {
    if (nilly(v)) { return "[]"; };
    if (atomic(v)) { var x = "'"; return x.concat(v); };
    if (celly(v)) {
        var x = "[";
        x = x.concat(crapprint(v[0]));
        return x.concat(crapcdr(v[1]));
    };
    if (v.den !== undefined) {
        var x = v.num.toString();
        if (v.den == 1) { return x; };
        x = x.concat("/").concat(v.den.toString());
        return x;
    };
    if (v.str !== undefined) {
        var x = '"'; // no attempt to escape things
        x = x.concat(v.str).concat('"');
        return x;
    };
    return "";
};

function render(state) {
    switch (state.tag) {
    case "use": return crapprint(state.val);
    case "handle": return state.cmd.concat(" unhandled! ")/*.concat(crapprint(state.kont))*/;
    };
};
