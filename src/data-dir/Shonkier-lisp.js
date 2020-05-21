/****************************************************************************/
/** machine                                                                **/
/****************************************************************************/

function shonkier(state) {

    var x y; // dogsbody
    
    // setting up the machine stack
    
    var lox = [];  // list of local nonhandlers
    var hox = [];  // list of handlers with their local nonhandlers

    var fr = []; // frame just popped

    function done() {
        return (nilly(lox) && nilly(hox))
    };
    
    function push(f) {
        if (isHandler(f)) {
            hox = [[f, lox] ,hox];
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

    function cont(k) {
        lox = cat(k[0], lox);
        k = k[1];
        while (k.length > 1) {
            hox = [[k[0], lox], hox];
            lox = k[0][1];
            k = k[1];
        };
    };

    // main machine loop
    
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
                state = use([x.u, state.val]);
                continue;
            case "AppL": x  = grab(fr[1], ["rho", "as"]);
                if (state.val === false) { abort(); };
                if (state.val === true || nilly(state.val)) {
                    if (celly(x.as)) {
                        state = eval(state.env, x.as[0]);
                        continue;
                    };
                    state = complain("Unary", []);
                    continue;
                };
                if (atomic(state.val)) {
                    state = app(state.val, [], state.env, handily([],x.as));
                    continue;
                };
                switch (state.val[0]) {
                case "Prim": y = grab(state.val[1], ["p", "hss"]);
                    state = app(["Prim", [y.p, []]], [], state.env, handily(y.hss,x.as));
                    continue;
                case "Fun": y = grab(state.val[1], ["k", "sig", "hss", "cs"]);
                    cont(y.k);
                    state = app(["Fun", [y.sig, [y.cs, []]]], [], state.env, handily(y.hss, x.as));
                    continue;
                case "Thunk": y = grab(state.val[1], ["c"]);
                    if (celly(c) && c[0] === "Request") {
                        // ["Request", [[cmd, args], [kont, []]]]
                        state = handle(c[1][0][0], c[1][0][1], c[1][1][0]);
                        continue;
                    };
                    state = use(c);
                    continue;
                case "Env": y = grab(state.val[1], ["rho"]);
                    if (celly(x.as)) {
                        state = eval([y.rho, state.env], x.as[0]);
                        continue;
                    };
                    state = complain("Unary", []);
                    continue;
                };
                continue;
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
            case "Cell": x = grab(t[1], ["a","b"]);
                push(CellL(state.env, x.b));
                state = eval(state.env, x.a);
                continue;
            case "App" : x = grab(t[1], ["f","as"]);
                push(AppL(state.env, x.as));
                state = eval(state.env, x.f);
                continue;
            case "Semi" : x = grab(t[1], ["l","r"]);
                push(AppL(state.env, [x.r, []]));
                state = eval(state.env, x.l);
                continue;
            };
            continue;
        case "app":
            if (celly(state.has)) {
                push(AppR(state.funy, state.vz, [has[0][0], state.env], has[1]));
                state = eval(state.env, has[0][1]);
                continue;
            };
            y = destrevapp(state.vz, []);
            if (atomic(state.funy)) {
                state = request(state.funy, y);
                continue;
            };
            switch (state.funy[0]) {
            case "Prim": x = grab(state.funy[1], ["p"]);
                state = prim[x.p](y);
                continue;
            case "Fun": x = grab(state.funy[1], ["sig", "cs"]);
                state = call(x.sig, x.cs, y);
                continue;
            };
            continue;
        };
    };
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

function handle(c, as, k) {
    return { halting: true,
             tag: "handle",
             cmd: a,
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
             vz: cz,
             has: has
           };
};

function call(rho, cs, as) {
    return { halting: false,
             tag: "call",
             clauses: cs,
             args: as
           };
};

// derived states

function request(a, vs) { return handle(a, vs, [[],[]]); };
function abort(); { return request("abort", []); };
var complain = request;


/****************************************************************************/
/** context frames                                                         **/
/****************************************************************************/

function CellL(rho, t) { return ["CellL",[rho,[t,[]]]]; };

function CellR(v, rho) { return ["CellR",[v,[rho,[]]]]; };

function AppL(rho,as) { return ["AppL",[rho,[as,[]]]]; };


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
    var xz = [];
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
function celly(x) { return (hasLength(x) && x.length > 1); };
    
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
    var haz = [];
    var h;
    while (celly(as)) {
        if (celly(hss)) { h = hss[0]; hss = hss[1]; } else { h = []; };
        haz = [[h,as[0]], haz];
        as = as[1];
    };
    return destrevapp(haz, []);
};
