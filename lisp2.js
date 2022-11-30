// ********************
// * THIS IS LISP2.JS *
// ********************

// Lisp interperter in Javascript by M. Ishikawa
// ISL Inc. Toyohashi JAPAN 2020-2022

// *** GLOBAL VAR
var _G = new function() {
    // ********************************************************************************
    // ### class ExprBuff
    this.ExprBuff = function(predef_buff) {
	this.init_str = predef_buff;
	this.buff = "";
	this.load();
    }
    this.ExprBuff.prototype = {
	// init_str: __index__, // 初期化ファイルより生成される変数
	load: function() {
	    _G.lexer.init(this.init_str);
	    _G.reader.init(_G.lexer);
	    while(true) {
		let exp = _G.reader.parse(null);
		if(! exp) break;
		_G.eval.eval_sex(exp, _G.top_env);
	    }
	    console.log("Predef-buff has been consumed.");
	},

	addData: function(data) {
	    // REPLの１サイクル
	    this.buff += data;
	    // フロントエンドにEmacsを使う前提. Emacsは必ずレジョンの終わりにchar(0)を送ってくるので
	    // char(0)が見つかるまでパースを開始しない. これで今のところよしとする.
	    var idx = this.buff.indexOf(String.fromCharCode(0));
	    if(idx != -1) { // char(0)を見つけたのでパース開始
		var emacs_buff = this.buff.substr(0,idx) + "\n";
		this.buff = this.buff.slice(idx+3); // terminator + CR + LF分を除去
		// console.log(emacs_buff);

		_G.lexer.init(emacs_buff);
		_G.reader.init(_G.lexer);

		var exp = null;
		while(true) {
		    try { // REPL1回分
			var stack_trace = [];
			exp = _G.reader.parse(null);
			if(! exp) break; // throw "reader fails, immature input";
			var env = _G.cons(_G.cons(_G.sym_stack_trace, stack_trace), _G.top_env); // stack traceのための変数をセット
			var result = _G.eval.eval_sex(exp, env);
			var result_str = _G.printer.print_sex(result);
			// console.log(result); // top-level式の評価結果をブラウザのconsoleにも送る
			sock_send(result_str + '\n');
			_G.X = result;
		    }
		    catch(e) {
			console.error("***TOP-LEVEL> ");
			console.error(e);
			console.error(stack_trace);

			sock_send("\n");
			for(let i=stack_trace.length-1; i >= 0; i--) {
			    sock_send("[" + i + "] " + stack_trace[i].slice(0,_G.max_length_err) + "\n");
			}
			sock_send("***TOP-LEVEL> " + e + "\n\n");
		    }
		} // while
		sock_send(_G.pwp(null,null) + "=> "); // send package prompt
	    } // if
	}, // addData
    }
    
    // ********************************************************************************    
    // ### class DbConn
    this.DbConn = function(id_str, hashed_pass) {
	this.id_str = id_str;
	this.hashed_pass = hashed_pass;
	this.p = new Promise(function(resolve, reject) { resolve(0); }); // 即成功するpromise
    }

    // ********************************************************************************        
    // ### class LispException
    var LispException = function(message, expr=null) {
	this.message = message;
	this.expr = expr;
    }

    // ********************************************************************************        
    // ### class Cons
    var Cons = function(car, cdr) {
	this.car = car;
	this.cdr = cdr;
    }
    Cons.prototype = {
	toString: function() { return "Cons"; },
    }
    
    // ********************************************************************************        
    // ### class Token
    var Token = function() {
	this.k = null; // token kind, look at _G.tk_xxx
	this.o = null;
    }

    // ********************************************************************************    
    // ### class Package
    var Package = function(p=null) {
	this.hash = {};
	this.parent = p;
    }
    Package.prototype = {
	// 追加する時は、自分のhashに入れる
	add: function(s,v) {
	    this.hash[s.name] = v;
	},

	// 探す時はparetハッシュまで探す
	get: function(s, setq_f=false) {
	    if(s.name in this.hash) return this.hash[s.name];
	    else if(setq_f) return _G.nil; // setq binds in this package only, not in the parent
	    else if(this.parent != null) return this.parent.get(s);
	    else return _G.nil;
	},
	
	toString: function() { return "Package"; },
    }

    // ********************************************************************************    
    // ### class Sym
    var Sym = function(name) {
	this.name = name;
    }
    Sym.prototype = {
	toString: function() {
	    return "'" + this.name;
	}
    }

    // ********************************************************************************    
    // ### class SymTab
    var SymTab = function() {
	this.tab = {}
    }
    SymTab.prototype = {
	intern: function(name) {
	    if(name in this.tab)
		return this.tab[name];
	    else {
		this.tab[name] = new Sym(name);
		return this.tab[name];
	    }
	},
	overwrite: function(s) {
	    this.tab[s.name] = s;
	},
    }

    // ********************************************************************************    
    // ### class Clj
    var Clj = function(expr,env) {
	this.expr = expr;
	this.env = env;
    }

    // ********************************************************************************
    // ### class Lexer
    var Lexer = function() {
	this.INPUT = null;
	this.state = null;
	this.buff = null;
	this.tk = new Token();
    }
    Lexer.prototype = {
	init: function(single_expr_str) {
	    this.INPUT = single_expr_str; // Array.from(single_expr_str);
	    // this.INPUT_len = this.INPUT.length;
	    this.INPUT_ptr = 0;

	    this.state = 0;
	    this.buff = "";
	    return null;
	},
	getc: function() {
	    // if(this.INPUT_ptr < this.INPUT_len)
	    //    return this.INPUT[this.INPUT_ptr++];
	    if(this.INPUT_ptr < this.INPUT.length)
		return this.INPUT.charAt(this.INPUT_ptr++);
	    else return null;
	},
	pushback: function(c) {
	    /*
	      if(c == this.INPUT[this.INPUT_ptr-1])
	      this.INPUT_ptr--;
	    */
	    if(this.INPUT_ptr > 0 && c == this.INPUT.charAt(this.INPUT_ptr-1))
		this.INPUT_ptr--;
	    else
		throw "Lexer: can't push back";
	},
	is_digit: function(c) {
	    return c != null && '0' <= c && c <= '9'; // null is taken as '0'
	},
	is_alpha: function(c) {
	    if('a' <= c && c <= 'z') return true;
	    if('A' <= c && c <= 'Z') return true;
	    return "#!$%&*+-/<=>?@~:_[]".indexOf(c) >= 0;
	},
	is_ws: function(c) {
	    return " \t\n\r\000".indexOf(c) >= 0;
	},
	is_delim: function(c) {
	    return "()\'.`,".indexOf(c) >= 0;
	},
	get_token: function() {
	    while(true) {
		var c = this.getc();

		switch(this.state) {
		    // 文字列"~...." では\"と\'と\nだけエスケープできる
		    // さらに''(2つの連続したシングルクオート)で"(ダブルクオート1つ)の意味になる
		    // Emacsからみて一つのlispオブジェクトにしたいので"の意味を大きく変えられない
		case 100: // seen "
		    if(c == '~')
			this.state = 101;
		    else {
			this.state = 3;
			if(c != null) this.pushback(c);
		    }
		    break;
		case 101: // seen "~
		    if(c == '\\') // seen "~...\
			this.state = 102;
		    else if(c == "'")
			this.state = 103;
		    else if(c == '\"') { // reached the end
			this.state = 0;
			this.tk.o = this.buff;
			this.tk.k = _G.tk_object;
			return this.tk;
		    }
		    else
			this.buff += c;
		    break;
		case 102: // seen \ after "~
		    if(c == '\"' || c == "'")
			this.buff += c;
		    // else if(c == 'n')
		    //     this.buff += "\n";		    
		    else
			this.buff += '\\' + c;
		    this.state = 101;
		    break;
		case 103: // seen '
		    if(c == "'")
			this.buff += '"';
		    else
			this.buff += "'" + c;
		    this.state = 101;
		    break;

		case 0:
		    if(c == null) {
			this.state = 0;
			return null;
		    }
		    // 割り算の/と区別がつかないのでうまくいかない
		    // else if(c == '/') { // 正規表現
		    //     this.buff = "";
		    //     this.state = 500;
		    // }
		    else if(c == '\"') {
			this.buff = "";
			this.state = 100;
		    }
		    else if(this.is_ws(c)) {
			// skip a white space
		    }
		    else if(this.is_delim(c)) {
			if(c == '.') {
			    var c1 = this.getc();
			    if(this.is_digit(c1)) {
				this.buff = c + c1;
				this.state = 1;
				break;
			    }
			    this.pushback(c1);
			}
			else if(c == '\'') {
			    var c1 = this.getc();
			    var c2 = this.getc();
			    if(c2 == '\'') {
				this.tk.o = c1;
				this.tk.k = _G.tk_object;
				return this.tk; // char like 'a'
			    }
			    else {
				this.pushback(c2);
				this.pushback(c1);
			    }
			}
			else if(c == ',') {
			    var c1 = this.getc();
			    if(c1 == '@') {
				this.tk.o = _G.sym_commaat;
				this.tk.k = _G.tk_symbol;
				return this.tk;
			    }
			    else this.pushback(c1);
			}

			this.tk.o = c;
			this.tk.k = _G.tk_delim;
			return this.tk;
		    }
		    else if(this.is_digit(c) || this.is_alpha(c)) {
			this.buff = c;
			this.state = 1;
		    }
		    else if(c == ';') {
			this.state = 2;
		    }
		    else {
			this.state = 0;
			console.log(this.INPUT.substring(this.INPUT_ptr-30, this.INPUT_ptr+30));
			throw "Lexer: bogus_input(" + c  + ")";
		    }
		    break; // 0

		    // case 500: // 正規表現
		    // 	if(c != '/') {
		    // 	    this.buff += c;
		    // 	}
		    // 	else if(c == '/') { 
		    // 	    this.tk.o =  new RegExp(this.buff);
		    // 	    this.tk.k = _G.tk_object;
		    // 	    return this.tk;
		    // 	}
		    // 	break;
		    
		case 1: // parsing a symbol or number
		    if(this.is_digit(c) || this.is_alpha(c)) {
			this.buff += c;
		    }
		    else {
			if(c == '.') {
			    var c1 = this.getc();
			    if(this.is_digit(c1)) {
				this.buff += c;
				this.pushback(c1);
				break;
			    }
			    this.pushback(c1);
			}
			this.state = 0;
			this.pushback(c);

			var num = Number(this.buff);
			if(! isNaN(num)) {
			    this.tk.o = num;
			    this.tk.k = _G.tk_object;
			    return this.tk;
			}
			else {
			    this.tk.o = _G.intern(this.buff);
			    this.tk.k = _G.tk_symbol;
			    return this.tk;
			}
		    }
		    break; // 1

		case 2: // in a comment
		    if(c == '\n' || c == '\r') this.state = 0;
		    break;

		case 3: // seen opening "
		    if(c == '\"') {
			this.state = 0;
			this.tk.o = this.buff;
			this.tk.k = _G.tk_object;
			return this.tk;
		    }
		    else if(c == '\\') // escape the next char
			this.state = 4;
		    else
			this.buff += c;
		    break;
		case 4: // backslash escape
		    if(c == 'n') c = '\n';
		    else if(c == 'r') c = '\r';
		    else if(c == 't') c = '\t';
		    this.buff +=  c;
		    this.state = 3;
		    break;

		default:
		    this.state = 0;
		    throw "Lexer: token_panic";
		} // switch
	    } // while
	},
	test: function() {
	    var lex = new Lexer();
	    lex.init("a b 3.3 () \"xxx\" 'f' , ,@ (a . b) 14 $#-1");
	    while(true) {
		var tk = lex.get_token();
		console.log(JSON.stringify(tk));
		if(tk == null) break;
	    }
	}
    }

    // ********************************************************************************
    // ### class Reader
    var Reader = function() {
	this.lexer = null;
    }
    Reader.prototype = {
	init: function(lexer) {
	    this.lexer = lexer;
	},
	parse: function(tk) {
	    if(tk == null) tk = this.lexer.get_token();

	    if(tk == null) return null; // no more tokens

	    if(tk.k == _G.tk_object) return tk.o;

	    else if(tk.k == _G.tk_symbol) {
		if(tk.o.name == "nil") return _G.nil;
		if(tk.o == _G.sym_commaat) {
		    tk = this.lexer.get_token();
		    var a = this.parse(tk);
		    return _G.cons(_G.sym_commaat, _G.cons(a, _G.nil));
		}
		return tk.o;
	    }

	    else if(tk.k == _G.tk_delim && tk.o == '\'') {
		tk = this.lexer.get_token();
		var a = this.parse(tk);
		return _G.cons(_G.sym_quote, _G.cons(a, _G.nil));
	    }

	    else if(tk.k == _G.tk_delim && tk.o == '`') {
		tk = this.lexer.get_token();
		var a = this.parse(tk);
		return _G.cons(_G.sym_bq, _G.cons(a, _G.nil));
	    }

	    else if(tk.k == _G.tk_delim && tk.o == ',') {
		tk = this.lexer.get_token();
		var a = this.parse(tk);
		return _G.cons(_G.sym_comma, _G.cons(a, _G.nil));
	    }

	    else if(tk.k == _G.tk_delim && tk.o == '(') { // start of '('
		tk = this.lexer.get_token();
		if(tk.k == _G.tk_delim && tk.o == ')')
		    return _G.nil;

		// parse the form of '(' expr + ')'
		var car = this.parse(tk);
		var hd = _G.cons(car, _G.nil);
		var tl = hd;

		tk = this.lexer.get_token();
		while(true) {
		    if(tk.k == _G.tk_delim && tk.o == ')')
			return hd;

		    // parsing dot-pair
		    if(tk.k == _G.tk_delim && tk.o == '.') {
			tk = this.lexer.get_token();
			var cdr = this.parse(tk);

			tk = this.lexer.get_token();
			if(tk.k == _G.tk_delim && tk.o == ')') {
			    tl.cdr = cdr;
			    return hd;
			}
			else // dot-pair expected
			    throw "Reader: dot-pair expected";
		    }
		    // parsing dot-pair ends

		    // the list continues
		    car = this.parse(tk); // parse the 2nd, 3rd,... elements
		    tl.cdr = _G.cons(car, _G.nil); // attach at the end
		    tl = tl.cdr; // move the tail pointer

		    tk = this.lexer.get_token();
		} // while untile I see the end of the list; ')'
	    }
	    else { // parse error
		throw "Readeer: parse error";
	    }
	}, // parse
    }
    
    // ********************************************************************************
    function isDOMElement(obj) {
	try {
	    //Using W3 DOM2 (works for FF, Opera and Chrom)
	    return obj instanceof HTMLElement;
	}
	catch(e){
	    //Browsers not supporting W3 DOM2 don't have HTMLElement and
	    //an exception is thrown and we end up here. Testing some
	    //properties that all elements have. (works on IE7)
	    return (typeof obj==="object") &&
		(obj.nodeType===1) && (typeof obj.style === "object") &&
		(typeof obj.ownerDocument ==="object");
	}
    }
    
    // ********************************************************************************
    // ### class Printer
    var Printer = function() {
    }
    Printer.prototype = {
	print_seq_no: 0,
	print_sex: function(expr,debug=false) {
	    var prstr = "";

	    if(_G.atom(expr)) {
		// if(typeof(expr) == "undefined") prstr += "<undefined>";
		if(expr === void 0) prstr += "<js-undefined>";
		else if(expr == null) prstr += "<js-null>";
		else if(typeof(expr) == "string") {
		    let str = expr;
		    str = str.replace(/\\/g, "\\\\");
		    str = str.replace(/\"/g, "\\\"");
		    prstr += "\"" + str + "\"";
		}
		else if(expr instanceof Sym) prstr += expr.name;
		else if(expr instanceof Clj) prstr += "Clj:" + this.print_sex(expr.expr);
		else if(expr == _G.nil) prstr += "nil";
		else if(Array.isArray(expr)) prstr += "[" + expr.toString() + "]"
		else if(expr instanceof jQuery) {
		    prstr += "jQuery-object:";
		    let A = expr.toArray();
		    for(let i=0; i<A.length; i++) {
			prstr += "\n  (" + i + ") " + A[i].outerHTML;
		    }
		    //expr.toArray().forEach(function(i) { prstr += "\n" + i.outerHTML});
		}
		else if(isDOMElement(expr)) prstr += expr.outerHTML;
		else { // other Javascript object including numbers
		    function probj(expr) {
			return JSON.stringify(expr,
					      (function () {
						  //　Object||ArrayならリストにINして循環参照チェック
						  var checkList = [];
						  return function(key,value){
						      if( key==='' ){ // initial case
							  checkList.push(value);
							  return value;
						      }
						      if( value instanceof Node ){
							  return "<node>"; // undefined;
						      }
						      // Object,Arrayなら循環参照チェック
						      if( typeof value==='object' && value!==null ){
							  return checkList.every(function(v,i,a){
							      return value!==v;
							  }) ? value: "<circular>"; // undefined;
						      }
						      return value;
						  }}
					      )(),2);
		    }
		    let str = expr.toString();
		    if(str.substring(0,8) == "[object ") {
			if(debug== true) {
			    let a = [this.print_seq_no, expr];
			    str = "[" + this.print_seq_no++ + "," + str.substring(7);
			    console.log(a); // lispだけではうまく表示できなのでconsoleに頼る
			}
		    }
		    prstr += str;
		}
	    }
	    else if(_G.consp(expr)) {
		var opp = '(';
		var sym0 = expr.car; // may be 'quote

		// expr is a form of (quote e), NOT (quote e1 e2...)
		// same logic applies to ` , ,@
		if( _G.consp(expr.cdr) && expr.cdr.cdr == _G.nil) {
		    if(sym0 == _G.sym_quote) opp = '\'';
		    else if(sym0 == _G.sym_bq) opp = '`';
		    else if(sym0 == _G.sym_comma) opp = ',';
		    else if(sym0 == _G.sym_commaat) opp = ',@';
		}
		prstr += opp.toString(); // print(opp);

		for(var i=0;; i++) {
		    // 最初のシンボルがquoteなどオープンパレン(以外なら飛ばす
		    if(i > 0 || opp == '(')
			prstr += this.print_sex(expr.car,debug); //print_sex(expr.Car());

		    expr = expr.cdr;
		    if(expr == _G.nil) break;

		    if(_G.atom(expr)) {
			prstr += " . ";
			prstr += this.print_sex(expr,debug);
			break;
		    }
		    if(opp == '(') prstr += " "; // print(" ");
		}
		if(opp == '(') prstr += ")"; //print(")");
	    }
	    else {
		throw "Printer: unprintable";
	    }
	    return prstr;
	}, // print_sex
    }
    
    // ********************************************************************************
    // ### class Subr
    var Subr = function(f, man_str="") {
	this.f = f;
	this.man_str = man_str;
    }
    Subr.prototype = {
	toString: function() {
	    return "<Subr> " + this.man_str + this.f;
	},
    }
    
    // ********************************************************************************
    // ### class Eval
    var Eval = function() {
    }
    Eval.prototype = {
	check_fp: function(fp, dic={}) {

	    function symbol_check(s,dic) {
		if(! (s instanceof Sym)) throw "symbol expected after &-keyword instead of '" + _G.printer.print_sex(s) + "'";
		if(s.name[0] == '&' || s.name[0] ==':') throw "formal param starts w/ & or : '" + s.name + "'";
		if(s.name in dic) throw "multiple decl of formal parm '" + s.name + "'"; else dic[s.name] = 1;
	    }

	    var s;
	    while(fp != _G.nil) {
		if(fp.car == _G.sym_andrest) { // &rest
		    fp = fp.cdr;
		    s = fp.car; // next symbol of &rest
		    symbol_check(s,dic);
		    break;
		}
		else if(fp.car == _G.sym_andoptional) { // &optional
		    fp = fp.cdr;
		    while(fp != _G.nil) {
			s = fp.car;

			if(s instanceof Cons) { // デフォルト値のあるケース &optional (a 3 ap)
			    if(s.cdr.cdr.cdr != _G.nil) throw "malformed &optional parm";
			    let exist_p = s.cdr.cdr.car;
			    if(exist_p != _G.nil) {
				if(exist_p instanceof Sym) symbol_check(exist_p,dic);
				else throw	"malformed &optional exist-p";
			    }
			    s = s.car;
			}

			symbol_check(s,dic);
			fp = fp.cdr;
		    }
		    break;
		}
		else if(fp.car == _G.sym_andkey) { // &key
		    fp = fp.cdr;
		    while(fp != _G.nil) {
			s = fp.car;
			if(_G.consp(s)) {
			    // var v = s.cdr.car;
			    var p = s.cdr.cdr.car;
			    if(p != _G.nil) {
				symbol_check(p,dic);
			    }
			    s = s.car;
			}
			symbol_check(s,dic);
			fp = fp.cdr;
		    }
		    break;
		}
		else { // normal binding
		    s = fp.car;
		    // fpもapもリストなら再帰的にチェック
		    if(_G.consp(s)) this.check_fp(s,dic);
		    // 通常のsymbl-valueバインド
		    else { symbol_check(s,dic); }
		}
		fp = fp.cdr;
	    } // while

	},
	pairing: function(fp, ap, env, eval_args=false) {
	    var hd = _G.nil;
	    var s, v;

	    try {
		// 本当はformal parms.のチェックをランタイムにしても意味が薄いのだが
		// とりあえずやっている. defunの時に一通りやるともっといい.
		while(fp != _G.nil) {
		    if(fp.car == _G.sym_andrest) { // &rest
			fp = fp.cdr;
			s = fp.car; // next symbol of &rest

			var rest_of_args = ap;
			if(eval_args) rest_of_args = this.eval_list(rest_of_args,env)

			hd = _G.cons(_G.cons(s,rest_of_args), hd);
			break;
		    }
		    else if(fp.car == _G.sym_andoptional) { // &optional
			fp = fp.cdr;
			while(fp != _G.nil) {
			    s = fp.car;
			    v = _G.nil;

			    if(s instanceof Cons) { // デフォルト値の指定されていたケース &optional (a 3)
				var exist_p = s.cdr.cdr.car;
				v = this.eval_sex(s.cdr.car,env);
				s = s.car;
			    }

			    if(ap != _G.nil) { // 実パラメターが供された
				v = ap.car;
				if(eval_args) v = this.eval_sex(v, env);
			    }

			    if(exist_p != _G.nil) {
				if(ap != _G.nil) hd = _G.cons(_G.cons(exist_p,_G.sym_true), hd); // 指定されたのでt
				else hd = _G.cons(_G.cons(exist_p,_G.nil), hd);
			    }

			    hd = _G.cons(_G.cons(s,v), hd);
			    fp = fp.cdr;
			    ap = ap.cdr;
			}
			break;
		    }
		    else if(fp.car == _G.sym_andkey) { // &key
			fp = fp.cdr;
			var AL = _G.nil; // a-list ((key . val) ....)
			var supp_dic = {};
			while(fp != _G.nil) {
			    v = _G.nil;
			    s = fp.car;
			    if(_G.consp(s)) { // (symbol default supplied-p) is specified
				var t3 = s;
				v = this.eval_sex(t3.cdr.car,env); // default valuled caliculation
				s = t3.car; // symbol

				var supp_sym = t3.cdr.cdr.car;
				if(supp_sym instanceof Sym) { // supplied-pが指定された
				    AL = _G.cons(_G.cons(supp_sym,_G.nil), AL);
				    supp_dic[s.name] = supp_sym.name;
				}
			    }
			    AL = _G.cons(_G.cons(s,v), AL);
			    fp = fp.cdr;
			}

			// ALを実引数の内容に合わせて変更
			while(ap != _G.nil) {
			    var kw = ap.car;
			    if(! ((kw instanceof Sym) && kw.name[0] == ':')) throw "keyword symbol expected";

			    if(ap.cdr == _G.nil) throw "need param for the keyword " + kw.name;
			    v = ap.cdr.car;
			    if(eval_args) v = this.eval_sex(v, env);

			    s = _G.intern(kw.name.substr(1));
			    this.assoc(s,AL).cdr = v;

			    if(s.name in supp_dic) // もしsupplied-pがあれば
				this.assoc(_G.intern(supp_dic[s.name]),AL).cdr = _G.sym_true;

			    ap = ap.cdr.cdr;
			}

			hd = this.concat2(AL, hd);
			break;
		    }
		    else { // normal binding
			if(ap == _G.nil) throw "too few arguments";

			s = fp.car;
			v = ap.car;
			if(_G.consp(s)) {
			    if(_G.consp(v)) {
				// fpもapもリストなら再帰的にバインド
				var sub_pairs = this.pairing(s,v,env,eval_args);
				hd = this.concat2(sub_pairs,hd);
			    }
			    else throw "nested parm expected"
			}
			else { // 通常のsymbl-valueバインド
			    // if(! (s instanceof Sym)) throw "flaw in lambda: symbol expected in formal parm list";
			    if(eval_args) v = this.eval_sex(v, env);
			    hd = _G.cons(_G.cons(s,v), hd);
			}
		    }

		    fp = fp.cdr;
		    ap = ap.cdr;
		} // while
		return hd;
	    }
	    catch(e) {
		let fp_str = _G.printer.print_sex(fp);
		let ap_str = ap == _G.nil? "()": _G.printer.print_sex(ap);
		_G._err.push(env);
		this.assoc(_G.sym_stack_trace, env).cdr.push("@" + (_G._err.length-1) + ": when binding '" + ap_str + "' to '" + fp_str + "'");
		throw e;
	    }
	},
	assoc: function(s, env, setq_f=false) {
	    // search a list of dot-pairs, and return the dot-pair
	    for(; env != _G.nil; env=env.cdr) {
		var pair = env.car; // 通常はdot-pair
		if(pair instanceof Cons) {
		    // sがstringの場合は強制的に文字列比較
		    if((typeof(s)=="string") && (s==pair.car.toString())) return pair;
		    else if(s == pair.car) return pair;
		}
		// pair is NOT a cons => assume it is a hash
		else if((s instanceof Sym) && (pair instanceof Package)) {
		    return pair.get(s, setq_f);
		}
	    }
	    return _G.nil;
	},
	safe_concat2: function(list1, list2) { // 非破壊的、遅いのでDEBUG用
	    if(list1 == _G.nil) return list2;
	    return _G.cons(list1.car, this.safe_concat2(list1.cdr, list2));
	},
	concat2: function(list1, list2) { // 破壊的
	    if(list1 == _G.nil) return list2;
	    var p = list1;
	    while(p.cdr != _G.nil) p = p.cdr; // find the end
	    p.cdr = list2;
	    return list1;
	},
	eval_sex: function(expr, env) {
	    try {
		if(expr instanceof Sym) {
		    let c0 = expr.name.charAt(0);
		    if(c0 == ':') return expr; // :keyword is evaluated to itself
		    if(c0 == '@') { // @で始まるシンボルはマクロで置き換える
			const hook_sym = _G.intern("*at-hook-handler*");
			return this.apply(hook_sym,_G.cons(expr,_G.nil),env);
		    }

		    // package::symbol -- パッケージの中のシンボル評価
		    let a = expr.name.split("::");
		    if(a.length == 2) {
			let pnm = ":" + a[0];
			if(pnm in _G.packages) {
			    let pair = _G.packages[pnm].get(_G.intern(a[1]));
			    if(pair != _G.nil) return pair.cdr;
			    else throw "undefined symbol " + a[1] + " in package " + pnm;
			}
			else throw "failed to locate a package '" + pnm + "'";
		    }

		    // 通常のシンボル評価
		    let pair = this.assoc(expr, env);
		    if(pair != _G.nil) return pair.cdr; // found
		    throw "symbol undefined: '" + expr.name + "'";
		}

		// cons以外
		if(_G.atom(expr)) return expr;

		// e is a list => func or macro call
		// 1st elem must be evaluated as a form of: (lambda (a b c..) exp) or may be a macro
		// 2nd elem is a list of actual arguments; they are evaluated and bound to
		// formal params eventually
		return this.apply(expr.car,expr.cdr,env);
	    }
	    catch(e) { // eval_sex
		let expr_str = _G.printer.print_sex(expr);
		_G._err.push(env);
		// console.log(expr_str);
		this.assoc(_G.sym_stack_trace, env).cdr.push("@" + (_G._err.length-1) + ": eval() working on '" + expr_str + "'");
		throw e; // catchしたエラーに余分なものを足すと良くないことが起こる
	    }
	},
	/*
	  eval_array: function(elist, env) {
	  var aa = [];
	  while(elist != _G.nil) {
	  aa.push(this.eval_sex(elist.car, env));
	  elist = elist.cdr;
	  }
	  return aa;
	  },
	*/
	eval_list: function(elist, env) {
	    var vlist = _G.nil;
	    while(elist != _G.nil) {
		vlist = this.concat2(
		    vlist,
		    _G.cons(this.eval_sex(elist.car, env), _G.nil));
		elist = elist.cdr;
	    }
	    return vlist;
	},
	apply2: function(f, args, env) {
	    return this.apply(f,args,env);
	},
	apply: function(f, args, env, expand=false, eval_args=true, arg_eval_env=false) {
	    // fもargsもまだ評価されていいない状態で渡される
	    // eval_args: 実引数を評価するかどうか
	    // arg_eval_env: 実引数を評価する環境(cljなどのケース)

	    // まずfを評価してfuncに
	    var func = this.eval_sex(f, env);
	    // func may be a form of (lambda (a b c...) expr), macro , or native fun

	    if(func instanceof Subr) { // native
		// return _G.check_undef(func.f(args, env)); undefinedを返す関数もあるので意味のないチェック
		return func.f(args, env);
	    }

	    if(func instanceof Clj) { // closure
		// expand,eval_args=true, arg_eval_env=env
		return this.apply(func.expr,args,func.env,false,true,env);
	    }

	    if(! (func instanceof Cons)) {
		return _G.js_method(func,args,env);
	    }

	    var ftype = func.car;

	    if(ftype == _G.sym_lambda) { // function
		var after_lambda = func.cdr; // skip lambda
		// ((a b c...) fexpr)

		// skip a string right next to lambda, it's a comment
		let next2lambda = after_lambda.car;
		if(typeof next2lambda === "string" || next2lambda instanceof String)
		    after_lambda = after_lambda.cdr;

		// get the list of formal params
		var fformals = after_lambda.car; // (a b c...)

		// get the function body expr
		var fexpr = after_lambda.cdr;
		if(fexpr.cdr == _G.nil) fexpr = fexpr.car; // bodyは単一式
		else fexpr = _G.cons(_G.sym_progn, fexpr); // bodyは式のリスト

		try {
		    // _wrapや_tryのcatchでは引数を評価しない
		    var arg_assocl = this.pairing(fformals, args, (arg_eval_env? arg_eval_env: env), eval_args);
		    // 本番では破壊的なconcat2の方が速い
		    return this.eval_sex(fexpr, this.safe_concat2(arg_assocl, env));
		}
		catch(e) {
		    let f_str = _G.printer.print_sex(f);
		    let arg_assocl_str = _G.printer.print_sex(arg_assocl);

		    _G._err.push(env);
		    let idx = _G._err.length - 1;
		    if(arg_assocl) {
			this.assoc(_G.sym_stack_trace, env).cdr.push("@" + idx + ": apply() calling '" + f_str
								     + "' with binding '" + arg_assocl_str + "'");
		    }
		    else {
			this.assoc(_G.sym_stack_trace, env).cdr.push("@" + idx + ": apply() preparing to call '" + f_str
								     + "', but failed to bind parameters");
		    }
		    throw e;
		}
	    }

	    else if(ftype == _G.sym_macro) { // macro
		// func is a form of (macro (a b c...) mexpr)
		var after_macro = func.cdr;

		var mformals = after_macro.car; // (a b c...)

		var mexpr = after_macro.cdr;
		if(mexpr.cdr == _G.nil) mexpr = mexpr.car; // bodyは単一式
		else throw "macro can't take a list of exprs, use progn to concatenate"

		// 一回目の評価は、引数評価せず環境に積んで、式を評価 = 展開
		var macro_env = this.concat2(this.pairing(mformals,args,(arg_eval_env? arg_eval_env: env)), env);
		var expanded = this.eval_sex(mexpr, macro_env);

		// for macroexpand
		if(expand) return expanded;

		// 二回目の評価は、一回目の評価結果を式としてさらに評価
		// 今度は一切引数を環境に積んだりせず、ただ式を評価するのみ
		return this.eval_sex(expanded, env);
	    }

	    else { // neither function nor macro
		let ftype_str = _G.printer.print_sex(ftype);
		throw "apply() found '" + ftype_str + "' is not callable";
	    }
	},
    }

    // ********************************************************************************
    // ********************************************************************************
    // ********************************************************************************

    this.max_length_err = 200; // 長いエラーメッセージを切る位置
    
    this.symtab = new SymTab();
    this.intern = function(name) { return this.symtab.intern(name); }
    this.overwrite = function(s) { return this.symtab.overwrite(s); }

    this.tk_unknown = 0;
    this.tk_symbol = 1;
    this.tk_delim = 2;
    this.tk_object = 3;

    this.sym_nil = this.intern("nil");
    this.sym_true = this.intern("t");
    this.sym_bq = this.intern("_bq");
    this.sym_commaat = this.intern("_comma@");
    this.sym_comma = this.intern("_comma");
    this.sym_quote = this.intern("quote");
    this.sym_andoptional = this.intern("&optional");
    this.sym_andrest = this.intern("&rest");
    this.sym_andkey = this.intern("&key");
    this.sym_lambda = this.intern("lambda");
    this.sym_progn = this.intern("progn");
    this.sym_macro = this.intern("macro");
    this.sym_this = this.intern("this");
    this.sym_catch = this.intern(":catch");
    this.sym_finally = this.intern(":finally");
    this.sym_stack_trace = this.intern("***stack-trace***");

    this.strict_mode = false; // pairing check フォーマルと実引数の数のマッチをチェック
    /*
      this.check_undef = function(o) {
      if(o === void 0) throw "undefined object detected";
      return o;
      }
    */
    this.cons = function(car,cdr) { return new Cons(car,cdr); }

    this.nil = new Cons(null,null); this.nil.car = this.nil.cdr = this.nil;
    this.nil.toString = function () { return "NIL"; }
    this.nil.Iam = "NIL";

    this.consp = function(expr) {
	return (expr != _G.nil) && (expr instanceof Cons);
    }
    this.atom = function(expr) { return !this.consp(expr); }

    this.lexer = new Lexer();
    this.reader = new Reader();
    this.printer = new Printer();
    this.eval = new Eval();

    this.base_package = new Package();
    this.packages = {":base": this.base_package};
    this.top_env = this.cons(this.base_package,this.nil);
    this.add_top_env = function(s,v) {
	// グローバル環境にはhashを使うが、keyとして認められるのはstringだけなので
	// symbolのnameを使って環境に登録する。本当はsybolそのものをkeyにしたいがしょうがない.
	if(s instanceof Sym) this.top_env.car.add(s,this.cons(s,v));
	// if(s instanceof Sym) this.base_package.add(s,this.cons(s,v));
	else throw "add_top_env: not a symbol";
    }

    this.add_top_env(this.sym_true,this.sym_true); // t
    // この処理系では、lambdaもmacroも関数として呼ばれた場合はそれ自身を返すよう定義してある
    // よっていちいちクオートしなくてよい
    // this.add_top_env(this.sym_lambda,this.sym_lambda); // lambda
    // this.add_top_env(this.sym_macro,this.sym_macro); // macro

    this.add_top_env(this.intern("*base-package*"),this.base_package);
    this.add_top_env(this.intern("js-window"),window); // js window
    this.add_top_env(this.intern("js-document"),document); // js document
    this.add_top_env(this.intern("js-location"),location); // js location
    this.add_top_env(this.intern("js-null"),null); // js null
    this.add_top_env(this.intern("js-true"),true); // js true
    this.add_top_env(this.intern("js-false"),false); // js false
    this.add_top_env(this.intern("Object"),Object); // js Object
    this.add_top_env(this.intern("js-object"),Object); // js Object
    this.add_top_env(this.intern("$"),$); // $ of jQuery
    // this.add_top_env(this.intern("Handsontable"),Handsontable); // Handsontable
    this.add_top_env(this.intern("Promise"),Promise); // js promise
    this.add_top_env(this.intern("js-promise"),Promise); // js promise
    this.add_top_env(this.intern("void0"),void 0); // undefined
    this.add_top_env(this.intern("js-void0"),void 0); // undefined
    this.add_top_env(this.intern("js-undefined"),undefined); // undefined
    this.add_top_env(this.intern("js-nan"),NaN); // NaN, not a number
    this.add_top_env(this.intern("js-NaN"),NaN); // NaN, not a number    

    /*
    this._err_no = 0;
    */
    this._err = [];
    this.add_top_env(this.intern("*err"), this._err); // *errという配列にエラー時の環境を保存
    
    /*
      this.format = function(format){
      var i = 0,
      j = 0,
      r = "",
      next = function(args){
      j += 1; i += 1;
      return args[j] !== void 0 ? args[j] : "";
      };

      for(i=0; i<format.length; i++){
      if(format.charCodeAt(i) === 37){
      switch(format.charCodeAt(i+1)){
      case 115: r += next(arguments); break;
      case 100: r += Number(next(arguments)); break;
      default: r += format[i]; break;
      }
      } else {
      r += format[i];
      }
      }
      return r;
      };
    */

    this.js_method = function(func,args,env) {
	function B(x) {
	    // javascript 関数呼び出しで結果がfalseの場合だけnilを返す
	    // 美しくないが便利なので
	    if((typeof x) == "boolean" && (! x)) return _G.nil;
	    return x;
	}

	// syntax sugar (:Math:cos 3)のようなフォームは (js-windos :Math:cos 3)と解釈される
	if((func instanceof Sym) && func.name.charAt(0) == ":") {
	    args = _G.cons(func,args);
	    func = window;
	}

	var vlist = _G.eval.eval_list(args,env);
	// var aa = _G.eval.eval_array(args,env);
	var n = llength(vlist);

	if(n >= 1 && (vlist.car instanceof Sym)) {
	    var name = vlist.car.name;
	    let char0 = name.charAt(0);
	    let charLast = name.slice(-1);
	    let sym_colon = this.intern(':');

	    // A:
	    // (js-obj : idx : idx ... : idx) or (js-obj : idx : idx ... : idx val)
	    // (js-obj : idx)のような単純なケースもここで処理するので、下のBでまで到達しない
	    if(name == ':') {
		let p = vlist;
		if(p.cdr == _G.nil) throw("object call parse error, index after ':' is expected");

		while(p != _G.nil) {
		    if(p.car != sym_colon) throw("object call parse error, ':' is expected");

		    if(p.cdr != _G.nil && p.cdr.cdr == _G.nil) { // (.... : 0) 参照
			return B(func[p.cdr.car]);
		    }

		    if(p.cdr != _G.nil && p.cdr.cdr.cdr == _G.nil) {  // (.... : 0 val) 代入
			// (js-obj : idx :)は:を代入してしまうので例外的に認めない
			if(p.cdr.cdr.car == sym_colon) throw "object call parse error, can't assingn ':'";
			return B(func[p.cdr.car] = p.cdr.cdr.car);
		    }

		    func = func[p.cdr.car];
		    p = p.cdr.cdr;
		}
	    }

	    // B: かつては(js-obj : 0)のような単独ケースはここで処理していた
	    // (js-obje :idx:idx: 2)参照 or (js-obj :idx:idx: 2 100)代入のみここで処理
	    // もしAに不都合があるなら、Aをまるまる削除してもダウン互換の問題は生じないはず
	    if(char0 == ':') {
		if(charLast == ':') {
		    // (js-obj : 0)のように':'単独ケース、もしくは(js-obj :prop1:prop2: 3)のような連続ケース
		    if(name != ':') { // 連続ケースは順番にパース
			let names = name.slice(0,-1).split(":");
			let property = names[1];
			for(let i=2; i < names.length; i++) {
			    func = func[property];
			    property = names[i];
			}
			func = func[property];
		    }
		    switch(n-1) {
		    case 1:
			// (js-obj : 0) or (js-obj : "property") access
			return B(func[vlist.cdr.car]);
		    case 2:
			// (js-obj : 0 value) or (js-obj : "property" value) assign
			return B(func[vlist.cdr.car] = vlist.cdr.cdr.car);
		    default:
			throw "inncorrect number of args";
		    }
		}
		// JS object property access
		else if(charLast == '>')  {
		    let names = name.slice(0,-1).split(":");
		    let property = names[1];
		    // syntax sugar (js-obj :x:y:z>)
		    for(let i=2; i < names.length; i++) {
			func = func[property];
			property = names[i];
		    }
		    return B(func[property]);
		}
		else if(charLast == '<')  {
		    if(n != 2) throw "property assignment needs just one arg";
		    let names = name.slice(0,-1).split(":");
		    let property = names[1];
		    // syntax sugar (js-obj :x:y:z<)
		    for(let i=2; i < names.length; i++) {
			func = func[property];
			property = names[i];
		    }
		    return B(func[property] = vlist.cdr.car);
		}
		else { // JS method call
		    // :symbol => (o :method a1 a2 ...) = o.method(a1,a2,...) = o["method"](a1,a2,...) in js

		    // syntax sugar
		    // (((js-obj :x>) :y>) :f 1 2 3) => (js-obj :x:y:f 1 2 3)
		    let names = name.split(":");
		    let method = names[1];
		    for(let i=2; i < names.length; i++) {
			func = func[method];
			method = names[i];
		    }
		    if(typeof(func[method]) != "function") {
			throw "tried to call a method '" + method + "' of '" + func + "'";
		    }

		    vlist = vlist.cdr; // skip the symbol
		    switch(n-1) { // presumably method call
		    case 0:
			return B(func[method]());
		    case 1:
			return B(func[method](vlist.car));
		    case 2:
			return B(func[method]
				 (vlist.car,vlist.cdr.car));
		    case 3:
			return B(func[method]
				 (vlist.car,vlist.cdr.car,vlist.cdr.cdr.car));
		    case 4:
			return B(func[method]
				 (vlist.car,vlist.cdr.car,vlist.cdr.cdr.car,
				  vlist.cdr.cdr.cdr.car));
		    case 5:
			return B(func[method]
				 (vlist.car,vlist.cdr.car,vlist.cdr.cdr.car,
				  vlist.cdr.cdr.cdr.car, vlist.cdr.cdr.cdr.cdr.car));
		    default:
			throw "too many arguments for js";
		    }
		}
	    } // symbol starts w/ ':'
	    else throw "method selector symbol must start w/ ':'";
	} // 1st arg is Sym
	else { // n==0 or 1st arg is not a symbol => normal call: (obj a1 a2 ...)
	    if(typeof(func) != "function") {
		console.error(func); // vlist
		throw "can't call '" + _G.printer.print_sex(func) + "'";
	    }
	    switch(n) {
	    case 0:
		return B(func());
	    case 1:
		return B(func(vlist.car));
	    case 2:
		return B(func(vlist.car,vlist.cdr.car));
	    case 3:
		return B(func(vlist.car,vlist.cdr.car, vlist.cdr.cdr.car));
	    case 4:
		return B(func(vlist.car,vlist.cdr.car,vlist.cdr.cdr.car,
			      vlist.cdr.cdr.cdr.car));
	    case 5:
		return B(func(vlist.car,vlist.cdr.car, vlist.cdr.cdr.car,
			      vlist.cdr.cdr.cdr.car,vlist.cdr.cdr.cdr.cdr.car));
	    default:
		throw "too many arguments for js";
	    }
	}
    }; // js_method

    // ----------
    // function _sjis_len(args, env) {
    // 	function getLen(str){
    // 	    var result = 0;
    // 	    for(var i=0;i<str.length;i++){
    // 		let chr = str.charCodeAt(i);
    // 		if((chr >= 0x00 && chr < 0x81) ||
    // 		   (chr === 0xf8f0) ||
    // 		   (chr == 0x2212) || // ad-hoc by ishikawa to handle '−'
    // 		   (chr >= 0xff61 && chr < 0xffa0) ||
    // 		   (chr >= 0xf8f1 && chr < 0xf8f4)){
    // 		    //半角文字の場合は1を加算
    // 		    result += 1;
    // 		}else{
    // 		    //それ以外の文字の場合は2を加算
    // 		    result += 2;
    // 		}
    // 	    }
    // 	    //結果を返す
    // 	    return result;
    // 	}
    // 	check_args(args,1,1);
    // 	var a = _G.eval.eval_sex(args.car,env);
    // 	return getLen(a);
    // }
    // this.add_top_env(this.intern("sjis-len"), new Subr(_sjis_len));
    // ----------
    function _defpackage(args, env) {
	check_args(args,1,1);
	var s = args.car;
	if(s instanceof Sym) {
	    if(s.name in _G.packages) throw "defpackage: package " + s.name + " already exists";
	    // console.log("defining " + s.name);
	    _G.packages[s.name] = new Package(_G.base_package);
	    return s;
	}
	else throw "defpackage: needs a symbol";
    }
    this.add_top_env(this.intern("defpackage"), new Subr(_defpackage));
    // ----------
    function _in_package(args, env) {
	check_args(args,1,1);
	var s = _G.eval.eval_sex(args.car,env);
	if(s instanceof Sym && s.name in _G.packages) {
	    _G.top_env = _G.cons(_G.packages[s.name],_G.nil);
	    // return s;
	    return _G.top_env;
	}
	throw "in-package: failed to find a package";
    }
    this.add_top_env(this.intern("in-package"), new Subr(_in_package));
    // ----------
    function _ls(args, env) { // 現在のパッケージ内でバインドされているシンボル一覧
	let hd = _G.nil;
	let h = _G.top_env.car.hash;
	for(let key in h) hd = _G.cons(h[key].car,hd);
	return hd;
    }
    this.add_top_env(this.intern("ls"), new Subr(_ls));
    // ----------
    function _lsp(args, env) { // lsパケージ
	let hd = _G.nil;
	let h = _G.packages;
	for(let key in h) {
	    let s = _G.intern(key);
	    if(_G.top_env.car == h[key]) hd = _G.cons(_G.cons(s,_G.nil),hd);
	    else hd = _G.cons(s,hd);
	}
	return hd;
    }
    this.add_top_env(this.intern("lsp"), new Subr(_lsp));
    // ----------
    function _pwp(args, env) { // print working package symbol
	let h = _G.packages;
	for(let key in h) {
	    let s = _G.intern(key);
	    if(_G.top_env.car == h[key]) return key;
	}
	return ":unknown";
    }
    this.add_top_env(this.intern("pwp"), new Subr(_pwp));
    this.pwp = _pwp;
    // ----------
    /*
    function _pwp(args, env) { // print working package
	for(let key in _G.packages) {
	    if(_G.top_env.car == _G.packages[key])
		return _G.intern(key);
	}
	throw "pwp: fatal error";
    }
    this.add_top_env(this.intern("pwp"), new Subr(_pwp));
    this.add_top_env(this.intern("print-working-package"), new Subr(_pwp));
    */
    // ----------
    function _json_str(args, env) {
	check_args(args,1,1);
	return JSON.stringify(_G.eval.eval_sex(args.car,env));
    }
    this.add_top_env(this.intern("json-string<"), new Subr(_json_str));
    // ----------
    function _js_alert(args, env)
    {
	alert(_2string(args,env));
	return _G.nil;
    }
    this.add_top_env(this.intern("js-alert"), new Subr(_js_alert));
    // ----------
    function _js_info(args, env) {
	check_args(args,1,1);
	var o = _G.eval.eval_sex(args.car,env);
	var propNames = [];
	while (o) {
	    propNames.push(Object.getOwnPropertyNames(o));
	    o = Object.getPrototypeOf(o);
	}
	return propNames;
    }
    this.add_top_env(this.intern("js-info"), new Subr(_js_info));
    this.add_top_env(this.intern("info"), new Subr(_js_info));
    // ----------
    // 引数が無い場合は、windowsのFunctionを呼び出す安全な形式js-safe-evalを用意
    function _js_eval(args, env) {
	check_args(args,1);
	let vlist = _G.eval.eval_list(args,env);
	if(typeof(vlist.car) != "string") throw "js-eval: needs a string to evaluate";

	let ARGS = [];
	while(vlist != _G.nil) {
	    ARGS.push(vlist.car);
	    vlist = vlist.cdr;
	}
	return eval("(" + ARGS[0] + ")"); // ()で式評価の文脈を強制
    }
    this.add_top_env(this.intern("js-eval"), new Subr(_js_eval));
    this.add_top_env(this.intern("js-safe-eval"), new Subr(_js_eval));
    // ----------
    /* 予想以上にメモリストレスが大きいので一時使用禁止
    function _js_safe_eval(args, env) {
	check_args(args,1,1);
	let x = _G.eval.eval_sex(args.car,env);
	if(typeof(x) != "string") throw "needs a string here";
	return Function("'use strict'; return (" + x + ");" )();
    }
    this.add_top_env(this.intern("js-safe-eval"), new Subr(_js_safe_eval));
    */
    // ----------
    function _js_isNaN(args, env) {
	check_args(args,1,1);
	var a = _G.eval.eval_sex(args.car,env);
	if(isNaN(a)) return _G.sym_true;
	return _G.nil;
    }
    this.add_top_env(this.intern("js-isNaN"), new Subr(_js_isNaN));
    // ----------
    function _js_isArray(args, env) {
	check_args(args,1,1);
	var a = _G.eval.eval_sex(args.car,env);
	if(Array.isArray(a)) return _G.sym_true;
	return _G.nil;
    }
    this.add_top_env(this.intern("js-isArray"), new Subr(_js_isArray));
    // ----------
    function _js_log(args, env) {
	check_args(args,1,1);
	console.log(_G.eval.eval_sex(args.car,env));
	return _G.nil;
    }
    this.add_top_env(this.intern("js-log"), new Subr(_js_log));
    this.add_top_env(this.intern("js-console"), new Subr(_js_log));    
    // ----------
    function _js_warn(args, env) {
	check_args(args,1,1);
	console.warn(_G.eval.eval_sex(args.car,env));
	return _G.nil;
    }
    this.add_top_env(this.intern("js-warn"), new Subr(_js_warn));
    // ----------
    function _js_error(args, env) {
	check_args(args,1,1);
	console.error(_G.eval.eval_sex(args.car,env));
	return _G.nil;
    }
    this.add_top_env(this.intern("js-error"), new Subr(_js_error));
    // ----------
    // javascriptではfalsyのして定義された値(false,0,-0,0n,"",null,undefined,NaN)以外は全部真    
    function _js_falsy(args, env) {
	check_args(args,1,1);
	if(_G.eval.eval_sex(args.car,env)) return _G.nil;
	else return _G.sym_true;
    }
    this.add_top_env(this.intern("js-falsy"), new Subr(_js_falsy));
    this.add_top_env(this.intern("js-falsy?"), new Subr(_js_falsy));    
    // ----------
    function _js_truthy(args, env) {
	check_args(args,1,1);
	if(_G.eval.eval_sex(args.car,env)) return _G.sym_true;
	else return _G.nil;
    }
    this.add_top_env(this.intern("js-truthy"), new Subr(_js_truthy));
    this.add_top_env(this.intern("js-truthy?"), new Subr(_js_truthy));    
    // ----------
    function _js_uniq(args, env) { // js配列をuniqしてjs配列を返す
	check_args(args,1,1);
	let a = _G.eval.eval_sex(args.car,env);
	return [...new Set(a)];
    }
    this.add_top_env(this.intern("js-uniq"), new Subr(_js_uniq));
    // ----------
    function _js_typeof(args, env) {
	check_args(args,1,1);
	var a = _G.eval.eval_sex(args.car,env);
	return typeof(a);
    }
    var _f = new Subr(_js_typeof);
    this.add_top_env(this.intern("js-typeof<"), _f);
    this.add_top_env(this.intern("js-typeof"), _f);
    // ----------
    function _js_new(args, env) { // ex. (js-new Handsontable grid data)
	var vlist = _G.eval.eval_list(args,env);
	var n = llength(vlist.cdr);
	switch(n) {
	case 0:
	    return new (vlist.car)();
	case 1:
	    return new (vlist.car)(vlist.cdr.car);
	case 2:
	    return new (vlist.car)(
		vlist.cdr.car,vlist.cdr.cdr.car);
	case 3:
	    return new (vlist.car)(
		vlist.cdr.car,vlist.cdr.cdr.car,
		vlist.cdr.cdr.cdr.car);
	case 4:
	    return new (vlist.car)(
		vlist.cdr.car,vlist.cdr.cdr.car,
		vlist.cdr.cdr.cdr.car,
		vlist.cdr.cdr.cdr.cdr.car);
	case 5:
	    return new (vlist.car)(
		vlist.cdr.car,vlist.cdr.cdr.car,
		vlist.cdr.cdr.cdr.car,
		vlist.cdr.cdr.cdr.cdr.car,
		vlist.cdr.cdr.cdr.cdr.cdr.car);
	default:
	    throw "too many args for new";
	}
    }
    this.add_top_env(this.intern("js-new"), new Subr(_js_new));
    // ----------
    function _zen2han(args, env) {
	check_args(args,1,1);
	var str = _G.eval.eval_sex(args.car,env);
	var regex = /[Ａ-Ｚａ-ｚ０-９！＂＃＄％＆＇（）＊＋，－．／：；＜＝＞？＠［＼］＾＿｀｛｜｝]/g;

	// 入力値の全角を半角の文字に置換
	value = str
	    .replace(regex, function(s) {
		return String.fromCharCode(s.charCodeAt(0) - 0xfee0);
	    })
	    .replace(/[‐－―]/g, "-") // ハイフンなど
	    .replace(/[～〜]/g, "~") // チルダ
	    .replace(/　/g, " "); // スペース

	return value;
    }
    this.add_top_env(this.intern("old_zen2han"), new Subr(_zen2han));
    // ----------
    function _atom(args, env) {
	check_args(args,1,1);
	var a = _G.eval.eval_sex(args.car,env);
	if(_G.atom(a)) return _G.sym_true;
	else return _G.nil;
    }
    this.add_top_env(this.intern("atom"), new Subr(_atom, "(atom EXPR) - EXPRがatomかどうか判定."));
    // ----------
    function _instanceof(args, env) {
	check_args(args,2,2);
	var a = _G.eval.eval_sex(args.car,env);
	var b = _G.eval.eval_sex(args.cdr.car,env);
	if(a instanceof b) return _G.sym_true;
	else return _G.nil;
    }
    this.add_top_env(this.intern("js-instanceof"), new Subr(_instanceof));
    // ----------
    function _consp(args, env) {
	check_args(args,1,1);
	var a = _G.eval.eval_sex(args.car,env);
	if((a instanceof Cons) && a != _G.nil) return _G.sym_true;
	else return _G.nil;
    }
    this.add_top_env(this.intern("consp"), new Subr(_consp, "(consp EXPR) - EXPRがconsかどうか判定. nilはconsではない."));
    // ----------
    function _jqueryp(args, env) {
	check_args(args,1,1);
	var a = _G.eval.eval_sex(args.car,env);
	if(a instanceof jQuery) return _G.sym_true;
	else return _G.nil;
    }
    this.add_top_env(this.intern("jQueryp"), new Subr(_jqueryp, "(jQueryp EXPR) - jQueryオブジェクトかどうか判定."));
    // ----------
    function is_string(a) {
	return (a instanceof String || typeof a === "string");
    }

    function _stringp(args, env) {
	check_args(args,1,1);
	var a = _G.eval.eval_sex(args.car,env);
	if(is_string(a)) return _G.sym_true;
	else return _G.nil;
    }
    this.add_top_env(this.intern("stringp"), new Subr(_stringp, "(stringp EXPR) - EXPRが文字列かどうか判定."));
    // ----------
    function _and(args, env)
    {
	var o = _G.sym_true;
	for(; args != _G.nil; args=args.cdr) {
	    o = _G.eval.eval_sex(args.car,env);
	    if(o == _G.nil) return _G.nil;
	}
	return o;
    }
    this.add_top_env(this.intern("and"), new Subr(_and));
    // ----------
    function _or(args, env)
    {
	var o = _G.nil;
	for(; args != _G.nil; args=args.cdr) {
	    o = _G.eval.eval_sex(args.car,env);
	    if(o != _G.nil) return o;
	}
	return _G.nil;
    }
    this.add_top_env(this.intern("or"), new Subr(_or));
    // ----------
    function _nth(args, env)
    {
	check_args(args,2,2); // (nth 1 '(a b c)) => a
	var vlist = _G.eval.eval_list(args,env);
	var n = vlist.car;
	if(isNaN(n)) throw "nth: index is not a number";
	var p = vlist.cdr.car;
	if(n < 1) return _G.nil;

	if(Array.isArray(p) || (p instanceof jQuery)) { // js array
	    let a =  p[n-1];
	    if(a === undefined) return _G.nil;
	    return a;
	}

	for(var i=1; i<n && p != _G.nil; i++) p = p.cdr;
	return p.car;
    }
    this.add_top_env(this.intern("nth"), new Subr(_nth));
    // ----------
    function _listp(args, env) {
	check_args(args,1,1);
	var a = _G.eval.eval_sex(args.car,env);
	if(a instanceof Cons) return _G.sym_true;
	else return _G.nil;
    }
    this.add_top_env(this.intern("listp"), new Subr(_listp, "(listp EXPR) - EXPRがlistかどうか判定. nilはlist."));
    // ----------
    function _symp(args, env) {
	check_args(args,1,1);
	var a = _G.eval.eval_sex(args.car,env);
	if(a instanceof Sym) return _G.sym_true;
	else return _G.nil;
    }
    this.add_top_env(this.intern("symp"), new Subr(_symp));
    // ----------
    function _check_fp(args, env) {
	check_args(args,1,1);
	var a = _G.eval.eval_sex(args.car,env);
	_G.eval.check_fp(a);
	return _G.nil;
    }
    this.add_top_env(this.intern("check-fp"), new Subr(_check_fp));
    // ----------
    function _add(args, env) {
	var sum = 0;
	var vlist = _G.eval.eval_list(args,env);
	if(typeof(vlist.car) == "string" || vlist.car instanceof String) sum = "";
	while(vlist != _G.nil) {
	    sum += vlist.car;
	    vlist = vlist.cdr;
	}
	return sum;
    }
    this.add_top_env(this.intern("+"), new Subr(_add));
    // ----------
    function _cat(args, env) {
	var sum = "";
	var vlist = _G.eval.eval_list(args,env);
	while(vlist != _G.nil) {
	    sum += vlist.car;
	    vlist = vlist.cdr;
	}
	return sum;
    }
    this.add_top_env(this.intern("cat"), new Subr(_cat));
    // ----------
    function _mul(args, env) {
	var mul = 1;
	var vlist = _G.eval.eval_list(args,env);
	while(vlist != _G.nil) {
	    mul *= vlist.car;
	    vlist = vlist.cdr;
	}
	return mul;
    }
    this.add_top_env(this.intern("*"), new Subr(_mul));
    // ----------
    function _sub(args, env) {
	if(args == _G.nil) return 0;
	var vlist = _G.eval.eval_list(args,env);
	var v1 = vlist.car;
	vlist = vlist.cdr;
	if(vlist == _G.nil) return -v1;
	while(vlist != _G.nil) {
	    v1 -= vlist.car;
	    vlist = vlist.cdr;
	}
	return v1;
    }
    this.add_top_env(this.intern("-"), new Subr(_sub));
    // ----------
    function _idiv(args, env) {
	check_args(args,1);
	var vlist = _G.eval.eval_list(args,env);
	var v1 = vlist.car;
	vlist = vlist.cdr;
	if(vlist == _G.nil) return (1/v1|0);
	while(vlist != _G.nil) {
	    v1 = (v1/vlist.car|0);
	    vlist = vlist.cdr;
	}
	return v1;
    }
    this.add_top_env(this.intern("div"), new Subr(_idiv));
    // ----------
    function _div(args, env) {
	check_args(args,1);
	var vlist = _G.eval.eval_list(args,env);
	var v1 = vlist.car;
	vlist = vlist.cdr;
	if(vlist == _G.nil) return 1/v1;
	while(vlist != _G.nil) {
	    v1 /= vlist.car;
	    vlist = vlist.cdr;
	}
	return v1;
    }
    this.add_top_env(this.intern("/"), new Subr(_div));
    // ----------
    function _mod(args, env) {
	check_args(args,2,2);
	var a = _G.eval.eval_sex(args.car,env);
	var b = _G.eval.eval_sex(args.cdr.car,env);
	return a % b;
    }
    this.add_top_env(this.intern("%"), new Subr(_mod));
    // ----------
    function _quote(args, env) {
	if(args == _G.nil) throw "wrong number of args";
	return args.car;
    }
    this.add_top_env(this.sym_quote, new Subr(_quote));
    // ----------
    var gensym_seq_no = 0;
    function _gensym(args, env) {
	return _G.intern("#:G{" + gensym_seq_no++ + "}");
    }
    this.add_top_env(this.intern("gensym"), new Subr(_gensym));
    // ----------
    function _lambda(args, env) {
	return _G.cons(_G.sym_lambda,args);
    }
    this.add_top_env(this.sym_lambda, new Subr(_lambda));
    // ----------
    function _macro(args, env) {
	return _G.cons(_G.sym_macro,args);
    }
    this.add_top_env(this.sym_macro, new Subr(_macro));
    // ----------
    function append(a,b) {
	if(a != _G.nil) return _G.cons(a.car, append(a.cdr,b));
	else return b;
    }
    function _bq(args, env) {
	function parse(expr,env) {
	    if(_G.atom(expr)) return expr;
	    var tag = expr.car;
	    if(tag == _G.sym_bq) return expr; // nested ` will not be evaluated
	    if(tag == _G.sym_comma || tag == _G.sym_commaat)
		return _G.eval.eval_sex(expr.cdr.car,env);

	    var hd = _G.nil;
	    while(true) {
		if(expr == _G.nil) {
		    break;
		}
		else if(_G.atom(expr)) { // `(1 2 3 . 4)の4
		    hd = append(hd,expr);
		    break;
		}
		else if(expr.car == _G.sym_comma) { // `(3 . ,x)の,x
		    var v = _G.eval.eval_sex(expr.cdr.car,env);
		    hd = append(hd,v);
		    expr = expr.cdr.cdr;
		}
		else if(expr.car.car == _G.sym_commaat) { // `(1 2 ,@a 3 4)の,@a
		    var v = _G.eval.eval_sex(expr.car.cdr.car,env);
		    hd = append(hd,v);
		    expr = expr.cdr;
		}
		else { // `(1 2 X 3 4)のX、atomでもconsでも
		    var v = parse(expr.car,env);
		    var cons = _G.cons(v,_G.nil);
		    hd = append(hd,cons);
		    expr = expr.cdr;
		}
		/*
		  else if(! (cons instanceof Cons))
		  throw _G.printer.print_sex(expr.car) + " evaluates to a non-list: "
		  + _G.printer.print_sex(cons);
		*/
	    }
	    return hd;
	}
	check_args(args,1,1);
	return parse(args.car,env);
    }
    this.add_top_env(this.sym_bq, new Subr(_bq));
    // ----------
    function _parse(args, env) {
	// 文字列からs式を生成
	// 複数のs式が合った場合、最初の1つのみ対象となる.
	check_args(args,1);
	var str = _G.eval.eval_sex(args.car,env);

	if(! is_string(str)) throw "parse: needs a string here";

	var lexer = new Lexer();
	var reader = new Reader();

	// lexerのDFAが発狂するので、strが必ず改行で終わるようにする
	let c = str.slice(-1);
	if(c != "\n" && c != "\r") str = str + "\n";

	lexer.init(str);
	reader.init(lexer);

	try {
	    return reader.parse(null);
	}
	catch(e) {
	    console.error(lexer.INPUT.slice(lexer.INPUT_ptr-1)); // エラー以後の入力
	    throw "parse: malformed input string";
	}
    }
    this.add_top_env(this.intern("parse"), new Subr(_parse));
    // ----------
    function _eat(args, env) {
	// 文字列をリスププログラムとして評価
	// 複数のs式を受け付ける
	check_args(args,1);
	var str = _G.eval.eval_sex(args.car,env);

	if(! is_string(str)) throw "eat: needs a string here";

	var lexer = new Lexer();
	var reader = new Reader();

	// lexerのDFAが発狂するので、strが必ず改行で終わるようにする
	let c = str.slice(-1);
	if(c != "\n" && c != "\r") str = str + "\n";

	lexer.init(str);
	reader.init(lexer);

	let last_exp = _G.nil;
	while(true) {
	    try {
		var exp = reader.parse(null);
	    }
	    catch(e) {
		console.error(lexer.INPUT.slice(lexer.INPUT_ptr-1)); // エラー以後の入力
		throw "eat: malformed input string";
	    }

	    if(exp == null) break;
	    // console.log( _G.printer.print_sex(exp) );
	    last_exp = _G.eval.eval_sex(exp,env);
	}
	return last_exp;
    }
    this.add_top_env(this.intern("eat"), new Subr(_eat));
    // ----------
    function _loop(args, env) {
	check_args(args,1);
	var label = _G.eval.eval_sex(args.car,env);
	if(! (label instanceof	Sym)) throw "label symbol is required for loop";

	try {
	    while(true) {
		var body = args.cdr;
		while(body != _G.nil) {
		    var val = _G.eval.eval_sex(body.car,env);
		    body = body.cdr;
		}
	    }
	}
	catch(e) {
	    if(e === label) return _G.nil
	    else if(e instanceof Cons && e.car === label) return e.cdr;
	    else throw(e);
	}
    }
    this.add_top_env(this.intern("loop"), new Subr(_loop));
    // ----------
    function _block(args, env) {
	check_args(args,1);
	var label = _G.eval.eval_sex(args.car,env);
	if(! (label instanceof	Sym)) throw "label symbol is required for block";

	try {
	    var body = args.cdr;
	    while(body != _G.nil) {
		var val = _G.eval.eval_sex(body.car,env);
		body = body.cdr;
	    }
	    return val;
	}
	catch(e) {
	    if(e === label) return _G.nil
	    else if(e instanceof Cons && e.car === label) return e.cdr;
	    else throw(e);
	}
    }
    this.add_top_env(this.intern("block"), new Subr(_block));
    // ----------
    function _progn(args, env) {
	var vlist = _G.eval.eval_list(args,env);
	while(vlist != _G.nil) {
	    if(vlist.cdr == _G.nil) return vlist.car;
	    vlist = vlist.cdr;
	}
	return _G.nil;
    }
    this.add_top_env(this.sym_progn, new Subr(_progn));
    // ----------
    function _if(args, env) {
	var test = _G.eval.eval_sex(args.car, env);
	var a = args.cdr.car;
	var b = args.cdr.cdr;
	if(test != _G.nil) return _G.eval.eval_sex(a,env);
	else return _progn(b,env);
    }
    this.add_top_env(this.intern("if"), new Subr(_if));
    // ----------
    function _assoc(args, env) {
	check_args(args,2,2);
	var vlist = _G.eval.eval_list(args,env);
	return _G.eval.assoc(vlist.car,vlist.cdr.car);
    }
    this.add_top_env(this.intern("assoc"), new Subr(_assoc));
    // ----------
    function _av(args, env) {
	check_args(args,2,2);
	var vlist = _G.eval.eval_list(args,env);
	return _G.eval.assoc(vlist.car,vlist.cdr.car).cdr;
    }
    this.add_top_env(this.intern("av"), new Subr(_av));
    // ----------
    function _set(args, env) {
	check_args(args,2,2);
	var s = _G.eval.eval_sex(args.car,env);
	return _setq(_G.cons(s,args.cdr),env);
    }
    this.add_top_env(this.intern("set"), new Subr(_set));
    // ----------
    function _setq(args, env) {
	check_args(args,2,2);
	var s = args.car;
	var v = _G.eval.eval_sex(args.cdr.car,env);

	if(! (s instanceof Sym)) throw "set/setq: symbol is requiired";
	if(s == _G.sym_true) throw "set/setq: can't assign to a constant";

	let a = s.name.split("::");

	// package::symbolの場合
	if(a.length == 2) {
	    let pnm = ":" + a[0];
	    if(pnm in _G.packages) {
		let s = _G.intern(a[1]);
		let pair = _G.packages[pnm].get(s);
		if(pair != _G.nil) pair.cdr = v;
		else _G.packages[pnm].add(s,_G.cons(s,v));
	    }
	    else throw "set/setq: failed to locate a package " + a[0];
	}
	else {
	    let pair = _G.eval.assoc(s,env,true); // search this package only, not to the parent
	    if(pair != _G.nil) pair.cdr = v;
	    else _G.add_top_env(s,v);
	}
	return v;
    }
    this.add_top_env(this.intern("setq"), new Subr(_setq));
    // ----------
    function _clj(args, env) {
	check_args(args,1,1);
	var f = _G.eval.eval_sex(args.car,env);
	return new Clj(f,env);
    }
    this.add_top_env(this.intern("clj"), new Subr(_clj));
    // ----------
    function _dot(args, env) { // method call to a js object
	check_args(args,1);
	var func = _G.eval.eval_sex(args.car,env);
	return _G.js_method(func,args.cdr,env);
    }
    this.add_top_env(this.intern("->"), new Subr(_dot));
    // ----------
    // wrapされた関数が呼ばれる時には、this,arg0...nがバインドされている.
    function _wrap(a_rgs, env) {
	check_args(a_rgs,1);

	return function() {
	    try {
		var hd = _G.cons((this),_G.nil); // thisを最初の引数に
		var tl = hd;
		for(var i=0; i<arguments.length; i++) {
		    tl.cdr = _G.cons((arguments[i]),_G.nil);
		    tl = tl.cdr;
		}

		// 引数の評価を避けるように修正 2019/12/30
		var stack_trace = [];
		var env2 = _G.cons(_G.cons(_G.sym_stack_trace, stack_trace), env); // stack traceのための変数をセット
		return _G.eval.apply(a_rgs.car,hd,env2,false,false);
	    }
	    catch(e) {
		var f = _G.printer.print_sex(a_rgs.car);
		stack_trace.push("# invoking function '" + f + "'");
		
		sock_send("\n");
		for(let i=stack_trace.length-1; i >= 0; i--) {
		    sock_send("[" + i + "] " + stack_trace[i].slice(0,_G.max_length_err) + "\n");
		}
		sock_send("***WRAP> " + e + "\n");
		console.error("***WRAP> inside of " + f + "\n" + e + "");

		throw e;
	    }
	}
    }
    this.add_top_env(this.intern("wrap"), new Subr(_wrap));
    // ----------
    function _async(args, env) {
	// (async f p) promise pの結果を引数にfを呼ぶためのクロジャーを返す.
	// クロジャーが呼ばれる時、pのresolve値は自動的に最初の引数としてバインドされる.
	// fの残りの引数は自由に設定してよく、それらにはarg0...nがバインドされている.
	// したがって、典型的なfは(lambda (p a1 a2 a3) exprs...)となる.
	// クロジャーが呼ばれた時の戻り値は、新たなpromiseとなる.
	check_args(args,2);
	var p  = _G.eval.eval_sex(args.cdr.car,env);
	return async function() {
	    // 2021/10/8
	    // async関数の中からのthrowは例外ではなくrejectの意味になるので
	    // catchしなくてもいいと考えコメントアウト
	    // catchもできるが、それは例外ではなくrejectのキャッチ(then,catchと同じ意味のcatchになる)
	    // try {
		var v = await p; // awaitはpromise以外の式でもいいみたい
		var hd = _G.cons(v,_G.nil);
		var tl = hd;
		for(var i=0; i<arguments.length; i++) {
		    tl.cdr = _G.cons((arguments[i]),_G.nil);
		    tl = tl.cdr;
		}
		// 引数の評価を避けるように修正 2019/12/30
		return _G.eval.apply(args.car,hd,env,false,false);
	    // }
	    // catch(e) {
	    // 	// async関数の中のtry..catchでは
	    // 	// promiseのrejectを捕まえてしまうみたい. びっくり!
	    // 	// async関数内でのthrowはrejectと同じ意味だからこうなるのか...
	    // 	var f = _G.printer.print_sex(args.car);
	    // 	sock_send(";;ASYNC-CATCH> inside <<" + f + ": " + e + ">>\n");
	    // 	console.log(";;ASYNC-CATCH> inside <<" + f + ": " + e + ">>"); 
	    // 	// これを捕まえてしまうとpromise -> catchがうまく動かない
	    // 	throw e;
	    // }
	}
    }
    this.add_top_env(this.intern("async"), new Subr(_async));
    // ----------
    function _try(args, env)
    {
	check_args(args,1);
	var catchf = null;
	var finf = null;
	for(var i=0; i<2; i++)
	    if(args.car == _G.sym_catch || args.car == _G.sym_finally) {
		if(args.car == _G.sym_catch) catchf = args.cdr.car;
		else finf = args.cdr.car;
		args = args.cdr.cdr;
	    }
	if(catchf != null && finf != null) {
	    try {
		var r = _G.nil;
		for(; args != _G.nil; args=args.cdr) r = _G.eval.eval_sex(args.car,env);
		return r;
	    }
	    catch(e) { return _G.eval.apply(catchf,_G.cons(e,_G.nil),env,false,false); }
	    finally { _G.eval.apply(finf,_G.nil,env); }
	}
	else if(catchf != null) {
	    try {
		var r = _G.nil;
		for(; args != _G.nil; args=args.cdr) r = _G.eval.eval_sex(args.car,env);
		return r;
	    }
	    catch(e) { return _G.eval.apply(catchf,_G.cons(e,_G.nil),env,false,false); }
	}
	else if(finf != null) {
	    try {
		var r = _G.nil;
		for(; args != _G.nil; args=args.cdr) r = _G.eval.eval_sex(args.car,env);
		return r;
	    }
	    finally { _G.eval.apply(finf,_G.nil,env); }
	}
	else {
	    var r = _G.nil;
	    for(; args != _G.nil; args=args.cdr) r = _G.eval.eval_sex(args.car,env);
	    return r;
	}
    }
    this.add_top_env(this.intern("try"), new Subr(_try));
    // ----------
    function _eval(args, env)
    {
	check_args(args,1,1);
	var a  = _G.eval.eval_sex(args.car,env);
	return _G.eval.eval_sex(a,env);
    }
    this.add_top_env(this.intern("eval"), new Subr(_eval));
    // ----------
    function _throw(args, env)
    {
	var n = check_args(args,1,2);
	var obj = _G.eval.eval_sex(args.car,env);

	if(n == 1)
	    throw obj;
	else {
	    let expr = _G.eval.eval_sex(args.cdr.car,env);
	    throw new LispException(obj, expr);
	}

	/*
	  check_args(args,1);
	  var vlist = _G.eval.eval_list(args,env);
	  var mess = "";
	  for(;vlist != _G.nil; vlist=vlist.cdr)
	  mess += (vlist.car).toString();
	  throw mess;
	*/
    }
    this.add_top_env(this.intern("throw"), new Subr(_throw));
    // ----------
    function _funcall(args, env)
    {
	check_args(args,1);
	return _G.eval.apply(args.car,args.cdr,env); // applyで引数を評価するのでここではしない
    }
    this.add_top_env(this.intern("funcall"), new Subr(_funcall));
    // Javascriptからlispを呼ぶ場合
    this.funcall = function(f,a,b,c,d,e) {
	try {
	    var args = _G.nil;
	    if(e) args = _G.cons(e,args);
	    if(d) args = _G.cons(d,args);
	    if(c) args = _G.cons(c,args);
	    if(b) args = _G.cons(b,args);
	    if(a) args = _G.cons(a,args);
	    args = _G.cons(this.intern(f),args);
	    return _funcall(args,_G.top_env);
	}
	catch(e) {
	    sock_send("JS-FUNCALL> " + e);
	}
    };
    // ----------
    function QL(x) {
	// xの中身を一つずつquote
	if(x == _G.nil) return x;
	if(! (x instanceof Cons)) throw "the last arg of apply must be a list";
	return _G.cons(_G.cons(_G.sym_quote,_G.cons(x.car,_G.nil)), QL(x.cdr));
    }
    function _apply(args, env)
    {
	// lisp applyの実装は意外と難しい
	// CommonLispだと、スペシャルフォームやmacroは呼び出せない
	// ただしcarなどのnative関数は可能

	// lispのapplyでは引数を評価するフォームしか呼べない
	check_args(args,2);
	var f = args.car; // f will be evaluated in the real apply

	var vlist = _G.eval.eval_list(args.cdr,env);
	// js applyは引数を評価してしまうので、ここで評価すると2回評価になってしまう.
	// 苦肉の策として引数を評価した後できるlistの中身をQLで一つずつquoteしている.
	// 意味的に正しくなるのかちょっと自信がない.
	// applyの引数評価なしのフラグを使わないのかという理由は下記コメントを参照

	var tl = hd = _G.nil;
	for(; vlist != _G.nil; vlist=vlist.cdr) {
	    let elem = _G.cons(vlist.car, _G.nil);
	    if(hd == _G.nil) {
		tl = hd = elem;
		if(vlist.cdr == _G.nil) { // singleton
		    hd = elem.car;
		    break;
		}
	    }
	    else {
		if(vlist.cdr == _G.nil) {
		    tl.cdr = elem.car;
		    break;
		}
		else {
		    tl.cdr = elem;
		    tl = elem;
		}
	    }
	}
	return _G.eval.apply(f,QL(hd),env);
	/* 引数はここで評価したので、applyでもう一度評価されると困る. ここで
	   fは組み込み関数かもしれないし、ユーザ定義のlambda関数かもしれない
	   組み込み関数の場合、引数が評価を評価するかどうかは関数次第、
	   lambda関数の場合は、基本的にすべて評価されるが、applyの引数で評価しないこともできる.
	   applyの引数で評価を禁止しても、fが組み込み関数なら
	   この引数は効かない. よって'で両方のケースに困らないように'している.
	*/

    }
    this.add_top_env(this.intern("apply"), new Subr(_apply));
    // ----------
    function _env(args, env) {
	return env;
    }
    this.add_top_env(this.intern("env"), new Subr(_env));
    // ----------
    function llength(args) {
	var n = 0;
	while(args != _G.nil) {
	    n++;
	    if(args instanceof Cons) args = args.cdr;
	    else throw("list was expected");
	}
	return n;
    }
    // ---
    function check_args(args,n=null,m=null) {
	var len = llength(args);
	if((n && m && (len < n || m < len)) ||
	   (n && len < n) ||
	   (m && m < len)) throw "wrong number of args";
	return len;
    }
    // ----------
    function _length(args,env) {
	check_args(args,1,1);
	var a = _G.eval.eval_sex(args.car,env);
	if(a instanceof Cons) return llength(a);
	return a.length;
    }
    this.add_top_env(this.intern("length"), new Subr(_length));
    // ----------
    function _car(args, env) {
	check_args(args,1,1);
	var x = _G.eval.eval_sex(args.car,env);
	if(x instanceof Cons) return x.car;

	// ad-hoc 配列をリストとみなすため
	if(Array.isArray(x) || (x instanceof jQuery)) {
	    if(x.length < 1) return _G.nil;
	    return x[0];
	}

	throw "can't take car";
    }
    this.add_top_env(this.intern("car"), new Subr(_car));
    // ----------
    function _cdr(args, env) {
	check_args(args,1,1);
	var x =	 _G.eval.eval_sex(args.car,env)
	if(x instanceof Cons) return x.cdr;

	// 配列をリストとみなすためのあがき
	if(Array.isArray(x) || (x instanceof jQuery)) {
	    if(x.length <= 1) return _G.nil;
	    return x.slice(1);
	}

	throw "can't take cdr";
    }
    this.add_top_env(this.intern("cdr"), new Subr(_cdr));
    // ----------
    function _neq(args, env) {
	var v = _eq(args,env);
	if(v == _G.nil) return _G.sym_true;
	else return _G.nil;
    }
    this.add_top_env(this.intern("/="), new Subr(_neq));
    this.add_top_env(this.intern("<>"), new Subr(_neq));
    // ----------
    function _eq(args, env) {
	check_args(args,2,2);
	var vlist = _G.eval.eval_list(args,env);
	if(vlist.car == vlist.cdr.car) return _G.sym_true;
	// 第1引数が文字列なら文字列比較
	if(((typeof vlist.car) == "string") && (vlist.car == vlist.cdr.car.toString())) return _G.sym_true;
	// 両方ともDateなら、getDate()で比較
	if((vlist.car instanceof Date) && (vlist.cdr.car instanceof Date) &&
	   (vlist.car.getDate() == vlist.cdr.car.getDate())) return _G.sym_true;
	return _G.nil;
    }
    this.add_top_env(this.intern("="), new Subr(_eq));
    this.add_top_env(this.intern("=="), new Subr(_eq));    
    // ----------
    function _stricteq(args, env) {
	check_args(args,2,2);
	var vlist = _G.eval.eval_list(args,env);
	if(vlist.car === vlist.cdr.car) return _G.sym_true;
	return _G.nil;
    }
    this.add_top_env(this.intern("==="), new Subr(_stricteq));
    // ----------
    function _not(args, env) {
	check_args(args,1,1);
	var a = _G.eval.eval_sex(args.car,env);
	if(a == _G.nil) return _G.sym_true;
	else return _G.nil;
    }
    this.add_top_env(this.intern("not"), new Subr(_not));
    // ----------
    function _less(args, env) {
	check_args(args,2,2);
	var vlist = _G.eval.eval_list(args,env);
	if(vlist.car < vlist.cdr.car) return _G.sym_true;
	return _G.nil;
    }
    this.add_top_env(this.intern("<"), new Subr(_less));
    // ----------
    function _less_e(args, env) {
	check_args(args,2,2);
	var vlist = _G.eval.eval_list(args,env);
	if(vlist.car <= vlist.cdr.car) return _G.sym_true;
	return _G.nil;
    }
    this.add_top_env(this.intern("<="), new Subr(_less_e));
    // ----------
    function _greater(args, env) {
	check_args(args,2,2);
	var vlist = _G.eval.eval_list(args,env);
	if(vlist.car > vlist.cdr.car) return _G.sym_true;
	return _G.nil;
    }
    this.add_top_env(this.intern(">"), new Subr(_greater));
    // ----------
    function _greater_e(args, env) {
	check_args(args,2,2);
	var vlist = _G.eval.eval_list(args,env);
	if(vlist.car >= vlist.cdr.car) return _G.sym_true;
	return _G.nil;
    }
    this.add_top_env(this.intern(">="), new Subr(_greater_e));
    // ----------
    function _cons(args, env) {
	check_args(args,2,2);
	var vlist = _G.eval.eval_list(args,env);
	return _G.cons(vlist.car,vlist.cdr.car);
    }
    this.add_top_env(this.intern("cons"), new Subr(_cons));
    // ----------
    function _list(args, env) {
	if(args == _G.nil) return _G.nil;
	return _G.cons(
	    _G.eval.eval_sex(args.car,env),
	    _list(args.cdr,env));
    }
    this.add_top_env(this.intern("list"), new Subr(_list));
    // ----------
    function _let(args,env,star=null)
    {
	check_args(args,1);
	var bindls = args.car;
	var expr = _G.cons(_G.sym_progn, args.cdr);
	var e = env;
	for(; bindls != _G.nil; bindls=bindls.cdr) {
	    var elem = bindls.car;
	    var sym, val;
	    if(elem instanceof Cons) {
		var pair = elem;
		sym = pair.car;
		val = pair.cdr.car;
	    }
	    else {
		sym = elem;
		val = _G.nil;
	    }
	    if(star) val = _G.eval.eval_sex(val,e);
	    else val = _G.eval.eval_sex(val,env);
	    e = _G.cons(_G.cons(sym,val),e);
	}
	return _G.eval.eval_sex(expr,e);
    }
    this.add_top_env(this.intern("let"), new Subr(_let));
    // ----------
    function _let_star(args,env) {
	return _let(args,env,true);
    }
    this.add_top_env(this.intern("let*"), new Subr(_let_star));
    // ----------
    function _cond(args,env)
    {
	for(var p=args; p != _G.nil; p=p.cdr) {
	    var clause = p.car;
	    if(clause != _G.nil) {
		var b = _G.eval.eval_sex(clause.car,env);
		if(b != _G.nil) {
		    if(clause.cdr == _G.nil) return b;
		    return _progn(clause.cdr,env);
		}
	    }
	}
	return _G.nil;
    }
    this.add_top_env(this.intern("cond"), new Subr(_cond));
    // ----------
    function _macro_expand(args, env)
    {
	check_args(args,1,1);
	var a = _G.eval.eval_sex(args.car,env);
	return _G.eval.apply(a.car, a.cdr, env, true);
    }
    this.add_top_env(this.intern("macroexpand"), new Subr(_macro_expand));
    // ----------
    function _car_bang(args, env)
    {
	check_args(args,2,2);
	var vlist = _G.eval.eval_list(args,env);
	if(! (vlist.car instanceof Cons)) throw _G.printer.print_sex(vlist.car) + " is not a cons";
	return vlist.car.car = vlist.cdr.car;
    }
    this.add_top_env(this.intern("car!"), new Subr(_car_bang));
    // ----------
    function _cdr_bang(args, env)
    {
	check_args(args,2,2);
	var vlist = _G.eval.eval_list(args,env);
	if(! (vlist.car instanceof Cons)) throw _G.printer.print_sex(vlist.car) + " is not a cons";
	return vlist.car.cdr = vlist.cdr.car;
    }
    this.add_top_env(this.intern("cdr!"), new Subr(_cdr_bang));
    // ----------
    function _foreach(args, env, skip_nil=null)
    {
	check_args(args,2);

	var idx = args.car;
	var idx_v; // idxにバインドされる値
	var ls = _G.eval.eval_sex(args.cdr.car,env);
	var hd, tl;
	hd = tl = _G.nil;
	var v = _G.nil; // foreachのbodyを順に評価し、その都度vに入れていく

	if(ls instanceof Cons) {
	    for(; ls!=_G.nil; ls=ls.cdr) {
		idx_v = ls.car;
		let e = _G.cons(_G.cons(idx,idx_v),env);
		for(let body=args.cdr.cdr; body != _G.nil; body=body.cdr) {
		    v = _G.eval.eval_sex(body.car,e);
		}
		if (skip_nil && v == _G.nil) {} // skip
		else {
		    let last_elem = _G.cons(v, _G.nil);
		    if (hd == _G.nil) hd = tl = last_elem;
		    else {
			tl.cdr = last_elem;
			tl = last_elem;
		    }
		}
	    }
	}
	else if(Array.isArray(ls) || ls instanceof jQuery) { // js配列
	    for(let i=0, n=ls.length; i<n; i++) {
		idx_v = ls[i];
		let e = _G.cons(_G.cons(idx,idx_v),env);
		for(let body=args.cdr.cdr; body != _G.nil; body=body.cdr) {
		    v = _G.eval.eval_sex(body.car,e);
		}
		if (skip_nil && v == _G.nil) {} // skip
		else {
		    let last_elem = _G.cons(v, _G.nil);
		    if (hd == _G.nil) hd = tl = last_elem;
		    else {
			tl.cdr = last_elem;
			tl = last_elem;
		    }
		}
	    }
	}
	else if(ls instanceof Object) { // js連想配列
	    for(let k of Object.keys(ls)) {
		idx_v = _G.cons(k,ls[k]);
		let e = _G.cons(_G.cons(idx,idx_v),env);
		for(let body=args.cdr.cdr; body != _G.nil; body=body.cdr) {
		    v = _G.eval.eval_sex(body.car,e);
		}
		if (skip_nil && v == _G.nil) {} // skip
		else {
		    let last_elem = _G.cons(v, _G.nil);
		    if (hd == _G.nil) hd = tl = last_elem;
		    else {
			tl.cdr = last_elem;
			tl = last_elem;
		    }
		}
	    }
	}
	else throw "foreach is not applicable here";

	return hd;
    }
    this.add_top_env(this.intern("foreach"), new Subr(_foreach));
    // ----------
    function _intern(args, env)
    {
	check_args(args,1,1);
	var a = _G.eval.eval_sex(args.car,env);
	return _G.intern(a);
    }
    this.add_top_env(this.intern("intern"), new Subr(_intern));
    // ----------
    function _clone(args, env)
    {
	check_args(args,2,2);
	var x = _G.eval.eval_sex(args.car,env);
	var y = _G.eval.eval_sex(args.cdr.car,env);
	if(! (x instanceof Sym && y instanceof Sym)) throw "clone needs 2 symbols";

	var xx = Object.create(x);
	xx.name = y.name;
	_G.overwrite(xx);
	return xx;
    }
    this.add_top_env(this.intern("clone"), new Subr(_clone));
    // ----------
    function _range(args, env)
    {
	check_args(args,1,2);
	var vlist = _G.eval.eval_list(args,env);

	var x1, x2;
	x1 = x2 = 0;
	switch(llength(vlist)) {
	case 1:
	    x2 = vlist.car;
	    break;
	case 2:
	    x1 = vlist.car;
	    x2 = vlist.cdr.car;
	    break;
	}
	var r = _G.nil;
	for(var i=x2-1; i>=x1; i--) {
	    r = _G.cons(i, r);
	}
	return r;
    }
    this.add_top_env(this.intern("range"), new Subr(_range));
    // ----------
    function _prln(args, env)
    {
	_pr(args,env);
	sock_send('\n');
	return _G.nil;
    }
    this.add_top_env(this.intern("prln"), new Subr(_prln));
    // ----------
    function _prstr(args, env)
    {
	return _2string(args,env,true);
    }
    this.add_top_env(this.intern("prstr"), new Subr(_prstr));
    // ----------
    function _pr(args, env)
    {
	sock_send(_2string(args,env,true));
	return _G.nil;
    }
    this.add_top_env(this.intern("pr"), new Subr(_pr));
    // ***
    function _2string(args, env, debug=false) {
	var vlist = _G.eval.eval_list(args,env);
	var buff = "";
	for(; vlist != _G.nil; vlist=vlist.cdr) {
	    if(typeof(vlist.car) == "string") buff += vlist.car;
	    else buff += _G.printer.print_sex(vlist.car,debug);
	}
	return buff;
    }
    // ----------
    function _stringify(args, env) {
	check_args(args,1);
	var a = _G.eval.eval_sex(args.car,env);
	if(a instanceof Sym) return a.name;
	var b = a.toString();
	return b;
    }
    this.add_top_env(this.intern("string<"), new Subr(_stringify));
    // ----------
    function _null(args, env) {
	check_args(args,1);
	var a = _G.eval.eval_sex(args.car,env);
	if(a == _G.nil) return _G.sym_true;
	else return _G.nil;
    }
    this.add_top_env(this.intern("null"), new Subr(_null));
    // ----------
    function _2num(args, env) {
	check_args(args,1);
	var a = _G.eval.eval_sex(args.car,env);
	if(typeof a == "string" || a instanceof String)
	    a = a.replace(/,/g, "");
	return Number(a);
    }
    this.add_top_env(this.intern("num<"), new Subr(_2num));
    // ----------
    function _with(args, env) {
	check_args(args,1);
	var ee = _G.eval.eval_sex(args.car,env);
	if(ee instanceof Cons) {
	    ee = append(ee,env);
	    var x = _G.nil;
	    for(args=args.cdr; args != _G.nil; args=args.cdr)
		x = _G.eval.eval_sex(args.car, ee);
	    return x;
	}
	throw("non alist is spplied for with");
    }
    this.add_top_env(this.intern("with"), new Subr(_with));
    this.add_top_env(this.intern("w/"), new Subr(_with));
    // ----------
    this.X = null;

} // _G

// ************************************************************
// * chat_serverが無ければwebsockで対話的リスプ環境を開かない *
// ************************************************************
var sock = null;
var sock_ready = false;
if(chat_server) sock = new WebSocket(chat_server); // lisp chat server addr

// socketが生成できた時
if(sock) { 
    sock.addEventListener('open',function(e){
	console.log('Socket 接続成功');
	sock_ready = true;
	sock_send(";;; *** welcome to LispASS ***\n");
	sock_send(_G.pwp(null,null) + "=> ");
    });

    sock.addEventListener('error',function(e){
	sock_ready = false;
	console.error('Socket エラー発生');
    });
    
    sock.addEventListener('close',function(e){
	sock_ready = false;
	console.log('Socket 切断');
	console.warn('おそらく既に別のlispが起動中');
    });
}

function sock_send(s) {
    if(sock_ready) sock.send(s);
    else {
	// 非対話時や、socket初期化前、エラー時にはconsole出力
	// しかしあまり表示が多く鬱陶しい
	// console.log(s);
    }
}

function start_lisp(predef_buff) {
    var sock_buff = new _G.ExprBuff(predef_buff); // 初期化lispコードを渡す

    // 初期化lispコード実行後に
    // チャットサーバーからデータを受け取るハンドラーをセット
    if(sock)
	sock.addEventListener('message',function(e){
	    sock_buff.addData(e.data);
	});
}

// ******************************
// * その他のユーティリティ関数 *
// ******************************
function zenkana2Hankana(str) {
    var kanaMap = {
        "ガ": "ｶﾞ", "ギ": "ｷﾞ", "グ": "ｸﾞ", "ゲ": "ｹﾞ", "ゴ": "ｺﾞ",
        "ザ": "ｻﾞ", "ジ": "ｼﾞ", "ズ": "ｽﾞ", "ゼ": "ｾﾞ", "ゾ": "ｿﾞ",
        "ダ": "ﾀﾞ", "ヂ": "ﾁﾞ", "ヅ": "ﾂﾞ", "デ": "ﾃﾞ", "ド": "ﾄﾞ",
        "バ": "ﾊﾞ", "ビ": "ﾋﾞ", "ブ": "ﾌﾞ", "ベ": "ﾍﾞ", "ボ": "ﾎﾞ",
        "パ": "ﾊﾟ", "ピ": "ﾋﾟ", "プ": "ﾌﾟ", "ペ": "ﾍﾟ", "ポ": "ﾎﾟ",
        "ヴ": "ｳﾞ", "ヷ": "ﾜﾞ", "ヺ": "ｦﾞ",
        "ア": "ｱ", "イ": "ｲ", "ウ": "ｳ", "エ": "ｴ", "オ": "ｵ",
        "カ": "ｶ", "キ": "ｷ", "ク": "ｸ", "ケ": "ｹ", "コ": "ｺ",
        "サ": "ｻ", "シ": "ｼ", "ス": "ｽ", "セ": "ｾ", "ソ": "ｿ",
        "タ": "ﾀ", "チ": "ﾁ", "ツ": "ﾂ", "テ": "ﾃ", "ト": "ﾄ",
        "ナ": "ﾅ", "ニ": "ﾆ", "ヌ": "ﾇ", "ネ": "ﾈ", "ノ": "ﾉ",
        "ハ": "ﾊ", "ヒ": "ﾋ", "フ": "ﾌ", "ヘ": "ﾍ", "ホ": "ﾎ",
        "マ": "ﾏ", "ミ": "ﾐ", "ム": "ﾑ", "メ": "ﾒ", "モ": "ﾓ",
        "ヤ": "ﾔ", "ユ": "ﾕ", "ヨ": "ﾖ",
        "ラ": "ﾗ", "リ": "ﾘ", "ル": "ﾙ", "レ": "ﾚ", "ロ": "ﾛ",
        "ワ": "ﾜ", "ヲ": "ｦ", "ン": "ﾝ",
        "ァ": "ｧ", "ィ": "ｨ", "ゥ": "ｩ", "ェ": "ｪ", "ォ": "ｫ",
        "ッ": "ｯ", "ャ": "ｬ", "ュ": "ｭ", "ョ": "ｮ",
        "。": "｡", "、": "､", "ー": "ｰ", "「": "｢", "」": "｣", "・": "･",
	"．": ".",
	"・": "/",
	"０": "0",
	"１": "1",
	"２": "2",
	"３": "3",
	"４": "4",
	"５": "5",
	"６": "6",
	"７": "7",
	"８": "8",
	"９": "9",
	"−": "-"
    }
    var reg = new RegExp('(' + Object.keys(kanaMap).join('|') + ')', 'g');
    return str
        .replace(reg, function (match) {
            return kanaMap[match];
        })
        .replace(/゛/g, 'ﾞ')
        .replace(/゜/g, 'ﾟ');
}
