(* json-stream-parser.sml
 *
 * COPYRIGHT (c) 2008 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure JSONStreamParser : sig

  (* callback functions for the different parsing events *)
    type 'a pos = ('a * AntlrStreamPos.sourcemap * AntlrStreamPos.span)
    type 'ctx callbacks = {
	null : 'ctx pos -> 'ctx,
	boolean : 'ctx pos * bool -> 'ctx,
	integer : 'ctx pos * IntInf.int -> 'ctx,
	float : 'ctx pos * real -> 'ctx,
	string : 'ctx pos * string -> 'ctx,
	startObject : 'ctx pos -> 'ctx,
	objectKey : 'ctx pos * string -> 'ctx,
	endObject : 'ctx pos -> 'ctx,
	startArray : 'ctx pos -> 'ctx,
	endArray : 'ctx pos -> 'ctx,
	error : 'ctx pos * string -> 'ctx
      }

    val parse : 'ctx callbacks -> (TextIO.instream * 'ctx) -> 'ctx

    val parseFile : 'ctx callbacks -> (string * 'ctx) -> 'ctx

  end = struct

    structure Lex = JSONLexer
    structure T = JSONTokens

  (* callback functions for the different parsing events *)
    type 'a pos = ('a * AntlrStreamPos.sourcemap * AntlrStreamPos.span)
    type 'ctx callbacks = {
	null : 'ctx pos -> 'ctx,
	boolean : 'ctx pos * bool -> 'ctx,
	integer : 'ctx pos * IntInf.int -> 'ctx,
	float : 'ctx pos * real -> 'ctx,
	string : 'ctx pos * string -> 'ctx,
	startObject : 'ctx pos -> 'ctx,
	objectKey : 'ctx pos * string -> 'ctx,
	endObject : 'ctx pos -> 'ctx,
	startArray : 'ctx pos -> 'ctx,
	endArray : 'ctx pos -> 'ctx,
	error : 'ctx pos * string -> 'ctx
      }

    fun error (cb : 'a callbacks, ctx, msg) = (
	  #error cb (ctx, msg);
	  raise Fail "error")

    fun parser (cb : 'a callbacks) (srcMap, inStrm, ctx) = let
          val smap = AntlrStreamPos.mkSourcemap ()
	  val lexer = Lex.lex smap
	  fun parseValue (strm : Lex.strm, ctx) = let
		val (tok, pos, strm) = lexer strm
                val ctx = (ctx, smap, pos)
		in
		  case tok
		   of T.LB => parseArray (strm, #startArray cb ctx)
		    | T.LCB => parseObject (strm, #startObject cb ctx)
		    | T.KW_null => (strm, #null cb ctx)
		    | T.KW_true => (strm, #boolean cb (ctx, true))
		    | T.KW_false => (strm, #boolean cb (ctx, false))
		    | T.INT n => (strm, #integer cb (ctx, n))
		    | T.FLOAT f => (strm, #float cb (ctx, f))
		    | T.STRING s => (strm, #string cb (ctx, s))
		    | _ => error (cb, ctx, "error parsing value")
		  (* end case *)
		end
	  and parseArray (strm : Lex.strm, ctx) = (case lexer strm
		 of (T.RB, pos, strm) => (strm, #endArray cb (ctx, smap, pos))
		  | _ => let
		      fun loop (strm, ctx) = let
			    val (strm, ctx) = parseValue (strm, ctx)
			  (* expect either a "," or a "]" *)
			    val (tok, pos, strm) = lexer strm
			    in
			      case tok
			       of T.RB => (strm, #endArray cb (ctx,smap,pos))
				| T.COMMA => loop (strm, ctx)
				| _ => error (cb, (ctx,smap,pos), "error parsing array")
			      (* end case *)
			    end
		      in
			loop (strm, ctx)
		      end
		(* end case *))
	  and parseObject (strm : Lex.strm, ctx) = let
		fun parseField (strm, ctx) = (case lexer strm
		       of (T.STRING s, pos, strm) => let
			    val ctx = #objectKey cb ((ctx,smap,pos), s)
			    in
			      case lexer strm
			       of (T.COLON, _, strm) => parseValue (strm, ctx)
				| (_,pos,_) =>
                                    error (cb, (ctx,smap,pos), "error parsing field")
			      (* end case *)
			    end
			| _ => (strm, ctx)
		      (* end case *))
		fun loop (strm, ctx) = let
		      val (strm, ctx) = parseField (strm, ctx)
		      in
			(* expect either "," or "}" *)
			case lexer strm
			 of (T.RCB, pos, strm) => (strm, #endObject cb (ctx,smap,pos))
			  | (T.COMMA, pos, strm) => loop (strm, ctx)
			  | (_,pos,_) =>
                              error (cb, (ctx,smap,pos), "error parsing object")
			(* end case *)
		      end
		in
		  loop (strm, ctx)
		end
	  in
	    #2 (parseValue (Lex.streamifyInstream inStrm, ctx))
	  end

    fun parse cb = let
	  val parser = parser cb
	  fun parse' (inStrm, ctx) =
		parser(AntlrStreamPos.mkSourcemap (), inStrm, ctx)
	  in
	    parse'
	  end

    fun parseFile cb = let
	  val parser = parser cb
	  fun parse (fileName, ctx) = let
		val inStrm = TextIO.openIn fileName
		val ctx = parser (AntlrStreamPos.mkSourcemap' fileName, inStrm, ctx)
		      handle ex => (TextIO.closeIn inStrm; raise ex)
		in
		  TextIO.closeIn inStrm;
		  ctx
		end
	  in
	    parse
	  end

  end
