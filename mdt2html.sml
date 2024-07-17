(* Removing tags from front of line *)
fun tags_helper([]) = ([],[])
  | tags_helper(x::xs) = 
  if size x = 0 then tags_helper(xs)
  else if hd(String.explode(x)) = #"<" andalso hd(rev(String.explode(x))) = #">"
  then let val sub = tags_helper(xs) in (x::(#1 sub),#2 sub) end 
  else ([],x::xs)

fun tags([]) = ([],[],[])
  | tags(x::y) = 
    let val f = tags_helper((x::y))
        val l = tags_helper((rev(#2 f)))
        val ftags = #1 f
        val ltags = rev(#1 l)
        val text = rev(#2 l)
    in (ftags,text,ltags)
    end

(* 1. Code blocks *)
fun space_count([]) = 0
  | space_count(x::xs) = if x = #" " then 1 + space_count(xs) else if x = #"\t" then 4 + space_count(xs) else 0

fun replace_in_cb([]) = []
  | replace_in_cb(x::xs) = 
    if x = #"*" orelse x = #"_" orelse x = #"-" orelse x = #"#" then Char.chr(92)::x::replace_in_cb(xs)
    else if x = #"<" then String.explode("&lt;")@replace_in_cb(xs)
    else if x = #">" then String.explode("&gt;")@replace_in_cb(xs)
    else x::replace_in_cb(xs)

fun codeblock(line,code) =
    let val l = String.explode(line)
        val count = space_count(l)
    in 
        if count >= 8 then
            if code then (true,String.implode(replace_in_cb(l)))
            else (true,"<pre><code>&nbsp;"^String.implode(replace_in_cb(l)))
        else 
            if code then (false,"</code></pre>\n "^String.implode(l))
            else (false,String.implode(l))
    end 
    
(* Input *)
fun data(file,code) = 
    let val line = TextIO.inputLine file
    in 
        if isSome(line) then 
        let val p = codeblock(valOf(line),code)
            val new_code = #1 p
            val new_line = #2 p
        in String.tokens (fn c => c = #" ") (new_line)::data(file,new_code)
        end
        else let val temp = TextIO.closeIn file in if code then [["</code></pre>\n"]] else [] : string list list end
    end 

(* Output *)
fun out_concat([]) = []
  | out_concat([x]) = String.explode(x)
  | out_concat(x::y::xs) = String.explode(x) @ [#" "] @ out_concat(y::xs)

fun string_concat([]) = []
  | string_concat(x::xs) = String.implode(out_concat(x))::string_concat(xs)

(* 2. Tables *)
fun escape_helper([]) = []
  | escape_helper([x]) = if Char.ord(x) = 92 then [] else [x]
  | escape_helper(x::xs) = if Char.ord(x) = 92 then escape_helper(tl(xs)) else x::escape_helper(xs)

fun table_end ([]) = ([],[])
  | table_end ([]::xs) = table_end(xs)
  | table_end((x::y)::xs) = 
    let val sub = table_end(xs) 
        val l = String.explode(x)
        val s = if not(null l) andalso hd(rev(l)) = #"\n" then rev(tl(rev(l))) else l
    in 
        if s = [#">",#">"] then ([x::y],xs) 
        else ((x::y)::(#1 sub),(#2 sub))
    end

fun table_cols([]) = ""
  | table_cols(x::xs) = 
    let val l = String.tokens (fn c => c = #"\n") (x)
    in 
        if not(null l) then "<TD>"^(hd (l))^"</TD>"^table_cols(xs)
        else table_cols(xs)
    end

fun table_rows([]) = []:string list list
  | table_rows(x::xs) = 
    if not(null(x)) then 
        let val l = escape_helper(out_concat(x))
        in ["<TR>"^table_cols(String.tokens (fn c => c = #"|")(String.implode(l)))^"</TR>\n"] :: table_rows(xs)
        end
    else table_rows(xs)

fun table([]) = []:string list list
  | table([]::xs) = []::table(xs)
  | table((x::y)::xs) =
    let val l = String.explode(x)
        val s = if not(null l) andalso hd(rev(l)) = #"\n" then rev(tl(rev(l))) else l
    in
        if s = [#"<",#"<"]
        then 
            let val p = table_end((x::y)::xs)
                val elements = rev(tl(rev(tl(#1 p))))
                val tb = rev(["</TABLE></CENTER>\n"]::rev(["<CENTER><TABLE border='1'>\n"]::table_rows(elements)))
            in tb @ table(#2 p)
            end
        else (x::y)::table(xs)
    end

(* 3. Blockquote *)
fun blockquote_helper([]) = []
  | blockquote_helper(p::q) = 
    let val l = String.explode(p)
    in 
        if null l then blockquote_helper(q)
        else if hd(l) = #">" then 
        rev("</blockquote>" :: rev("<blockquote>" :: blockquote_helper(String.implode(tl l)::q))) 
        else p::q
    end

fun blockquote([]) = []:string list list  
  | blockquote([]::xs) = blockquote(xs)
  | blockquote((x::y)::xs) = 
    if size x = 0 then blockquote(y::xs)
    else if hd(String.explode(x)) = #">"
    then blockquote_helper(x::y) :: blockquote(xs)
    else (x::y) :: blockquote(xs)

(* 4. Headings *)
fun hashcount([]) = 0
  | hashcount(x::xs) = if x = #"#" then 1 + hashcount(xs) else 0

fun removehash([]) = ""
  | removehash(x::xs) = if x = #"#" then removehash(xs) else String.implode(x::xs)

fun headings([]) = []:string list list  
  | headings([]::xs) = headings(xs)
  | headings((x::y)::xs) = 
    let val t = tags((x::y))
        val text = #2 t
    in 
        if null text then (x::y)::xs
        else if size (hd text) = 0 then headings(y::xs)
        else if hd (String.explode(hd text)) = #"#"
        then
            let val hsize = hashcount (String.explode(hd text))
                val first = removehash(String.explode(hd text))
                val s = Int.toString(hsize)
            in ((#1 t) @ ["<h"^s^">"] @ (first :: tl(text)) @ ["</h"^s^">"] @ (#3 t)) :: headings(xs)
            end 
        else ((#1 t) @ text @ (#3 t)) :: headings(xs)
    end 

(* 5.Lists and paragraphs *)
fun remove_lead_tabs([]) = []
  | remove_lead_tabs(x::xs) = if x = #"\t" orelse x = #" " then remove_lead_tabs(xs) else x::xs

fun numbered([]) = false
  | numbered(x::xs) =
    let val n = ord(x) - ord(#"0")
        val gt = n>=0
        val lt = n<10
        val is_int = gt andalso lt
    in 
        if is_int then numbered(xs)
        else if x = #"." then true 
        else false
    end 

fun is_li_helper([]) = false 
  | is_li_helper([#"-"]) = true
  | is_li_helper(x::xs) = 
    let val n = ord(x) - ord(#"0") 
        val gt = n>=0
        val lt = n<10
        val is_int = gt andalso lt
    in is_int andalso numbered(xs) 
    end 

fun is_li(x) = 
    let val p = tags(x)
        val text = #2 p
    in 
        if not(null text) andalso size(hd(text)) > 0 andalso is_li_helper(String.explode(hd text)) then
            if hd(String.explode(hd text)) = #"-" then SOME "ul" else SOME "ol"
        else NONE
    end 

fun paragraph_list([],ol,ul,false) = 
    if ul then [["</p></ul><p>"]] else if ol then [["</p></ol><p>"]] else []:string list list
  | paragraph_list([],ol,ul,true) = 
    if ul then [["</li>\n</p></ul><p>"]] else if ol then [["</li>\n</p></ol><p>"]] else []:string list list
  | paragraph_list(x::xs,ol,ul,li) = 
    if String.implode(remove_lead_tabs(out_concat(x))) = "\n" then 
        if li then ["</li>\n</p>\n<p>\n"]::paragraph_list(xs,ol,ul,false) 
        else ["</p>\n<p>\n"]::paragraph_list(xs,ol,ul,false)
    else
        let val flag = is_li(x)
        in 
            if not(isSome(flag)) then
                if li then x::paragraph_list(xs,ol,ul,true)
                else if ul then ("</p></ul>\n<p>\n"::x)::paragraph_list(xs,false,false,false)
                else if ol then ("</p></ol>\n<p>\n"::x)::paragraph_list(xs,false,false,false)
                else x::paragraph_list(xs,false,false,false)
            else if valOf(flag) = "ul" then 
                if ol then ("</p></ol>\n<ul><p><li>\n"::tl(x))::paragraph_list(xs,false,true,true)
                else if ul then ("<li>\n"::tl(x))::paragraph_list(xs,false,true,true)
                else ("</p><ul>\n<p><li>\n"::tl(x))::paragraph_list(xs,false,true,true)
            else 
                if ul then ("</p></ul>\n<ol><p><li>\n"::tl(x))::paragraph_list(xs,true,false,true)
                else if ol then ("<li>\n"::tl(x))::paragraph_list(xs,true,false,true)
                else ("</p><ol>\n<p><li>\n"::tl(x))::paragraph_list(xs,true,false,true)
        end

(* 6. Horizontal rule *)
fun hyphen_count([]) = (0,[])
  | hyphen_count(x::xs) = if x = #"-" then let val sub = hyphen_count(xs) in (1 + (#1 sub),#2 sub) end else (0,x::xs)

fun horizontal_rule([]) = []
  | horizontal_rule([x]) = if Char.ord(x) = 92 then [] else [x]
  | horizontal_rule(#"-"::xs) = 
    let val p = hyphen_count(#"-"::xs)
        val c = #1 p
        val l = #2 p
    in
        if c >= 3 then String.explode("<hr>")@horizontal_rule(l)
        else if c = 2 then String.explode("--")@horizontal_rule(l)
        else #"-"::horizontal_rule(l)
    end
  | horizontal_rule(x::xs) = if Char.ord(x) = 92 then Char.chr(92)::hd(xs)::horizontal_rule(tl(xs)) else x::horizontal_rule(xs) 

(* 7. Bold and italics *)
fun bold_italics([],bold,italics) = [] 
  | bold_italics([x],bold,italics) = if Char.ord(x) = 92 then [] else [x]
  | bold_italics(x::xs,bold,italics) = 
    if Char.ord(x) = 92 then Char.chr(92)::hd(xs)::bold_italics(tl(xs),bold,italics)
    else if x = #"*" then
        if null xs then []
        else if hd xs = #"*" then
            if bold then String.explode("</strong>")@bold_italics(tl xs,false,italics)
            else String.explode("<strong>")@bold_italics(tl xs,true,italics)
        else 
            if italics then String.explode("</em>")@bold_italics(xs,bold,false)
            else String.explode("<em>")@bold_italics(xs,bold,true)
    else x::bold_italics(xs,bold,italics)

(* 8. Underline *)
fun underscore_count([]) = (0,[],[])
  | underscore_count(x::xs) =
    if x = #" " then (0,[],x::xs)
    else if x = #"_" then let val sub = underscore_count(xs) in (1 + (#1 sub),x::(#2 sub),#3 sub) end
    else let val sub = underscore_count(xs) in (#1 sub,x::(#2 sub),#3 sub) end

fun underline_helper([],i,count) = []
  | underline_helper(x::xs,i,count) =
    if x = #"_" then
        if i = 0 then String.explode("<u>")@underline_helper(xs,i+1,count)
        else if i = count - 1 then String.explode("</u>")@underline_helper(xs,i+1,count)
        else #" "::underline_helper(xs,i+1,count)
    else x::underline_helper(xs,i,count)

fun underline([]) = []
  | underline([x]) = if Char.ord(x) = 92 then [] else [x]
  | underline(x::xs) = 
    if Char.ord(x) = 92 then Char.chr(92)::hd(xs)::underline(tl(xs))
    else if x = #"_" then 
        let val p = underscore_count(x::xs)
            val count = #1 p 
            val ul = #2 p
            val rest = #3 p
        in 
            if count > 1 then
                underline_helper(ul,0,count)@underline(rest)
            else ul@underline(rest)
        end
    else x::underline(xs)

(* 9. Hyperlinks *)
fun bracket_close(ch:char,[]:char list) = ([],[])
  | bracket_close(ch:char,x::xs:char list) = 
    if x = ch then ([x],xs) else let val sub = bracket_close(ch,xs) in (x::(#1 sub),#2 sub) end

fun hyperlink([]) = [] 
  | hyperlink([x]) = if Char.ord(x) = 92 then [] else [x]
  | hyperlink(x::xs) =  
    if Char.ord(x) = 92 then Char.chr(92)::hd(xs)::hyperlink(tl(xs))
    else if x = #"[" then
        let val p = bracket_close(#"]",x::xs)
            val name = rev(tl(rev(tl(#1 p))))
            val q = if hd(#2 p) = #"(" then bracket_close(#")",#2 p) else ([],#2 p)
            val url = #1 q
            val rest = #2 q
        in 
            if not(null url) andalso String.isPrefix("(http")(String.implode(url)) andalso hd(rev(url)) = #")" then
            String.explode("<a href = '") @ rev(tl(rev(tl(url)))) @ String.explode("'>") @ name @ String.explode("</a>") @ hyperlink(rest)
            else (#1 p) @ url @ hyperlink(rest)
        end
    else if x = #"<" then
        let val p = bracket_close(#">",x::xs)
            val url = #1 p
            val rest = #2 p
        in 
            if not(null url) andalso String.isPrefix("<http")(String.implode(url)) andalso hd(rev(url)) = #">" then
            String.explode("<a href = '") @ rev(tl(rev(tl(url)))) @ String.explode("'>") @ rev(tl(rev(tl(url)))) @ String.explode("</a>") @ hyperlink(rest)
            else url @ hyperlink(rest)
        end
    else x::hyperlink(xs)

(* 10. Escape characters *)
fun escape([]) = []
  | escape([x]) = if Char.ord(x) = 92 then [] else [x]
  | escape(x::xs) = if Char.ord(x) = 92 then hd(xs)::escape(tl(xs)) else x::escape(xs)

fun mdt2html(filename:string) = 
    let
        val input_file = TextIO.openIn (filename^".mdt")
        val output_file = TextIO.openOut (filename^".html")
        val html_open = TextIO.output(output_file,"<html>\n<body>\n<p>\n")
        val filetext = data(input_file,false)
        val intermediate_output = out_concat(string_concat(paragraph_list(headings(blockquote(table(filetext))),false,false,false)))
        val output_to_write = escape(bold_italics(underline(horizontal_rule(hyperlink(intermediate_output))),false,false))
        val write = TextIO.output(output_file,String.implode(output_to_write))
        val html_close = TextIO.output(output_file,"</p>\n</body>\n</html>\n")
    in  TextIO.closeOut output_file
    end