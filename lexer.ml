open TokenTypes

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)
let sanitize_string str =
    String.concat "" (String.split_on_char '"' str);;

let rec find_max lst curr_max = 
    match lst, curr_max with
    | [], (a, b) -> (a, b)
    | ((a, b) :: t), (c, d) -> if b > d then find_max t (a, b) else find_max t (c, d);;

let rec tok pos str =
    if pos >= String.length str then []
    else if pos = 0 then
        let matches = [] in
        let matches = if (Str.string_match (Str.regexp "true\\|false") str pos) then 
                    let token = Str.matched_string str in
                    let len = String.length token in
                    (Tok_Bool (bool_of_string token), len + 1) :: matches 
                else (Tok_DoubleSemi, -1) :: matches in
        let matches = if (Str.string_match (Str.regexp "[0-9]+") str pos) then 
                    let len = String.length (Str.matched_string str) in
                    let token = Str.matched_string str in
                    (Tok_Int (int_of_string token), len) :: matches 
                else (Tok_DoubleSemi, -1) :: matches in
        let matches = if (Str.string_match (Str.regexp "(-[0-9]+)") str pos) then 
                    let len = String.length (Str.matched_string str) in
                    let token = String.sub (Str.matched_string str) 1 (len - 2) in
                    (Tok_Int (int_of_string token), len) :: matches 
                else (Tok_DoubleSemi, -1) :: matches in
        let matches = if (Str.string_match (Str.regexp "\"[^\"]*\"") str pos) then 
                    (Tok_String (sanitize_string (Str.matched_string str)), String.length (Str.matched_string str)) :: matches 
                else (Tok_DoubleSemi, -1) :: matches in
        let matches = if (Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") str pos) then 
                        let token = Str.matched_string str in
                        let len = String.length token in
                        (Tok_ID (token), len) :: matches 
                else (Tok_DoubleSemi, -1) :: matches in
        let matches = if (Str.string_match (Str.regexp "(") str pos) then (Tok_LParen, 1) :: matches else (Tok_DoubleSemi, -1) :: matches in
        let matches = if (Str.string_match (Str.regexp ")") str pos) then (Tok_RParen, 1) :: matches else (Tok_DoubleSemi, -1) :: matches in
        let matches = if (Str.string_match (Str.regexp "=") str pos) then (Tok_Equal, 1) :: matches else (Tok_DoubleSemi, -1) :: matches in
        let matches = if (Str.string_match (Str.regexp "<>") str pos) then (Tok_NotEqual, 2) :: matches else (Tok_DoubleSemi, -1) :: matches in
        let matches = if (Str.string_match (Str.regexp ">") str pos) then (Tok_Greater, 1) :: matches else (Tok_DoubleSemi, -1) :: matches in
        let matches = if (Str.string_match (Str.regexp "<") str pos) then (Tok_Less, 1) :: matches else (Tok_DoubleSemi, -1) :: matches in
        let matches = if (Str.string_match (Str.regexp ">=") str pos) then (Tok_GreaterEqual, 2) :: matches else (Tok_DoubleSemi, -1) :: matches in
        let matches = if (Str.string_match (Str.regexp "<=") str pos) then (Tok_LessEqual, 2) :: matches else (Tok_DoubleSemi, -1) :: matches in
        let matches = if (Str.string_match (Str.regexp "||") str pos) then (Tok_Or, 2) :: matches else (Tok_DoubleSemi, -1) :: matches in
        let matches = if (Str.string_match (Str.regexp "&&") str pos) then (Tok_And, 2) :: matches else (Tok_DoubleSemi, -1) :: matches in
        let matches = if (Str.string_match (Str.regexp "not") str pos) then (Tok_Not, 3) :: matches else (Tok_DoubleSemi, -1) :: matches in
        let matches = if (Str.string_match (Str.regexp "if ") str pos) then (Tok_If, 2) :: matches else (Tok_DoubleSemi, -1) :: matches in
        let matches = if (Str.string_match (Str.regexp "then ") str pos) then (Tok_Then, 4) :: matches else (Tok_DoubleSemi, -1) :: matches in
        let matches = if (Str.string_match (Str.regexp "else ") str pos) then (Tok_Else, 4) :: matches else (Tok_DoubleSemi, -1) :: matches in
        let matches = if (Str.string_match (Str.regexp "+") str pos) then (Tok_Add, 1) :: matches else (Tok_DoubleSemi, -1) :: matches in
        let matches = if (Str.string_match (Str.regexp "-") str pos) then (Tok_Sub, 1) :: matches else (Tok_DoubleSemi, -1) :: matches in
        let matches = if (Str.string_match (Str.regexp "*") str pos) then (Tok_Mult, 1) :: matches else (Tok_DoubleSemi, -1) :: matches in
        let matches = if (Str.string_match (Str.regexp "/") str pos) then (Tok_Div, 1) :: matches else (Tok_DoubleSemi, -1) :: matches in
        let matches = if (Str.string_match (Str.regexp "\\^") str pos) then (Tok_Concat, 1) :: matches else (Tok_DoubleSemi, -1) :: matches in
        let matches = if (Str.string_match (Str.regexp "let ") str pos) then (Tok_Let, 3) :: matches else (Tok_DoubleSemi, -1) :: matches in
        let matches = if (Str.string_match (Str.regexp "def ") str pos) then (Tok_Def, 3) :: matches else (Tok_DoubleSemi, -1) :: matches in
        let matches = if (Str.string_match (Str.regexp "in ") str pos) then (Tok_In, 2) :: matches else (Tok_DoubleSemi, -1) :: matches in
        let matches = if (Str.string_match (Str.regexp "rec ") str pos) then (Tok_Rec, 3) :: matches else (Tok_DoubleSemi, -1) :: matches in
        let matches = if (Str.string_match (Str.regexp "fun ") str pos) then (Tok_Fun, 3) :: matches else (Tok_DoubleSemi, -1) :: matches in
        let matches = if (Str.string_match (Str.regexp "->") str pos) then (Tok_Arrow, 2) :: matches else (Tok_DoubleSemi, -1) :: matches in
        let matches = if (Str.string_match (Str.regexp ";;") str pos) then (Tok_DoubleSemi, 2) :: matches else (Tok_DoubleSemi, -1) :: matches in
        match (find_max matches (Tok_DoubleSemi, -1)) with
        | (a, b) -> a :: (tok (pos + b) str)
    else
        if (Str.string_match (Str.regexp "[0-9]+") str pos) then 
            let token = Str.matched_string str in
            let len = String.length token in
            Tok_Int (int_of_string token) :: (tok (pos + len) str) 
        else if (Str.string_match (Str.regexp "(-[0-9]+)") str pos) then 
            let len = String.length (Str.matched_string str) in
            let token = String.sub (Str.matched_string str) 1 (len - 2) in
            Tok_Int (int_of_string token) :: (tok (pos + len) str) 
        else if (Str.string_match (Str.regexp "(") str pos) then 
            Tok_LParen :: (tok (pos + 1) str)
        else if (Str.string_match (Str.regexp ")") str pos) then 
            Tok_RParen :: (tok (pos + 1) str)
        else if (Str.string_match (Str.regexp "=") str pos) then 
            Tok_Equal :: (tok (pos + 1) str)
        else if (Str.string_match (Str.regexp "<>") str pos) then
            Tok_NotEqual :: (tok (pos + 2) str)
        else if (Str.string_match (Str.regexp ">") str pos) then 
            Tok_Greater :: (tok (pos + 1) str) 
        else if (Str.string_match (Str.regexp "<") str pos) then
            Tok_Less :: (tok (pos + 1) str)
        else if (Str.string_match (Str.regexp ">=") str pos) then
            Tok_GreaterEqual :: (tok (pos + 2) str)
        else if (Str.string_match (Str.regexp "<=") str pos) then
            Tok_LessEqual :: (tok (pos + 2) str)
        else if (Str.string_match (Str.regexp "||") str pos) then 
            Tok_Or :: (tok (pos + 2) str)
        else if (Str.string_match (Str.regexp "&&") str pos) then 
            Tok_And :: (tok (pos + 2) str)
        else if (Str.string_match (Str.regexp "not ") str pos) then
            Tok_Not :: (tok (pos + 3) str)
        else if (Str.string_match (Str.regexp "if ") str pos) then
            Tok_If :: (tok (pos + 2) str)
        else if (Str.string_match (Str.regexp "then ") str pos) then
            Tok_Then :: (tok (pos + 4) str)
        else if (Str.string_match (Str.regexp "else ") str pos) then 
            Tok_Else :: (tok (pos + 4) str)
        else if (Str.string_match (Str.regexp "->") str pos) then 
            Tok_Arrow :: (tok (pos + 2) str) 
        else if (Str.string_match (Str.regexp "+") str pos) then
            Tok_Add :: (tok (pos + 1) str) 
        else if (Str.string_match (Str.regexp "-") str pos) then 
            Tok_Sub :: (tok (pos + 1) str)
        else if (Str.string_match (Str.regexp "*") str pos) then 
            Tok_Mult :: (tok (pos + 1) str)
        else if (Str.string_match (Str.regexp "/") str pos) then 
            Tok_Div :: (tok (pos + 1) str)
        else if (Str.string_match (Str.regexp "\\^") str pos) then 
            Tok_Concat :: (tok (pos + 1) str)
        else if (Str.string_match (Str.regexp "let ") str pos) then 
            Tok_Let :: (tok (pos + 3) str)
        else if (Str.string_match (Str.regexp "def ") str pos) then 
            Tok_Def :: (tok (pos + 3) str)
        else if (Str.string_match (Str.regexp "in ") str pos) then 
            Tok_In :: (tok (pos + 2) str)
        else if (Str.string_match (Str.regexp "rec ") str pos) then 
            Tok_Rec :: (tok (pos + 3) str) 
        else if (Str.string_match (Str.regexp "fun ") str pos) then 
            Tok_Fun :: (tok (pos + 3) str)
        else if (Str.string_match (Str.regexp ";;") str pos) then 
            Tok_DoubleSemi :: (tok (pos + 2) str) 
        else if (Str.string_match (Str.regexp "true\\|false") str pos) then 
            let token = Str.matched_string str in
            let len = String.length token in
            Tok_Bool (bool_of_string token) :: (tok (pos + len) str) 
        else if (Str.string_match (Str.regexp "\"[^\"]*\"") str pos) then 
            let token = sanitize_string (Str.matched_string str) in
            let len = String.length token in
            Tok_String (token) :: (tok (pos + len + 3) str)
        else if (Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") str pos) then 
            let token = Str.matched_string str in
            let len = String.length token in
            Tok_ID (token) :: (tok (pos + len) str)
        else if (Str.string_match (Str.regexp "\t\\|\n\\| ") str pos) then
            tok (pos + 1) str
        else
            raise (InvalidInputException "INVALID STRING");;

let tokenize input = tok 0 (String.trim input);;

