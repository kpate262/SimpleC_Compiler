and private assignment (tokens, program) = 
    (tokens, program)
    |> matchToken lexer.Tokens.ID
    |> matchToken lexer.Tokens.Assign
    |> expr 
    |> matchToken lexer.Tokens.Semicolon
    
  
  and private ifstmt (tokens, program) = 
    (tokens, program)
    |> matchToken lexer.Tokens.If
    |> matchToken lexer.Tokens.OpenParen
    |> condition 
    |> matchToken lexer.Tokens.CloseParen
    |> then_part
    |> else_part
  
  
  and private condition (tokens, program) = 
    expr (tokens, program)
  
  
  and private then_part (tokens, program) = 
    stmt (tokens, program)
    
  and private else_part (tokens, program) =
    match tokens with
    | [] -> ([], program)
    | hd::_ when lexer.Tokens.Semicolon = (fst hd) -> empty (tokens, program)
    | hd::_ -> let (tail, pm) = matchToken lexer.Tokens.Else (tokens, program)
               stmt (tail, pm)
  
  and private output_value (tokens, program) =
    let (token, _) = List.head tokens
    
    if lexer.Tokens.Endl = token then matchToken lexer.Tokens.Endl (tokens, program)
    else expr_value (tokens, program)
    
  
  and private expr (tokens, program) = 
    let (tail, pm) = expr_value (tokens, program)
    let (tailToken, _) = List.head tokens
    
    if lexer.Tokens.Semicolon = tailToken then (tail, pm)
    else (tail, pm)
         //|> expr_op
         |> expr_value
    
  
  and private expr_value (tokens, program) = 
    let (token, _) = List.head tokens
    
    if lexer.Tokens.ID = token then matchToken lexer.Tokens.ID (tokens, program)
    else if lexer.Tokens.Int_Literal = token 
         then matchToken lexer.Tokens.Int_Literal (tokens, program)
         else if lexer.Tokens.Str_Literal = token 
              then matchToken lexer.Tokens.Str_Literal (tokens, program)
              else if lexer.Tokens.Bool_Literal = token 
                   then matchToken lexer.Tokens.Bool_Literal (tokens, program)
                   else failwith ("expecting expression value, but found " + (string token))
  
  
  and private expr_op (tokens, program) = 
    match tokens with
    | [] -> ([], program)
    | hd::_ when lexer.Tokens.Plus = (fst hd) -> matchToken lexer.Tokens.Plus (tokens, program)
    | hd::_ when lexer.Tokens.Minus = (fst hd) -> matchToken lexer.Tokens.Minus (tokens, program)
    | hd::_ when lexer.Tokens.Times = (fst hd) -> matchToken lexer.Tokens.Times (tokens, program)
    | hd::_ when lexer.Tokens.Divide = (fst hd) -> matchToken lexer.Tokens.Divide (tokens, program)
    | hd::_ when lexer.Tokens.LT = (fst hd) -> matchToken lexer.Tokens.LT (tokens, program)
    | hd::_ when lexer.Tokens.LTE = (fst hd) -> matchToken lexer.Tokens.LTE (tokens, program)
    | hd::_ when lexer.Tokens.GT = (fst hd) -> matchToken lexer.Tokens.GT (tokens, program)
    | hd::_ when lexer.Tokens.GTE = (fst hd) -> matchToken lexer.Tokens.GTE (tokens, program)
    | hd::_ when lexer.Tokens.EQ = (fst hd) -> matchToken lexer.Tokens.EQ (tokens, program)
    | hd::_ when lexer.Tokens.Power = (fst hd) -> matchToken lexer.Tokens.Power (tokens, program)
    | hd::_ when lexer.Tokens.NE = (fst hd) -> matchToken lexer.Tokens.NE (tokens, program)
    | _::_ -> failwith ("expecting valid operator, but found " + (string token))
    