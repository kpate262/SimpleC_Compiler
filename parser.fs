//
// Parser for simple C programs.  This component checks 
// the input program to see if it meets the syntax rules
// of simple C.  The parser returns a tuple containing 
// 3 values:  
//
//   (result, msg, program)
//
// where result is true or false (legal or not legal), 
// msg is a success or syntax error message, and program
// is a list of instructions if parsing was successful.
//
// <<Kisan Patel>>
// U. of Illinois, Chicago
// CS 341, Spring 2019
// Project #05
//

#light

namespace compiler

module parser =
  //
  // NOTE: all functions in the module must be indented.
  //

  //
  // These are debug routines that output the tokens, or
  // program, respectively.  They are written so you can
  // inject these into a pipeline to output the current 
  // state of the tokens or program.
  //
  let private __outputTokens (tokens, program) =
    printfn "Tokens: %A" tokens
    (tokens, program)

  let private __outputProgram (tokens, program) =
    printfn "Program: %A" program
    (tokens, program)


  //
  // matchToken
  //
  let private matchToken expected_token (tokens, program) =
    let (token, _) = List.head tokens
    //
    // if the token matches the expected token, keep parsing by
    // returning the rest of the tokens.  Otherwise throw an
    // exception because there's a syntax error, effectively 
    // stopping compilation:
    //
    if expected_token = token then  
      (List.tail tokens, program)
    else
      failwith ("expecting " + (string expected_token) + ", but found " + (string token))
 
  
 // let rec private stmts (tokens, program) =
 //   stmt (tokens, program)
  
  let rec private morestmts (tokens, program) =
    let (tail, pm) = stmt (tokens, program)
    let (tailToken, _) = List.head tail
    if lexer.Tokens.CloseBrace = tailToken then (tail, pm)
    else morestmts (tail, pm)
  
  
  and private else_part (tokens, program) =
    match tokens with
    | [] -> ([], program)
    | hd::_ when lexer.Tokens.Semicolon = (fst hd) -> empty (tokens, program)
    | hd::_ -> let (tail, pm) = matchToken lexer.Tokens.Else (tokens, program)
               stmt (tail, pm)
  
  and private then_part (tokens, program) = 
    stmt (tokens, program)
  
  and private condition (tokens, program) = 
    expr (tokens, program)
  
  and private ifstmt (tokens, program) = 
    (tokens, program)
    |> matchToken lexer.Tokens.If
    |> matchToken lexer.Tokens.OpenParen
    |> condition 
    |> matchToken lexer.Tokens.CloseParen
    |> then_part
    |> else_part
  
  
  and private stmt (tokens, program) =
   // match tokens with
   // | [] -> ([], program)
    //| hd::_ when lexer.Tokens.CloseBrace = (fst hd) -> (tokens, program)
   // | hd::_ when lexer.Tokens.Int = (fst hd) -> vardecl (tokens, program)
   // | hd::_ when lexer.Tokens.Cin = (fst hd) -> input (tokens, program)
    //| hd::_ when lexer.Tokens.Semicolon = (fst hd) -> empty (tokens, program)
    //| hd::_ when lexer.Tokens.Output = (fst hd) -> output (tokens, program)
   // | hd::_ when lexer.Tokens.Assign = (fst hd) -> assignment (tokens, program)
   // | hd::_  -> ifstmt (tokens, program)
    
  
  
    let (token, _) = List.head tokens
    
    if lexer.Tokens.Int = token then vardecl (tokens, program)
    else if lexer.Tokens.Cin = token then input (tokens, program)
         else if lexer.Tokens.Semicolon = token then empty (tokens, program)
              else if lexer.Tokens.Cout = token then output (tokens, program)
                   else if lexer.Tokens.ID = token then assignment (tokens, program)
                        else if lexer.Tokens.If = token then ifstmt (tokens, program)
                             else failwith ("expecting valid expression, but found " + (string token))
                                  
  
  and private stmts (tokens, program) =
    let (tail, pm) = stmt (tokens, program)
    let (tailToken, _) = List.head tail
    if lexer.Tokens.CloseBrace = tailToken then (tail, pm)
    else morestmts (tail, pm)
    
  and private empty (tokens, program) =
    matchToken lexer.Tokens.Semicolon (tokens, program)
    
       
  and private vardecl (tokens, program) =
    (tokens, program)
    |> matchToken lexer.Tokens.Int 
    |> matchToken lexer.Tokens.ID
    |> matchToken lexer.Tokens.Semicolon

  and private input (tokens, program) =
    (tokens, program)
    |> matchToken lexer.Tokens.Cin
    |> matchToken lexer.Tokens.Input
    |> matchToken lexer.Tokens.ID
    |> matchToken lexer.Tokens.Semicolon

  and private output (tokens, program) =
    (tokens, program)
    |> matchToken lexer.Tokens.Cout
    |> matchToken lexer.Tokens.Output
    |> output_value
    |> matchToken lexer.Tokens.Semicolon
    
  and private output_value (tokens, program) =
    let (token, _) = List.head tokens
    
    if lexer.Tokens.Endl = token then matchToken lexer.Tokens.Endl (tokens, program)
    else expr_value (tokens, program)
    
    
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
  
  and private assignment (tokens, program) = 
    (tokens, program)
    |> matchToken lexer.Tokens.ID
    |> matchToken lexer.Tokens.Assign
    |> expr 
    |> matchToken lexer.Tokens.Semicolon
  
  and private expr (tokens, program) = 
    let (tail, pm) = expr_value (tokens, program)
    //(tail, pm)
    let (tailToken, _) = List.head tail
    
    if lexer.Tokens.Semicolon = tailToken then (tail, pm)
    else (tail, pm)
         |> expr_op
         |> expr_value
  
  
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
    | hd::_ -> failwith ("expecting valid operator, but found " + (string (fst hd)))
  
  
  
  
  
  //
  // simpleC
  // 
  let private simpleC (tokens, program) = 
    let (T1, P1) = matchToken lexer.Tokens.Void (tokens, program)
    let (T2, P2) = matchToken lexer.Tokens.Main (T1, P1)
    let (T3, P3) = matchToken lexer.Tokens.OpenParen (T2, P2)
    let (T4, P4) = matchToken lexer.Tokens.CloseParen (T3, P3)
    let (T5, P5) = matchToken lexer.Tokens.OpenBrace (T4, P4)
    let (T6, P6) = stmts (T5,P5)
    let (T7, P7) = matchToken lexer.Tokens.CloseBrace (T6, P6)
    let (T8, P8) = matchToken lexer.Tokens.EOF (T7, P7)
    (T8, P8)
  //
  // parse tokens
  //
  // Given a list of tokens, parses the list and determines
  // if the list represents a valid simple C program.  Returns
  // a tuple containing 3 values:  
  //
  //   (result, msg, program)
  //
  // where result is true or false (legal or not legal), 
  // msg is a success or syntax error message, and program
  // is a list of instructions if parsing was successful.
  //
  let parse tokens = 
    try
      let (_, program) = simpleC (tokens, [])
      (true, "success", List.rev program)
    with 
      | ex -> (false, ex.Message, [])
