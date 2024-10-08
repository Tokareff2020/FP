open System

type Token =
    | OPERATOREN_PARENTHESIS
    | CLOSE_PARENTHESIS
    | OPERATOREN_CURLY
    | CLOSE_CURLY
    | SEMI_COLON
    | COMMA_SEPARATOR
    | DOT_SEPARATOR
    | PLUS
    | MINUS
    | ASSIGN
    | OPERATOR of string
    | ID of string
    | LITERAL_STRING of string
    | NUMERIC of float

let charListToString (chars: char list) = 
    chars |> List.toArray |> String

let tokenize sourceCode =
    let staticTokens = Map [
        ('(', Token.OPERATOREN_PARENTHESIS);
        (')', Token.CLOSE_PARENTHESIS);
        ('{', Token.OPERATOREN_CURLY);
        ('}', Token.CLOSE_CURLY);
        (';', Token.SEMI_COLON);
        (',', Token.COMMA_SEPARATOR);
        ('.', Token.DOT_SEPARATOR);
    ]

    let operatorTokens = Map [
        ('+', Token.OPERATOR("+"));
        ('-', Token.OPERATOR("-"));
        ('*', Token.OPERATOR("*"));
        ('/', Token.OPERATOR("/"));
        
        ('=', Token.OPERATOR("="));
        ('>', Token.OPERATOR(">"));
        ('<', Token.OPERATOR("<"));
        ('|', Token.OPERATOR("|"));
        ('&', Token.OPERATOR("&"));
    ]

    let rec parseString acc = function
    | '\\'::'"'::t -> (charListToString (List.rev acc)), t
    | '"'::t -> (charListToString (List.rev acc)), t
    | h::t -> parseString (h::acc) t
    | [] -> failwith "parseString@ EOF before closing \" found"

    let rec parseBlockComment = function
    | '@'::t -> t
    | _::t -> parseBlockComment t
    | [] -> failwith "parseBlockComment@ EOF before closing comment"

    let rec parseLineComment = function
    | '\n'::t -> t
    | _::t -> parseLineComment t
    | [] -> []

    let rec parseIdentifier acc = function
    | h::t when Char.IsWhiteSpace(h) -> (charListToString (List.rev acc)), t
    | h::t when Char.IsLetter(h) || Char.IsDigit(h) || h = '_' -> parseIdentifier (h::acc) t
    | h::t when h = '(' || h = ')' || h = '{' || h = '}' -> (charListToString (List.rev acc)), (h::t)
    | [] -> (charListToString (List.rev acc)), []
    | h::_ -> failwith ("parseIdentifier@ Unexpected symbol met: " + (string h))

    let rec parseNumber acc = function
    | h::t when Char.IsWhiteSpace(h) -> (charListToString (List.rev acc)), t
    | h::t when Char.IsDigit(h) -> parseNumber (h::acc) t
    | '.'::t -> parseNumber ('.'::acc) t
    | h::t when h = '(' || h = ')' -> (charListToString (List.rev acc)), (h::t)
    | [] -> (charListToString (List.rev acc)), []
    | h::_ -> failwith ("parseNumber@ Unexpected symbol met while reading digit: " + (string h))

    let rec tokenizeHelper acc = function
    | h::t when Char.IsWhiteSpace(h) -> tokenizeHelper acc t
    | h::t when staticTokens |> Map.containsKey h -> tokenizeHelper ((staticTokens |> Map.find h)::acc) t
    | '"'::t | '\\'::'"'::t -> 
        let parsedString, remaining = parseString [] t
        tokenizeHelper (Token.LITERAL_STRING( parsedString)::acc) remaining
    | '@'::t -> 
        let remaining = parseBlockComment t
        tokenizeHelper acc remaining
    | '#'::t -> 
        let remaining = parseLineComment t
        tokenizeHelper acc remaining

    | h::t when Char.IsLetter(h) ->
        let parseIdentifier, remaining = parseIdentifier [] (h::t)
        tokenizeHelper (Token.ID(parseIdentifier)::acc) remaining

    | h::t when Char.IsDigit(h) ->
        let parseNumber, remaining = parseNumber [] (h::t)
        try 
            let parsedNumber = System.Double.Parse(parseNumber, System.Globalization.CultureInfo.InvariantCulture)
            tokenizeHelper (Token.NUMERIC(parsedNumber)::acc) remaining
        with
            _ -> failwith ("tokenizeHelper@ Unrecognizable number met: " + parseNumber)
    | '-'::h::t when Char.IsDigit(h) ->
        let parseNumber, remaining = parseNumber [] (h::t)
        try 
            let parsedNumber = System.Double.Parse("-" + parseNumber, System.Globalization.CultureInfo.InvariantCulture)
            tokenizeHelper (Token.NUMERIC(parsedNumber)::acc) remaining
        with
            _ -> failwith ("tokenizeHelper@ Unrecognizable number met: " + parseNumber)
    | h::' '::t when (operatorTokens |> Map.tryFind h).IsSome ->
         tokenizeHelper ((operatorTokens |> Map.find h)::acc) t

    | h::_ -> failwith ("tokenizeHelper@ Unsupported symbol met: " + (string h))
    | [] -> List.rev acc

    tokenizeHelper [] sourceCode


type Expr =
    | NUMERIC of double
    | LITERAL_STRING of string
    | CONST_BOOL of bool
    | ID of string
    | CONDITION of Expr * Expr * Expr
    | ASSIGN of string * Expr
    | MUTATE of string * Expr
    | FUNCTION_DEF of string * Expr * Expr * env * int
    | FUNCTION_CALL of string * Expr * int
    | OPERATOR of string * Expr list
    | OUTPUT of Expr

    | PARAM_LIST of Expr list
    | EXPR_LIST of Expr list
    | OP_SYMBOL of string
    | PLACEHOLDER of string
and env = Map<string, Expr>

let parse tokenList = 
    let rec parseIdentifiers acc = function
        | Token.ID(id)::t -> parseIdentifiers (id::acc) t
        | Token.CLOSE_CURLY::t -> List.rev acc, t
        | _ -> failwith "parse_function_parameters@ not found expected id"

    let keywordList = ["let"; "set"; "func"; "if"; "then"; "else"; "print"]

    let rec parseExpression acc = function
        | [] -> List.rev acc, []

        | Token.ID(expr)::t when (List.tryFind (fun x -> x = expr) keywordList).IsSome -> parseExpression (Expr.PLACEHOLDER(expr)::acc) t

        | Token.NUMERIC(n)::t -> parseExpression (Expr.EXPR_LIST([Expr.NUMERIC(n)])::acc) t
        | Token.ID("true")::t -> parseExpression (Expr.EXPR_LIST([Expr.CONST_BOOL(true)])::acc) t
        | Token.ID("false")::t -> parseExpression (Expr.EXPR_LIST([Expr.CONST_BOOL(false)])::acc) t
        | Token.ID(id)::t -> parseExpression (Expr.EXPR_LIST([Expr.ID(id)])::acc) t
        | Token.LITERAL_STRING(s)::t -> parseExpression (Expr.EXPR_LIST([Expr.LITERAL_STRING(s)])::acc) t

        | Token.CLOSE_CURLY::t -> List.rev acc, t
        | Token.OPERATOREN_CURLY::t ->
            let parsedArgs, remainingTokens = parseExpression [] t
            if List.forall (fun x -> match x with | Expr.EXPR_LIST([Expr.ID(_)]) -> true | _ -> false) parsedArgs 
            then parseExpression (Expr.PARAM_LIST(parsedArgs)::acc) remainingTokens
            else failwith ("parseExpression@ non ids inside function args: " + (sprintf "%A" parsedArgs))

        | Token.CLOSE_PARENTHESIS::t -> List.rev acc, t
        | Token.OPERATOREN_PARENTHESIS::t ->
            let parsedExprs, remainingTokens = parseExpression [] t
            match parsedExprs with
            | Expr.OP_SYMBOL(op)::t -> parseExpression (Expr.EXPR_LIST([Expr.OPERATOR(op, t)])::acc) remainingTokens
            | Expr.PLACEHOLDER("let")::Expr.EXPR_LIST([Expr.ID(id)])::Expr.EXPR_LIST(list)::[] -> parseExpression (Expr.EXPR_LIST([Expr.ASSIGN(id, Expr.EXPR_LIST(list))])::acc) remainingTokens
            | Expr.PLACEHOLDER("set")::Expr.EXPR_LIST([Expr.ID(id)])::Expr.EXPR_LIST(list)::[] -> parseExpression (Expr.EXPR_LIST([Expr.MUTATE(id, Expr.EXPR_LIST(list))])::acc) remainingTokens

            | Expr.PLACEHOLDER("func")::Expr.EXPR_LIST([Expr.ID(id)])::(Expr.PARAM_LIST(argsList) as args)::(Expr.EXPR_LIST(_) as body)::[] -> 
                parseExpression (EXPR_LIST([Expr.FUNCTION_DEF(id, args, body, Map<string, Expr>[], List.length argsList)])::acc) remainingTokens

            | Expr.EXPR_LIST([Expr.ID(id)])::t ->
                if List.forall (fun x -> match x with | Expr.EXPR_LIST(_) -> true | _ -> false) t
                then parseExpression (EXPR_LIST([Expr.FUNCTION_CALL(id, Expr.EXPR_LIST(t), List.length t)])::acc) remainingTokens
                else failwith ("parseExpression@ wrong function call syntax! Used as parameters for function call: " + (sprintf "%A" t)) 

            | Expr.EXPR_LIST(list)::t -> 
                let rec collectExprLists acc = function
                | (Expr.EXPR_LIST(_) as list)::t' -> collectExprLists (list::acc) t'
                | [] -> List.rev acc, []
                | waste -> 
                    printfn "unmatched expr: %A" waste
                    failwith "collectExprLists@ misformat expression"

                let lists, _ = collectExprLists [] (EXPR_LIST(list)::t)
                parseExpression (Expr.EXPR_LIST(lists)::acc) remainingTokens
            | Expr.PLACEHOLDER("if")::(Expr.EXPR_LIST(_) as cond)
                ::Expr.PLACEHOLDER("then")::(Expr.EXPR_LIST(_) as expr1)
                ::Expr.PLACEHOLDER("else")::(Expr.EXPR_LIST(_) as expr2)::[] -> 
                    parseExpression (Expr.EXPR_LIST([Expr.CONDITION(cond, expr1, expr2)])::acc) remainingTokens
            | Expr.PLACEHOLDER("if")::(Expr.EXPR_LIST(_) as cond)
                ::Expr.PLACEHOLDER("then")::(Expr.EXPR_LIST(_) as expr1)::[] ->
                    parseExpression (Expr.EXPR_LIST([Expr.CONDITION(cond, expr1, PLACEHOLDER(""))])::acc) remainingTokens

            | Expr.PLACEHOLDER("print")::(Expr.EXPR_LIST(_) as body)::[] -> parseExpression (Expr.EXPR_LIST([Expr.OUTPUT(body)])::acc) remainingTokens

            | (Expr.NUMERIC(_) as numExpr)::[] -> parseExpression (EXPR_LIST([numExpr])::acc) remainingTokens
            | (Expr.LITERAL_STRING(_) as strExpr)::[] -> parseExpression (EXPR_LIST([strExpr])::acc) remainingTokens
            | waste -> failwith ("parseExpression@ wrong parenthesis structure: " + (sprintf "%A" waste))

        | Token.OPERATOR(op)::t -> parseExpression (Expr.OP_SYMBOL(op)::acc) t
        | waste -> 
            printfn "unexpected token: %A" waste
            failwith "parseExpression@ unexpected token"

    let parsedExpr, remainingTokens = parseExpression [] tokenList
    if remainingTokens <> [] then 
        printfn "unparsed part: %A" remainingTokens
        failwith "parseExpression@ misformat expression"
    Expr.EXPR_LIST(parsedExpr)

let eval env ast = 
    let lookup name env = env |> Map.find name

    let arithmeticOps = Map [
        ("+", ((function x -> x), (function (Expr.NUMERIC(x), Expr.NUMERIC(y)) -> Expr.NUMERIC(x + y))));
        ("-", ((function Expr.NUMERIC(x) -> Expr.NUMERIC(-x)), (function (Expr.NUMERIC(x), Expr.NUMERIC(y)) -> Expr.NUMERIC(x - y))));
        ("*", ((function x -> x), (function (Expr.NUMERIC(x), Expr.NUMERIC(y)) -> Expr.NUMERIC(x * y))));
        ("/", ((function Expr.NUMERIC(x) -> Expr.NUMERIC(1. / x)), (function (Expr.NUMERIC(x), Expr.NUMERIC(y)) -> Expr.NUMERIC(x / y))));
    ]
    let boolOps = Map [
        ("&", ((function x -> x), (function (Expr.CONST_BOOL(x), Expr.CONST_BOOL(y)) -> Expr.CONST_BOOL(x && y))));
        ("|", ((function x -> x), (function (Expr.CONST_BOOL(x), Expr.CONST_BOOL(y)) -> Expr.CONST_BOOL(x || y))));
    ]
    let binaryOps = Map [
        (">", (function (Expr.NUMERIC(x), Expr.NUMERIC(y)) -> Expr.CONST_BOOL(x > y)));
        ("<", (function (Expr.NUMERIC(x), Expr.NUMERIC(y)) -> Expr.CONST_BOOL(x < y)));
    ]

    let rec evalArgsBool evalFunction env acc = fun x ->
        match x with
        | h::t -> 
            let evaluated, newEnv = evalFunction env h
            match evaluated with
            | Expr.CONST_BOOL(_) -> 
                evalArgsBool evalFunction newEnv (evaluated::acc) t
            | Expr.EXPR_LIST([Expr.CONST_BOOL(_) as boolean]) ->
                evalArgsBool evalFunction newEnv (boolean::acc) t
            | Expr.NUMERIC(n) -> 
                evalArgsBool evalFunction newEnv (Expr.CONST_BOOL(Convert.ToBoolean n)::acc) t
            | Expr.EXPR_LIST([Expr.NUMERIC(n)]) ->
                evalArgsBool evalFunction newEnv (Expr.CONST_BOOL(Convert.ToBoolean n)::acc) t
            | waste -> failwith ("checkBool@ unevaluatable bool expression: " + (sprintf "%A" waste))
        | [] -> List.rev acc, env

    let rec evalArgsNumeric evalFunction env acc = fun x ->
        match x with
        | h::t -> 
            let evaluated, newEnv = evalFunction env h
            match evaluated with
            | Expr.NUMERIC(_) -> 
                evalArgsNumeric evalFunction newEnv (evaluated::acc) t
            | Expr.EXPR_LIST([Expr.NUMERIC(_) as number]) ->
                evalArgsNumeric evalFunction newEnv (number::acc) t
            | waste -> failwith ("evalArgsNumeric@ unevaluatable numeric expression: " + (sprintf "%A" waste))
        | [] -> List.rev acc, env

    let rec evalBranch env = function
        | Expr.NUMERIC(_) as number -> number, env
        | Expr.LITERAL_STRING(_) as string -> string, env
        | Expr.CONST_BOOL(_) as boolean -> boolean, env
        | Expr.ID(id) -> evalBranch env (lookup id env)

        | Expr.EXPR_LIST([Expr.NUMERIC(_) as number]) -> number, env
        | Expr.EXPR_LIST([Expr.LITERAL_STRING(_) as string]) -> string, env
        | Expr.EXPR_LIST([Expr.CONST_BOOL(_) as boolean]) -> boolean, env
        | Expr.EXPR_LIST([Expr.ID(id)]) -> lookup id env, env

        | Expr.EXPR_LIST([Expr.OPERATOR(_) as op]) -> evalBranch env op

        | Expr.OPERATOR(op, t) when (Map.tryFind op arithmeticOps).IsSome ->
            let (unaryLambda, binaryLambda) = Map.find op arithmeticOps
            let evaluatedList, newEnv = evalArgsNumeric evalBranch env [] t
            match List.length evaluatedList with
            | 0 -> failwith "evalBranch@ operator + can't have 0 arguments"
            | 1 -> unaryLambda (List.head evaluatedList), newEnv
            | _ -> List.reduce (fun x y -> binaryLambda (x, y)) evaluatedList, newEnv
        | Expr.OPERATOR(op, t) when (Map.tryFind op boolOps).IsSome ->
            let (unaryLambda, binaryLambda) = Map.find op boolOps
            let evaluatedList, newEnv = evalArgsBool evalBranch env [] t
            match List.length evaluatedList with
            | 0 -> failwith "evalBranch@ operator + can't have 0 arguments"
            | 1 -> unaryLambda (List.head evaluatedList), newEnv
            | _ -> List.reduce (fun x y -> binaryLambda (x, y)) evaluatedList, newEnv

        | Expr.OPERATOR("=", t) ->
            match List.length t with
            | 2 ->
                let first::second::[] = t
                let evalFirst, newEnv = evalBranch env first
                let evalSecond, newEnv' = evalBranch newEnv second
                (function 
                    | (Expr.NUMERIC(x), Expr.NUMERIC(y)) -> Expr.CONST_BOOL(x = y)
                    | (Expr.CONST_BOOL(x), Expr.CONST_BOOL(y)) -> Expr.CONST_BOOL(x = y)
                    | (Expr.LITERAL_STRING(x), Expr.LITERAL_STRING(y)) -> Expr.CONST_BOOL(x = y)
                    | _ -> failwith ("evalBranch@ given unsupported arguments: " + (sprintf "%A %A" evalFirst evalSecond))
                ) (evalFirst, evalSecond), newEnv'
            | _ -> failwith ("evalBranch@ operator " + (sprintf "%s" "=") + " can't have not 2 arguments")
        | Expr.OPERATOR(op, t) when (Map.tryFind op binaryOps).IsSome ->
            let evaluatedList, newEnv = evalArgsNumeric evalBranch env [] t
            let functor = Map.find op binaryOps
            match List.length evaluatedList with
            | 2 -> 
                let first::second::[] = evaluatedList
                functor (first, second), newEnv
            | _ -> failwith ("evalBranch@ operator " + (sprintf "%s" op) + " can't have not 2 arguments")

        | Expr.ASSIGN(id, list) -> Expr.PLACEHOLDER (""), (Map.add id list env)
        | Expr.MUTATE(id, list) -> Expr.PLACEHOLDER (""), (Map.add id list env)
        | Expr.CONDITION(cond, expr1, expr2) ->
            let evalCondition, newEnv = evalBranch env cond
            match evalCondition with 
            | Expr.NUMERIC(n) -> if Convert.ToBoolean n then (expr1, newEnv) else (expr2, newEnv)
            | Expr.CONST_BOOL(b) -> if b then (expr1, newEnv) else (expr2, newEnv)
            | waste -> failwith ("evalBranch@ unevaluatable cond expression: " + (sprintf "%A" waste))
        | Expr.EXPR_LIST(list) ->
            let rec evalLists env = function
                | h::t -> 
                    let firstEvaluated, newEnv = evalBranch env h
                    match firstEvaluated with
                    | Expr.PLACEHOLDER(_) as placeholder -> evalLists newEnv t
                    | _ ->
                        let evaluated, newEnv' = evalBranch newEnv firstEvaluated
                        match evaluated with
                        | Expr.NUMERIC(_) | Expr.LITERAL_STRING(_) | Expr.CONST_BOOL(_)-> 
                            if List.length t <> 0 then printfn "evalPlaceholderLists@ warning# useless members at the end of list"
                            evaluated, newEnv'
                        | _ -> evalLists newEnv' t
                | [] ->
                    Expr.PLACEHOLDER(""), env
            evalLists env list
        | Expr.FUNCTION_DEF(id, args, body, _, arity) ->
            Expr.PLACEHOLDER(""), (Map.add id (Expr.FUNCTION_DEF(id, args, body, env, arity)) env)
        | Expr.FUNCTION_CALL(id, Expr.EXPR_LIST(args), arity) ->
            let envFunction = Map.tryFind id env
            if envFunction.IsNone then failwith ("evalBranch@ use of undeclared function " + id)
            else 
                let (Expr.FUNCTION_DEF(_, Expr.PARAM_LIST(envArgs), body, envEnv, envArity)) = envFunction.Value
                if arity <> envArity 
                then failwith ("evalBranch@ function use with different arity: expected " + (sprintf "%A" envArity) + " got: " + (sprintf "%A" arity))
                else
                    let rec addEnvArgs env = function
                    | (Expr.EXPR_LIST([Expr.ID(h1)])::t1), (h2::t2) -> 
                        let evalH2, newEnv =  evalBranch env h2
                        addEnvArgs (Map.add h1 evalH2 newEnv) (t1, t2)
                    | ([], []) -> env
                    | waste -> failwith ("evalBranch@ Some serious thing happened diring concatenations of maps: " + (sprintf "%A" waste))

                    let newEnv = addEnvArgs env (envArgs, args) 
                    let mergedEnv = Map.fold (fun acc key value -> Map.add key value acc) env newEnv
                    let mergedEnv2 = Map.fold (fun acc key value -> Map.add key value acc) mergedEnv envEnv

                    evalBranch mergedEnv2 body
        | Expr.OUTPUT(body) ->
            let evaludatedBody, newEnv = evalBranch env body
            match evaludatedBody with
            | Expr.PLACEHOLDER(_) -> failwith ("evalBranch.print@ unevaluatable placeholder value to print\n")
            | _ ->
                let evaluatedEval, newEnv' = evalBranch newEnv evaludatedBody
                match evaluatedEval with
                | Expr.NUMERIC(num) -> 
                    printfn "%f" num
                    PLACEHOLDER(""), newEnv
                | Expr.LITERAL_STRING(str) -> 
                    printfn "\"%s\"" str
                    PLACEHOLDER(""), newEnv
                | Expr.CONST_BOOL(boolean) ->
                    if boolean then
                        printfn "true"
                        PLACEHOLDER(""), newEnv
                    else
                        printfn "false"
                        PLACEHOLDER(""), newEnv
                | Expr.ID(idVal) as id ->
                    let evaluatedId, newEnv'' = evalBranch newEnv' id
                    printfn "id: %s = %A" idVal evaluatedId
                    PLACEHOLDER(""), newEnv''
        | Expr.PLACEHOLDER(_) -> failwith ("evalBranch@ placeholder value is invaluatable\n")
        | waste -> failwith ("evalBranch@ wrong structure to evaluate" + (sprintf "%A\n" waste))

    match ast with
    | h -> evalBranch env h


let test env = 
    let testCase sourceCode =
        try
            printfn "Source: %s" sourceCode
            let tokenized = tokenize ("(" + sourceCode + ")" |> Seq.toList)
            let expr = parse tokenized
            let res, newEnv = eval env expr
            printfn "Evaluated: %A\n" res
            ()
        with ex ->
            printfn "Error: %s\n" ex.Message
    testCase "(123)"
    testCase "(\"Once upon a midnight dreary, when I pondered weak and weary...\")"
    testCase "(+ 2 3)"
    testCase "(+ 1.9 9.2)"
    testCase "(let var 32)"
    testCase "((let var 2) (+ var 3))"
    testCase "((let var ( + 100 101 ) ) var)"
    testCase "(if (45) then (56) else (67))"
    testCase "(let var (if 1 then 4 else 5))"
    testCase "((let var 1) (+ var 1))"
    testCase "((let var1 1) (let var2 2) (+ var1 var2))"
    testCase "(func var1 {arg1 arg2 } ( + arg1 arg2 ))"
    testCase "((func var1 {arg1 arg2} ( + arg1 arg2 ) ) (var1 1 (if 0 then 15 else 8 )))"
    testCase "((func var1 {arg1 arg2} ( + arg1 arg2 ) ) (let varfor_func 7) ( var1 varfor_func (if 0 then 15 else 8)))"
    testCase "((func var1 {arg1 arg2} ( + arg1 arg2 ) ) (let arg2 7) (var1 2 1))"
    testCase "((let closure_var 8 ) (func var1 {arg1 arg2} (+ arg1 arg2 closure_var)) (let varfor_func 7) (var1 varfor_func (if 0 then 15 else 8)))"
    testCase "((func var1 {arg1} ( if ( = arg1 1 ) then arg1 else (* ( var1 ( - arg1 1 ) ) arg1 ) ) ) ( var1 5 ))"
    testCase "((func var1 {} (if true then 1 else false)) (var1))"
    testCase "((func var1 {a} (if (> a 1) then ((print a) (var1 (- a 1))) else \"function ended\")) (var1 5))"
    testCase "(let var (+ 1 2)))"
    testCase "(let var (+ 1 2)))"
    testCase "(if (| true 1) then 1 else 2))"
    testCase "(if (& true (< 1 0)) then 1 else 2)"
    testCase "((let var 1) (func arg {} (+ var 1)) (arg))"
    testCase "((let var 1) (print var) (set var 2) (print var))"

let env = Map<string, Expr> []
test env