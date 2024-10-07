type Token = 
    | OPEN_PARENTHESIS
    | CLOSE_PARENTHESIS
    | OPEN_CURLY
    | CLOSE_CURLY
    | SEMI_COLON
    | COMMA_SEPARATOR
    | DOT_OPERATOR
    | PLUS
    | MINUS
    | MULTIPLY
    | DIVIDE
    | ASSIGN
    | GREATER_THAN
    | LESS_THAN
    | CUSTOM_OP of string
    | IDENTIFIER of string
    | LITERAL_STRING of string
    | NUMERIC of float

let charListToString (chars: char list) = 
    chars |> List.toArray |> System.String

let lexer (inputChars: char list) =
    let staticTokens = 
        Map [
            ('(', Token.OPEN_PARENTHESIS)
            (')', Token.CLOSE_PARENTHESIS)
            ('{', Token.OPEN_CURLY)
            ('}', Token.CLOSE_CURLY)
            (';', Token.SEMI_COLON)
            (',', Token.COMMA_SEPARATOR)
            ('.', Token.DOT_OPERATOR)
        ]

    let operatorTokens = 
        Map [
            ('+', Token.CUSTOM_OP("+"))
            ('-', Token.CUSTOM_OP("-"))
            ('*', Token.CUSTOM_OP("*"))
            ('/', Token.CUSTOM_OP("/"))
            ('=', Token.CUSTOM_OP("="))
            ('>', Token.CUSTOM_OP(">"))
            ('<', Token.CUSTOM_OP("<"))
            ('|', Token.CUSTOM_OP("|"))
            ('&', Token.CUSTOM_OP("&"))
        ]

    let rec parseString acc = function
        | '\\'::'"'::remaining -> (charListToString (List.rev acc)), remaining
        | '"'::remaining -> (charListToString (List.rev acc)), remaining
        | head::tail -> parseString (head::acc) tail
        | [] -> failwith "parseString@ End of input before closing quote"

    let rec parseBlockComment = function
        | '@'::tail -> tail
        | _::tail -> parseBlockComment tail
        | [] -> failwith "parseBlockComment@ End of input before closing comment"

    let rec parseLineComment = function
        | '\n'::tail -> tail
        | _::tail -> parseLineComment tail
        | [] -> []

    let rec parseIdentifier acc = function
        | head::tail when System.Char.IsWhiteSpace(head) -> (charListToString (List.rev acc)), tail
        | head::tail when System.Char.IsLetter(head) || System.Char.IsDigit(head) || head = '_' -> parseIdentifier (head::acc) tail
        | head::tail when head = '(' || head = ')' || head = '{' || head = '}' -> (charListToString (List.rev acc)), (head::tail)
        | [] -> (charListToString (List.rev acc)), []
        | head::_ -> failwith ("parseIdentifier@ Unexpected character: " + (string head))

    let rec parseNumber acc = function
        | head::tail when System.Char.IsWhiteSpace(head) -> (charListToString (List.rev acc)), tail
        | head::tail when System.Char.IsDigit(head) -> parseNumber (head::acc) tail
        | '.'::tail -> parseNumber ('.'::acc) tail
        | head::tail when head = '(' || head = ')' -> (charListToString (List.rev acc)), (head::tail)
        | [] -> (charListToString (List.rev acc)), []
        | head::_ -> failwith ("parseNumber@ Unexpected character while reading number: " + (string head))

    let rec tokenizeHelper acc = function
        | head::tail when System.Char.IsWhiteSpace(head) -> tokenizeHelper acc tail // Skip whitespaces
        | head::tail when staticTokens |> Map.containsKey head -> 
            tokenizeHelper ((staticTokens |> Map.find head)::acc) tail
        | '"'::tail | '\\'::'"'::tail -> 
            let parsedString, remaining = parseString [] tail
            tokenizeHelper (Token.LITERAL_STRING(parsedString)::acc) remaining
        | '@'::tail -> 
            let remaining = parseBlockComment tail // Block comments
            tokenizeHelper acc remaining
        | '#'::tail -> 
            let remaining = parseLineComment tail // Line comments
            tokenizeHelper acc remaining
        | head::tail when System.Char.IsLetter(head) ->
            let parsedId, remaining = parseIdentifier [] (head::tail)
            tokenizeHelper (Token.IDENTIFIER(parsedId)::acc) remaining
        | head::tail when System.Char.IsDigit(head) ->
            let parsedNum, remaining = parseNumber [] (head::tail)
            try 
                let numValue = System.Double.Parse(parsedNum, System.Globalization.CultureInfo.InvariantCulture)
                tokenizeHelper (Token.NUMERIC(numValue)::acc) remaining
            with
                _ -> failwith ("tokenizeHelper@ Unable to parse number: " + parsedNum)
        | '-'::head::tail when System.Char.IsDigit(head) ->
            let parsedNum, remaining = parseNumber [] (head::tail)
            try 
                let numValue = System.Double.Parse("-" + parsedNum, System.Globalization.CultureInfo.InvariantCulture)
                tokenizeHelper (Token.NUMERIC(numValue)::acc) remaining
            with
                _ -> failwith ("tokenizeHelper@ Unable to parse negative number: " + parsedNum)
        | head::' '::tail when (operatorTokens |> Map.tryFind head).IsSome ->
            tokenizeHelper ((operatorTokens |> Map.find head)::acc) tail
        | head::_ -> failwith ("tokenizeHelper@ Unsupported character: " + (string head))
        | [] -> List.rev acc

    tokenizeHelper [] inputChars
