namespace AST

module AST =
  type id = string
  type id_t = string
  type op_t = string

  type DataType = 
    | Int of int
    | Float of float
    | String of string
    | Bool of bool

  type Expr =
    | InfixOpExpr of op_t*Expr*Expr
    | Const of DataType
    | Id of id
    | Call of id*(list<Expr>)

  type Statement = 
    | NULL
    | Terminator of string
    | Expr of Expr
    | Func of id*(list<id_t*id>)*list<Statement>
    | Return of Expr
    | Assign3 of id_t*id*Expr
    | Assign2 of id*Expr
    | If of Expr*list<Statement>*option<list<Statement>>
    | While of Expr*list<Statement>
    
