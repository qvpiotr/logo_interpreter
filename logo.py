import ply.lex as lex
import ply.yacc as yacc
import turtle
import random

# LEXER

reserved = {
    'if': 'IF',
    'ifelse': 'IFELSE',
    'while': 'WHILE',
    'until': 'WHILE',
    'do': 'DO',
    'true': 'TRUE',
    'false': 'FALSE',
    'make': 'MAKE',
    'print': 'PRINT',
    'repeat': 'REPEAT',
    'repcount': 'REPCOUNT',
    'to': 'TO',
    'end': 'END',
    'fd': 'FORWARD',
    'forward': 'FORWARD',
    'bk': 'BACK',
    'back': 'BACK',
    'rt': 'RIGHT',
    'right': 'RIGHT',
    'lt': 'LEFT',
    'left': 'LEFT',
    'pu': 'PENUP',
    'penup': 'PENUP',
    'pd': 'PENDOWN',
    'pendown': 'PENDOWN',
    'st': 'SHOW',
    'showturtle': 'SHOW',
    'ht': 'HIDE',
    'hideturtle': 'HIDE',
    'setx': 'SETX',
    'sety': 'SETY',
    'seth': 'SETH',
    'setheading': 'SETH',
    'setxy': 'SETXY',
    'setpos': 'SETXY',
    'setpencolor': 'SETPC',
    'setpc': 'SETPC',
    'setcolor': 'SETPC',
    'setbgcolor': 'SETBC',
    'setpensize': 'SETPS',
    'setwidth': 'SETPS',
    'setpw': 'SETPS',
    'home': 'HOME',
    'speed': 'SPEED',
    'clean': 'CLEAN',
    'cs': 'RESET',
    'clearscreen': 'RESET',
    'reset': 'RESET',
    'random': 'RANDOM'
}

# tokens keywords
tokens = [
    'STRING',
    'FLOAT',
    'INT',
    'LBR',
    'RBR',
    'LPAR',
    'RPAR',
    'QUOTE',
    'COLON',
    'COMMA',
    'PLUS',
    'MINUS',
    'TIMES',
    'DIVIDE',
    'POWER',
    'EQUALS',
    'GTE', 
    'LTE', 
    'GT', 
    'LT', 
    'NE',
] + list(set(reserved.values()))


# tokens
t_LBR    = r'\['
t_RBR    = r'\]'
t_LPAR   = r'\('
t_RPAR   = r'\)'
t_QUOTE  = r'"'
t_COLON  = r':'
t_COMMA  = r','
t_PLUS   = r'\+'
t_MINUS  = r'-'
t_TIMES  = r'\*'
t_DIVIDE = r'/'
t_POWER  = r'\^'
t_EQUALS = r'='
t_GTE    = r'>='
t_LTE    = r'<='
t_GT     = r'>'
t_LT     = r'<'
t_NE     = r'!='


def t_STRING(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    t.type = reserved.get(t.value,'STRING')
    return t

def t_FLOAT(t):
    r'\d*\.\d+'
    t.value = float(t.value)
    return t

def t_INT(t):
    r'\d+'
    t.value = int(t.value)
    return t

# ignore tab char
t_ignore = " \t"

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_error(t):
    print("Illegal characters")
    t.lexer.skip(1)


# PARSER

precedence = (
    ('right', 'RANDOM'),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE'),
    ('right', 'UMINUS'),
    ('left', 'POWER'),
)


def p_statement_list(p):
    '''
    statement_list : statement_list statement
                   | statement 
    '''
    if len(p)==3:
        p[0] = p[1] + p[2]
    else:
        p[0] = p[1]

def p_statement(p):
    '''
    statement : instruction
              | repeat_statement
              | if_statement
              | ifelse_statement
              | while_statement
              | dowhile_statement
              | variable_declaration
              | procedure_definition
              | procedure_call
              | print_statement
    '''
    p[0] = [p[1]]

def p_instruction(p):
    '''
    instruction : SETXY LBR expression COMMA expression RBR
                | FORWARD expression
                | BACK expression
                | RIGHT expression
                | LEFT expression
                | SETX expression
                | SETY expression
                | SETH expression
                | SPEED expression
                | SETPS expression
                | SETPC word
                | SETBC word
                | PENUP
                | PENDOWN
                | SHOW
                | HIDE
                | HOME
                | CLEAN
                | RESET
    '''
    x = reserved[p[1]]
    if len(p) == 7:
        p[0] = (x, p[3], p[5])
    elif len(p) == 3:
        p[0] = (x, p[2])
    else:
        p[0] = (x, None)

def p_repeat_statement(p):
    '''
    repeat_statement : REPEAT expression LBR statement_list RBR
    '''
    p[0] = (reserved[p[1]], p[2], p[4])

def p_if_statement(p):
    '''
    if_statement : IF condition LBR statement_list RBR
    '''
    p[0] = (reserved[p[1]], p[2], p[4])

def p_ifelse_statement(p):
    '''
    ifelse_statement : IFELSE condition LBR statement_list RBR LBR statement_list RBR
    '''
    p[0] = (reserved[p[1]], p[2], p[4], p[7])

def p_while_statement(p):
    '''
    while_statement : WHILE condition LBR statement_list RBR
    '''
    p[0] = (reserved[p[1]], p[2], p[4])

def p_dowhile_statement(p):
    '''
    dowhile_statement : DO LBR statement_list RBR WHILE condition
    '''
    p[0] = (reserved[p[1]], p[3], p[6])

def p_variable_declaration(p):
    '''
    variable_declaration : MAKE word expression
    '''
    p[0] = (reserved[p[1]], p[2], p[3])


def p_procedure_definition(p):
    '''
    procedure_definition : TO STRING LBR parameter_list RBR statement_list END
    '''
    args = [a for a in p[4] if a is not None]
    p[0] = ('DEF', p[2], args, p[6])

def p_parameter_list(p):
    '''
    parameter_list : parameter_list COMMA parameter
                   | parameter
    '''
    if len(p)==4:
        p[0] = p[1] + p[3]
    else:
        p[0] = p[1]

def p_parameter(p):
    '''
    parameter : name
              | empty
    '''
    p[0] = [p[1]]

def p_procedure_call(p):
    '''
    procedure_call : STRING LBR expression_list RBR
    '''
    args = [a for a in p[3] if a is not None]
    p[0] = ('CALL', p[1], args)

def p_expression_list(p):
    '''
    expression_list : expression_list COMMA expression
                    | expression
                    | empty
    '''
    if len(p)==4:
        p[0] = p[1] + [p[3]]
    else:
        p[0] = [p[1]]

def p_print_statement_word(p):
    '''
    print_statement : PRINT word
    '''
    p[0] = ('PRINT_WORD', p[2])

def p_print_statement_expression(p):
    '''
    print_statement : PRINT expression
    '''
    p[0] = ('PRINT_EXPR', p[2])

def p_expression_int_float(p):
    '''
    expression : INT
               | FLOAT
    '''
    p[0] = p[1]

def p_expression_var(p):
    '''
    expression : name
    '''
    p[0] = ('VAR', p[1])

def p_expression_repcount(p):
    '''
    expression : REPCOUNT
    '''
    p[0] = ('REPCOUNT', None)

def p_expression_group(p):
    '''
    expression : LPAR expression RPAR
    '''
    p[0] = p[2]

# ~
def p_expression_uminus(p):
    '''
    expression : MINUS expression %prec UMINUS
    '''
    p[0] = ('UMINUS', p[2])

def p_expression_random(p):
    '''
    expression : RANDOM expression
    '''
    p[0] = (reserved[p[1]], p[2])

def p_expression(p):
    '''
    expression : expression TIMES expression
               | expression DIVIDE expression
               | expression PLUS expression
               | expression MINUS expression
               | expression POWER expression
    '''
    p[0] = (p[2], p[1], p[3])

def p_condition_true_false(p):
    '''
    condition : TRUE
              | FALSE
    '''
    p[0] = (p[1]=='true')

# < > <= >= lt gt le ge
def p_condition(p):
    '''
    condition : expression GT expression
              | expression LT expression
              | expression GTE expression
              | expression LTE expression
              | expression EQUALS expression
              | expression NE expression
    '''
    p[0] = (p[2], p[1], p[3])

def p_word(p):
    '''
    word : QUOTE STRING
    '''
    p[0] = p[2]

def p_name(p):
    '''
    name : COLON STRING
    '''
    p[0] = p[2]

def p_empty(p):
    '''
    empty :
    '''
    p[0] = None

def p_error(p):
    if p:
        print("Syntax error at token", p.type)
        # Just discard the token and tell the parser it's okay.
        parser.errok()
    else:
        print("Syntax error at EOF")

# INTERPRETER

turtle.title('logo_interpreter')
turtle.shape("turtle")
turtle.mode('logo')
turtle.tracer(1,5)
turtle.speed(0)

env = { }
env['LOOP'] = False
env['REPCOUNT'] = None

def run(program):
    for statement in program:
        execute(statement)

def execute(s):
    global env
    x = s[0]
    arg1 = s[1]
    arg2 = s[2] if len(s)>2 else None
    arg3 = s[3] if len(s)>3 else None

    # instructions
    if x == 'SETXY':
        turtle.setposition(calc(arg1), calc(arg2))
    elif x == 'FORWARD':
        turtle.forward(calc(arg1))
    elif x == 'BACK':
        turtle.back(calc(arg1))
    elif x == 'RIGHT':
        turtle.right(calc(arg1))
    elif x == 'LEFT':
        turtle.left(calc(arg1))
    elif x == 'SETX':
        turtle.setx(calc(arg1))
    elif x == 'SETY':
        turtle.sety(calc(arg1))
    elif x == 'SETH':
        turtle.setheading(calc(arg1))
    elif x == 'SETPS':
        turtle.pensize(calc(arg1))
    elif x == 'SETPC':
        turtle.pencolor(arg1)
    elif x == 'SETBC':
        turtle.bgcolor(arg1)
    elif x == 'SPEED':
        a = calc(arg1)
        if a<0:
            turtle.tracer(0,0)
        elif a==0:
            turtle.tracer(1,0)
            turtle.speed(a)
        else:
            turtle.tracer(1,5)
            turtle.speed(a-1)
    elif x == 'PENUP':
        turtle.penup()
    elif x == 'PENDOWN':
        turtle.pendown()
    elif x == 'SHOW':
        turtle.showturtle()
    elif x == 'HIDE':
        turtle.hideturtle()
    elif x == 'HOME':
        turtle.home()
    elif x == 'CLEAN':
        turtle.clear()
    elif x == 'RESET':
        turtle.reset()


    elif x == 'REPEAT':
        for i in range(0, int(calc(arg1))):
            env['LOOP'] = True
            env['REPCOUNT'] = i+1
            run(arg2)
        env['LOOP'] = False
    elif x == 'IF':
        if eval(arg1):
            run(arg2)
    elif x == 'IFELSE':
        if eval(arg1):
            run(arg2)
        else: 
            run(arg3)
    elif x == 'WHILE':
        while eval(arg1):
            run(arg2)
    elif x == 'DO':
        run(arg1)
        while eval(arg2):
            run(arg1)

    elif x == 'MAKE':
        env[arg1] = calc(arg2)

    elif x == 'DEF':
        env[arg1] = {
            'args' : [a for a in arg2 if a is not None],
            'body' : arg3
        }
    elif x == 'CALL':
        try:
            proc = env[arg1]
            arg2 = [a for a in arg2 if a is not None]
            if type(proc) != dict:
                print(f"{arg1} is not a procedure")
                return
            if len(arg2) != len(proc['args']):
                print(f"expected {len(proc['args'])} arguments, but got {len(arg2)}")
                return
            old_env = env.copy()
            for arg, arg_expr in zip(proc['args'], arg2):
                env[arg] = calc(arg_expr)
            run(proc['body'])
            env = old_env.copy()
        except LookupError:
            print(f"Undeclared procedure {arg1}")

    elif x == 'PRINT_WORD':
        print(arg1)
    elif x == 'PRINT_EXPR':
        print(calc(arg1))

def calc(e):
    global env
    if type(e) == tuple:
        id = e[0]
        arg1 = e[1]
        arg2 = e[2] if len(e)>2 else None
        if id == '+':
            return calc(arg1) + calc(arg2)
        elif id == '-':
            return calc(arg1) - calc(arg2)
        elif id == '*':
            return calc(arg1) * calc(arg2)
        elif id == '/':
            return calc(arg1) / calc(arg2)
        elif id == '^':
            return calc(arg1) ** calc(arg2)
        elif id == 'VAR':
            try:
                return env[arg1]
            except LookupError:
                print(f"Undefined variable {arg1}")
                return
        elif id == 'REPCOUNT':
            if env['LOOP']:
                return env['REPCOUNT']
            else:
                return None
        elif id == 'UMINUS':
            return (-1)* calc(arg1)
        elif id == 'RANDOM':
            return random.randrange(calc(arg1))

    else:
        return e

def eval(c):
    global env
    if type(c) != tuple:
        return c
    else:
        op = c[0]
        arg1 = calc(c[1])
        arg2 = calc(c[2])
        if op == '>':
            return arg1 > arg2
        elif op == '<':
            return arg1 < arg2
        elif op == '>=':
            return arg1 >= arg2
        elif op == '<=':
            return arg1 <= arg2
        elif op == '=':
            return arg1 == arg2
        elif op == '!=':
            return arg1 != arg2


lexer = lex.lex()
parser = yacc.yacc()


def main():

    while True:
        try:
            cmd = input('\n >>')
            if cmd.strip() == 'exit': break
        except EOFError:
            break
        lines = [cmd]
        while True:
            line = input(' >>')
            if line:
                lines.append(line)
            else:
                break
        s = '\n'.join(lines)

        p = parser.parse(s)

        if p is None:
            pass
        else:
            try:
                run(p)
                turtle.update()
            except Exception as e:
                print(f'Interpreter error (Python): {e}')
                lexer.input(s)
                tokens = [(tok.type, tok.value) for tok in lexer]
                print('tokens : ', tokens)
                print('AST    : ', p)
                print('Env    : ', env)
                continue
    try:
        turtle.bye()
    except:
        print('Excuse the turtle his manners...')
    print('\n')

if __name__=='__main__':
    main()



