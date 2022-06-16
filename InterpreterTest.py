import ply.lex as lex
import ply.yacc as yacc

variables = {}

reserved = {
    'if' : 'IF',
    'then' : 'THEN'
}

tokens = (
    'INT', 'FLOAT',
    'ID', 'NAME', 'LNAME',
    'PLUS', 'MINUS', 'TIMES', 'DIVIDE',
    'EQUAL', 'EQUALS', 'NOTEQUALS', 'BIGGER', 'BIGGEROR', 'SMALLER', 'SMALLEROR',
    'LPAREN', 'RPAREN'
) + tuple(reserved.values())

t_LPAREN  = r"\("
t_RPAREN  = r"\)"
t_PLUS = r'\+'
t_MINUS = r'\-'
t_TIMES = r'\*'
t_DIVIDE = r'\/'
t_EQUAL = r'\='
t_EQUALS = r'\=\='
t_NOTEQUALS = r'\!\='
t_BIGGEROR = r'\<\='
t_BIGGER = r'\<'
t_SMALLEROR = r'\>\='
t_SMALLER = r'\>'

t_ignore = r' '

def t_FLOAT(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t

def t_INT(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value,'ID')
    return t

def t_NAME(t):
    r'\"(\s*\w*\s*)*\"'
    return t

def t_LNAME(t):
    r'\'(\s*\w*\s*)*\''
    return t

def t_error(t):
    print("Illegal Characters")
    t.lexer.skip(1)

lexer = lex.lex()

variables = {}

precedence = (
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE'),
    ('left', 'LPAREN', 'RPAREN'),)

def p_result(t):
    '''
    result : expression
           | if_statement
           | empty
    '''
    print(t[1])

def p_var_assign(t):
    '''
    result : ID EQUAL expression
    '''
    variables[t[1]] = t[3]

def p_expression_int_float(t):
    '''
    expression : INT
               | FLOAT
    '''
    t[0] = t[1]

def p_expr_str(t):
    '''
    expression : NAME
               | LNAME
    '''
    t[0]=t[1][1:-1]

def p_expression_var(t):
    '''
    expression : ID
    '''
    if t[1] not in variables:
        print("Undeclared variable found")
        t[0] = ''
    else:
        t[0] = variables[t[1]]

def p_expression(t):
    '''
    expression : expression TIMES expression
               | expression DIVIDE expression
               | expression PLUS expression
               | expression MINUS expression
    '''
    if t[2] == '+':
        t[0] = t[1] + t[3]

    elif t[2] == '-':
        t[0] = t[1] - t[3]

    elif t[2] == '*':
        t[0] = t[1] * t[3]

    elif t[2] == '/':
        try:
            t[0] = t[1] / t[3]
        except ZeroDivisionError:
            print("Cant divide by 0")
            t[0] = ''

def p_if_statement(t):
    '''
    if_statement : IF compare THEN expression
    '''
    if t[2]:
        t[0] = t[4]
    else:
        t[0] = ''

def p_comp(t):
    '''
    compare : expression EQUALS expression
            | expression NOTEQUALS expression
            | expression BIGGER expression
            | expression BIGGEROR expression
            | expression SMALLER expression
            | expression SMALLEROR expression
    '''
    if t[2] == '==':
        t[0] = t[1] == t[3]

    elif t[2] == '!=':
        t[0] = t[1] != t[3]

    elif t[2] == '>':
        t[0] = t[1] > t[3]

    elif t[2] == '<':
        t[0] = t[1] < t[3]

    elif t[2] == '>=':
        t[0] = t[1] >= t[3]

    elif t[2] == '<=':
        t[0] = t[1] <= t[3]

def p_factor_paren(t):
    '''
    expression : LPAREN expression RPAREN
    '''
    t[0] = t[2]

def p_empty(t):
    '''
    empty : 
    '''
    t[0] = ''

def p_error(t):
    print("Syntax error!")

parser = yacc.yacc()

while True:
    try:
        s = input('>> ')
    except EOFError:
        break
    parser.parse(s)