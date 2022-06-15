import ply.lex as lex
import ply.yacc as yacc

variables = {}

reserved = {
    'if' : 'IF',
    'then' : 'THEN'
}

tokens = (
    'INT', 'FLOAT',
    'NAME',
    'PLUS', 'MINUS', 'TIMES', 'DIVIDE',
    'EQUAL',
    'EQUALS', 'NOTEQUALS', 'BIGGER', 'BIGGEROR', 'SMALLER', 'SMALLEROR',
    'LPAREN', 'RPAREN'
) + tuple(reserved.values())

t_LPAREN  = r"\("
t_RPAREN  = r"\)"
t_PLUS = r'\+'
t_MINUS = r'\-'
t_TIMES = r'\*'
t_DIVIDE = r'\/'
t_EQUALS = r'\=\='
t_EQUAL = r'\='
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

def t_IF(t):
    r'if'
    return t

def t_THEN(t):
    r'then'
    return t

def t_NAME(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = 'NAME'
    return t

def t_error(t):
    print("Illegal Characters")
    t.lexer.skip(1)

lexer = lex.lex()

precedence = (
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE')
)

def p_calc(t):
    '''
    calc : expression
         | var_assign
         | if_statement
         | empty
    '''
    print(run(t[1]))

def p_var_assign(t):
    '''
    var_assign : NAME EQUAL expression
               | NAME EQUAL NAME
    '''
    t[0] = ('=', t[1], t[3])

def p_if_statement(t):
    '''
    if_statement : IF compare THEN NAME
    '''
    t[0] = ('==', t[2], t[4])

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

def p_expression(t):
    '''
    expression : expression TIMES expression
               | expression DIVIDE expression
               | expression PLUS expression
               | expression MINUS expression
    '''
    t[0] = (t[2], t[1], t[3])

def p_expression_int_float(t):
    '''
    expression : INT
               | FLOAT
    '''
    t[0] = t[1]

def p_expression_var(t):
    '''
    expression : NAME
    '''
    t[0] = ('var', t[1])

def p_empty(t):
    '''
    empty : 
    '''
    t[0] = None

def p_factor_paren(t):
  "expression : LPAREN expression RPAREN"
  t[0] = t[2]

def p_error(t):
    print("Syntax error!")

parser = yacc.yacc()

def run(t):
    if type(t) == tuple:
        if t[0] == '+':
            return run(t[1]) + run(t[2])

        elif t[0] == '-':
            return run(t[1]) - run(t[2])

        elif t[0] == '*':
            return run(t[1]) * run(t[2])

        elif t[0] == '/':
            try:
                return run(t[1]) / run(t[2])
            except ZeroDivisionError:
                print("Cant divide by 0")
                return 0

        elif t[0] == '=':
            variables[t[1]] = run(t[2])

        elif t[0] == 'var':
            if t[1] not in variables:
                return "Undeclared variable found"
            else:
                return variables[t[1]]

        elif t[0] == '==':
            if t[1]:
                print(t[2])
            else:
                pass

    else:
        return t

while True:
    try:
        s = input('>> ')
    except EOFError:
        break
    parser.parse(s)






