import ply.lex as lex
import ply.yacc as yacc

import os
import time

variables = {}

reserved = {
    'si' : 'IF',
    'entonces' : 'THEN',
    'mas' : 'PLUS',
    'menos' : 'MINUS',
    'por' : 'TIMES',
    'dividido' : 'DIVIDE',
    'dance' : 'DANCE',
    'log' : 'PRINT'
}

tokens = (
    'INT', 'FLOAT',
    'ID', 'NAME', 'LNAME',
    'EQUAL', 'EQUALS', 'NOTEQUALS', 'BIGGER', 'BIGGEROR', 'SMALLER', 'SMALLEROR',
    'LPAREN', 'RPAREN'
) + tuple(reserved.values())

t_LPAREN  = r"\("
t_RPAREN  = r"\)"
t_PLUS = r'mas'
t_MINUS = r'menos'
t_TIMES = r'por'
t_DIVIDE = r'dividido'
t_IF = r'si'
t_THEN = r'entonces'
t_DANCE = r'dance'
t_PRINT = r'log'
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

def t_LNAME(t):
    r'\'(\s*\w*\s*)*\''
    return t

def t_NAME(t):
    r'\"(\s*\w*\s*)*\"'
    return t

def t_error(t):
    print("\033[1;31;40m   (  Illegal Characters  )\033[0;37;40m")
    t.lexer.skip(1)

lexer = lex.lex()

variables = {}
ErrorVariables = {}
ErrorVariables['ErrorCount'] = 0

precedence = (
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE'),
    ('left', 'LPAREN', 'RPAREN'),)

def p_result(t):
    '''
    result : expression
           | if_statement
           | print
           | empty
    '''
    Answer(t[1])

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
    try:
        t[0] = variables[t[1]]

    except LookupError:
        print('')
        print("\033[1;33;40m   (  Variable indefinida '%s'  )\033[0;37;40m" % t[1])
        print("┐(ﾟ ～ﾟ )┌")
        time.sleep(1)
        t[0] = None

def p_expression(t):
    '''
    expression : expression TIMES expression
               | expression DIVIDE expression
               | expression PLUS expression
               | expression MINUS expression
    '''
    if t[2] == 'mas':
        t[0] = t[1] + t[3]

        if ErrorVariables['ErrorCount'] > 0:
            ErrorVariables['ErrorCount'] -= 1

    elif t[2] == 'menos':
        t[0] = t[1] - t[3]

        if ErrorVariables['ErrorCount'] > 0:
            ErrorVariables['ErrorCount'] -= 1

    elif t[2] == 'por':
        t[0] = t[1] * t[3]

        if ErrorVariables['ErrorCount'] > 0:
            ErrorVariables['ErrorCount'] -= 1

    elif t[2] == 'dividido':
        try:
            t[0] = t[1] / t[3]

            if ErrorVariables['ErrorCount'] > 0:
                ErrorVariables['ErrorCount'] -= 1
        except ZeroDivisionError:
            print("")
            print("\033[1;33;40m   (  Cant divide by 0  )\033[0;37;40m")
            print("(ﾉﾟοﾟ)ﾉ")
            time.sleep(1)
            t[0] = None

def p_expression_dance(t):
    '''
    expression : DANCE
    '''
    dance()
    t[0] = None

def p_print(t):
    '''
    print : PRINT LPAREN expression RPAREN
    '''
    print(t[3])

def p_if_statement(t):
    '''
    if_statement : IF compare THEN expression
                 | IF compare THEN print
    '''
    if t[2]:
        t[0] = t[4]
    else:
        t[0] = None

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
    t[0] = None

def p_error(t):
    print("")
    print("\033[1;31;40m   (  Syntax Error!  )\033[0;37;40m")
    print("(╬≖_≖)")
    time.sleep(1)
    ErrorVariables['ErrorCount'] += 1

def dance():
    tmp = 0

    for i in range(8):
        if tmp == 0:
            print("♪┏(・o･)┛♪")
            time.sleep(0.7)
            os.system('CLS')

            tmp = 1
        else:
            print("♪┗ ( ･o･) ┓♪")
            time.sleep(0.7)
            os.system('CLS')

            tmp = 0

    if ErrorVariables['ErrorCount'] > 1:
        ErrorVariables['ErrorCount'] -= 2
    else:
        ErrorVariables['ErrorCount'] = 0

def Answer(t):
    if ErrorVariables['ErrorCount'] > 15:
        print("\033[1;37;40m     Te equivocaste mucho, no te quiero contestar!\033[0;37;40m")
        print("٩ (╬ʘ益ʘ╬) ۶")
        time.sleep(1)
        return
    
    if t != None:
        print("          ", t)

def AIMessage():

    if ErrorVariables['ErrorCount'] == 6:
        print("           _/==+\ Reglas del Lenguaje /+==\_\n",
              '            "   "  =================  "   "  ')
        print("---------------------------------------------------------")
        print("                      Suma = 'mas'\n",
              "                    Resta = 'menos'\n",
              "                 Multiplicacion = 'por'\n",
              "                  Division = 'dividido'\n")
        print("---------------------------------------------------------")
        ErrorVariables['ErrorCount'] += 1
        return input('\033[1;47;40m(ﾉﾟοﾟ)ﾉ\033[0;37;40m')

    if ErrorVariables['ErrorCount'] == 10:
        print("No se si me estas tratando enojar o que?")
        ErrorVariables['ErrorCount'] += 1
        return input('\033[1;47;40m(￣ｰ￣)\033[0;37;40m')

    if ErrorVariables['ErrorCount'] < 7:
        return input('\033[1;47;40m( /・・)ノ\033[0;37;40m')

    elif ErrorVariables['ErrorCount'] > 6 and ErrorVariables['ErrorCount'] < 10:
        return input('\033[1;47;40m(¬▂¬)\033[0;37;40m')

    elif ErrorVariables['ErrorCount'] > 10:
        return input('\033[1;47;40m(╬≖_≖)\033[0;37;40m')

parser = yacc.yacc()

os.system('CLS')

while True:
    try:
        s = AIMessage()
        os.system('CLS')
    except EOFError:
        break
    parser.parse(s)