import re
import ply.lex as lex
import ply.yacc as yacc
import turtle
from frontEnd import *
max_size = 10  # Maximum elements in any datastructure due to limeted space in screen

totVar = [] # stores all variables declared as stack and queue
Queueint = [] # stores Queue variable names decalred as int
Queuestring = []
Queuefloat = []
Arrayint = [] # stores Array variable names decalred as int
Arraystring = []
Arrayfloat = []

Stackint =[]    # stores stack variable names declared as int
Stackstring =[]
Stackfloat=[]
LinkedList = []
CouDic = {}  # stores length of queue or stack
idsDic = {}
idsDic1 = {}
idsDic2 = {}
lin = 0
patternfloat = re.compile("([0-9]+)[.]([0-9]+)")
patternint = re.compile("[0-9]+")
patternchar = re.compile("([a-zA-Z]|[_])([0-9a-zA-Z]|(_))+")

data=''
tokens = (
	'LPARS',
	'RPARS',
	'NUMBER',
	'FLOAT',
	'PLUS',
	'MINUS',
	'MULT',
	'DIVI',
	'SEMICO',
	'EQUAL',
	'LE',
	'GE',
	'LL',
	'GG',
	'EE',
	'STRI',
	'COMMA',
	'INSER',
	'DELET',
	'ERASE',
	'APPEND',
    'ID',
    'LSBS',
    'RSBS',
	'OSB',
	'CSB',
	)
t_EE = r'=='
t_GE = r'>='
t_LE = r'<='
t_LL = r'<'
t_GG = r'>'
t_LSBS  = r'{'
t_RSBS  = r'}'
t_COMMA = r','
t_INSER = r'(.insert)'
t_DELET = r'(.delete)'
t_ERASE = r'(.erase)'
t_APPEND = r'(.append)'
t_PLUS    = r'\+'
t_MINUS   = r'-'
t_MULT   = r'\*'
t_DIVI  = r'/'
t_LPARS  = r'\('
t_RPARS  = r'\)'
t_SEMICO = r';'
t_EQUAL = r'='
t_OSB = r'\['
t_CSB = r'\]'

# A regular expression rule with some action code
def t_FLOAT(t):
	r'[\d]+[.][\d]+'
	t.value = float(t.value)
	return t

def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t

# A regular expression for detecting the string in double codes
def t_STRI(t):
	r'(\")[a-zA-Z]+[a-zA-Z0-9]*(\")'
	t.value = str(t.value)
	return t

# Define a rule so we can track line numbers
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# A string containing ignored characters (spaces and tabs)
t_ignore  = ' \t'

# Error handling rule
def t_error(t):
	if t.value[1]!='\n':
	    print "Illegal character '%s' " % t.value[0]
	t.lexer.skip(1)

RESERVED = {
	'Queue' : 'DSQ',
	'Stack' : 'DSS',
	'linkedlist':'DSL',
	'int'   : 'DTI',
	'float'	: 'DTF',
	'string':	'DTS',
	'if' : 'IF',
	'else' : 'ELSE',
	'for' : 'FOR',
	'in' : 'IN',
	'range' : 'RANGE',
	'Array' : 'ARRAY',
	'prints' : 'PRINTS',
	'Create' : 'CREATE'
}

tokens = tokens + tuple(RESERVED.values())

def t_ID(t):
	r'[a-zA-Z_][a-zA-Z0-9_]*'
	t.type = RESERVED.get(t.value, 'ID')    ##
	return t

t_ignore_COMMENT = r'\#.*'

#################################  parsing ####################################################
def match(k):												# getting know the datatype of the found token value
	if k in idsDic:
		if patternfloat.match(str(idsDic[k])):
			return "float"
		elif patternint.match(str(idsDic[k])):
			return "int"
		else:
			return "string"
	else:
		if patternfloat.match(str(k)):
			return "float"
		elif patternint.match(str(k)):
			return "int"
		else:
			return "string"

# From following onwards, we defined some grammar and performing some action according to the given program
def p_statement(p) :
	'''
	statement : start
	'''

def p_start(p):
	'''
	start : declaration start

	      | operations start

		  | expression SEMICO start

		  | IF condition SEMICO start

		  | IF  conditione SEMICO start

		  | FOR conditionw SEMICO start

		  | PRINTS something SEMICO start

		  | empty
	'''
	if p[1] == 'if':
		p[0] = [p[1],p[2]]
	else:
		p[0] = p[1]

def p_pri(p):
	'''
	something : ID
			  | STRI PLUS ID
			  | STRI
	'''
	if len(p) == 2:
		if p[1] in idsDic:
			print idsDic[p[1]]
		else:
			print p[1]
	else:
		 print p[1] + str(idsDic[p[3]])

def p_s(p):
	if (p[0] in Queueint) or (p[0] in Queuefloat) or (p[0] in Queuestring):
		printOperation(p[0] + ".insert(" + str(p[2]) + ")")
		queuePush(p[0],p[2])
		CouDic[p[0]] = CouDic[p[0]]+1
	elif (p[0] in Stackint) or (p[0] in Stackfloat) or (p[0] in Stackstring):
		printOperation(p[0] + ".insert(" + str(p[2]) + ")")
		stackPush(p[0],p[2])
		CouDic[p[0]] = CouDic[p[0]]+1
	else:
		s = -1
		for [i, j] in Arrayint:
			if i == p[0] and match(str(p[2]))=='int':
				s = j
				break
		for [i, j] in Arrayfloat:
			if i == p[0] and match(str(p[2]))=='float':
				s = j
				break
		for [i, j] in Arraystring:
			if i == p[0] and match(str(p[2]))=='string':
				s = j
				break
		if s!=-1:
			printOperation(p[0] + ".append(" + str(p[2]) + ")")
			arrayInsert(arrayid=p[0], element=str(p[2]), index=CouDic[p[0]], size=s )
			CouDic[p[0]] = CouDic[p[0]]+1
		else:
			print str(p[0])+" and "+str(p[2])+ " datatype mismatch\n"


def p_conw(p):
	'''
	conditionw : LPARS expressionz RPARS LSBS operations RSBS
	'''
	k = p[5][3]
	k1 = p[2][0]
	for p[2][0] in range(1,p[2][1]):
		if k in idsDic and k!=k1:
			p[5][2] = idsDic[k]
		if k == k1:
			p[5][2] = p[2][0]
		p_s(p[5])

def p_expz(p):
	'''
	expressionz : ID IN RANGE NUMBER
	'''
	idsDic[p[1]] = 0
	p[0] = [p[1] , p[4]]

def p_empty(p):
	'''
	empty :
	'''
	pass

def p_cone(p):
	'''
	conditione : LPARS expression RPARS LSBS start1 RSBS SEMICO ELSE LSBS start2 RSBS
	'''
	if int(p[2]) > 0:
		idsDic.update(idsDic1)
		idsDic1.clear()
		idsDic2.clear()
	else:
		idsDic.update(idsDic2)
		idsDic2.clear()
		idsDic1.clear()

	p[0] = [p[2],p[5]]

def p_con(p):
	'''
	condition : LPARS expression RPARS LSBS start1 RSBS
				| LPARS expression RPARS LSBS IF conditionz SEMICO RSBS

	'''
	if int(p[2]) > 0 and len(p) == 7:
		idsDic.update(idsDic1)
		idsDic1.clear()
		idsDic2.clear()
		p[0] = [p[2],p[5]]
	else:
		if int(p[2])>0 and int(p[6][1])>0:
			idsDic.update(idsDic1)
			idsDic1.clear()
			idsDic2.clear()
			p[0] = [p[2],p[6][1]]

def p_mn(p):
	'''
	conditionz : LPARS expression RPARS LSBS start1 RSBS
	'''
	if int(p[2]) > 0 and len(p) == 7:
		p[0] = [p[2],p[5]]

def p_exp(p):
	'''
	start1 : expressiona SEMICO start1

		   | empty
	'''
	p[0]=p[1]
def p_expp1(p):
	'''
	start2 : expressionb SEMICO start2
		   | empty
	'''
def p_Dec(p):
	'''
	declaration : CREATE ds dt ID SEMICO
				| CREATE ds ID SEMICO
				| CREATE ds dt ID OSB NUMBER CSB SEMICO
	'''
	if len(p) == 5:
		if p[3] in totVar :
			print "variable ",p[3]," re declared"
		if p[2] == "linkedlist":
			totVar.append(p[3])
			CouDic[p[3]] = 0                  ##### call create linked list here p[2] has variable name ################
			LinkedList.append(p[3])
			createLinked(p[3])
		p[0] = [p[2],p[3]]
	else:
		if p[4] in totVar:
			print "variable ",p[4]," re declared" ############# error here ##############
		if p[2] =='Queue':
			totVar.append(p[4])
			CouDic[p[4]] = 0
			printOperation("Create "+ p[3] + " Queue "+ p[4])
			createQueue(p[4])
			if p[3] == 'int':
				Queueint.append(p[4])
			elif p[3] == 'float':
				Queuefloat.append(p[4])
			elif p[3] == 'string':
				Queuestring.append(p[4])
		elif p[2] =='Array':
			totVar.append(p[4])
			CouDic[p[4]] = 0
			printOperation("Creating "+ str(p[3]) + " Array "+ str(p[4]) + " of Size "+ str(p[6]))
			createArray(p[4], p[6])
			if p[3] == 'int':
				Arrayint.append([ p[4], p[6] ])
			elif p[3] == 'float':
				Arrayfloat.append([ p[4], p[6] ])
			elif p[3] == 'string':
				Arraystring.append([ p[4], p[6] ])
		elif p[2] == 'Stack':
			printOperation("Create "+ p[3] + " Stack "+ p[4])
			createStack(p[4])
			totVar.append(p[4])
			CouDic[p[4]] = 0
			if p[3] == 'int':
				Stackint.append(p[4])
			elif p[3] == 'float':
				Stackfloat.append(p[4])
			elif p[3] == 'string':
				Stackstring.append(p[4])
		p[0] = [p[2],p[3],p[4]]
def p_operations(p):
	'''
	operations : ID oper  SEMICO
			   | ID OSB NUMBER CSB EQUAL NUMBER SEMICO
			   | ID OSB NUMBER CSB EQUAL FLOAT SEMICO
			   | ID OSB NUMBER CSB EQUAL STRI SEMICO
	'''
	if p[2] == '[':
		s = -1
		for [i, j] in Arrayint:
			if i == p[1] and match(str(p[6])) == 'int':
				s = j
				break
		for [i, j] in Arrayfloat:
			if i == p[1] and match(str(p[6]))== 'float':
				s = j
				break
		for [i, j] in Arraystring:
			if i == p[1] and match(str(p[6]))=='string':
				s = j
				break

		if s!=-1:
			printOperation("Inserting "+ str(p[6]) + " into "+ str(p[1]) + " at index " + str(p[3]))
			arrayInsert(p[1], p[3], p[6], s)
		else:
			print str(p[1]) + " and " + str(p[6]) + " datatypes are mismatch"
	else:
		t=p[2][0]
		if p[2][0] == '.erase' :
			if p[1] not in totVar:
				print "trying to delete not declared variable"
			elif p[1] in Queueint:
				Queueint.remove(p[1])
				printOperation(p[1] + ".erase()")
				eraseQueue(p[1])
				del CouDic[p[1]]
			elif p[1] in Queuefloat:
				Queuefloat.remove(p[1])
				printOperation(p[1] + ".erase()")
				eraseQueue(p[1])
				del CouDic[p[1]]
			elif p[1] in Queuestring:
				Queuestring.remove(p[1])
				printOperation(p[1] + ".erase()")
				eraseQueue(p[1])
				del CouDic[p[1]]
			elif p[1] in Stackint:
				printOperation(p[1] + ".erase()")
				eraseStack(p[1])
				Stackint.remove(p[1])
				del CouDic[p[1]]
			elif p[1] in Stackfloat:
				printOperation(p[1] + ".erase()")
				eraseStack(p[1])
				Stackfloat.remove(p[1])
				del CouDic[p[1]]
			elif p[1] in Stackstring:
				printOperation(p[1] + ".erase()")
				eraseStack(p[1])
				Stackstring.remove(p[1])
				del CouDic[p[1]]
			elif p[1] in LinkedList:
				printOperation(p[1] + ".erase()")
				eraseLinked(p[1])							# call erase linkedlist here p[1] has variable name###
				LinkedList.remove(p[1])
				del CouDic[p[1]]


		elif p[2][0] == '.insert':
			t=p[2][1]
			if p[2][1] in idsDic:
				p[2][1] = idsDic[p[2][1]]
			if p[1] not in totVar:
				print p[1],"variable not declared"
			elif p[1] in LinkedList:
				if CouDic[p[1]] == max_size:
					print "Max Size of Linked list reached"
				else :
					insertNode(p[2][1],p[2][2])
					CouDic[p[1]] = CouDic[p[1]]+1
					                              ##### call insert linked list here p[2][1] = position , p[2][2] = value #####
			elif p[1] in Queueint:
				if CouDic[p[1]] == max_size:
					print "MAX Size of Queue Reached"
				if match(str(p[2][1]))=='int':
					printOperation(p[1] + ".insert(" + str(p[2][1]) + ")")
					queuePush(p[1],p[2][1])
					CouDic[p[1]] = CouDic[p[1]]+1
				else:
					print str(p[1])+" and "+str(p[2][1])+ " datatype  mismatch\n"
			elif p[1] in Queuefloat:
				if CouDic[p[1]] == max_size:
					print "MAX Size of Queue Reached"
				if match(str(p[2][1]))=='float':
					printOperation(p[1] + ".insert(" + str(p[2][1]) + ")")
					queuePush(p[1],p[2][1])
					CouDic[p[1]] = CouDic[p[1]]+1
				else:
					print str(p[1])+" and "+str(p[2][1])+ " datatype mismatch\n"
			elif p[1] in Queuestring:
				if CouDic[p[1]] == max_size:
					print "MAX Size of Queue Reached"
				if match(str(p[2][1]))=='string':
					printOperation(p[1] + ".insert(" + str(p[2][1]) + ")")
					queuePush(p[1],p[2][1])
					CouDic[p[1]] = CouDic[p[1]]+1
				else:
					print str(p[1])+ " and "+str(p[2][1])+" datatype mismatch\n"
			elif p[1] in Stackint:
				if CouDic[p[1]] == max_size:
					print "MAX Size of Stack Reached"
				if match(str(p[2][1]))=='int':
					printOperation(p[1] + ".insert(" + str(p[2][1]) + ")")
					stackPush(p[1],p[2][1])
					CouDic[p[1]] = CouDic[p[1]]+1
				else:
					print str(p[1])+" and "+str(p[2][1])+ " datatype  mismatch\n"
			elif p[1] in Stackfloat:
				if CouDic[p[1]] == max_size:
					print "MAX Size of Stack Reached"
				if match(str(p[2][1]))=='float':
					printOperation(p[1] + ".insert(" + str(p[2][1]) + ")")
					stackPush(p[1],p[2][1])
					CouDic[p[1]] = CouDic[p[1]]+1
				else:
					print str(p[1])+" and "+str(p[2][1])+ " datatype mismatch\n"
			elif p[1] in Stackstring:
				if CouDic[p[1]] == max_size:
					print "MAX Size of Stack Reached"
				if match(str(p[2][1]))=='string':
					printOperation(p[1] + ".insert(" + str(p[2][1]) + ")")
					stackPush(p[1],p[2][1])
					CouDic[p[1]] = CouDic[p[1]]+1
				else :
					print str(p[1])+ " and "+str(p[2][1])+" datatype mismatch\n"
			elif p[1] in Stackfloat:
				if CouDic[p[1]] == max_size:
					print "MAX Size of Stack Reached"
				if match(str(p[2][1]))=='float':
					printOperation(p[1] + ".insert(" + str(p[2][1]) + ")")
					stackPush(p[1],p[2][1])
					CouDic[p[1]] = CouDic[p[1]]+1
				else:
					print str(p[1])+" and "+str(p[2][1])+ " datatype mismatch\n"
			elif p[1] in Stackstring:
				if CouDic[p[1]] == max_size:
					print "MAX Size of Stack Reached"
				if match(str(p[2][1]))=='string':
					printOperation(p[1] + ".insert(" + str(p[2][1]) + ")")
					stackPush(p[1],p[2][1])
					CouDic[p[1]] = CouDic[p[1]]+1
				else :
					print str(p[1])+ " and "+str(p[2][1])+" datatype mismatch\n"

		elif p[2][0] == '.append':
				t=p[2][1]
				if p[2][1] in idsDic:
					p[2][1] = idsDic[p[2][1]]
				if CouDic[p[1]] == max_size:
					print "MAX Size of Array Reached"
				if match(str(p[2][1]))=='int':
					s = -1
					for [i, j] in Arrayint:
						if i == p[1]:
							s = j
							break
					if s!=-1:
						printOperation(p[1] + ".append(" + str(p[2][1]) + ")")
						arrayInsert(arrayid=p[1], element=str(p[2][1]), index=CouDic[p[1]], size=s )
						CouDic[p[1]] = CouDic[p[1]]+1
					else:
						print str(p[1])+" and "+str(p[2][1])+ " datatype  mismatch\n"

				elif match(str(p[2][1]))=='float':
					s = -1
					for [i, j] in Arrayfloat:
						if i == p[1]:
							s = j
							break
					if s!=-1:
						printOperation(p[1] + ".append(" + str(p[2][1]) + ")")
						arrayInsert(arrayid=p[1], element=str(p[2][1]), index=CouDic[p[1]], size=s )
						CouDic[p[1]] = CouDic[p[1]]+1
					else:
						print str(p[1])+" and "+str(p[2][1])+ " datatype  mismatch\n"

				elif match(str(p[2][1]))=='string':
					s = -1
					for [i, j] in Arraystring:
						if i == p[1]:
							s = j
							break
					if s!=-1:
						printOperation(p[1] + ".append(" + str(p[2][1]) + ")")
						arrayInsert(arrayid=p[1], element=str(p[2][1]), index=CouDic[p[1]], size=s)
						CouDic[p[1]] = CouDic[p[1]]+1
					else:
						print str(p[1])+" and "+str(p[2][1])+ " datatype  mismatch\n"

				else:
					print str(p[1])+" and "+str(p[2][1])+ " datatype  mismatch\n"


		elif p[2][0] == '.delete':  #TODO changed
			if p[1] not in totVar:
				print p[1],"variable not declared"
			elif ((p[1] in Queueint) or (p[1] in Queuefloat) or (p[1] in Queuestring)) :
				if CouDic[p[1]] == 0:
					print "Min Size of queue Reached "
				else:
					printOperation(p[1] + ".pop()")
					queuePop(p[1])
	 				CouDic[p[1]] = CouDic[p[1]]-1
			elif (p[1] in Stackint) or (p[1] in Stackfloat) or (p[1] in Stackstring) :
				if CouDic[p[1]] == 0:
					print "Min SIze of Stack Reached"
				else:
					printOperation(p[1] + ".pop()")
					stackPop(p[1])
					CouDic[p[1]] = CouDic[p[1]]-1
			elif p[1] in LinkedList:
				if CouDic[p[1]] == 0:
					print "Min Size of linkedlist Reached"
				else:
					deleteNode(p[2][1])		######################## call delete linklist here p[2][1] has position #####
					CouDic[p[1]] = CouDic[p[1]]-1

		p[0] = [p[1],p[2][0],p[2][1],t]
def p_oper(p):
	'''
	oper : INSER LPARS NUMBER RPARS

		| INSER LPARS ID RPARS

		| INSER LPARS NUMBER COMMA NUMBER RPARS

		| INSER LPARS NUMBER COMMA ID RPARS

		| DELET LPARS RPARS

		| DELET LPARS NUMBER RPARS

		| ERASE LPARS RPARS

		| INSER LPARS FLOAT RPARS

		| INSER LPARS NUMBER COMMA FLOAT RPARS

		| DELET LPARS FLOAT RPARS

		| APPEND LPARS NUMBER RPARS

		| APPEND LPARS ID RPARS

		| APPEND LPARS FLOAT RPARS
	'''
	if len(p) == 7:
		p[0] = [p[1],p[3],p[5]]
	else:
		p[0] = [p[1],p[3]]

def p_ds(p):
	'''
	ds : DSQ

		| DSS
		| DSL
		| ARRAY
	'''
	p[0] = p[1]

def p_dt(p):
	'''
	dt : DTI

		| DTF

		| DTS
	'''
	p[0] = p[1]
def p_expression_plus(p):
    '''
    expression : expression PLUS term
    '''
    p[0] = p[1] + p[3]
def p_expression_minus(p):
    '''
    expression : expression MINUS term
    '''
    p[0] = p[1] - p[3]

def p_expression_term(p):
    '''
    expression : term
    '''
    p[0] = p[1]

def p_term_times(p):
    '''
    term : term MULT factor
    '''
    p[0] = p[1] * p[3]

def p_term_div(p):
    '''
    term : term DIVI factor
    '''
    p[0] = p[1] / p[3]

def p_term_factor(p):
    '''
    term : factor

    '''
    p[0] = p[1]
def p_fact_nu(p):
	'''
	factor : ID
	'''
	if p[1] in idsDic:
		p[0] = idsDic[p[1]]
	else :
		p[0] = p[1]
def p_factor_num(p):
    '''
    factor : NUMBER
    		| FLOAT

    '''
    p[0] = p[1]
def p_factor_equal(p):
	'''
	term : ID EQUAL factor
		| ID EQUAL factor PLUS factor
		| factor EE factor
		| factor GG factor
		| factor LL factor
		| factor LE factor
		| factor GE factor
	'''
	if (len(p) == 4) and (p[2] == '='):

		if (match(str(p[3])) == 'int') or (match(str(p[3])) == 'float'):
			p[0] = p[3]
			idsDic[p[1]] = p[3]
		else:
				if p[3] in idsDic:
					idsDic[p[1]] = idsDic[p[3]]
					p[0] = idsDic[p[3]]
				else:
					print "Error"
	elif len(p) == 4:
		p[0] = 0
		if ((match(str(p[3])) == 'int') or (match(str(p[3])) == 'float')) and ((match(str(p[1])) == 'int') or (match(str(p[1])) == 'float')):
			if p[2] == '==':
				if p[1] == p[3]:
					p[0] = 1
			elif p[2] == '>=':
				if p[1] >= p[3]:
					p[0] = 1
			elif p[2] == '<=':
				if p[1] <= p[3]:
					p[0]= 1
			elif p[2] == '<':
				if p[1] < p[3]:
					p[0]= 1
			elif p[2] == '>':
				if p[1] > p[3]:
					p[0]=1
		elif (match(str(p[3])) == 'int') or (match(str(p[3])) == 'float'):
			if p[1] in idsDic:
				p[1] = idsDic[p[1]]
				if p[2] == '==':
					if p[1] == p[3]:
						p[0] = 1
				elif p[2] == '>=':
					if p[1] >= p[3]:
						p[0] = 1
				elif p[2] == '<=':
					if p[1] <= p[3]:
						p[0]= 1
				elif p[2] == '<':
					if p[1] < p[3]:
						p[0]= 1
				elif p[2] == '>':
					if p[1] > p[3]:
						p[0]=1
			else:
				print "Error"
		elif  (match(str(p[1])) == 'int') or (match(str(p[1])) == 'float'):
			if p[3] in idsDic:
				p[3] = idsDic[p[3]]
				if p[2] == '==':
					if p[1] == p[3]:
						p[0] = 1
				elif p[2] == '>=':
					if p[1] >= p[3]:
						p[0] = 1
				elif p[2] == '<=':
					if p[1] <= p[3]:
						p[0]= 1
				elif p[2] == '<':
					if p[1] < p[3]:
						p[0]= 1
				elif p[2] == '>':
					if p[1] > p[3]:
						p[0]=1
			else:
				print "Error"
		else:

			if (p[1] in idsDic) and (p[3] in idsDic):
				p[1] = idsDic[p[1]]
				p[3] = idsDic[p[3]]
				if p[2] == '==':
					if p[1] == p[3]:
						p[0] = 1
				elif p[2] == '>=':
					if p[1] >= p[3]:
						p[0] = 1
				elif p[2] == '<=':
					if p[1] <= p[3]:
						p[0]= 1
				elif p[2] == '<':
					if p[1] < p[3]:
						p[0]= 1
				elif p[2] == '>':
					if p[1] > p[3]:
						p[0]=1
			else :
				if p[2] == '==':
					if p[1] == p[3]:
						p[0] = 1
				elif p[2] == '>=':
					if p[1] >= p[3]:
						p[0] = 1
				elif p[2] == '<=':
					if p[1] <= p[3]:
						p[0]= 1
				elif p[2] == '<':
					if p[1] < p[3]:
						p[0]= 1
				elif p[2] == '>':
					if p[1] > p[3]:
						p[0]=1


	else:
		if (match(str(p[3])) == 'int') or (match(str(p[3])) == 'float'):
				if (match(str(p[5])) == 'int') or (match(str(p[5])) == 'float'):
					idsDic[p[1]] = p[3] + p[5]
					p[0] = p[3]+p[5]
				else :
					if p[5] in idsDic:
						idsDic[p[1]] = p[3] + idsDic[p[5]]
						p[0] = idsDic[p[1]]

					else:
						print "Error at",p[5]
		else :
				if (match(str(p[5])) == 'int') or (match(str(p[5])) == 'float') :
					if p[3] in idsDic:
						idsDic[p[1]] = idsDic[p[3]] + p[5]
						p[0] = p[3]+p[5]
				else :
					if (p[5] in idsDic) and (p[3] in idsDic):
						idsDic[p[1]] = idsDic[p[3]] + idsDic[p[5]]
						p[0] = idsDic[p[1]]

					else:
						print "error at",p[3],p[5]

def p_factor_expr(p):
    '''
    factor : LPARS expression RPARS SEMICO
    '''
    p[0] = p[2]

def p_expression_plus1(p):
    '''
    expressiona : expressiona PLUS terma
    '''
    p[0] = p[1] + p[3]

def p_expression_minus1(p):
    '''
    expressiona : expressiona MINUS terma
    '''
    p[0] = p[1] - p[3]

def p_expression_terma(p):
    '''
    expressiona : terma
    '''
    p[0] = p[1]

def p_term_times1(p):
    '''
    terma : terma MULT factora
    '''
    p[0] = p[1] * p[3]

def p_term_div1(p):
    '''
    terma : terma DIVI factora
    '''
    p[0] = p[1] / p[3]

def p_term_factora(p):
    '''
    terma : factora

    '''
    p[0] = p[1]

def p_fact_nu1(p):
	'''
	factora : ID
	'''
	if p[1] in idsDic:
		p[0] = idsDic[p[1]]
	else :
		p[0] = p[1]

def p_factor_num1(p):
    '''
    factora : NUMBER
            | FLOAT

    '''
    p[0] = p[1]

def p_factor_equal1(p):
	'''
	terma : ID EQUAL factora
		| ID EQUAL factora PLUS factora

	'''
	if len(p) == 4:
		if (match(str(p[3])) == 'int') or (match(str(p[3])) == 'float'):
			p[0] = p[3]
			idsDic1[p[1]] = p[3]
		else:
				if p[3] in idsDic:
					idsDic1[p[1]] = idsDic[p[3]]
					p[0] = idsDic[p[3]]
				else:
					print "Error"
	else:
		if (match(str(p[3])) == 'int') or (match(str(p[3])) == 'float'):
				if (match(str(p[5])) == 'int') or (match(str(p[5])) == 'float'):
					idsDic1[p[1]] = p[3] + p[5]
					p[0] = p[3]+p[5]
				else :
					if p[5] in idsDic:
						idsDic1[p[1]] = p[3] + idsDic[p[5]]
						p[0] = idsDic1[p[1]]

					else:
						print "Error at",p[5]
		else :
				if (match(str(p[5])) == 'int') or (match(str(p[5])) == 'float') :
					if p[3] in idsDic:
						idsDic1[p[1]] = idsDic[p[3]] + p[5]
						p[0] = p[3]+p[5]
				else :
					if (p[5] in idsDic) and (p[3] in idsDic):
						idsDic1[p[1]] = idsDic[p[3]] + idsDic[p[5]]
						p[0] = idsDic1[p[1]]

					else:
						print "Error at",p[3],p[5]

def p_factor_expr1(p):
    '''
    factora : LPARS expressiona RPARS SEMICO
    '''
    p[0] = p[2]

def p_expression_plus2(p):
    '''
    expressionb : expressionb PLUS termb
    '''
    p[0] = p[1] + p[3]

def p_expression_minus2(p):
    '''
    expressionb : expressionb MINUS termb
    '''
    p[0] = p[1] - p[3]

def p_expression_term2(p):
    '''
    expressionb : termb
    '''
    p[0] = p[1]

def p_term_times2(p):
    '''
    termb : termb MULT factorb
    '''
    p[0] = p[1] * p[3]

def p_term_div2(p):
    '''
    termb : termb DIVI factorb
    '''
    p[0] = p[1] / p[3]

def p_term_factor2(p):
    '''
    termb : factorb

    '''
    p[0] = p[1]

def p_fact_nu2(p):
	'''
	factorb : ID
	'''
	if p[1] in idsDic:
		p[0] = idsDic[p[1]]
	else :
		p[0] = p[1]

def p_factor_num2(p):
    '''
    factorb : NUMBER

            | FLOAT
    '''
    p[0] = p[1]

def p_factor_equal2(p):
	'''
	termb : ID EQUAL factorb
		| ID EQUAL factorb PLUS factorb

	'''
	if len(p) == 4:
		if (match(str(p[3])) == 'int') or (match(str(p[3])) == 'float'):
			p[0] = p[3]
			idsDic2[p[1]] = p[3]
		else:
				if p[3] in idsDic:
					idsDic2[p[1]] = idsDic[p[3]]
					p[0] = idsDic[p[3]]
				else:
					print "Error"
	else:
		if (match(str(p[3])) == 'int') or (match(str(p[3])) == 'float'):
				if (match(str(p[5])) == 'int') or (match(str(p[5])) == 'float'):
					idsDic2[p[1]] = p[3] + p[5]
					p[0] = p[3]+p[5]
				else :
					if p[5] in idsDic:
						idsDic2[p[1]] = p[3] + idsDic[p[5]]
						p[0] = idsDic2[p[1]]

					else:
						print "Error at",p[5]
		else :
				if (match(str(p[5])) == 'int') or (match(str(p[5])) == 'float') :
					if p[3] in idsDic:
						idsDic2[p[1]] = idsDic[p[3]] + p[5]
						p[0] = p[3]+p[5]
				else :
					if (p[5] in idsDic) and (p[3] in idsDic):
						idsDic2[p[1]] = idsDic[p[3]] + idsDic[p[5]]
						p[0] = idsDic2[p[1]]

					else:
						print "Error at",p[3],p[5]

def p_factor_expr2(p):
    '''
    factorb : LPARS expressionb RPARS SEMICO
    '''
    p[0] = p[2]

def p_error(p):

	print "Syntax Error at " ,p

#main

turtle.setup( width = 1366, height = 768, startx = None, starty = None)
s = turtle.Shape("compound")
poly1 = ((0,0),(50,0),(50,50),(0,50))
s.addcomponent(poly1,"white","black")
turtle.register_shape("myshape",s)
codefile = open('SampleCodes/input_code.txt','r+')
data = codefile.read()
size = 9
if len(data.split('\n')) > 27:
	size = 8

erasableWrite((530,-220), data ,font = ("courier", size, "normal"),align = "center")

initGui()

lexer = lex.lex()
lexer.input(data)

# Tokenize
print "..........................Tokens Recognized:...........................\n"
while True:
    tok = lexer.token()
    if not tok: break      # No more input
    print tok
print "#######################################################################"
print "Lexical Analysis Done \n"
print ".......................Parsing ................................\n"
print "Parsing Done \n"

yacc.yacc()
yacc.parse(data)

turtle.mainloop()
