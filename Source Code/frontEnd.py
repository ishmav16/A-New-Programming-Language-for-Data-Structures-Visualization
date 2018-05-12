import turtle
import time
arrayPositon = [-380, 300]
arrayPositons = [0, 0, 0, 0]
arrayinit = {}
stackPositon = [-650,-290]
stackinit = {}
stackPositons = [0, 0, 0, 0]
queuePosition = [5, 30]
queueinit = {}
queuePositons = [0, 0, 0, 0]

############linked list
presentpostion = [-330,-300]
linkposition = [-300,-200]
positions = []
nodeAddress = {}
address = 0
lines = []
linkedname = []
prFlag = 0
pr = 0


def erasableWrite(diagonal, name, font, align, reuse = None, clr = "Black" ):
    eraser  =  turtle.Turtle() if reuse is None else reuse
    eraser.hideturtle()
    eraser.speed(0)
    eraser.up()
    eraser.setposition(diagonal)
    eraser.color(clr)
    eraser.write(name, font = font, align = align)
    return eraser

def printOperation(st):
    global prFlag
    global pr
    if prFlag :
        pr.clear()


    pr = erasableWrite((510,-290), st ,font = ("Arial", 12, "bold"),align = "center", clr = "Red")

    prFlag = 1

def createArray(arrayid, size):
    alex = turtle.Turtle()
    alex.hideturtle()
    alex.penup()
    alex.speed(1)
    position = -1
    for i in range(0,len(arrayPositons)):
        if arrayPositons[i] == 0:
            position = i
            break
    arrayPositons[position] = 1
    pos1 = 300-80*position
    alex.setposition(arrayPositon[0], pos1)
    alex.color("Green")
    alex.pendown()
    alex.forward(60*size)
    alex.left(90)
    alex.forward(40)
    alex.left(90)
    alex.forward(60*size)
    alex.left(90)
    alex.forward(40)
    alex.left(90)

    for i in range(0, size):
        alex.forward(60)
        alex.left(90)
        alex.forward(40)
        alex.backward(40)
        alex.right(90)
        namepos = (arrayPositon[0]+25+(i*60), pos1-25)
        arrayName = erasableWrite(namepos, i ,font = ("Arial", 12, "bold"),align = "center", clr="Green")

    namepos = (arrayPositon[0]-45, pos1)
    arrayName = erasableWrite(namepos,arrayid,font = ("Arial", 12, "bold"),align = "center", clr="Green")
    temp = []
    temp.append(alex)
    vals = {}
    temp.append(vals)
    arrayinit[arrayid] = temp
    return


def arrayInsert(arrayid, index, element, size):
    array = arrayinit[arrayid]
    alex = array[0]
    vals = array[1]
    alex.hideturtle()
    alex.left(90)
    alex.forward(40)
    alex.left(90)
    alex.forward((size-(index+1))*60)
    pos = alex.position()
    alex.forward(60)
    alex.left(90)
    alex.forward(40)
    pos1 = alex.position()
    z1 = (pos[0]+pos1[0])/2
    z2 = (pos[1]+pos1[1])/2
    diag = (z1,z2)
    if index in vals.keys():
        er = vals[index]
        er.clear()
    eraseble  =  erasableWrite(diag, element, font = ("Arial", 10, "bold"), align = "center")
    vals[index] = eraseble
    alex.left(90)
    alex.forward((size-index)*60)
    return


def createStack(stackid):
    alex = turtle.Turtle()
    alex.hideturtle()
    alex.penup()
    alex.speed(1)
    position = -1
    for i in range(0,len(stackPositons)):
        if stackPositons[i] == 0:
            position = i
            break
    stackPositons[position] = 1
    pos1 = 100*position-500
    alex.setposition(pos1,stackPositon[1])
    alex.pendown()
    alex.forward(50)
    alex.left(90)
    alex.forward(300)
    alex.left(90)
    alex.forward(50)
    alex.left(90)
    alex.forward(300)
    namepos = (pos1+25,stackPositon[1]-25)
    stackName = erasableWrite(namepos,stackid,font = ("Arial", 12, "bold"),align = "center")
    temp = []
    temp.append(alex)
    stackWords = []
    temp.append(stackWords)
    stackElements = []
    temp.append(stackElements)
    temp.append(stackName)
    stackinit[stackid] = temp
    return

def stackPush(stackid,pushElement):
    stack = stackinit[stackid]
    StackBill = stack[0]
    pos = StackBill.position()
    StackBill.left(90)
    StackBill.forward(50)
    StackBill.left(90)
    StackBill.forward(40)
    StackBill.left(90)
    pos1 = StackBill.position()
    StackBill.forward(50)
    StackBill.left(90)
    z1 = (pos[0]+pos1[0])/2
    z2 = (pos[1]+pos1[1])/2
    diag = (z1,z2)
    eraseble  =  erasableWrite(diag,pushElement, font = ("Arial", 10, "normal"), align = "center")
    texteraser = stack[1]
    texteraser.append(eraseble)
    stack[1] = texteraser
    stack[0] = StackBill
    elements = stack[2]
    elements.append(pushElement)
    stack[2] = elements
    stackinit[stackid] = stack
    return

def stackPop(stackid):
    stack = stackinit[stackid]
    StackBill = stack[0]
    StackBill.pencolor("white")
    StackBill.left(90)
    StackBill.forward(50)
    StackBill.pencolor("black")
    StackBill.right(90)
    StackBill.forward(40)
    StackBill.right(90)
    StackBill.forward(50)
    StackBill.left(90)
    texteraser = stack[1]
    temp = texteraser.pop()
    temp.clear()
    stack[1] = texteraser
    stack[0] = StackBill
    elements = stack[2]
    elements.pop()
    stack[2] = elements
    stackinit[stackid] = stack
    return

def eraseStack(stackid):
    stack = stackinit[stackid]
    alex = stack[0]
    pos = alex.position()
    for i in stack[1]:
        i.clear()
    alex.clear()
    alex.hideturtle()
    stackinit.pop(stackid)
    pos1 = int(pos[0])
    position = 500+pos1
    post = position/100
    stackPositons[post] = 0
    stackName = stack[3]
    stackName.clear()
    return

def createQueue(queueId):
    alex  =  turtle.Turtle()
    alex.hideturtle()
    alex.penup()
    alex.speed(1)
    position = -1
    for i in range(0,len(queuePositons)):
        if queuePositons[i] == 0:
            position = i
            break
    queuePositons[position] = 1
    alex.setposition(queuePosition[0], 15-(position*80))
    alex.right(90)
    alex.pendown()
    alex.forward(50)
    alex.left(90)
    alex.forward(350)
    alex.left(90)
    alex.forward(50)
    alex.left(90)
    alex.forward(350)
    temp = []
    temp.append(alex)
    queueWords = []
    temp.append(queueWords)
    queueELements = []
    temp.append(queueELements)
    namepos = (queuePosition[0]-25, 15-(position*80)-25)
    queuename = erasableWrite(namepos,queueId,font = ("Arial", 12, "bold"),align = "center")
    temp.append(queuename)
    queueinit[queueId] = temp
    return

def queuePush(queueId,pushElement):
    queue = queueinit[queueId]
    queueBill = queue[0]
    queueBill.left(90)
    pos = queueBill.position()
    queueBill.forward(50)
    queueBill.left(90)
    queueBill.forward(50)
    pos1 = queueBill.position()
    queueBill.left(90)
    queueBill.forward(50)
    queueBill.left(90)
    z1 = (pos[0]+pos1[0])/2
    z2 = (pos[1]+pos1[1])/2
    diag = (z1,z2)
    eraseble  =  erasableWrite(diag,pushElement, font = ("Arial", 10, "normal"), align = "center")
    texteraser = queue[1]
    texteraser.append(eraseble)
    queue[1] = texteraser
    queue[0] = queueBill
    elements = queue[2]
    elements.append(pushElement)
    queue[2] = elements
    queueinit[queueId] = queue
    return

def queuePop(queueId):
    queue = queueinit[queueId]
    queueBill = queue[0]
    queueBill.left(90)
    queueBill.pencolor("white")
    queueBill.forward(50)
    queueBill.right(90)
    queueBill.pencolor("black")
    queueBill.forward(50)
    queueBill.right(90)
    queueBill.forward(50)
    queueBill.left(90)
    elements = queue[2]
    elements.pop(0)
    queue[2] = elements
    texteraser = queue[1]
    temp  =  texteraser.pop(0)
    #print temp
    temp.clear()
    i = 0
    while i<len(texteraser):
        eraseble  =  erasableWrite(temp.position(),elements[i],font = ("Arial", 10, "normal"), align = "center")
        temp  =  texteraser[i]
        texteraser[i].clear()
        texteraser[i] = eraseble
        # print temp.position()
        i = i+1
    queue[1] = texteraser
    queue[0] = queueBill
    queueinit[queueId] = queue
    return

def eraseQueue(queueId):
    queue = queueinit[queueId]
    alex = queue[0]
    pos = alex.position()
    for i in queue[1]:
        i.clear()
    alex.clear()
    alex.hideturtle()
    queueinit.pop(queueId)
    pos1 = int(pos[1])
    position = 300-pos1
    post = position/100
    queuePositons[post] = 0
    queuename = queue[3]
    queuename.clear()
    #print queuePositons
    return

def createLinked(linkedId):
    pos1 = (linkposition[0]-25,linkposition[1]-30)
    lin  =  erasableWrite(pos1,linkedId,font = ("Arial", 12, "bold"), align = "center")
    linkedname.append(lin)
    return

def createNode(value):
    alex  =  turtle.Turtle()
    alex.speed(1)
    alex.shape("myshape")
    alex.penup()
    alex.hideturtle()
    alex.setposition(presentpostion[0],presentpostion[1])
    alex.showturtle()
    presentpostion[0] = presentpostion[0]
    global address
    address = address+1
    temp = []
    temp.append(value)
    temp.append(-100)
    pos1 = alex.position()
    k = (pos1[0]+50)
    z = (pos1[1]-50)
    diag = ((k+pos1[0])/2,(z+pos1[1])/2)
    eraseble  =  erasableWrite(diag,value, font = ("Arial", 10, "normal"), align = "center")
    temp.append(eraseble)
    temp.append(alex)
    nodeAddress[address] = temp
    return address

def movenodes(move,add):
    while (len(move))>0:
        node = move.pop()
        temp = add.pop()
        bill = node[3]
        bill.penup()
        bill.forward(70)
        node[3] = bill
        eraseble = node[2]
        eraseble.clear()
        NodePosition = bill.position()
        k = (NodePosition[0]+50)
        z = (NodePosition[1]-50)
        diag = ((k+NodePosition[0])/2,(z+NodePosition[1])/2)
        eraseble  =  erasableWrite(diag,node[0], font = ("Arial", 10, "normal"), align = "center")
        node[2] = eraseble
        node[3] = bill
        nodeAddress[temp] = node
    return

def insertNode(pos,value):
    createdad = createNode(value)
    if pos<len(positions):
        if pos>0:
            j = positions[pos-1]
            previous = nodeAddress[j]
            previous[1] = createdad
        present = nodeAddress[createdad]
        present[1] = positions[pos]
        nodeAddress[createdad] = present
        address = positions[pos]
        mainnode = nodeAddress[address]
        temp = address
        positions[pos] = createdad
        move = []
        add = []
        while temp!= -100:
            node = nodeAddress[temp]
            pos = pos+1
            if len(positions) == pos:
                positions.append(temp)
            else:
                positions[pos] = temp
            move.append(node)
            add.append(temp)
            temp = node[1]
        movenodes(move,add)
        alex = mainnode[3]
        mani = nodeAddress[createdad]
        eraseble = mani[2]
        eraseble.clear()
        z = mani[3]
        z.penup()
        z.setposition(alex.position()[0]-70,alex.position()[1])
        NodePosition = z.position()
        k = (NodePosition[0]+50)
        y = (NodePosition[1]-50)
        diag = ((k+NodePosition[0])/2,(y+NodePosition[1])/2)
        eraseble  =  erasableWrite(diag,mani[0], font = ("Arial", 10, "normal"), align = "center")
        mani[2] = eraseble
        mani[3] = z
        nodeAddress[createdad] = mani
    elif pos == len(positions):
        if pos == 0:
            mani = nodeAddress[createdad]
            z = mani[3]
            z.penup()
            eraseble = mani[2]
            eraseble.clear()
            z.setposition(linkposition)
        else:
            address = positions[pos-1]
            node = nodeAddress[address]
            node[1] = createdad
            nodeAddress[address] = node
            alex = node[3]
            mani = nodeAddress[createdad]
            z = mani[3]
            z.penup()
            eraseble = mani[2]
            eraseble.clear()
            z.setposition(alex.position()[0]+70,alex.position()[1])
            # z.forward(70)
        NodePosition = z.position()
        k = (NodePosition[0]+50)
        y = (NodePosition[1]-50)
        diag = ((k+NodePosition[0])/2,(y+NodePosition[1])/2)
        eraseble  =  erasableWrite(diag,mani[0], font = ("Arial", 10, "normal"), align = "center")
        mani[2] = eraseble
        mani[3] = z
        nodeAddress[createdad] = mani
        positions.append(createdad)
    else:
        print "err:no position found in linked"
    return

def deleteNode(pos):
    delposition = positions[pos]
    itemdel = nodeAddress.pop(delposition)
    alex = itemdel[3]
    alex.hideturtle()
    alex.clear()
    texterase = itemdel[2]
    texterase.clear()
    if pos>0:
        previous = positions[pos-1]
        prevElement = nodeAddress[previous]
        prevElement[1] = itemdel[1]
        nodeAddress[previous] = prevElement
    temp = itemdel[1]
    positions.remove(delposition)
    while temp!= -100:
        node = nodeAddress[temp]
        alex = node[3]
        texteraser = node[2]
        diag = (texteraser.position()[0]-70,texteraser.position()[1])
        texteraser.clear()
        alex.penup()
        alex.backward(70)
        eraseble  =  erasableWrite(diag,node[0], font = ("Arial", 10, "normal"), align = "center")
        node[2] = eraseble
        node[3] = alex
        nodeAddress[temp] = node
        temp = node[1]
    return

def eraseLinked(s):
    for i in positions:
        node  =  nodeAddress.pop(i)
        alex = node[3]
        alex.clear()
        alex.hideturtle()
        texterase = node[2]
        texterase.clear()
    eraser = linkedname.pop()
    eraser.clear()
    clearlines()
    return

def linklines():
    alex = turtle.Turtle()
    alex.hideturtle()
    alex.penup()
    alex.setposition(linkposition[0]+50,linkposition[1]-25)
    if len(positions)>1:
        i = 0
        while i<len(positions)-1:
            alex.pendown()
            alex.forward(20)
            alex.penup()
            alex.forward(50)
            i = i+1
    lines.append(alex)
    return

def clearlines():
    if len(lines)>0:
        erase = lines.pop()
        erase.clear()
    return

def initGui():
# stackPositon = [-650,-290]
# stackinit = {}
# stackPositons = [0,0,0,0,0]
# queuePosition = [5,150]
    pos1 = (-530, 40)
    stackname = erasableWrite(pos1,"Stack Partition:",font = ("Arial", 12, "bold"),align = "center")
    #[50,300]
    pos2 = (5, 40)
    queuename = erasableWrite(pos2,'Queue Partition:',font = ("Arial", 12, "bold"),align = "center")

    erasableWrite((stackPositon[0]+120, 325),'Array Partition:',font = ("Arial", 12, "bold"),align = "center")
    erasableWrite((430,305),'Code:',font = ("Arial", 12, "bold"),align = "center")
    erasableWrite((520,-260),'Current Executing Operation:',font = ("Arial", 12, "bold"),align = "center")

    alex  =  turtle.Turtle()
    alex.hideturtle()
    alex.penup()
    alex.speed(5)
    alex.setposition(390,380)
    alex.pendown()
    alex.left(270)
    alex.forward(800)
    alex.setposition(390,-230)
    alex.left(90)
    alex.forward(300)
