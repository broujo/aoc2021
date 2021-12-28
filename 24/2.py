#!/usr/bin/env python

from z3 import *
import re
from string import ascii_lowercase
from functools import reduce

inp = re.compile('^inp (.*)$')
cmdre = re.compile('^(.*) (.*) (.*)$')

vars = {}

s = Solver()

for c in ascii_lowercase:
    vars[c] = 0
    x = Int(c + "%d" % vars[c])
    s.add(x == 0)

f = open("input.txt", "r")
for l in f:
    l = l.rstrip()
    minp = inp.match(l)
    mcmd = cmdre.match(l)
    if minp:
        var = minp.group(1)
        vars[var] += 1
        x = Int(var + "%d" % vars[var])
        s.add (x > 0, x < 10)

    elif mcmd:
        cmd = mcmd.group(1)
        vara = mcmd.group(2)
        varb = mcmd.group(3)
        
        asrc = Int(vara + "%d" % vars[vara])
        vars[vara] += 1

        adst = Int(vara + "%d" % vars[vara])
        b = None
        if re.match(r"[-]?\d+", varb):
            b = int(varb)
        else:
            b = Int(varb + "%d" % vars[varb])

        if cmd == "add":
            s.add (adst == asrc + b)
        elif cmd == "mul":
            s.add (adst == asrc * b)
        elif cmd == "div":
            s.add (adst == asrc / b)
        elif cmd == "mod":
            s.add (adst == asrc % b)
        elif cmd == "eql":
            s.add (If(asrc == b, adst == 1, adst == 0))
        else:
            print("Command %s not found" % cmd)

zlast = Int("z" + "%d" % vars["z"])
s.add(zlast == 0)

w = []
for wi in range(14):
    wi = wi + 1
    wi = Int("w%d" % wi)
    w.append(wi)

model = None
while s.check() == sat:
    model = s.model()
    ws = ""
    for wi in w:
        ws += str(s.model()[wi])

    print("Found %s" % ws)
    wi = int(ws)
    
    expr = [x * 10 ** (14 - 1 - i) for i, x in enumerate(w)]

    s.add(reduce(lambda x, y: x+y, expr) < wi)

ws = ""
for wi in w:
    ws += str(model[wi])

print(ws)
