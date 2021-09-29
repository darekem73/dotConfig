#!/usr/bin/python

import random

def dictionary():
    z = []
    for i in range(10):
        z.append(chr(48+i))
    for i in range(26):
        z.append(chr(97+i))
    return z

print("".join(random.choices(dictionary(),k=8)))
