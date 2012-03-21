#!/usr/bin/python

import subprocess

tests = [
# check the one liners
("""1+1""", False),
("""aoeu""", False),
("""number x = 0""", False),

("""1+1;""", False),
("""aoeu;""", False),
("""number x = 0;""", False),

# check the one liner basics wrapped in main
("""
func main:number() {
}""", True),
("""
func main:number() {}""", True), # check whitespace
("""
func main:number() {
  1+1;
}""", True),
("""
func main:number() {
  aoeu;
}""", True),
("""
func main:number() {
  number a = 1+a;
}""", True),
("""
func main:number(char x) {
  number a = 1+a;
}""", True),
("""
func main:number(char x) {
  number a = 1+a;
}""", True),

# check the multiple lines
("""
func main:number(char x) {
  number a = 1+a;
  number b;
}""", True),

# check array syntax
("""
func main:number(char x) {
  number[] a = b;
}""", True),
("""
func main:number(char x) {
  char[][][][] a = b;
}""", True),

# check function syntax
("""
func main:void(char x) {
  func:void() b;
}""", True),
("""
func main:void(char x) {
  func:number[] (func:number(), func:char()) b;
}""", True),
("""
func main:void(char x) {
  func:number[] (func:number(), func:char()) b;
}""", True),

# anti-check simple statements
# check arithmetic expressions
# check strings

# check array expressions (also, append)
# check func
# if
# for
# list comprehensions
# 
]

for test in tests:
    # run each test, check the return value
    pass
