#!/usr/bin/python
# -*- coding: utf-8 -*-

import subprocess
import time
import sys

########################################
## TOC
# simple statements
# comments
# check simple types
# check arithmetic expressions
# check boolean expressions
# check char literals
# check strings literals
# string concatenation
# check array types
# check array literals
# check array indexing/slicing
# check array expressions
# func types
# func calls
# func definitions (normal)
# func definitions (anonymous)
# if
# normal for
# list comprehensions
# imports
# foreach
# some examples

tests = [
####################
# simple statements
("""number x = 0;""", True),
("""number x;""", True),
("""func:void() x;""", True),
("""func:void()[] x;""", True),

("""1+1""", False),
("""aoeu""", False),
("""number x = 0""", False),
("""1+1;""", False),
("""aoeu;""", False),

("""func main:void() { }""", True),
("""func main:void() { ; }""", True),
("""func m:void() { }""", True),

("""func m:void()""", False),
("""func m:() {}""", False),
("""func m:(int) {}""", False),
("""func m() {}""", False),
("""func void() {}""", False),

####################
# comments
("""func main:void() {
  /* hello world */
}""", True),
("""func main:void() {
  // hello world
}""", True),
("""func main:void() {
  // /* hello world */
}""", True),
("""func main:void() {
  /* // hello world */
}""", True),
("""func main:void() {
  /* hello /* world */
}""", True),
("""func main:void() {
  /* /* hello world */ // */
}""", True),

("""func main:void() {
  /* /* hello world */ */
}""", False),
####################
# check simple types
("""func main:void(char x) { }""", True),
("""func main:void(number x) { }""", True),
("""func main:void(boolean x) { }""", True),
("""func main:void() { char x; }""", True),
("""func main:void() { number x; }""", True),
("""func main:void() { boolean x; }""", True),

("""func main:void() { int x; }""", False),
("""func main:void() { double x; }""", False),
#("""func main:void() { void x; }""", False), # catch type void in semantic
("""func main:void() { char* x; }""", False),
("""func main:void() { char& x; }""", False),
("""func main:void() { &char x; }""", False),
####################
# check arithmetic expressions
("""
func main:void() { 2+2; }""", True),
("""
func main:void() {
  (2*2-5.0)/(.988999+3.0%9);
}""", True),
("""func main:void() { 089; }""", True),

("""func main:void() { 10.; }""", False),
("""func main:void() { 10e4; }""", False),
("""func main:void() { 10e-2; }""", False),
("""func main:void() { 0b10; }""", False),
("""func main:void() { 0x10; }""", False),
("""func main:void() { 10..9; }""", False),
("""func main:void() { a++; }""", False),
("""func main:void() { a+++; }""", False),
("""func main:void() { a-; }""", False),
####################
# check boolean expressions
("""func main:void() { true; }""", True),
("""func main:void() { false; }""", True),
("""func main:void() { !!!false; }""", True),
("""
func main:void() {
  true && false;
}""", True),
("""
func main:void() {
  true && not (false || true && (false || !true));
}""", True),

("""func main:void() { ||; }""", False),
("""func main:void() { &&; }""", False),
("""func main:void() { |; }""", False),
("""func main:void() { &; }""", False),
("""func main:void() { x & x; }""", False),
("""func main:void() { x | x; }""", False),
("""func main:void() { x ^ x; }""", False),
####################
# check comparisons
("""func main:void() { true == x; }""", True),
("""func main:void() { x*y > y/19.08; }""", True),
("""func main:void() { y/19.08 < x*y; }""", True),
("""func main:void() { 10.05/40.5 >= x+y; }""", True),
("""func main:void() { x <= y; }""", True),
("""func main:void() { true - - true; }""", True),

("""func main:void() { true <; }""", False),
("""func main:void() { > true; }""", False),
("""func main:void() { true >> true; }""", False),
("""func main:void() { true >>> true; }""", False),
("""func main:void() { true <> true; }""", False),
("""func main:void() { 2**10; }""", False),
("""func main:void() { ==; }""", False),
("""func main:void() { true === x; }""", False),
####################
# check char literals
("""func main:void() { 'a'; }""", True),
(r"""func main:void() { '\n'; }""", True),
(r"""func main:void() { '\'; }""", True),
("""func main:void() { char x = 'b'; }""", True),

("""func main:void() { ''; }""", False),
(r'func main:void() { \'\\\'; }', False),
("""func main:void() { 'ಠ_ಠ'; }""", False),
####################
# check strings literals
("""func main:void() { ""; }""", True),
("""func main:void() { "a"; }""", True),
("""func main:void() { "a"; }""", True),
("""func main:void() { "hello world 29.0"; }""", True),
(r"""func main:void() { "\"\n\\"; }""", True),
(r"""func main:void() { "\"a"; }""", True),

("""func main:void() { "a"b"; }""", False),
("""func main:void() { "a\\\\"b"; }""", False),
("""func main:void() { "ab; }""", False),
####################
# string concatenation
("""func main:void() { "ab" + x; }""", True),
("""func main:void() { "ab" + "aoeu"; }""", True),

("""func main:void() { "ab" +; }""", False),
####################
# check array types
("""func main:void(char[] x) { }""", True),
("""func main:void(number[] x) { }""", True),
("""func main:void(boolean[] x) { }""", True),
("""func main:void(char[][][] x) { }""", True),
("""func main:void() { char[][] x; }""", True),
("""func main:void() { char[10] x; }""", True),

("""func main:void() { char[[]] x; }""", False),
("""func main:void() { char() x; }""", False),
####################
# check array literals
("""func main:void() { [1]; }""", True),
("""func main:void() { [1, 2, 3.1]; }""", True),
("""func main:void() { ['a','b']; }""", True),
("""func main:void() { ["hello","world"]; }""", True),
("""func main:void() { [true, false]; }""", True),
("""func main:void() { [ [1,2,3], [4,5,6], [7,8,9] ]; }""", True),

("""func main:void() { [1,[2]; }""", False),
("""func main:void() { [,]; }""", False),
("""func main:void() { [,,,x]; }""", False),
####################
# check array indexing/slicing
("""func main:void() { a[0]; }""", True),
("""func main:void() { a[x]; }""", True),
("""func main:void() { a[x+5]; }""", True),
("""func main:void() { a[f()]; }""", True),
("""func main:void() { f()[1]; }""", True),
("""func main:void() { f()[g()]; }""", True),
####################
# check array expressions
("""func main:void() { number[] x = [10, 20.0]; }""", True),
("""func main:void() { x[10] = [10.0, 20]; }""", True),
####################
# func types
("""func main:void() {
  func:void() x;
}""", True),
("""func main:void() {
  func:void(number) x = b;
}""", True),
("""func main:void() {
  func:void(number, char) x = b;
}""", True),
("""func main:void() {
  func:void(number a) x = b;
}""", True),
("""func main:void() {
  func:void(char[] a) x = b;
}""", True),
("""func main:void() {
  func:func:void()(number a) x;
}""", True),
("""func main:void() {
  func:void(func:void() documentation) x;
}""", True),

("""func main:void() {
  func(number, char) x;
}""", False),
####################
# func calls
("""func main:void() { f(); }""", True),
("""func main:void() { f(x); }""", True),
("""func main:void() { f(x, y); }""", True),
("""func main:void() { f(10.0, 5.0); }""", True),
("""func main:void() { f("thing", "mu"); }""", True),
("""func main:void() { f('1', "thing", 105.24); }""", True),
("""func main:void() { f(g(h())); }""", True),
# returns a function, which we call again
("""func main:void() { f(1)(2)(3); }""", True),

("""func main:void() { f((); }""", False),
####################
# func definitions (normal)
("""func main:void() {
  func foo:number() {
    ;
  }
}""", True),
("""func main:void() {
  func foo:number() {
    return 2+2;
  }
}""", True),
("""func main:void() {
  func foo:void() {
    func bar:void() {
      func baz:void() {
      }
    }
  }
}""", True),

("""func main:void() {
  func:number() {
    return 2+2;
  }
}""", False),
####################
# func definitions (anonymous)
("""func main:void() {
  x = func:void() { };
}""", True),
("""func main:void() {
  x = func:func:void()(char[] s) {
    return func:void() { };
  };
}""", True),
("""func main:void() {
  func:void() x = func:void() { };
}""", True),
("""func main:void() {
  func:number(char[]) x = func:number(char[] s) {
    return 2;
  };
}""", True),

("""func main:void() {
  x = func:void() { }
}""", False),
####################
# if
("""func main:void() {
  if(something) {
    do_something();
  }
}""", True),
("""func main:void() {
  if(something) {
    do_something();
  } else {
    do_something_else();
  }
}""", True),
("""func main:void() {
  if(something) {
    do_something();
  } elif(something_else) {
    do_something_else();
  } else {
    do_the_other_thing();
  }
}""", True),
("""func main:void() {
  if(something) {
    if(and_something) {
      do_something();
    }
  }
}""", True),
("""func main:void() {
  if(x<5) {
    do_something();
  }
}""", True),

("""func main:void() {
  if something {
    do_something();
  }
}""", False),
("""func main:void() {
  if (something)
    do_something();
}""", False),
("""func main:void() {
  if something
    do_something();
}""", False),
("""func main:void() {
  if x<10
    do_something();
}""", False),
####################
# normal for
("""func main:void() {
  for(;;) {
  }
}""", True),
("""func main:void() {
  for(x = 10; x < y; x = x +1) {
    do_something();
  }
}""", True),
("""func main:void() {
  for(number x = 10; x < y; x = x + 1) {
    do_something_else();
  }
}""", True),
("""func main:void() {
  for(x < 10; y < 10; z < 10) {
    do_things();
  }
}""", True),

("""func main:void() {
  for() { }
}""", False),
("""func main:void() {
  for(;;);
}""", False),
("""func main:void() {
  for(;;;) { }
}""", False),
("""func main:void() {
  for { }
}""", False),
("""func main:void() {
  for(;;)
}""", False),
####################
# list comprehensions
("""func main:void() {
  [1 for number x in y];
}""", True),
("""func main:void() {
  [1 for number x in [1,2,3]];
}""", True),
("""func main:void() {
  x = [1 for (number x) in c];
}""", True),
("""func main:void() {
  x[1] = [1 for number x, number y in [1,2], [3,4]];
}""", True),
("""func main:void() {
  x[1] = [1 for number x, number y in ([1,2], [3,4])];
}""", True),
("""func main:void() {
  x[1] = [1 for (number x, number y) in ([1,2], [3,4])];
}""", True),
("""func main:void() {
  return [[x*y for number y in bb] for number x in aa];
}""", True),

("""func main:void() {
  return [x for x];
}""", False),
("""func main:void() {
  return [x for x in x];
}""", False),
("""func main:void() {
  return [x for number x in number list];
}""", False),
("""func main:void() {
  return [for number x in list];
}""", False),
####################
# imports
("""import math; number a;""", True),
("""import math; import types; number a;""", True),

("""import math;""", False), # need to have at least 1 expression
("""import x<10;""", False),
("""import;""", False),
("""import; number a;""", False),
("""import 480; number a;""", False),
####################
# foreach
####################
# some examples
("""func matrix_multiply:void (number[][] a, number[][] b) {
    func sum:number (number a, number b) {
        return a + b;
    }
    func dotprod:void (number[] a, number[] b) {
        return reduce(sum, [x*y for number x, number y in a,b]);
    }
    if (len(a) != len(b[0])) {
        return nil;
    }
    return [[dotprod(row, col) for number[] col in transpose(b)]
            for number[] row in a];
}""", True),
("""func main:void()
{
    number[] arr;
    fill(arr, rand(100));
    char[] even = "is even";
    char[] odd = "is odd";

    each(arr, func:void(number element) {
        if (element % 2 == 0) {
            print(element + " " + even);
        } else {
            print(element + " " + odd);
        }
    });
}""", True),
]

################################################################################
# testing scaffold

count_errors = 0
str_errors = ""

DEBUG = False

for i in range(len(tests)):
    test = tests[i]
    # run each test, check the return value
    proc = subprocess.Popen(["bin/dotpar"],
                            stdin=subprocess.PIPE,
                            stdout=subprocess.PIPE,
                            stderr=subprocess.STDOUT)
    
    proc.communicate(test[0]+"\n")
    # wait a bit
    time.sleep(0.001)
    # poll
    status = proc.poll()
    if DEBUG:
        print status,test[1]
    # end it if it's still running
    if status is None:
        proc.terminate()
    # check if we're
    if (status == 0) == test[1]:
        sys.stdout.write('.')
    else:
        str_errors += ("#%3d test failed: %s expected\n" %
                       (i,
                        {True: "Pass", False: "Failure"}[test[1]]))
        str_errors += test[0]+"\n\n"
        sys.stdout.write('F')
        count_errors += 1
    if (i+1) % 80 == 0:
        print ""

if count_errors != 0:
    print "\n"
    print "Failures:"
    print str_errors

    print("total failures: %d/%d" % (count_errors, len(tests)) )
else:
    print ""
    print("No failures, have a good day!")
