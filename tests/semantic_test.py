#!/usr/bin/python
# -*- coding: utf-8 -*-

import subprocess
import time
import sys

########################################
## TOC
#invalid assignments
#check varaibles are declared/initialized
#duplicate variable declaration
#check for return statements
#scope checking
#can't declare variable of type void
#function call correct number of arguments/ type of arguments
#make sure not doing operations on invalid types
#check booleans in for loops and conditionals
#list comprehension multiple list = multiple arguments

tests = [
# invalid assignments:
("""func main:void() {
  number x = 10;
}""", True),

("""func main:void() {
  number 10 = x;
}""", False),

# check varaibles are declared/initialized
("""func main:void() {
  number x =5;
  number y;
  y = x+2;
}
""", True),

("""main:void() {
  number x = 10;
  number z = x + y;
  }
""", False),

("""func main:void() {
  number x = 5;
  y = x + 2;
}""", False),

# duplicate variable declaration
("""func main:void() {
  func sum:void() { }
  func multiply:void() { }
}""", True),

("""func main:void() {
  func sum:void(number x) { }
  func sum:void() { }
}""", False),
("""func main:void() {
  number x = 10;
  number x = 10;
}""", False),

# check for return statements:
("""func main:void() {
  func sum:number(number x){
    x = x +1;
    return x;
  }
}""", True),

("""func main:void() {
  func sum:number(number x){
    x = x +1;
  }
}""", False),
  

# scope checking
("""func main:void() {
    number y;
    for(number x = 10; x < y; x = x +1) {
      y = x;
    }

    y = y +1;
  }""", True),
("""func main:void() {
    number y;
    number z = 0;
    for(number x = 10; x < y; x = x +1) {
      y = x;
      number z = 1;
    }
    y = y +1;
  }""", True),
("""func main:void() {
    number y;
    number z = 0;
    for(number x = 10; x < y; x = x +1) {
      y = x;
      number z = 1;
      number z = 0;
    }
    y = y +1;
  }""", False),
("""func main:void() {
    
    number z = 0;
    for(number x = 10; x < y; x = x +1) {
      y = x;
      number z = 1;
      number z = 0;
    }
    y = y +1;
  }""", False),


("""func main:void() {
    func foo:void() { }
    for(number x = 10; x < y; x = x +1) {
      func foo:void() { }
    }
  }""", True),
("""func main:void() {
    func foo:void() { }
    for(number x = 10; x < y; x = x +1) {
      func foo:void() { }
      func foo:void() { } 
    }
  }""", False),

("""func main:void() {
    number x = 0;
    func foo:number(number z) {
      if( x == 0 ) {
        number y = 2;
    } else {
        number y = 4;
    }
    return y;
    }
    number z = foo(x);
}""", False),
    


# disallow declarations of variables of type void
("""func main:void() {
  void x;
}
""", False),
("""func main:void() {
  void x = 10;
}
""", False),
("""func main:void() {
  func foo:void(void x) {
  }
}
""", False),
("""func main:void() {
  func foo:void(void[] x) {
  }
}
""", False),

# ensure function call arity/types matches up
("""func main:void() {
  func foo:void(number x) {
  }
  foo(10);
}
""", True),
("""func main:void() {
  func foo:void(number x, number y) {
  }
  foo(10, 20.45);
}""", True),
("""func main:void() {
  func foo:void(char x) {}
  foo('a');
}""", True),
("""func main:void() {
  func foo:void(char[] x) {}
  foo("aaa");
}""", True),
("""func main:void() {
  func foo:void(number[] x) {}
  foo([1, 2]);
}""", True),
("""func main:void() {
  number[] x = [1,2,3];
  char y = x[1];
}""", False),

("""func main:void() {
  number[] x = [1,2,3];
  number y = x[1];
}""", True),
("""func main:void() {
  number[] x = [1,2,3];
  number[] y = x[1];
}""", False),
("""func main:void() {
  number[][] x = [[1,2,3]];
  number[] y = x[1];
}""", True),

("""func main:void() {
  func foo:void(number[][] x) {}
  number[] y = [1,2];
  foo(y);
}""", False),
("""func main:void() {
  func foo:void(number[][] x) {}
  number[][] y = [[1,2]];
  foo(y);
}""", True),

("""func main:void() {
  func foo:void(number x) {}
  foo([1,2]);
}""", False),

("""func main:void() {
  func foo:void(boolean x) {}
  foo(true);
}""", True),
("""func main:void() {
  func foo:void(func:void() x) {}
  func bar:void() {}
  foo(bar);
}""", True),
("""func main:void() {
  func foo:void(func:void(number) x) {}
  func bar:void(number y) {}
  foo(bar);
}""", True),

#Anon Functions

("""func main:void() {
func foo:void(func:void(number) x) {}
foo(func:void(number y) { y=y*y; });
}""", True),

("""func main:void() {
func foo:void(func:number(number) x) {}
foo(func:number(number y) { return y*y; });
}""", True),

("""func main:void() {
func foo:void(func:void(number) x) {}
foo(func:number(number y) { return y*y; })
}""", False),
("""func main:void() {
  func foo:void(number x) {}
  foo('a');
}""", False),
("""func main:void() {
  func foo:void(number x, number y) {}
  foo('a', 10);
}""", False),
("""func main:void() {
  func foo:void(number x, number y) {}
  foo(10, 'a');
}""", False),
("""func main:void() {
  func foo:void(number x, char y) {}
  foo('a', 10);
}""", False),
("""func main:void() {
  func foo:void(number x, char y) {}
  foo(10);
}""", False),
("""func main:void() {
  func foo:void(number x, char y) {}
  foo(10, 'a', 5);
}""", False),

# make sure operations match up with type
("""func main:void() {
  10 + 20;
}""", True),
("""func main:void() {
  true && false;
}""", True),
("""func main:void() {
  10 + 'a';
}""", False),
("""func main:void() {
  'a' + 10;
}""", False),
("""func main:void() {
  10 + "aa";
}""", False),
("""func main:void() {
  10 + true;
}""", False),
("""func main:void() {
  true + true;
}""", False),
("""func main:void() {
  10 && 10;
}""", False),
("""func main:void() {
  true && 10;
}""", False),
("""func main:void() {
  true && 'a';
}""", False),
("""func main:void() {
  true && "a";
}""", False),
("""func main:void() {
  10 && true;
}""", False),

# check booleans in for loops and conditionals

("""func main:void() {
  if(true) {  }
}""", True),
("""func main:void() {
  number x = 1;
  if(x == 10) {  }
}""", True),
("""func main:void() {
  for(;true;) {  }
}""", True),
("""func main:void() {
  number x = 1;
  for(;x == 10;) {  }
}""", True),

("""func main:void() {
  number x = 1;
  if(x = 10) {  }
}""", False),
("""func main:void() {
  if(10) {  }
}""", False),
("""func main:void() {
  if('a') {  }
}""", False),
("""func main:void() {
  if("a") {  }
}""", False),
("""func main:void() {
  number x = 1;
  for(;x = 10;) {  }
}""", False),
("""func main:void() {
  for(;10;) {  }
}""", False),
("""func main:void() {
  for(;'a';) {  }
}""", False),
("""func main:void() {
  for(;"a";) {  }
}""", False),

# list comprehension multiple list = multiple arguments
("""func main:void() {
  number[] xx = [1, 2];
  number[] yy = [1, 2];
  [x*y for number x, number y in xx,yy];
}""", True),

("""func main:void() {
  number[] xx = [1, 2];
  number[] yy = [1, 2];
  [x*y for number x in xx,yy];
}""", False),

("""func main:void() {
  number[] xx = [1, 2];
  number[] yy = [1, 2];
  [x*y for number x, number y in xx];
}""", False),

("""func main:void() {
  number[] xx = [1, 2];
  number[] yy = [1, 2];
  [x*y for number x, number y in xx,yy if (x!=1)];
}""", True),
("""func main:void() {
  number[] xx = [1, 2];
  number[] yy = [1, 2];
  [x*y for number x, number y in xx,yy if (x==1)];
}""", True),

("""func fib:number(number n) {
  if (n == 0) {
    return 0;
  }
  elif (n == 1) {
    return 1;
  } else {
    return fib(n-1) + fib(n-2);
  } 
}""",True),

("""
function fizzBuzz:void() {
  char[] fb = "FizzBuzz";
  char[] f = "Fizz";
  char[] b = "Buzz";
  for (number i = 1; i < 101; i = i + 1){
    if (i % 15 == 0){
      println(fb);
    } elif(i % 3 == 0) {
      println(f);
    } elif(i % 5 == 0) {
      println(b);
    } else {
      println(i);
    }
  }
}
""",True),

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
