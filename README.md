To install scala build tools
https://www.scala-sbt.org/1.0/docs/Installing-sbt-on-Linux.html

To compile simply type

`sbt compile`

on the command line prompt.


To run simply type

`sbt run`

on command line prompt.

This will run the "console" application for Lettuce.


You will see the prompt ">" to type your program on the console.

Type a program like so


~~~
let x = 20 in 
 let f = function(x) x - 45 in 
  f(x) ;;
~~~

You should see the output

~~~
--- Debug --- 
let x = 20 in    let f = function(x) x - 45 in      f(x) 
 --- Debug --- 
--- Debug --- 
TopLevel(Let(x,ConstNum(20.0),Let(f,FunDef(List(x),Minus(Ident(x),ConstNum(45.0))),FunCall(Ident(f),List(Ident(x))))))
--- Debug ---
Returned value : NumValue(-25.0)
~~~

Next, you can try another program

~~~
let x = newref(20) in
  let y = deref(x) in
    let z = assignref x <- 45 in
       deref(x) - y + z
       ;;

~~~

It will print

~~~
--- Debug --- 
let x = newref(20) in  let y = deref(x) in    let z = assignref x <- 45 in       deref(x) - y + z       
 --- Debug --- 
--- Debug --- 
TopLevel(Let(x,NewRef(ConstNum(20.0)),Let(y,DeRef(Ident(x)),Let(z,AssignRef(Ident(x),ConstNum(45.0)),Minus(DeRef(Ident(x)),Plus(Ident(y),Ident(z)))))))
--- Debug ---
Returned value : NumValue(-20.0)
~~~

To exit the console type

~~~
> exit;;
~~~

## Test Cases

To run tests type

`sbt test`

Test cases can be examined in the files in the directory

~~~
src/test/scala/edu/colorado/csci3155/LettuceAST/
~~~

### Exploring the Source Code

Go to the directory

~~~
src/main/scala/edu/colorado/csci3155/LettuceAST
~~~

#### LettuceConsole

The main function is defined in the file `LettuceConsole.scala`. It implements the prompting from user
and parses/interprets the user inputs.

TODO: add line number support in error messages from parser.

#### LettuceParser

The recursive descent parser using scala combinator parsing library.

#### LettuceAST.scala

The abstract syntax tree

#### Interpreter

`LettuceInterpreter.scala` has the main interpreter code.

`LettuceValue.scala` implements the values used in the interpreter.

`LettuceEnvironment.scala` implements the environment form identifiers to values.

`LettuceStore.scala` uses the store for mutable references.

`ErrorHandling.scala` has the exceptions thrown.

TODO: Missing types/type inference in Lettuce.

TODO: Objects need to be added.


