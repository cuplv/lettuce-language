To install scala build tools
https://www.scala-sbt.org/1.0/docs/Installing-sbt-on-Linux.html

To compile simply type

$ sbt compile


To run simply type

$ sbt run

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

$sbt test

Test cases can be examined in the files in the directory

~~~
src/test/scala/edu/colorado/csci3155/LettuceAST/
~~~

