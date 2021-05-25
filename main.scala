import scala.io.StdIn.readLine
import scala.util.matching.Regex

//S -: E$
//E -: C | EE | E'|'E | E'?' | '(' E ')'
//C -: '0' | '1' | ... | '9'| 'a' | 'b' | ... | 'z' | '.'

//S  -: E$
//E  -: T '|' E | T
//T  -: F T | F
//F  -: A '?' | A
//A  -: C | '(' E ')'

//S  -: E$
//E  -: T E2
// E2 -: '|' E3
//E2 -: NIL
//E3 -: T E2
//T  -: F T2
//T2 -: F T2
//T2 -: NIL
//F  -: A F2
//F2 -: '?' F2
//F2 -: NIL
//A  -: C
//A  -: '(' A2
//A2 -: E ')'

//This program is a parser that performs a pattern match on strings.
//S-:E$
abstract class S

//E-:T | E2
case class E(left: T, right: Option[E2]) extends S

//E2-:'|' | E3
case class E2(left: E3) extends S

//E3-: T | E2
case class E3(left: T, right: Option[E2]) extends S

//T-: F | T2
case class T(left: F, right: Option[T2]) extends S

//T2-: F | T2
case class T2(left: F, right: Option[T2]) extends S

//F-: A | F2
case class F(left: A, right: Option[F2]) extends S

//F2-: '?' | F2
//F2-: NIL
case class F2(left: Option[F2]) extends S

//A-:'(' | A2
abstract class A extends S

//A2-: E | ')'
case class A2(left: E) extends A

//A-: C
case class C(left: Char) extends A


class RecursiveDescent(input:String) {

  val cRegex: Regex = "^[0-9a-zA-Z. ]".r
  var index = 0

  //S-: E$
  def parseS(): S = parseE()    //Start of the tree.

  //E-:T | E2
  def parseE(): E = E(parseT(), parseE2()) //E has 2 different paths it can follow T or E2.

  //E2-: '|' | E3
  def parseE2(): Option[E2] = {   //E2 is optional so it can follow '|' or E3.
    if(index < input.length && input(index) == '|'){ //If the index is less that the input length and the index of the input, it equals '|'.
      index += 1  //Then it will increment.
      Some(E2(parseE3())) //E2 will call parseE3.
    }
    else None //If none of the conditions apply, then nothing happens.
  }

  //E3-: T | E2
  def parseE3(): E3 = { //When parseE3 gets called, it has 2 path, T or E2.
    E3(parseT(), parseE2())
  }
  //T-: F | T2
  def parseT(): T = { //parseT has 2 paths, either F or T2.
    T(parseF(), parseT2())
  }
  //T2-: F | T2
  def parseT2() : Option[T2] = { //T2 is optional.
    if(index < input.length && input(index) != ')' //So, if the index is less than the input length and index is casted to input is not equal to ")".
      && input(index) != '?' && input(index) != '|'){ //And if index is casted to input is not equal to "?" and index is casted to input is not equal to "|".
      Some(T2(parseF(), parseT2())) //Then T2 has the paths F and T2
    }
    else None //Or if none applies, then return nothing.
  }
  //F-: A | F2
  def parseF() : F = {//F has 2 paths A and F2.
    F(parseA(), parseF2())
  }

  //F2-: F2
  //F2-: None
  def parseF2() : Option[F2] = { //F2 is optional.
    if(index < input.length && input.charAt(index) == '?'){ //If index is less than input length and the input char is at the index are equal to '?'.
      index += 1      //Increments the index.
      Some(F2(parseF2())) //F2 only parses F2 or None.
    }
    else None //If none of the conditions apply, evaluate to none.
  }
  //A-: '(' | A2
  def parseA() : A = { //A has 2 options, either '(' or A2.
    if(index < input.length && input.charAt(index) == '('){
      index += 1
      parseA2()
    } else parseC()
  }
  //A2-: E | ')'
  def parseA2() : A2 = { //A2 has 2 paths, E or ')'.
    val a2 = A2(parseE())
    if(index < input.length && input.charAt(index) == ')') {
      index += 1
      a2
    } else throw new RuntimeException("Invalid") //Type Any needed an exception thrown to work.
  }
  //A-: C
  def parseC(): C = {//A also separately can parse C.
    val cStr = input.charAt(index)
    index += 1     //The index then increments
    C(cStr)
  }

}

object Main {
  var index = 0

  def matches(input: Any, str: String): Boolean = { //This function is where the matching occurs.
    input match {
      case E(t, e2)=>{  //Case E evaluates t and e2.
        if(e2.isEmpty) //if e2 is empty, return the match t and the string
          return matches(t, str)
        val s = index

        var result = matches(t, str) //stores the match in result.
        if(!result){ //if the result is false then index = s.
          index = s
          result = matches(e2, str) //then result = the match of e2 and the string.
        }
        result
      }

      case Some(t2:T2)=>{
        matches(t2, str)
      }
      case Some(f2: F2)=>{
        matches(f2, str)
      }
      case Some(e2: E2)=>{
        matches(e2, str)
      }
      case E2(e3)=>{ //E2 evaluates e3.
        matches(e3, str) //matches e3 to the string.
      }
      case E3(t, e2)=>{
        if(e2.isEmpty)
          return matches(t, str)
        val s = index

        var result = matches(t, str)
        if(!result) {                   /*If the result is false, index is equal to sI
                                          and the result becomes equal to matches*/
          index = s
          result = matches(e2, str)
        }
        result
      }
      case T(f, t2)=>{
        if(t2.isEmpty)
          return matches(f, str)
        matches(f, str) & matches(t2, str)
      }
      case T2(f, t2)=>{
        if(t2.isEmpty)
          return matches(f, str)
        matches(f, str) & matches(t2, str)
      }
      case F(a, f2)=>{
        if(f2.isEmpty){
          return matches(a, str)
        }
        val s = index
        var result = matches(a, str)

        if (!result) {              /*If result is false, index is equal to sI and then result
                                       evaluates to false, then it becomes the result.*/
          index = s
          result = !result
        }
        result
      }

      case F2(f2)=>{
        matches(f2, str)
      }

      case A2(e)=>{
        matches(e, str)
      }

      case C(c2)=>{
        if(index >= str.length)
          return false
        if(c2 == '.') return {
          index += 1
          true
        }
        val result = c2 == str(index)
        if(result)
          index += 1
        result
      }

    }
  }

  def main(args: Array[String]){

    val str = readLine("pattern? ")
    val r = new RecursiveDescent(str)
    val pattern:S = r.parseE()

    while(str != "q"){
      val str = readLine("string? ")
      val i: Boolean = matches(pattern, str)

      if(i && Main.index == str.length){ //If t and the index are equal to the string length, then print "match".
        println("match ")
      }else {
        println("no match " )
        System.exit(0) //Exit when there is no match.
      }
     index = 0  //Set the index back to zero each time or it evaluates every pattern to "no match".

    }

  }

}
