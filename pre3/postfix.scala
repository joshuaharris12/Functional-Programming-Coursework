// Shunting Yard Algorithm
// by Edsger Dijkstra
// ========================

object CW8a {

// type of tokens
type Toks = List[String]

// the operations in the basic version of the algorithm
val ops = List("+", "-", "*", "/")

// the precedences of the operators
val precs = Map("+" -> 1,
		"-" -> 1,
		"*" -> 2,
		"/" -> 2)

// helper function for splitting strings into tokens
def split(s: String) : Toks = s.split(" ").toList


// (1) Implement below the shunting yard algorithm. The most
// convenient way to this in Scala is to implement a recursive 
// function and to heavily use pattern matching. The function syard 
// takes some input tokens as first argument. The second and third 
// arguments represent the stack and the output of the shunting yard 
// algorithm.
//
// In the marking, you can assume the function is called only with 
// an empty stack and an empty output list. You can also assume the
// input os  only properly formatted (infix) arithmetic expressions
// (all parentheses will be well-nested, the input only contains 
// operators and numbers).

// You can implement any additional helper function you need. I found 
// it helpful to implement two auxiliary functions for the pattern matching:  
// 

def is_op(op: String) : Boolean = op match {
	case "+" | "-" | "*" | "/" => true
	case _ => false
}

def prec(op1: String, op2: String) : Boolean = (precs.get(op1).get >= precs.get(op2).get) match {
	case true => true
	case false => false
}

def is_brackets(s: String) : Boolean = s match {
	case "(" | ")" => true
	case _ => false
}

def syard(toks: Toks, st: Toks = Nil, out: Toks = Nil) : Toks = (toks == Nil) match {
	case true => (st == Nil) match {
		case true => out
		case false => st.head match {
			case "(" => syard(toks, st.tail, out)
			case _ => syard(toks, st.tail, out :+ st.head)
		}
	}
	case false => is_op(toks.head) match {
		case true => (st == Nil) match {
			case true => syard(toks.tail, toks.head :: st, out)
			case false => (st.head == "(") match {
				case true => syard(toks.tail, toks.head :: st, out)
				case false => prec(st.head, toks.head)  match {
					case true => syard(toks, st.tail, out :+ st.head)
					case false => syard(toks.tail, toks.head :: st, out)
				}
			}
		}
		case false => is_brackets(toks.head) match {
			case false => syard(toks.tail, st, out :+ toks.head)
			case true => toks.head match {
				case "(" => syard(toks.tail, toks.head :: st, out)
				case ")" => (st.head == "(") match {
					case true => syard(toks.tail, st, out)
					case false => syard(toks, st.tail, out :+ st.head)
				} 
			}
		}
	}
}


// test cases
//syard(split("3 + 4 * ( 2 - 1 )"))  // 3 4 2 1 - * +
//syard(split("10 + 12 * 33"))       // 10 12 33 * +
//syard(split("( 5 + 7 ) * 2"))      // 5 7 + 2 *
//syard(split("5 + 7 / 2"))          // 5 7 2 / +
//syard(split("5 * 7 / 2"))          // 5 7 * 2 /
//syard(split("9 + 24 / ( 7 - 3 )")) // 9 24 7 3 - / +

//syard(split("3 + 4 + 5"))           // 3 4 + 5 +
//syard(split("( ( 3 + 4 ) + 5 )"))    // 3 4 + 5 +
//syard(split("( 3 + ( 4 + 5 ) )"))    // 3 4 5 + +
//syard(split("( ( ( 3 ) ) + ( ( 4 + ( 5 ) ) ) )")) // 3 4 5 + +

 
// (2) Implement a compute function that evaluates an input list
// in postfix notation. This function takes a list of tokens
// and a stack as argumenta. The function should produce the 
// result as an integer using the stack. You can assume 
// this function will be only called with proper postfix 
// expressions.    

def compute(toks: Toks, st: List[Int] = Nil) : Int =  (toks == Nil) match {
	case true => st.head
	case false => is_op(toks.head) match {
		case true => compute(toks.tail, doOperation(st.head.toInt, st.tail.head.toInt, toks.head) :: st.tail.tail)
		case false => compute(toks.tail, toks.head.toInt :: st)
	}
}

def doOperation(value1: Int, value2: Int, op: String) : Int = op match {
	case "+" => value1 + value2
	case "-" => value2 - value1
	case "/" => value2 / value1
	case "*" => value1 * value2
}


// test cases
// compute(syard(split("3 + 4 * ( 2 - 1 )")))  // 7
// compute(syard(split("10 + 12 * 33")))       // 406
// compute(syard(split("( 5 + 7 ) * 2")))      // 24
// compute(syard(split("5 + 7 / 2")))          // 8
// compute(syard(split("5 * 7 / 2")))          // 17
// compute(syard(split("9 + 24 / ( 7 - 3 )"))) // 15



}


