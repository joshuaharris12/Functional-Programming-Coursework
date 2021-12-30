// Shunting Yard Algorithm 
// including Associativity for Operators 
// =====================================

object CW8b {


// type of tokens
type Toks = List[String]

// helper function for splitting strings into tokens
def split(s: String) : Toks = s.split(" ").toList

// left- and right-associativity
abstract class Assoc
case object LA extends Assoc
case object RA extends Assoc


// power is right-associative,
// everything else is left-associative
def assoc(s: String) : Assoc = s match {
  case "^" => RA
  case _ => LA
}


// the precedences of the operators
val precs = Map("+" -> 1,
  		"-" -> 1,
		"*" -> 2,
		"/" -> 2,
                "^" -> 4)

// the operations in the basic version of the algorithm
val ops = List("+", "-", "*", "/", "^")

// (3) Implement the extended version of the shunting yard algorithm.
// This version should properly account for the fact that the power 
// operation is right-associative. Apart from the extension to include
// the power operation, you can make the same assumptions as in 
// basic version.


def is_brackets(s: String) : Boolean = s match {
	case "(" | ")" => true
	case _ => false
}

def is_op(op: String) : Boolean = op match {
	case "+" | "-" | "*" | "/" | "^" => true
	case _ => false
}


def prec(op1: String, op2: String) : Int = (precs.get(op1).get >= precs.get(op2).get) match {
	case true => (precs.get(op1).get > precs.get(op2).get) match {
    case true => 1
    case false => 0
  }
	case false => -1
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
					case 1 => syard(toks, st.tail, out :+ st.head)
					case -1 => syard(toks.tail, toks.head :: st, out)
          case 0 => st.head match {
            case "^" => syard(toks.tail, toks.head :: st, out)
            case _ => syard(toks, st.tail, out :+ st.head)
          }
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
// syard(split("3 + 4 * 8 / ( 5 - 1 ) ^ 2 ^ 3"))  // 3 4 8 * 5 1 - 2 3 ^ ^ / +


// (4) Implement a compute function that produces an Int for an
// input list of tokens in postfix notation.

def compute(toks: Toks, st: List[Int] = Nil) : Int = (toks == Nil) match {
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
  case "^" => Math.pow(value2, value1).toInt
}

// test cases
// compute(syard(split("3 + 4 * ( 2 - 1 )")))   // 7
// compute(syard(split("10 + 12 * 33")))       // 406
// compute(syard(split("( 5 + 7 ) * 2")))      // 24
// compute(syard(split("5 + 7 / 2")))          // 8
// compute(syard(split("5 * 7 / 2")))          // 17
// compute(syard(split("9 + 24 / ( 7 - 3 )"))) // 15
// compute(syard(split("4 ^ 3 ^ 2")))      // 262144
// compute(syard(split("4 ^ ( 3 ^ 2 )")))  // 262144
// compute(syard(split("( 4 ^ 3 ) ^ 2")))  // 4096
// compute(syard(split("( 3 + 1 ) ^ 2 ^ 3")))   // 65536

}
