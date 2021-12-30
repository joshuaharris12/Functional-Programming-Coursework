// Core Part about finding a single tour for a board using the
// Warnsdorf Rule
//==============================================================

object CW9b {
// add comment to try and get knight1 to run 

// !!! Copy any function you need from file knight1.scala !!!
//
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.

type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions


def is_legal(dim: Int, path: Path, x: Pos) : Boolean = {
  val xPos = x._1
  val yPos = x._2
  if (xPos >= dim || yPos >= dim || xPos < 0 || yPos < 0) false
  else {
    if (path.isEmpty) true
    else !path.contains((xPos, yPos))
  }
}

def legal_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
  val deltas = List((1,2), (2,1), (2,-1), (1,-2), (-1,-2), (-2,-1), (-2,1), (-1,2))
  val xPos = x._1
  val yPos = x._2
  for (delta <- deltas if (is_legal(dim, path, ((xPos + delta._1), (yPos + delta._2))))) yield {
    ((xPos + delta._1), (yPos + delta._2))
  }
}

// def get_all_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
//     val deltas = List((1,2), (2,1), (2,-1), (1,-2), (-1,-2), (-2,-1), (-2,1), (-1,2))
//     val xPos = x._1
//     val yPos = x._2
//     for (delta <- deltas if (is_legal(dim, path, ((xPos + delta._1), (yPos + delta._2))))) yield {
//     ((xPos + delta._1), (yPos + delta._2))
//   }
// }

//(6) Complete the function that calculates a list of onward
//    moves like in (2) but orders them according to Warnsdorf’s 
//    rule. That means moves with the fewest legal onward moves 
//    should come first.


def ordered_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
    val moves = legal_moves(dim, path, x)
    val movesPairs = for(v <- moves) yield {
        (v, legal_moves(dim, v::path, v).length)
    }
    val sortedMoves = movesPairs.sortBy(_._2)
    for (s <- sortedMoves) yield {
        s._1
    }
}


//(7) Complete the function that searches for a single *closed* 
//    tour using the ordered_moves function from (6). This
//    function will be tested on a 6 x 6 board. 


def first_closed_tour_heuristics(dim: Int, path: Path) : Option[Path] = {
  find_closed_tour(dim, List(path))
}

def is_a_move(dim: Int, position: Pos, toCompare: Pos) : Boolean = {
  val deltas = List((1,2), (2,1), (2,-1), (1,-2), (-1,-2), (-2,-1), (-2,1), (-1,2))
  val xPos = position._1
  val yPos = position._2
  val positions = for (delta <- deltas if (xPos < dim && yPos < dim && xPos >= 0 && yPos >= 0)) yield {
    ((xPos + delta._1), (yPos + delta._2))
  }
  positions.contains(toCompare)
}

def find_closed_tour(dim: Int, paths: List[Path]) : Option[Path] = {
  if (paths.isEmpty) None
  else {
    if (paths.head.length == dim * dim) {
      if (is_a_move(dim, paths.head.head, paths.head.last)) Some(paths.head)
      else {
        val currentPath = paths.head
        val newOrderedMovesPaths = for (om <- ordered_moves(dim, currentPath, currentPath.head)) yield {
          om::currentPath
        }
        val newPaths = newOrderedMovesPaths:::paths.tail
        find_closed_tour(dim, newPaths)
      }
    }
    else {
      val currentPath = paths.head
      val newOrderedMovesPaths = for (om <- ordered_moves(dim, currentPath, currentPath.head)) yield {
          om::currentPath
      }
      val newPaths = newOrderedMovesPaths:::paths.tail
      find_closed_tour(dim, newPaths)
    }
  }
}


//(8) Same as (7) but searches for *non-closed* tours. This 
//    version of the function will be called with dimensions of 
//    up to 30 * 30.

def first_tour_heuristics(dim: Int, path: Path) : Option[Path] = {
  find_tour(dim, List(path))
}

def find_tour(dim: Int, paths: List[Path]) : Option[Path] = {
  if (paths.isEmpty) None
  else {
    if (paths.head.length == dim * dim) Some(paths.head)
    else {
      val currentPath = paths.head
      val newOrderedMovesPaths = for (om <- ordered_moves(dim, currentPath, currentPath.head)) yield {
          om::currentPath
      }
      val newPaths = newOrderedMovesPaths:::paths.tail
      find_closed_tour(dim, newPaths)
    }
  }
}
}

