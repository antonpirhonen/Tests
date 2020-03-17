

object BacktrackSymmetric extends App {
  
  def solve(n: Int): Int = {
    
    val map = Array.ofDim[Int](n,n)
    //Symmetry allows us to search only half of the combinations
    map(0)(0)=1
    
    def isValid(x: Int) = x >= 0 && x < n
    
    //A method for validating neighbors
    def canVisit(loc: (Int, Int)): Boolean = {
      val (row, col) = loc
      isValid(row) && isValid(col) && map(row)(col) == 0
    }
    
    //Returns the neighbor in a tuple
    def neighInDir(loc: (Int, Int), dir: Int): (Int,Int) = {
      val (row, col) = loc
      dir match {
        case 0 => return (row-1,col)
        case 1 => return (row, col+1)
        case 2 => return (row+1,col)
        case 3 => return (row,col-1)
        case _ => throw new Error("Invalid direction")
      }
    }
    
    var results = 0
    var depth = 1 //We have the top left corner visited by default
    
    def visit(loc: (Int, Int)): Unit = {
      depth += 1
      val (row, col) = loc
      map(row)(col) = 1
      
//      println("---")
//      map.foreach(arr => println(arr.mkString(",")))
//      Thread.sleep(500L)
      
      for (dir <- 0 until 4) {
        val neigh = neighInDir(loc, dir)
        if (canVisit(neigh)) {
          visit(neigh)
        }
      }
      
      if (depth == n*n) results += 1 //; if(results%1000==0) println(s"Results: $results")}
      depth -= 1
      map(row)(col)=0
    }
    
    if (n == 1) return 1 else visit((0,1))
    println(s"Possible traversals for $n: ${results*2}")
    results*2
  }
  
  for (i <- 1 to 6) solve(i)
  
}