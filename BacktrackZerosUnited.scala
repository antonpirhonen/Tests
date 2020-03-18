import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet

object BacktrackZerosUnited extends App {
  
  val start = System.nanoTime()
  
  def solve(n: Int): Int = {
    
    val map = Array.ofDim[Int](n,n)
    map(0)(0)=1 //Symmetry with square grid
    
    def isValid(x: Int) = x >= 0 && x < n
    
    def printMap() = {
      println("---")
      map.foreach(row => println(row.mkString(",")))
      Thread.sleep(500)
    }
    
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
    
    def adjacentZero(loc: (Int, Int)): (Int, Int) = {
      val (row, col) = loc
      for (dir <- 0 to 3) {
        val neigh = neighInDir(loc, dir)
        if (canVisit(neigh)) return neigh
      }
      throw new Error("No neighbors that are zero, should not happen...")
    }
    
    def markAdjacentZeros(loc: (Int, Int)): HashSet[(Int, Int)] = {
    	val found = HashSet[(Int, Int)]()
    	
    	def inner(loc: (Int, Int)): Unit = {
    		val (row, col)=loc
    		map(row)(col)=2
    		found+=loc
    		for (dir <- 0 to 3) {
        	val neigh = neighInDir(loc, dir)
        	if (canVisit(neigh)) {
  			    inner(neigh)
  		    }
        }
    	}
    	inner(loc)
      //printMap()
    	found
    }
      
    def canHaveSolutions(zeroLoc: (Int, Int)): Boolean = {
	    val marked = markAdjacentZeros(zeroLoc)
	    val res = !map.flatten.contains(0)
    	for (loc <- marked) {
      		map(loc._1)(loc._2)=0
  	  }
  	  res
    }
    
    
    var results = 0
    var depth = 1
    
    def visit(loc: (Int, Int)): Unit = {
      depth += 1
      val (row, col) = loc
      map(row)(col) = 1
      
      //All tiles walked
      if (depth == n*n) {
        //println("result found")
        //printMap()
        //Thread.sleep(1000)
        map(row)(col)=0
        depth -=1
        results += 1
        return
      }
      
      if (depth < n*n - 3 && !canHaveSolutions(adjacentZero(loc))) {
        map(row)(col)=0
        depth -= 1
        return
      }
      
//      println("---")
//      map.foreach(arr => println(arr.mkString(",")))
//      Thread.sleep(500L)
      
      for (dir <- 0 until 4) {
        val neigh = neighInDir(loc, dir)
        if (canVisit(neigh)) {
          visit(neigh)
        }
      }
      
      map(row)(col)=0
      depth -= 1
    }
    
    if (n == 1) return 1 else visit((0,1))
    println(s"Possible traversals for $n: ${results*2}")
    results*2
  }
  
  solve(7)
  println(s"Time taken: ${(System.nanoTime() - start)*10E-10} seconds")
  
  //2 to 4 foreach solve
}