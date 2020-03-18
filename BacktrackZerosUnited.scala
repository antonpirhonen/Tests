import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet

object BacktrackZerosUnited extends App {
  
  def solve(n: Int): Int = {
    
    val map = Array.ofDim[Int](n,n)
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
    
/*    def canFinish(d: Int): Boolean = {
      if (d < n*n - 1) {
        var flag = true
        for {
          row <- 0 until n
          col <- 0 until n
        } {
          if (map(row)(col) == 0) {
            flag = flag && Array(0,1,2,3).map(i => neighInDir((row, col), i)).filter(canVisit).map(x => map(x._1)(x._2)).contains(0)
          }
        }
        //map.foreach(arr => println(arr.mkString(",")))
        println(s"Can finish: $flag")
        Thread.sleep(1000)
        flag
      }
      else true
    }*/
    
    def adjacentZero(loc: (Int, Int)): (Int, Int) = {
      val (row, col) = loc
      for (dir <- 0 to 3) {
        val neigh = neighInDir(loc, dir)
        if (canVisit(neigh)) return neigh
      }
      throw new Error("No neighbors that are zero, should not happen...")
    }
    
    /*def zeroNeighbors(loc: (Int, Int)): List[(Int, Int)] = {
      var res: List[(Int, Int)] = List()
      for (dir <- 0 to 3) {
        val neigh = neighInDir(loc, dir)
        if (canVisit(neigh)) res = neigh +: res
      }
      res
    }*/
    /*
    def zerosUnited(startLoc: (Int, Int)): Boolean = {
      var zeroLocations: ArrayBuffer[(Int, Int)] = ArrayBuffer()
      def visit(loc: (Int, Int)): Unit = {
        zeroLocations+=loc
        val (row, col) = loc
        map(row)(col) = -1
        for (neigh <- zeroNeighbors(loc)) {
          if (map(neigh._1)(neigh._2) == 0)visit(neigh) //Spaghetti
        }
      }
      visit(startLoc)
      println(s"zeroLocations.size = ${zeroLocations.size} and map.flatten.count(_==0) = ${map.flatten.count(_==0)}")
      val united = zeroLocations.size
      zeroLocations.foreach( loc => map(loc._1)(loc._2) = 0 )
      united == map.flatten.count(_==0)
    }*/
    
    
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
  	found
  }
    
  def canHaveSolutions(zeroLoc: (Int, Int)): Boolean = {
	    val marked = markAdjacentZeros(zeroLoc)
    	if (map.flatten.contains(0)) return false
    	for (loc <- marked) {
    		map(loc._1)(loc._2)=0
	  }
	  true
  }
    
    
      
    var results = 0
    var depth = 0
    
    def visit(loc: (Int, Int)): Unit = {
      depth += 1
      val (row, col) = loc
      map(row)(col) = 1
      
      //All tiles walked
      if (depth == n*n) {
        depth -=1
        results += 1
        return
      }
      
      if (depth < n*n - 1 && !canHaveSolutions(adjacentZero(loc))) {
        depth -= 1
        return
      }
      
//      //Zeros not united, no possible solutions
//      if (depth < n*n-2 && !zerosUnited(zeroNeighbors(loc).head)) {
//        depth -= 1
//        map(row)(col)=0
//        println("Zeros not united")
//        return
//      }
      
      println("---")
      map.foreach(arr => println(arr.mkString(",")))
      Thread.sleep(500L)
      
      for (dir <- 0 until 4) {
        val neigh = neighInDir(loc, dir)
        if (canVisit(neigh)) {
          visit(neigh)
        }
      }
      
      depth -= 1
      map(row)(col)=0
    }
    
    visit((0,0))
    println(s"Possible traversals for $n: $results")
    results
  }
  
  //1 to 5 foreach solve
  solve(3)
}