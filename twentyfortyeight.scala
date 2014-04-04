/*
  4 Great Cyclic Blind Strategies see:
  http://artent.net/2014/03/18/an-ai-for-2048-part-2-cyclic-blind-strategies/
  Strategies: drur, dldr, drdl, dlul, coded below.
*/
object twentyfortyeight extends App {
  type row = List[Int]
  type board = List[row]
  var log = true

  def blank:board = List.fill[row](4)(List.fill[Int](4)(0))

  def dash(top:Boolean=false) = List.fill[Char](23)( if (top) '-' else '_').mkString

  def emptySpots(g:board):List[Int] = {
    util.Random.shuffle(
      g.flatten
      .zipWithIndex
      .filter(_._1 == 0)
      .map(_._2))
  }

  def emptySpot(g:board):Option[Int] = {
    val spots = emptySpots(g)
    if (spots.size > 0) Some(spots.head) else None
  }

  def next(g:board):board = {
    val spot = emptySpot(g)
    if (spot.isDefined) next(g,spot.get) else g
  }

  def next(g:board, spot:Int):board = {
    g.flatten.updated(spot,
      if (util.Random.nextDouble > 0.9 ) 4 else 2)
    .grouped(4)
    .toList
  }

  def max(g:board):Int = g.flatten.max

  def sum(g:board):Int = g.flatten.sum

  var moves:Int = 0 // throwaway var to track how many moves we play
  def print(g:board, direction:String="") = {
    printf("\n%d.%s\n", { moves += 1; moves} , direction)
    printf("/%s\\\n", dash(top=true))
    g.foreach{ row =>
      val disp = row map { i => if (i==0) " " else i.toString }
      printf("|%5s|%5s|%5s|%5s|\n|%s|\n", disp(0), disp(1), disp(2), disp(3), dash())
    }
  }

  def slide(r:row) =
    r./:(List.empty[Int])((l,b) => if (b==0) l else b::l).reverse

  def padRite(r:row) = List.tabulate[Int](4){i=> if (i<r.size) r(i) else 0}

  def padLeft(r:row) = List.fill[Int](4-r.size)(0) ++ r

  def add(r:row) =
    r.tail./:(List(r.head)) {
      (l,b) => if(b==l.head) (b+b)::l.tail else b::l
    }.reverse

  def rowLeft = slide _ andThen padRite _ andThen add _ andThen padRite _

  def rowRite = slide _ andThen padLeft _ andThen add _ andThen padLeft _

  def start = next(next(next(blank)))

  def pause = { Thread.sleep(2000); reset }
  def reset = {moves = 0}

  def moveLeft(g:board) = g.map(rowLeft)
  def printLeft(g:board) = {
    if (log) print(g,"Left");
    moveLeft(g)
  }

  def moveRite(g:board) = g.map(rowRite)
  def printRite(g:board) = {
    if (log) print(g,"Right");
    moveRite(g)
  }

  def moveUp(g:board) = clockwise(moveLeft(clockwise(g)))
  def printUp(g:board) = {
    if (log) print(g,"Up");
    moveUp(g)
  }

  def moveDn(g:board) = clockwise(moveRite(clockwise(g)))
  def printDn(g:board) = {
    if (log) print(g,"Down");
    moveDn(g)
  }

  def clockwise(g:board) = List(0,1,2,3).map {
    i => g.map { row => row(i) }
  }

  def notDone(gp:(board, Option[Int])) = {
    val (g,prev) = gp
    (emptySpot(g).isDefined) ||
    (moveLeft(g) != g) ||
    (moveRite(g) != g) ||
    (moveDn(g) != g) ||
    (moveUp(g) != g)
  }

  def notDone(g:board) = {
    (emptySpot(g).isDefined) ||
    (moveLeft(g) != g) ||
    (moveRite(g) != g) ||
    (moveDn(g) != g) ||
    (moveUp(g) != g)
  }

  def func = Seq(printDn _, printLeft _, printRite _, printUp _)

  def mkFunc(x:List[Int]):board=>board = {
    x./:(List.empty[board=>board]){ (l,i) =>
      val thisfunc = func(i) andThen next _
      thisfunc::l
    }.reverse
    .reduce(_ andThen _)
  }

  def drur =  mkFunc(List(0,2,3,2))
  def dldr =  mkFunc(List(0,1,0,2))
  def drdl =  mkFunc(List(0,2,0,1))
  def dlul =  mkFunc(List(0,1,3,1))

  def greedyStrategy(gp:(board, Option[Int])) = {
    val (g,prev) = gp

    val best = util.Random.shuffle(
      List(0,1,2,3)
      .filterNot { i =>
        if(prev.isDefined) { i == prev.get} else false
      }.map { i =>
        val strat = func(i)
        val b = strat(g)
        val score:Int = max(b)// + sum(b)
        //println
        val tup = (score,i)
        //println(tup)
        tup
      }
    ).maxBy(x=> x._1)._2
    val tup = (next(func(best)(g)), Some(best))
    tup
  }

  def expectimax(g:board):board = {
    /* suppose you go up
    then 2 can show up at e empty spots
    So there's 4e ways of moving
    compute max of all those
    // take avg of all these maxes
    */
    val bestStrat = List(0,1,2,3).map {
      direction =>
        val strat = func(direction)
        val gg = strat(g)
        val spots = emptySpots(gg)
        //println("Choices:" + spots.size)
        val score = spots.map {
          spot =>
            val umax = max(moveUp(next(gg, spot)))
            val dmax = max(moveDn(next(gg, spot)))
            val lmax = max(moveLeft(next(gg, spot)))
            val rmax = max(moveRite(next(gg, spot)))
            List(umax,dmax,lmax,rmax).max
        }.sum/(1.0d+spots.size)
        val tup = (direction,score)
        //println(tup)
        tup
    }.maxBy(x=>x._2)._1
    //println("Best Strat:" + bestStrat)
    next(func(bestStrat)(g))
  }

  def runCyclic(times:Int, strat:board=>board) {
    for(i<- 1 to times) {
      val boards = Iterator.iterate(start)(drur).takeWhile(notDone).toList
      printf("%d\t%d\t%d\n", i,boards.size, max(boards.last))
      reset
    }
  }

  def runGreedy(times:Int) {
    for(i<- 1 to times) {
      val tup:(board, Option[Int]) = (start, None)
      val boards = Iterator.iterate(tup)(greedyStrategy).takeWhile(notDone).toList
      printf("%d\t%d\t%d\n", i,boards.size, max(boards.last._1))
      reset
    }
  }


  def runExpectimax(times:Int) {
    for(i<- 1 to times) {
      val boards = Iterator.iterate(start)(expectimax).takeWhile(notDone).toList
      printf("%d\t%d\t%d\n", i,boards.size, max(boards.last))
      reset
    }
  }


  //runCyclic(100,drur)
  //runGreedy(100)
  log = false
  runExpectimax(100)
}
