/*
  4 Great Cyclic Blind Strategies see:
  http://artent.net/2014/03/18/an-ai-for-2048-part-2-cyclic-blind-strategies/
  Strategies: drur, dldr, drdl, dlul, coded below.
*/
object twentyfortyeight extends App {
  type row = List[Int]
  type board = List[row]

  def blank:board = List.fill[row](4)(List.fill[Int](4)(0))

  def dash(top:Boolean=false) = List.fill[Char](23)( if (top) '-' else '_').mkString

  def emptySpot(g:board):Option[Int] = {
    val spots = util.Random.shuffle(
        g.flatten
        .zipWithIndex
        .filter(_._1 == 0)
        .map(_._2))
    if (spots.size > 0) Some(spots.head) else None
  }

  def next(g:board):board = {
    val spot = emptySpot(g)
    if (spot.isDefined) {
       g.flatten.updated(spot.get,
       if (util.Random.nextDouble > 0.9 ) 4 else 2)
      .grouped(4)
      .toList
    } else g
  }

  def max(g:board):Int = g.flatten.max

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

  def padRite(r:row) =
    List.tabulate[Int](4){i=> if (i<r.size) r(i) else 0}

  def padLeft(r:row) = List.fill[Int](4-r.size)(0) ++ r

  def add(r:row) =
    r.tail./:(List(r.head)) {
      (l,b) => if(b==l.head) (b+b)::l.tail else b::l
    }.reverse

  def rowLeft = slide _ andThen padRite _ andThen add _ andThen padRite _

  def rowRite = slide _ andThen padLeft _ andThen add _ andThen padLeft _

  def start = next(next(next(blank)))

  def pause = {Thread.sleep(2000); moves=0 }

  def moveLeft(g:board) = g.map(rowLeft)
  def printLeft(g:board) = {print(g,"Left"); moveLeft(g)}

  def moveRite(g:board) = g.map(rowRite)
  def printRite(g:board) = {print(g,"Right"); moveRite(g)}

  def moveUp(g:board) = clockwise(moveLeft(clockwise(g)))
  def printUp(g:board) = { print(g,"Up"); moveUp(g) }

  def moveDn(g:board) = clockwise(moveRite(clockwise(g)))
  def printDn(g:board) = {print(g,"Down"); moveDn(g)}

  def clockwise(g:board) = List(0,1,2,3).map {
    i => g.map { row => row(i) }
  }

  def notDone(g:board) = next(g) != g

  def drur =  printDn _    andThen next _ andThen
              printRite _  andThen next _ andThen
              printUp _    andThen next _ andThen
              printRite _  andThen next

  def dldr =  printDn _    andThen next _ andThen
              printLeft _  andThen next _ andThen
              printDn _    andThen next _ andThen
              printRite _  andThen next

  def drdl =  printDn _    andThen next _ andThen
              printRite _  andThen next _ andThen
              printDn _    andThen next _ andThen
              printLeft _  andThen next

  def dlul =  printDn _    andThen next _ andThen
              printLeft _  andThen next _ andThen
              printUp _    andThen next _ andThen
              printLeft _  andThen next

  for(i<- 1 to 100) {
    val strategy = drdl // change this to your fav strategy
    val boards = Iterator.iterate(start)(strategy).takeWhile(notDone).toList
    printf("%d\t%d\t%d\n", i,boards.size, max(boards.last))
    pause
  }
}
