import util.{Random=>rnd}

object twentyfortyeight extends App {

  type row = List[Int]
  type board = List[row]

  def blank =
    List.tabulate[List[Int]](4)(i=>List.tabulate[Int](4)(j=>0))

  def header = List.fill[Char](24)('_')

  def next(g:board) = {
    g.flatten.updated(
      rnd.shuffle(
        g.flatten
        .zipWithIndex
        .filter(_._1 == 0)
        .map(_._2)).head, if (rnd.nextDouble > 0.9 ) 4 else 2)
  }

  def invariant(g:board) = {
    require(g.size == 4)
    g.foreach(i=>require(i.size==4))
  }

  def print(g:board) = {
    g.foreach{ row =>
      println(header)
      println(row.mkString(" | ").replace('0',' '))
    }
    println(header)
  }

  def slide(r:row) =
    r./:(List.empty[Int])((l,b) => if (b==0) l else b::l)

  def pad(r:row) =
    r ++ List.fill[Int](0)(4-r.size)

  def add(r:row) =
    r.tail./:(List(r.head))((l,b) => if(b==l.head) (b+b)::l.tail else b::l)

  def moveLeft = slide _ andThen pad _ andThen add _ andThen pad _
}
