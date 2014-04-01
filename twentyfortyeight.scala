import util.{Random=>rnd}

object twentyfortyeight extends App {

  type row = List[Int]
  type board = List[row]

  def blank =
    List.tabulate[row](4)(i=>List.tabulate[Int](4)(j=>0))

  def header = List.fill[Char](23)('_').mkString
  def top = List.fill[Char](23)('-').mkString

  def next(g:board) = {
    g.flatten.updated(
      rnd.shuffle(
        g.flatten
        .zipWithIndex
        .filter(_._1 == 0)
        .map(_._2)).head ,
        if (rnd.nextDouble > 0.9 ) 4 else 2)
      .grouped(4)
      .toList
  }

  def invariant(g:board) = {
    require(g.size == 4)
    g.foreach(i=>require(i.size==4))
  }

  def print(g:board) = {
    printf("/%s\\\n", top)
    g.foreach{ row =>
      val disp = row map { i => if (i==0) " " else i.toString }
      printf("|%5s|%5s|%5s|%5s|\n|%s|\n", disp(0), disp(1), disp(2), disp(3), header)
    }
  }

  def slide(r:row) =
    r./:(List.empty[Int])((l,b) => if (b==0) l else b::l)

  def pad(r:row) =
    r ++ List.fill[Int](0)(4-r.size)

  def add(r:row) =
    r.tail./:(List(r.head))((l,b) => if(b==l.head) (b+b)::l.tail else b::l)

  def moveLeft = slide _ andThen pad _ andThen add _ andThen pad _

  List.range[Int](0,16)./:(blank){(g,i) =>
    print(g)
    next(g)
  }
}
