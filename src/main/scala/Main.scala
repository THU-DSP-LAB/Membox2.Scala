package MemboxS

object Main extends App{
  var list = scala.collection.mutable.ArrayBuffer(0, 1, 2, 3)
  list.remove(2)
  print(s"$list")
}
