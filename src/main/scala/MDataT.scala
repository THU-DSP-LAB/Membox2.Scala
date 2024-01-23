package MemboxS

import scala.collection.mutable.{ArrayBuffer}

abstract class MDataT(size: Int) {
  var data = new Array[Byte](size)
  def toBigInt = Helper.ByteArray2BigInt(data)
  def apply() = data
  def apply(ba: Array[Byte]) = {data = ba.take(size)}
}

object MDataT{
  def fromBigInt[T <: MDataT](n: BigInt): T = {
    def create(): T
    val y = create()
    y.data = Helper.BigInt2ByteArray(n, y().length)
    y
  }
}

class Int32 extends MDataT(4)

class Int64 extends MDataT(8)