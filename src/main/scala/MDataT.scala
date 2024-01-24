package MemboxS

import scala.collection.mutable.{ArrayBuffer}

abstract class MDataT(size: Int) {
  var data = new Array[Byte](size)
  def toBigInt = Helper.ByteArray2BigInt(data)
  def apply() = data
  def apply(ba: Array[Byte]) = {data = ba.take(size)}
  def getLength = data.length
}

object MDataT{
  def fromBigInt[T <: MDataT](n: BigInt): T = {
    val y = new T
    y.data = Helper.BigInt2ByteArray(n, y().length)
    y
  }
}

class Int32 extends MDataT(4)

class Int64 extends MDataT(8)