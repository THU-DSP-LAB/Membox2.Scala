package MemboxS

import scala.collection.mutable.{ArrayBuffer}

abstract class MDataT(_size: Int) {
  var data = new Array[Byte](size)
  def toBigInt = Helper.ByteArray2BigInt(data)
  def apply() = data
  def apply(ba: Array[Byte]) = {data = ba.take(size)}
  def size = _size
  def apply(x: BigInt): MDataT
}

object MDataT{
  def fromBigInt[T <: MDataT](n: BigInt): T = {
    val y = new T
    y.data = Helper.BigInt2ByteArray(n, y().length)
    y
  }
}

class Int32 extends MDataT(4){
  override def apply(x: BigInt): Int32 = {
    MDataT.fromBigInt[Int32](x)
  }
}

class Int64 extends MDataT(8){
  override def apply(x: BigInt): Int64 = {
    MDataT.fromBigInt[Int64](x)
  }
}