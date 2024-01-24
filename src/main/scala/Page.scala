package MemboxS

import scala.collection.mutable.ArrayBuffer

class Page(size: Int) {
  var data: Array[Byte] = Array.fill(size)(0.toByte)
}

class Block(val addr: BigInt, val size: Int, val create_pages: Boolean = true){
  var pages: Page = null
  if(create_pages){
    val num = (size + SV39.PageSize - 1) / SV39.PageSize
    pages = new Page(num * SV39.PageSize)
  }
  def end: BigInt = addr + size
  def <(thatBlock: Block): Boolean = end <= thatBlock.addr
  def >(thatBlock: Block): Boolean = addr >= thatBlock.end
  def <(thatAddr: BigInt): Boolean = end <= thatAddr
  def >(thatAddr: BigInt): Boolean = addr > thatAddr
  def ==(thatAddr: BigInt): Boolean = addr <= thatAddr && end > thatAddr

  def write(base: Int, length: Int, data: Array[Byte]): Boolean = {
    if(base + length > size) false
    else{
      pages.data = pages.data.take(base) ++ data.take(length) ++ pages.data.drop(base + length)
      true
    }
  }
  def read(base: Int, length: Int): (Boolean, Array[Byte]) = {
    if(base + length > size) (false, Array.empty)
    else (true, pages.data.slice(base, base + length))
  }
}

object Block{
  def apply(addr: BigInt, size: Int, create_pages: Boolean = true) = new Block(addr, size, create_pages)
}

class PhysicalMemory(range: BigInt) {
  var maxRange: BigInt = {
    val x = (range min SV39.MaxPhyRange)
    (x + SV39.PageSize - 1) / SV39.PageSize * SV39.PageSize
  }
  var blocks: ArrayBuffer[Block] = ArrayBuffer(Block(0, SV39.PageSize, false), Block(maxRange, SV39.PageSize, false))

  def insertBlock(blk: Block): Boolean = {
    val pos = blocks.lastIndexWhere(_ < blk)
    if (pos == -1 || (pos < blocks.size - 1 && !(blk < blocks(pos + 1))))
      false // Block conflict
    else {
      blocks.insert(pos + 1, blk)
      true
    }
  }

  def removeBlock(addr: BigInt): Boolean = {
    val pos = blocks.indexWhere(b => b.addr < addr && b.end > addr)
    if (pos == -1) false
    else {
      blocks.remove(pos)
      true
    }
  }

  def writeData(addr: BigInt, length: Int, data: Array[Byte]): Boolean = {
    val elem = blocks.find(b => addr >= b.addr && addr + length <= b.end)
    elem match {
      case Some(b) => {
        b.write((addr - b.addr).toInt, length, data)
        true
      }
      case None => false
    }
  }

  def writeWord[T <: MDataT](addr: BigInt, in: T): Boolean = {
    val dat = in.data
    writeData(addr, dat.length, dat)
  }

  def writeWords[T <: MDataT](addr: BigInt, num: Int, in: Array[T], mask: Array[Boolean] = Array.empty): Boolean = {
    val dat = in.map(_.data).reduceLeft((l, r) => l ++ r)
    if (mask.isEmpty) {
      writeData(addr, num * dat.length, dat)
    }
    else {
      (0 until num).filter(mask(_)).map { i =>
        writeWord(addr + i * in(i).getLength, in(i))
      }.reduceLeft(_ && _)
    }
  }

  def readData(addr: BigInt, length: Int): (Boolean, Array[Byte]) = {
    val elem = blocks.find(b => addr >= b.addr && addr + length <= b.end)
    elem match {
      case Some(b) => b.read((addr - b.addr).toInt, length)
      case None => (false, Array.empty)
    }
  }

  def readWord[T <: MDataT](addr: BigInt): (Boolean, T) = {
    val word = new T
    val (f, d) = readData(addr, word.getLength)
    word.data = d
    (f, word)
  }

  def readWords[T <: MDataT](addr: BigInt, num: Int, mask: Array[Boolean] = Array.empty): (Boolean, Array[T]) = {
    val length = (new T).getLength
    val res = (0 until num).map{ i => readWord(addr + i * length)}.unzip[Boolean, T]
    if(mask.isEmpty){
      (res._1.reduceLeft(_ && _), res._2.toArray)
    }
    else{
      val f = (res._1 zip mask).map(c => c._1 || !c._2).reduceLeft(_ && _)
      val res2 = (res._2 zip mask).map(c => {if(c._2) c._1 else MDataT.fromBigInt[T](0)}).toArray
      (f, res2)
    }
  }
}
