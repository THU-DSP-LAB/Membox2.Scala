package MemboxS

object Helper {
  def BigInt2ByteArray(n: BigInt, len: Int): Array[Byte] = n.toByteArray.takeRight(len).reverse.padTo(len, 0.toByte)
  def Hex2ByteArray(hex: String, len: Int): Array[Byte] = BigInt("00" ++ hex, 16).toByteArray.takeRight(len).reverse.padTo(len, 0.toByte)
  def ByteArray2BigInt(ba: Array[Byte]) = BigInt(0.toByte +: ba.reverse)
}

abstract class BaseSV{
  type WORD <: MDataT
  def VALowerCeil: BigInt
  def VAUpperFloor: BigInt
  def PageBits = 12
  def PageSize= 1 << PageBits
  def V = 1
  def R = 2
  def W = 4
  def X = 8
  def U = 16
  def G = 32
  def A = 64
  def D = 128
  def PageElem

  def PageLevels: Int
  def VAExtract(VA: BigInt, level: Int): BigInt
  def getVPN(VA: BigInt): BigInt
  def VACheck(VA: BigInt): Boolean = VA >= VALowerCeil && VA < VAUpperFloor
  def PTEToPA(PTE: BigInt): BigInt
  def SetPTE(PA: BigInt, mods: BigInt): BigInt
}
object SV32 extends BaseSV{
  override type WORD = Int32
  def MaxPhyRange = BigInt("100000000", 16)
  def PageElem = 1024
  def VALowerCeil = BigInt("07fffffff", 16)
  def VAUpperFloor = BigInt("080000000", 16)
  def PageLevels = 2

  def VAExtract(VA: BigInt, level: Int): BigInt = {
    level match{
      case 0 => (VA >> 22) & 0x3ff
      case _ => (VA >> 12) & 0x3ff
    }
  }
  def getVPN(VA: BigInt) = (VA >> 12) & 0xfffff
  def PTEToPA(PTE: BigInt) = (PTE & BigInt("0fffffc00", 16)) << 2
  def SetPTE(PA: BigInt, mods: BigInt) = ((PA & BigInt("0fffff000", 16)) >> 2) | mods
}

object SV39 extends BaseSV{
  override type WORD = Int64
  def MaxPhyRange = BigInt("0100000000000000", 16)
  def PageElem = 512
  def VALowerCeil = BigInt("0000004000000000", 16)
  def VAUpperFloor = BigInt("0ffffffc000000000", 16)
  def PageLevels: Int = 3
  def VAExtract(VA: BigInt, level: Int): BigInt = {
    level match{
      case 0 => (VA >> 30) & 0x1ff
      case 1 => (VA >> 21) & 0x1ff
      case _ => (VA >> 12) & 0x1ff
    }
  }
  def getVPN(VA: BigInt) = (VA >> 12) & 0x7ffffff
  def PTEToPA(PTE: BigInt) = (PTE & BigInt("003ffffffffffc00", 16)) << 2
  def SetPTE(PA: BigInt, mods: BigInt) = ((PA & BigInt("00fffffffffff000", 16)) >> 2) | mods
}