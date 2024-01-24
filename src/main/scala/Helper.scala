package MemboxS

object Helper {
  def BigInt2ByteArray(n: BigInt, len: Int): Array[Byte] = n.toByteArray.takeRight(len).reverse.padTo(len, 0.toByte)
  def Hex2ByteArray(hex: String, len: Int): Array[Byte] = BigInt("00" ++ hex, 16).toByteArray.takeRight(len).reverse.padTo(len, 0.toByte)
  def ByteArray2BigInt(ba: Array[Byte]) = BigInt(0.toByte +: ba.reverse)
}

object SV39{
  def MaxPhyRange = BigInt("0100000000000000", 16)
  def PageBits = 12
  def PageSize= 1 << PageBits
  def PageElem = 512
  def VALowerCeil = BigInt("0000004000000000", 16)
  def VAUpperFloor = BigInt("0ffffffc000000000", 16)
  def PageLevels: Int = 3
  def V = 1
  def R = 2
  def W = 4
  def X = 8
  def U = 16
  def G = 32
  def A = 64
  def D = 128
  def VAExtract(VA: Long, level: Int) = {
    level match{
      case 0 => (VA >> 30) & 0x1ff
      case 1 => (VA >> 21) & 0x1ff
      case _ => (VA >> 12) & 0x1ff
    }
  }
  def VACheck(VA: BigInt) = VA >= VALowerCeil && VA < VAUpperFloor
  def PTEToPA(PTE: BigInt) = (PTE & BigInt("003ffffffffffc00", 16)) << 2
  def SetPTE(PA: BigInt, mods: BigInt) = ((PA & BigInt("00fffffffffff000", 16)) >> 2) | mods
}