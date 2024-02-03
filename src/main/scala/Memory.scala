package MemboxS

class Memory[T <: BaseSV](max_range: BigInt, SV: T) {
  val pmm = new PhysicalMemory(max_range)

  def createRootPageTable(): BigInt = {
    val (flag, root) = pmm.findUsable(SV.PageSize)
    if(flag)
      pmm.insertBlock(new Block(root, SV.PageSize))
    root
  }

  def allocateMemory(root: BigInt, vaddr: BigInt, size: Int): BigInt = {
    val wordSize = (new SV.WORD).size
    val realsize = (size + SV.PageSize - 1) / SV.PageSize * SV.PageSize
    val (flag, paddr) = pmm.findUsable(realsize)
    if(!flag || pmm.insertBlock(new Block(paddr, size, true)))
      return 0
    var pos: BigInt = 0
    val pt_idx = scala.collection.mutable.Seq.fill[Int](SV.PageLevels)(-1)
    val pt_addr = scala.collection.mutable.Seq[BigInt](root, 0, 0)

    while(pos < realsize){
      for(level <- 0 until SV.PageLevels){ // SV39: 3, SV32: 2
        //may need a new entry in PT
        if(SV.VAExtract(vaddr + pos, level) != pt_idx(level)){
          pt_idx(level) = SV.VAExtract(vaddr + pos, level).toInt
          // entry not exist in PT
          if((pmm.readWord[SV.WORD](pt_addr(level) + pt_idx(level) * wordSize)._2.toBigInt & BigInt(SV.V))== 0) {
            level match {
              case (SV.PageLevels - 1) => {
                val pt_entry_tmp = SV.SetPTE(paddr + pos, SV.R|SV.W|SV.X|SV.V)
                pmm.writeWord[SV.WORD](root + pt_idx(level) * wordSize, new SV.WORD(pt_entry_tmp))
              }
              case _ => {
                val pt_addr_tmp = pmm.findUsable(SV.PageSize)
                if(!pt_addr_tmp._1) return 0
                // create and insert PT
                pmm.insertBlock(new Block(pt_addr_tmp._2, SV.PageSize, true))
                val pt_entry_tmp = SV.SetPTE(pt_addr_tmp._2, SV.V)
                pmm.writeWord[SV.WORD](root + pt_idx(level) * wordSize, new SV.WORD(pt_entry_tmp))
              }
            }
          }
        }
        if(level < SV.PageLevels - 1)
          pt_addr(level + 1) = SV.PTEToPA(pmm.readWord[SV.WORD](pt_addr(level) + pt_idx(level) * wordSize)._2.toBigInt)
      }

      pos += SV.PageSize
    }
    return paddr
  }
  def addrConvert(root: BigInt, vaddr: BigInt): BigInt = {
    val wordSize = (new SV.WORD).size
    var pt_addr = root
    for(level <- 0 until SV.PageLevels){
      val tmp = SV.PTEToPA(pmm.readWord[SV.WORD](pt_addr + SV.VAExtract(vaddr, level)*wordSize)._2.toBigInt)
      if(tmp == 0) return 0
      pt_addr = tmp
    }
    pt_addr | (vaddr & BigInt("0fff", 16))
  }
  def readDataVirtual(root: BigInt, vaddr: BigInt, size: Int): (Boolean, Array[Byte]) = {
    var vpn: BigInt = 0; var len: Int = 0
    var out: Array[Byte] = Array.empty
    var paddr = addrConvert(root, vaddr)
    for(it <- 0 until size){
      if(vpn != SV.getVPN(vaddr + it)){
        if(len > 0){
          out = out :+ pmm.readData(paddr, len)._2
          paddr = addrConvert(root, vaddr + it)
        }
        len = 0
        vpn = SV.getVPN(vaddr + it)
      }
      if(paddr == 0) return (false, Array.empty)
      len += 1
    }
    out = out :+ pmm.readData(paddr, len)._2
    return (true, out)
  }
  def readDataPhysical(paddr: BigInt, length: Int): (Boolean, Array[Byte]) = {
    pmm.readData(paddr, length)
  }

}
