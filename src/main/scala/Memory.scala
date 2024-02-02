package MemboxS

class Memory(max_range: BigInt, SV: BaseSV) {
  val pmm = new PhysicalMemory(max_range)

  def createRootPageTable(): BigInt = {
    val (flag, root) = pmm.findUsable(SV.PageSize)
    if(flag)
      pmm.insertBlock(new Block(root, SV.PageSize))
    root
  }

  def allocateMemory(root: BigInt, vaddr: BigInt, size: Int): BigInt = {
    val realsize = (size + SV.PageSize - 1) / SV.PageSize * SV.PageSize
    val (flag, paddr) = pmm.findUsable(realsize)
    if(!flag || pmm.insertBlock(new Block(paddr, size, true)))
      return 0
    var pos: BigInt = 0
    val pt_idx = scala.collection.mutable.Seq.fill[Int](SV.PageLevels)(-1)
    var pt1_addr: BigInt = 0

    while(pos < realsize){
      for(level <- 0 until SV.PageLevels){
        //may need a new entry in rootPT
        if(SV.VAExtract(vaddr + pos, level) != pt_idx(level)){
          pt_idx(level) = SV.VAExtract(vaddr + pos, level).toInt
          // entry not exist in rootPT
          if((pmm.readWord[Int64](vaddr + pos * (new Int64).getLength)._2.toBigInt & BigInt(SV.V))== 0) {
            val pt1_addr = pmm.findUsable(SV.PageSize)
            if(!pt1_addr._1) return 0
            // create and insert PT1
            pmm.insertBlock(new Block(pt1_addr._2, SV.PageSize, true))
            val rootpt_entry = SV.SetPTE(pt1_addr._2, SV.V)
            pmm.writeWord[Int64](root + pt_idx(level) * Int64.size, Int64(rootpt_entry))
          }
        }
      }

      pos += SV.PageSize
    }
  }
}
