package net.aklabs.helpers

object RegIdStrings {
  def getString(code_type: String, code: String): String = {
    code_type match {
      case "ean-13" =>
        val checkArr = Array(1, 3, 1, 3, 1, 3, 1, 3, 1, 3, 1, 3, 1)
        val seq = code.toSeq
        if (seq.size == 12) {
          var i = -1
          val checksum = seq.map(ch => {
            i += 1
            ch.toString.toInt * checkArr(i)
          }).sum
          val ch_sum = checksum % 10
          code + (if (ch_sum != 0) 10 - ch_sum else 0)
        } else if (seq.size == 13) code
        else throw new Exception("Incorrect size of barcode: " + seq.size + " for " + code_type)
      case _ => code
    }
  }

  def calculateEAN13Barcode(barcode: String):String ={
    if(barcode.length()==12){
      var even = true
      var sum = 0
      for (i <- 0 to 11) {
        var cur = Integer.parseInt(barcode.substring(i, i+1))
        if (even){
          sum+=cur
        }else{
          sum+=cur*3
        }
        even = !even
      }
      var newBarcode =""
      if((sum % 10) ==0){
        newBarcode = barcode+"0"
      }else{
        newBarcode =barcode + (10 - (sum % 10))
      }
      newBarcode
    }else{
      barcode
    }
  }
}
