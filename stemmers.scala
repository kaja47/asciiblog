package asciiblog.stemmers

import java.lang.StringBuilder

object CzechStemmerLight {

  def stem(input: String): String = {
    val sb = new StringBuilder(input.toLowerCase)
    removePossessives(removeCase(sb)).toString
  }

  private def endsWith(sb: StringBuilder, str: String) =
    sb.indexOf(str, sb.length-str.length) == sb.length-str.length

  private def palatalise(sb: StringBuilder): StringBuilder = {
    val len = sb.length

    if (endsWith(sb, "ci") ||
        endsWith(sb, "ce") ||
        endsWith(sb, "či") || //-či
        endsWith(sb, "če")) { //-če
      return sb.replace(len-2, len, "k")
    }

    if (endsWith(sb, "zi") ||
        endsWith(sb, "ze") ||
        endsWith(sb, "ži") || //-ži
        endsWith(sb, "že")) { //-že
      return sb.replace(len-2, len, "h")
    }

    if (endsWith(sb, "čtě") || //-čtě
        endsWith(sb, "čti") || //-čti
        endsWith(sb, "čté")) { //-čté
      return sb.replace(len-3, len, "ck")
    }

    if (endsWith(sb, "ště") || //-ště
        endsWith(sb, "šti") || //-šti
        endsWith(sb, "šté")) { //-šté
      return sb.replace(len-2, len, "sk")
    }

    sb.delete(len-1, len)
  }

  private def removePossessives(sb: StringBuilder): StringBuilder = {
    val len = sb.length

    if (len > 5) {
      if (endsWith(sb, "ov") ||
          endsWith(sb, "ův")) { //-ův
        return sb.delete(len-2, len)
      }

      if (endsWith(sb, "in")) {
        return palatalise(sb.delete(len-1, len))
      }
    }

    sb
  }

  private def removeCase(sb: StringBuilder): StringBuilder = {
    val len = sb.length

    if (len > 7 && endsWith(sb, "atech")) {
      return sb.delete(len-5, len)
    }

    if (len > 6) {
      if (endsWith(sb, "ětem")){ //-ětem
        return palatalise(sb.delete(len-3, len))
      }

      if (endsWith(sb, "atům")) { //-atům
        return sb.delete(len-4, len)
      }
    }

    if (len > 5) {
      if (endsWith(sb, "ech") ||
          endsWith(sb, "ich") ||
          endsWith(sb, "ích") || //-ích
          endsWith(sb, "ého") || //-ého
          endsWith(sb, "ěmi") || //-ěmi
          endsWith(sb, "emi") ||
          endsWith(sb, "ému") || //ému
          endsWith(sb, "ěte") || //-ěte
          endsWith(sb, "ěti") || //-ěti
          endsWith(sb, "iho") ||
          endsWith(sb, "ího") || //-ího
          endsWith(sb, "ími") || //-ími
          endsWith(sb, "imu")) {
        return palatalise(sb.delete(len-2, len))
      }

      if (endsWith(sb, "ách") || //-ách
          endsWith(sb, "ata") ||
          endsWith(sb, "aty") ||
          endsWith(sb, "ých") || //-ých
          endsWith(sb, "ama") ||
          endsWith(sb, "ami") ||
          endsWith(sb, "ové") || //-ové
          endsWith(sb, "ovi") ||
          endsWith(sb, "ými")) { //-ými
        return sb.delete(len-3, len)
      }
    }

    if (len > 4){
      if (endsWith(sb, "em")){
        return palatalise(sb.delete(len-1, len))
      }

      if (endsWith(sb, "es") ||
          endsWith(sb, "ém") || //-ém
          endsWith(sb, "ím")) { //-ím
        return palatalise(sb.delete(len-2, len))
      }

      if (endsWith(sb, "ům") || //-ům
          endsWith(sb, "at") ||
          endsWith(sb, "ám") || //-ám
          endsWith(sb, "os") ||
          endsWith(sb, "us") ||
          endsWith(sb, "ým") || //-ým
          endsWith(sb, "mi") ||
          endsWith(sb, "ou")) {
        return sb.delete(len-2, len)
      }
    }

    if (len > 3) {
      if (endsWith(sb, "e") ||
          endsWith(sb, "i") ||
          endsWith(sb, "í") || //-í
          endsWith(sb, "ě")) { //-ě
        return palatalise(sb)
      }

      if (endsWith(sb, "u") ||
          endsWith(sb, "y") ||
          endsWith(sb, "ů") || //-ů
          endsWith(sb, "a") ||
          endsWith(sb, "o") ||
          endsWith(sb, "á") || //-á
          endsWith(sb, "é") || //-é
          endsWith(sb, "ý")) { //-ý
        return sb.delete(len-1, len)
      }
    }

    sb
  }

}


/**
 * based on code by Ljiljana Dolamic  University of Neuchatel
 */
object CzechStemmerAggresive {

  def apply(w: String) = new Word(w).stem

  class Word(w: String) {

    private val buffer = new StringBuffer(w.toLowerCase)

    def stem: String = {
      removeCase()
      removePossessives()
      removeComparative()
      removeDiminutive()
      removeAugmentative()
      removeDerivational()
      buffer.toString
    }

    private def len = buffer.length

    private def endsWith(suffix: String) =
      buffer.substring(len-suffix.length, len) == suffix

    private def trim(n: Int): this.type = {
      buffer.delete(len-n, len)
      this
    }

    private def removeDerivational(): this.type = {
      if (len > 8 && endsWith("obinec")) {
        return trim(6)
      }

      if (len > 7) {
        if (endsWith("ionář")) {
          return trim(4).palatalise()
        }
        if (endsWith("ovisk") ||
            endsWith("ovstv") ||
            endsWith("ovišt") ||
            endsWith("ovník")) {
          return trim(5)
        }
      }

      if (len > 6){
        if (endsWith("ásek") ||
            endsWith("loun") ||
            endsWith("nost") ||
            endsWith("teln") ||
            endsWith("ovec") ||
            endsWith("ovík") ||
            endsWith("ovtv") ||
            endsWith("ovin") ||
            endsWith("štin")) {
          return trim(4)
        }
        if (endsWith("enic") ||
            endsWith("inec") ||
            endsWith("itel")){
          return trim(3).palatalise()
        }
      }

      if (len > 5){
        if (endsWith("árn")){
          return trim(3)
        }
        if (endsWith("ěnk")){
          return trim(2).palatalise()
        }
        if (endsWith("ián")||
            endsWith("ist")||
            endsWith("isk")||
            endsWith("išt")||
            endsWith("itb")||
            endsWith("írn")){
          return trim(2).palatalise()
        }
        if (endsWith("och")||
            endsWith("ost")||
            endsWith("ovn")||
            endsWith("oun")||
            endsWith("out")||
            endsWith("ouš")||
            endsWith("ušk")||
            endsWith("kyn")||
            endsWith("čan")||
            endsWith("kář")||
            endsWith("néř")||
            endsWith("ník")||
            endsWith("ctv")||
            endsWith("stv")){
          return trim(3)
        }
      }

      if (len > 4){
        if (endsWith("áč")||
            endsWith("ač")||
            endsWith("án")||
            endsWith("an")||
            endsWith("ář")||
            endsWith("as")){
          return trim(2)
        }
        if (endsWith("ec")||
            endsWith("en")||
            endsWith("ěn")||
            endsWith("éř")){
          return trim(1).palatalise()
        }
        if (endsWith("íř")||
            endsWith("ic")||
            endsWith("in")||
            endsWith("ín")||
            endsWith("it")||
            endsWith("iv")){
          return trim(1).palatalise()
        }
        if (endsWith("ob")||
            endsWith("ot")||
            endsWith("ov")||
            endsWith("oň")||
            endsWith("ul")||
            endsWith("yn")||
            endsWith("čk")||
            endsWith("čn")||
            endsWith("dl")||
            endsWith("nk")||
            endsWith("tv")||
            endsWith("tk")||
            endsWith("vk")){
          return trim(2)
        }
      }

      if (len > 3){
        if (endsWith("c")||
            endsWith("č")||
            endsWith("k")||
            endsWith("l")||
            endsWith("n")||
            endsWith("t")){
          trim(1)
        }
      }

      this
    }

    private def removeAugmentative(): this.type = {
      if ((len > 6) &&
          endsWith("ajzn")){
        return trim(4)
      }
      if ((len > 5) &&
          (endsWith("izn")||
           endsWith("isk"))){
        return trim(2).palatalise()
      }
      if ((len > 4) &&
          endsWith("ák")){
        return trim(2)
      }
      this
    }

    private def removeDiminutive(): this.type = {
      if ((len > 7) &&
          endsWith("oušek")){
        return trim(5)
      }

      if (len> 6){
        if (endsWith("eček")||
            endsWith("éček")||
            endsWith("iček")||
            endsWith("íček")||
            endsWith("enek")||
            endsWith("ének")||
            endsWith("inek")||
            endsWith("ínek")){
          return trim(3).palatalise()
        }
        if (endsWith("áček")||
            endsWith("aček")||
            endsWith("oček")||
            endsWith("uček")||
            endsWith("anek")||
            endsWith("onek")||
            endsWith("unek")||
            endsWith("ánek")){
          return trim(4)
        }
      }

      if (len > 5){
        if (endsWith("ečk")||
            endsWith("éčk")||
            endsWith("ičk")||
            endsWith("íčk")||
            endsWith("enk")||
            endsWith("énk")||
            endsWith("ink")||
            endsWith("ínk")){
          return trim(3).palatalise()
        }
        if (endsWith("áčk")||
            endsWith("ačk")||
            endsWith("očk")||
            endsWith("učk")||
            endsWith("ank")||
            endsWith("onk")||
            endsWith("unk")||
            endsWith("átk")||
            endsWith("ánk")||
            endsWith("ušk")){
          return trim(3)
        }
      }

      if (len > 4){
        if (endsWith("ek")||
            endsWith("ék")||
            endsWith("ík")||
            endsWith("ik")){
          return trim(1).palatalise()
        }
        if (endsWith("ák")||
            endsWith("ak")||
            endsWith("ok")||
            endsWith("uk")){
          return trim(1)
        }
      }

      if ((len > 3) && endsWith("k")){
        return trim(1)
      }

      this
    }

    private def removeComparative(): this.type = {
      if ((len > 5) &&
          (endsWith("ejš")|| //-ejš
           endsWith("ějš"))){ //-ějš
        return trim(2).palatalise()
      }
      this
    }

    private def palatalise(): this.type = {
      if (endsWith("ci")||
          endsWith("ce")||
          endsWith("či")||
          endsWith("če")){
        buffer.replace(len-2 , len, "k")
        return this
      }
      if (endsWith("zi")||
          endsWith("ze")||
          endsWith("ži")||
          endsWith("že")){
        buffer.replace(len-2 , len, "h")
        return this
      }
      if (endsWith("čtě")||
          endsWith("čti")||
          endsWith("čtí")){
        buffer.replace(len-3 , len, "ck")
        return this
      }
      if (endsWith("ště")||
          endsWith("šti")||
          endsWith("ští")){
        buffer.replace(len-2 , len, "sk")
        return this
      }
      return trim(1)
    }

    private def removePossessives(): this.type = {
      if (len > 5) {
        if (endsWith("ov")||
            endsWith("ův")){
          return trim(2)
        }
        if (endsWith("in")){
          return trim(1).palatalise()
        }
      }
      this
    }

    private def removeCase(): this.type = {
      if ((len > 7) &&
          endsWith("atech")){
        return trim(5)
      }
      if (len > 6){
        if (endsWith("ětem")){
          return trim(3).palatalise()
        }
        if (endsWith("atům")){
          return trim(4)
        }
      }
      if (len > 5){
        if (endsWith("ech")||
            endsWith("ich")||
            endsWith("ích")){
          return trim(2).palatalise()
        }
        if (endsWith("ého")||
            endsWith("ěmi")|| //-ěmu ???
            endsWith("emi")||
            endsWith("ému")||
            endsWith("ete")||
            endsWith("eti")||
            endsWith("iho")||
            endsWith("ího")||
            endsWith("ími")||
            endsWith("imu")){
          return trim(2).palatalise()
        }
        if (endsWith("ách")||
            endsWith("ata")||
            endsWith("aty")||
            endsWith("ých")||
            endsWith("ama")||
            endsWith("ami")||
            endsWith("ové")||
            endsWith("ovi")||
            endsWith("ými")){
          return trim(3)
        }
      }
      if (len > 4){
        if (endsWith("em")){
          return trim(1).palatalise()
        }
        if (endsWith("es")||
            endsWith("ém")||
            endsWith("ím")){
          return trim(2).palatalise()
        }
        if (endsWith("ům")||
            endsWith("at")||
            endsWith("ám")||
            endsWith("os")||
            endsWith("us")||
            endsWith("ým")||
            endsWith("mi")||
            endsWith("ou")){
          return trim(2)
        }
      }
      if (len > 3){
        if (endsWith("e")||
            endsWith("i")||
            endsWith("í")||
            endsWith("ě")){
          return palatalise()
        }
        if (endsWith("u")||
            endsWith("y")||
            endsWith("ů")||
            endsWith("a")||
            endsWith("o")||
            endsWith("á")||
            endsWith("é")||
            endsWith("ý")){
          return trim(1)
        }
      }
      this
    }

  }
}
