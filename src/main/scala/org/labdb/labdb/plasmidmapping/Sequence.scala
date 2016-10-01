package org.labdb.labdb.plasmidmapping

sealed trait Sequence {
  val sequence: String
}

trait TranslationResult {}
case class AminoAcid(code: String) extends TranslationResult {}
case class StartCodon() extends TranslationResult {}
case class StopCodon(codon: String) extends TranslationResult {}

object Sequence {
  case class DNA(dna: String) extends Sequence { val sequence = dna }
  case class RNA(rna: String) extends Sequence { val sequence = rna }
  case class Protein(protein: String) extends Sequence { val sequence = protein }

  object Implicits {
    implicit def seqTypeToString(s: Sequence): String = s.sequence
    implicit def dnaToString(d: DNA): String = d.sequence
    implicit def rnaToString(r: RNA): String = r.sequence
    implicit def proteinToString(p: Protein): String = p.sequence
    implicit def stringToDNA(s: String): DNA = DNA(s)
    implicit def stringToRNA(s: String): RNA = RNA(s)
    implicit def stringToProtein(s: String): Protein = Protein(s)
  }
  import Implicits._

  val DNA_DEGENERATES: Map[String, String] = Map(
    "r" -> "[ag]",
    "k" -> "[gt]",
    "y" -> "[ct]",
    "s" -> "[cg]",
    "m" -> "[ac]",
    "w" -> "[at]",
    "h" -> "[act]",
    "v" -> "[acg]",
    "b" -> "[cgt]",
    "d" -> "[agt]",
    "n" -> "[acgt]"
  )

  def translate(codon: DNA): TranslationResult = codon.sequence match {
    case "gct" | "gcc" | "gca" | "gcg"                 => AminoAcid("A")
    case "cgt" | "cgc" | "cga" | "cgg" | "aga" | "agg" => AminoAcid("R")
    case "aat" | "aac"                                 => AminoAcid("N")
    case "gat" | "gac"                                 => AminoAcid("D")
    case "tgt" | "tgc"                                 => AminoAcid("C")
    case "caa" | "cag"                                 => AminoAcid("Q")
    case "gaa" | "gag"                                 => AminoAcid("E")
    case "ggt" | "ggc" | "gga" | "ggg"                 => AminoAcid("G")
    case "cat" | "cac"                                 => AminoAcid("H")
    case "att" | "atc" | "ata"                         => AminoAcid("I")
    case "tta" | "ttg" | "ctt" | "ctc" | "cta" | "ctg" => AminoAcid("L")
    case "aaa" | "aag"                                 => AminoAcid("K")
    case "atg"                                         => AminoAcid("M")
    case "ttt" | "ttc"                                 => AminoAcid("F")
    case "cct" | "ccc" | "cca" | "ccg"                 => AminoAcid("P")
    case "tct" | "tcc" | "tca" | "tcg" | "agt" | "agc" => AminoAcid("S")
    case "act" | "acc" | "aca" | "acg"                 => AminoAcid("T")
    case "tgg"                                         => AminoAcid("W")
    case "tat" | "tac"                                 => AminoAcid("Y")
    case "gtt" | "gtc" | "gta" | "gtg"                 => AminoAcid("V")
    case codon @ ("taa" | "tga" | "tag")               => StopCodon(codon)
  }

  def translateAll(sequence: DNA): Protein = {
    sequence.sequence.grouped(3).flatMap(codon =>
      if (codon.trim.length == 3)
        Some(translate(DNA(codon)))
      else
        None
    ).flatMap {
      case AminoAcid(letter) => letter
      case StopCodon(_) => "."
    }.mkString
  }
}

