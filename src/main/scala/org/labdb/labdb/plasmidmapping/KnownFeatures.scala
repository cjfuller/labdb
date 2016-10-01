package org.labdb.labdb.plasmidmapping

object KnownFeatures {
  import Sequence.Implicits._
  val knownFeatures = List(
    Protein(
      name = "FLAG",
      sequence = "DYKDDDDK",
      exact = true,
      display = Map("feature_class" -> "affinity-tag")),
    Protein(
      name = "MBP",
      sequence = "MKIEEGKLVIWINGDKGYNGLAEVGKKFEKDTGIKVTVEHPDKLEEKFPQVAATGDGPDIIFWAHDRFGGYAQSGLLAEITPDKAFQDKLYPFTWDAVRYNGKLIAYPIAVEALSLIYNKDLLPNPPKTWEEIPALDKELKAKGKSALMFNLQEPYFTWPLIAADGGYAFKYENGKYDIKDVGVDNAGAKAGLTFLVDLIKNKHMNADTDYSIAEAAFNKGETAMTINGPWAWSNIDTSKVNYGVTVLPTFKGQPSKPFVGVLSAGINAASPNKELAKEFLENYLLTDEGLEAVNKDKPLGAVALKSYEEELAKDPRIAATMENAQKGEIMPNIPQMSAFWYAVRTAVINAASGRQTVDEALKDAQ",
      exact = false,
      display = Map("feature_class" -> "affinity-tag")),
    Protein(
      name = "6xHis",
      sequence = "HHHHHH",
      exact = true,
      display = Map("feature_class" -> "affinity-tag")),
    Protein(
      name = "myc",
      sequence = "EQKLISEEDL",
      exact = true,
      display = Map("feature_class" -> "affinity-tag")),
    Protein(
      name = "EGFP",
      sequence = "MVSKGEELFTGVVPILVELDGDVNGHKFSVSGEGEGDATYGKLTLKFICTTGKLPVPWPTLVTTLTYGVQCFSRYPDHMKQHDFFKSAMPEGYVQERTIFFKDDGNYKTRAEVKFEGDTLVNRIELKGIDFKEDGNILGHKLEYNYNSHNVYIMADKQKNGIKVNFKIRHNIEDGSVQLADHYQQNTPIGDGPVLLPDNHYLSTQSALSKDPNEKRDHMVLLEFVTAAGITLGMDELYK",
      exact = false,
      display = Map("feature_class" -> "fp-green")),
    Protein(
      name = "AmpR",
      sequence = "MSIQHFRVALIPFFAAFCLPVFAHPETLVKVKDAEDQLGARVGYIELDLNSGKILESFRPEERFPMMSTFKVLLCGAVLSRIDAGQEQLGRRIHYSQNDLVEYSPVTEKHLTDGMTVRELCSAAITMSDNTAANLLLTTIGGPKELTAFLHNMGDHVTRLDRWEPELNEAIPNDERDTTMPVAMATTLRKLLTGELLTLASRQQLIDWMEADKVAGPLLRSALPAGWFIADKSGAGERGSRGIIAALGPDGKPSRIVVIYTTGSQATMDERNRQIAEIGASLIKHW",
      exact = false,
      display = Map("feature_class" -> "antibiotic")),
    Protease(
      name = "precission",
      sequence = "LEVLFQGP",
      exact = true,
      display = Map("feature_class" -> "protease")),
    Protein(
      name = "strep",
      sequence = "AWRHPQFGG",
      exact = false,
      display = Map("feature_class" -> "affinity-tag")),
    Protein(
      name = "GST",
      sequence = "LGYWKIKGLVQPTRLLLEYLEEKYEEHLYERDEGDKWRNKKFELGLEFPNLPYYIDGDVKLTQSMAIIRYIADKHNMLGGCPKERAEISMLEGAVLDIRYGVSRIAYSKDFETLKVDFLSKLPEMLKMFEDRLCHKTYLNGDHVTHPDFMLYDALDVVLYMDPMCLDAFPKLVCFKKRIEAIPQIDKYLKSSKYIAWPLQGWQATFGGGDHPPKSD",
      exact = false,
      display = Map("feature_class" -> "affinity-tag")),
    Protease(
      name = "thrombin",
      sequence = "LVPRGS",
      exact = true,
      display = Map("feature_class" -> "protease")),
    Protein(
      name = "strepII",
      sequence = "WSHPQFEK",
      exact = true,
      display = Map("feature_class" -> "affinity-tag"))
  )
  val knownFeaturesByName = knownFeatures.map {
    (feat: SequenceFeatureType) => (feat.name, feat)
  }.toMap
}
