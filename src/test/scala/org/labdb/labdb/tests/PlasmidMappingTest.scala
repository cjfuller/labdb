package org.labdb.labdb.tests

import org.labdb.labdb.plasmidmapping.{KnownFeatures, PlasmidMap, ProteinSequence, RestrictionEnzymes, Sequence}
import org.scalatest._

import scala.io.Source

class PlasmidMappingTest extends FlatSpec with Matchers {
  "The plasmid mapper" should "locate restriction enzymes in a DNA sequence" in {
    val restrictionSites = PlasmidMap.mapRestrictionEnzymes(
      Sequence.DNA(PlasmidMappingTestData.sequence), RestrictionEnzymes.knownEnzymes)

    restrictionSites.filter(_.feature.name == "NdeI").head.pos should be (5148)
  }

  it should "locate protein features in a DNA sequence" in {
    val otherFeatureSites = PlasmidMap.mapOtherFeatures(
      Sequence.DNA(PlasmidMappingTestData.sequence), KnownFeatures.knownFeatures)

    val hisSites = otherFeatureSites.filter(_.feature.name == "6xHis")
    hisSites.head.pos should be (5083)
    hisSites.head.asInstanceOf[ProteinSequence].length should be (18)
    hisSites(1).pos should be (5206)
    hisSites(1).asInstanceOf[ProteinSequence].length should be (18)
  }
}




object PlasmidMappingTestData {
  val sequence = Source.fromURL(getClass.getResource("/sequence_data.txt")).mkString.trim
  val mapJSON = Source.fromURL(getClass.getResource("/map_data.json")).mkString.trim
}