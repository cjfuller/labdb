package org.labdb.labdb.plasmidmapping

import org.labdb.labdb.plasmidmapping.Sequence.DNA

class PlasmidMap(val _features: Seq[SequenceFeature]) {
  val features = _features.sortBy(_.pos)
  def withFeature(feature: SequenceFeature): PlasmidMap =
    new PlasmidMap(feature +: this.features)

  def withFeatures(features: Seq[SequenceFeature]): PlasmidMap =
    new PlasmidMap(features ++ this.features)
}

object PlasmidMap {
  def mapRestrictionEnzymes(sequence: DNA, enzymes: List[RestrictionEnzyme]) =
    enzymes.flatMap(_.locateAllIn(sequence))

  def mapOtherFeatures(sequence: DNA, features: List[SequenceFeatureType]) = {
    val doubledSequence = DNA((sequence.sequence + sequence.sequence).toLowerCase)
    val translationFrames = List(
      Sequence.translateAll(doubledSequence),
      Sequence.translateAll(DNA(doubledSequence.sequence.tail)),
      Sequence.translateAll(DNA(doubledSequence.sequence.tail.tail))
    )
    translationFrames.zipWithIndex.flatMap{ case (protein, frame) => {
        val results = KnownFeatures.knownFeatures.flatMap(_.locateAllIn(protein))
        // results has 1-indexed offsets in protein sequence, and we need to get that back to DNA:
        // subtract 1, multiply by 3, add 1, add frame offset
        // for lengths, just multiply by 3
        def proteinToDNAPos(proteinPos: Int): Int = (proteinPos - 1) * 3 + 1 + frame
        results.map {
          case ProteinSequence(f, pos, l, _, _) => ProteinSequence(f, proteinToDNAPos(pos), l * 3)
          case ProteaseSite(f, pos, _, _) => ProteaseSite(f, proteinToDNAPos(pos))
        }
      }}
    }.filter(_.pos <= sequence.sequence.length)

  def createForSequence(sequence: String): PlasmidMap = {
    val clean = DNA(sequence.trim.toLowerCase.replaceAll("\\s", ""))
    new PlasmidMap(mapRestrictionEnzymes(clean, RestrictionEnzymes.knownEnzymes) ++
                   mapOtherFeatures(clean, KnownFeatures.knownFeatures))
  }
}