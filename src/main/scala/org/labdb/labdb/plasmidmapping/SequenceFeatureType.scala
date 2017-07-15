package org.labdb.labdb.plasmidmapping

import scala.util.matching.Regex
import scala.util.matching.Regex.MatchData

import Sequence.Implicits._

trait SequenceFeatureType {
  val name: String
  val sequence: Sequence
  val sequenceRegex: Regex
  val exact: Boolean
  val display: Map[String, String]

  def locateAllIn(targetSequence: Sequence): List[SequenceFeature] = {
    // Double the sequence so that we pick up on circular features
    // TODO(colin): add a parameter to choose linear/circular
    val doubledSequence = targetSequence.sequence + targetSequence.sequence
    this.sequenceRegex.findAllIn(doubledSequence.toLowerCase)
      .matchData
      .toList
      .map((md: MatchData) => this.featureFromPosition(md.start))
      .filter(_.pos <= targetSequence.length
    )
  }
  def featureFromPosition(pos: Int): SequenceFeature
}

trait Binding extends SequenceFeatureType {}

case class Protein(
  name: String,
  sequence: Sequence.Protein,
  exact: Boolean,
  display: Map[String, String]
) extends SequenceFeatureType {
  val sequenceType = Sequence.Protein
  val sequenceRegex = sequence.sequence.toLowerCase.r

  override def featureFromPosition(pos: Int): SequenceFeature =
    ProteinSequence(feature=this, pos=pos+1, length=this.sequence.length)
}


case class Protease(
  name: String,
  sequence: Sequence.Protein,
  exact: Boolean,
  display: Map[String, String]
) extends Binding {
  val sequenceRegex = sequence.sequence.toLowerCase.r
  override def featureFromPosition(pos: Int): SequenceFeature = ProteaseSite(feature=this, pos=pos+1)
}


case class RestrictionEnzyme(
  name: String,
  sequence: Sequence.DNA,
  display: Map[String, String],
  cutsBefore: Integer
) extends Binding {
  val exact = true
  val sequenceRegex = Sequence.DNA_DEGENERATES.foldLeft(sequence.sequence.toLowerCase)((seqInProgress, degen) => {
    degen match {
      case (degenChar, replacement) => seqInProgress.replaceAll(degenChar, replacement)
    }
  }).r
  override def featureFromPosition(pos: Int): SequenceFeature =
    RestrictionSite(feature=this, pos=pos + 1 + this.cutsBefore)
}

// These concrete features consist of a feature type + an actual location.
trait SequenceFeature {
  val feature: SequenceFeatureType
  val pos: Int
  // While we don't use these next two server-side, they're useful for serializing to JSON
  // TODO(colin): write a better converter so that we don't have to have these two useless fields everywhere.
  val featureClass: String
  val featureExtent: String
}

trait PointFeature extends SequenceFeature {
}

trait RegionalFeature extends SequenceFeature {
  val length: Int
}

case class RestrictionSite(
  feature: RestrictionEnzyme,
  pos: Int,
  featureClass: String = "restriction",
  featureExtent: String = "point"
) extends PointFeature {
}

case class ProteinSequence(
  feature: Protein,
  pos: Int,
  length: Int,
  featureClass: String = "protein",
  featureExtent: String = "regional"
) extends RegionalFeature {
}

case class ProteaseSite(
  feature: Protease,
  pos: Int,
  featureClass: String = "protease",
  featureExtent: String = "point"
) extends PointFeature {
}
