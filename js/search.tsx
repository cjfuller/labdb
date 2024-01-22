// TODO: determine dynamically; share code with nav
export const Types = {
  Plasmid: "Plasmids",
  Oligo: "Oligos",
  Bacterium: "Bacteria",
  Sample: "Samples",
  Antibody: "Antibodies",
  Line: "TC",
  Yeaststrain: "Yeast",
  SeqLib: "SeqLib",
  RnaiClone: "RNAi clone",
};

export type TypeKey = keyof typeof Types;
