// TODO: determine dynamically; share code with nav
export const Types = {
  Plasmid: "Plasmids",
  Oligo: "Oligos",
  Bacterium: "Bacteria",
  Antibody: "Antibodies",
  Line: "Worms",
  RnaiClone: "RNAi clones",
  Dino: "Dinos",
};

export type TypeKey = keyof typeof Types;
