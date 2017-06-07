package org.labdb.labdb.plasmidmapping

object RestrictionEnzymes {
  import Sequence.Implicits._
  val normalEnzymeSet = Set(
    "AatII", "AccI", "Acc65I", "AclI", "AfeI", "AflII", "AflIII", "AgeI", "AhdI",
    "AleI", "AluI", "AlwNI", "AoxI", "ApaI", "ApaBI", "ApaLI", "ApoI", "AscI",
    "AseI", "Asi256I", "AsiSI", "AvaI", "AvaII", "AvrII", "BaeGI", "BamHI", "BanI",
    "BanII", "BclI", "BfaI", "BglI", "BglII", "BlpI", "BmtI", "BsaAI", "BsaBI",
    "BsaHI", "BsaJI", "BsaWI", "BsiEI", "BsiHKAI", "BsiWI", "BslI", "Bsp1286I",
    "BspEI", "BspHI", "BsrFI", "BsrGI", "BssHII", "BstAPI", "BstBI", "BstEII",
    "BstNI", "BstUI", "BstXI", "BstYI", "BstZ17I", "Bsu36I", "BtgI", "BthCI",
    "Cac8I", "ChaI", "ClaI", "CviAII", "CviKI", "CviQI", "DdeI", "DpnI", "DraI",
    "DraIII", "DrdI", "EaeI", "EagI", "EcoHI", "EcoNI", "EcoO109I", "EcoRI",
    "EcoRV", "Eco53kI", "EsaBC3I", "FatI", "FmuI", "Fnu4HI", "FseI", "FspI", "HaeI",
    "HaeII", "HaeIII", "HauII", "HhaI", "HinP1I", "HincII", "HindIII", "HinfI",
    "HpaI", "HpaII", "Hpy99I", "Hpy166II", "Hpy188I", "Hpy188III", "HpyCH4III",
    "HpyCH4IV", "HpyCH4V", "KasI", "KpnI", "LpnI", "MboI", "McaTI", "MfeI", "MluI",
    "MluCI", "MscI", "MseI", "MslI", "MspA1I", "MwoI", "NaeI", "NarI", "NciI",
    "NcoI", "NdeI", "NgoMIV", "NheI", "NlaIII", "NlaIV", "Nli3877I", "NotI", "NruI",
    "NsiI", "NspI", "PabI", "PacI", "PciI", "PflMI", "PluTI", "PmeI", "PmlI",
    "Ppu10I", "PpuMI", "PshAI", "PsiI", "Psp03I", "PspGI", "PspOMI", "PspXI",
    "PssI", "PstI", "PvuI", "PvuII", "RsaI", "RsrII", "SacI", "SacII", "SalI",
    "Sau96I", "SbfI", "ScaI", "SciI", "ScrFI", "SelI", "SexAI", "SfcI", "SfiI",
    "SfoI", "SgrAI", "SmaI", "SmlI", "SnaBI", "SpeI", "SphI", "SrfI", "Sse8647I",
    "SspI", "Sth302II", "StuI", "StyI", "StyD4I", "SwaI", "TaqI", "TfiI", "TseI",
    "Tsp45I", "TspRI", "Tth111I", "UnbI", "VpaK11AI", "XbaI", "XcmI", "XhoI",
    "XmaI", "XmnI", "ZraI")
  val knownEnzymes = List(
    RestrictionEnzyme(
      name = "BstOI",
      sequence = "CCWGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Bfi57I",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "EcoVIII",
      sequence = "AAGCTT",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BspEI",
      sequence = "TCCGGA",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Ssp27144I",
      sequence = "ATCGAT",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BsiHKAI",
      sequence = "GWGCWC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "SetI",
      sequence = "ASST",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "BshFI",
      sequence = "GGCC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "HpyBII",
      sequence = "GTNNAC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "Esp1396I",
      sequence = "CCANNNNNTGG",
      display = Map(),
      cutsBefore = 7),
    RestrictionEnzyme(
      name = "FunII",
      sequence = "GAATTC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "HpyCH4I",
      sequence = "CATG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "RsaI",
      sequence = "GTAC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Bsp68I",
      sequence = "TCGCGA",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "PauAII",
      sequence = "TTTAAA",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "AvrII",
      sequence = "CCTAGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BstNSI",
      sequence = "RCATGY",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "AflII",
      sequence = "CTTAAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Eco78I",
      sequence = "GGCGCC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "BspMII",
      sequence = "TCCGGA",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BciBII",
      sequence = "CCWGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Eco21kI",
      sequence = "CCNGG",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "HgiJI",
      sequence = "GGWCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Sru4DI",
      sequence = "ATTAAT",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BspNI",
      sequence = "CCWGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BshVI",
      sequence = "ATCGAT",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "AitI",
      sequence = "AGCGCT",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "HgiHII",
      sequence = "GRCGYC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "SviI",
      sequence = "TTCGAA",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "DsaV",
      sequence = "CCNGG",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "BspAI",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "AosII",
      sequence = "GRCGYC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "AstWI",
      sequence = "GRCGYC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "NspIV",
      sequence = "GGNCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "AsuII",
      sequence = "TTCGAA",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Sau3AI",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "RsaNI",
      sequence = "GTAC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Eco137kI",
      sequence = "CCNGG",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "Sfr303I",
      sequence = "CCGCGG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "NspII",
      sequence = "GDGCHC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "DraIII",
      sequence = "CACNNNGTG",
      display = Map(),
      cutsBefore = 6),
    RestrictionEnzyme(
      name = "PspGI",
      sequence = "CCWGG",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "Asp713I",
      sequence = "CTGCAG",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "DpaI",
      sequence = "AGTACT",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "Bsc4I",
      sequence = "CCNNNNNNNGG",
      display = Map(),
      cutsBefore = 7),
    RestrictionEnzyme(
      name = "BsiSI",
      sequence = "CCGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "HaeI",
      sequence = "WGGCCW",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "EcoRI",
      sequence = "GAATTC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "NflHII",
      sequence = "CCGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BanI",
      sequence = "GGYRCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "SalPI",
      sequence = "CTGCAG",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "BsoCI",
      sequence = "GDGCHC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "MluNI",
      sequence = "TGGCCA",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "SfoI",
      sequence = "GGCGCC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "AcvI",
      sequence = "CACGTG",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "Eco75KI",
      sequence = "GRGCYC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "Sfr274I",
      sequence = "CTCGAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Ssp1I",
      sequence = "TTCGAA",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Bce4I",
      sequence = "GCNNNNNNNGC",
      display = Map(),
      cutsBefore = 7),
    RestrictionEnzyme(
      name = "SspCI",
      sequence = "GCCGGC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "Tsp49I",
      sequence = "ACGT",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "BstT9I",
      sequence = "GGTNACC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "SspD5II",
      sequence = "ATGCAT",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "OkrAI",
      sequence = "GGATCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Eco72I",
      sequence = "CACGTG",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "AhlI",
      sequence = "ACTAGT",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BspLU4I",
      sequence = "CYCGRG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "HpySE526I",
      sequence = "ACGT",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "EcoO109I",
      sequence = "RGGNCCY",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BspKI",
      sequence = "GGCC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Hin6I",
      sequence = "GCGC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "MspYI",
      sequence = "YACGTR",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "BspF4I",
      sequence = "GGNCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "AccEBI",
      sequence = "GGATCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "KasI",
      sequence = "GGCGCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BliRI",
      sequence = "ATCGAT",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Eco29kI",
      sequence = "CCGCGG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "BspO4I",
      sequence = "CAGCTG",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "PauI",
      sequence = "GCGCGC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "AspHI",
      sequence = "GWGCWC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "Bst1107I",
      sequence = "GTATAC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "Asp745I",
      sequence = "GGWCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BstBI",
      sequence = "TTCGAA",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Alw21I",
      sequence = "GWGCWC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "BavAII",
      sequence = "GGNCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Asi256I",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "AcoI",
      sequence = "YGGCCR",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "PinBII",
      sequence = "TCCGGA",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Bme142I",
      sequence = "RGCGCY",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "Bst1I",
      sequence = "CCWGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "FnuDI",
      sequence = "GGCC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BliAI",
      sequence = "ATCGAT",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "SsrI",
      sequence = "GTTAAC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "SbvI",
      sequence = "GGCC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Bim19I",
      sequence = "TTCGAA",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "PfaAIII",
      sequence = "GCATGC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "DpnI",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "EcoICRI",
      sequence = "GAGCTC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "TscAI",
      sequence = "CASTGNN",
      display = Map(),
      cutsBefore = 7),
    RestrictionEnzyme(
      name = "BdiSI",
      sequence = "CTRYAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BscBI",
      sequence = "GGNNCC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "BssNI",
      sequence = "GRCGYC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "AclI",
      sequence = "AACGTT",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "MkrAI",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "Bce243I",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "FunI",
      sequence = "AGCGCT",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "SchZI",
      sequence = "CCGCGG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "PauAI",
      sequence = "RCATGY",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "BspLI",
      sequence = "GGNNCC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "FalII",
      sequence = "CGCG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "EclHKI",
      sequence = "GACNNNNNGTC",
      display = Map(),
      cutsBefore = 6),
    RestrictionEnzyme(
      name = "AssI",
      sequence = "AGTACT",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "ApoI",
      sequence = "RAATTY",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "AhaB8I",
      sequence = "GGTACC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "AflI",
      sequence = "GGWCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BmcAI",
      sequence = "AGTACT",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "BpcI",
      sequence = "CTRYAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "CspI",
      sequence = "CGGWCCG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "AgeI",
      sequence = "ACCGGT",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "NgoAIII",
      sequence = "CCGCGG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "BflI",
      sequence = "CCNNNNNNNGG",
      display = Map(),
      cutsBefore = 7),
    RestrictionEnzyme(
      name = "FspAI",
      sequence = "RTGCGCAY",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "EcoRV",
      sequence = "GATATC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "AcyI",
      sequence = "GRCGYC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Mpr154I",
      sequence = "CCGCGG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "HpaI",
      sequence = "GTTAAC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "NspMACI",
      sequence = "AGATCT",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "HpyF44III",
      sequence = "TGCA",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "CfaI",
      sequence = "RAATTY",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BpoAI",
      sequence = "ATTAAT",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "PinAI",
      sequence = "ACCGGT",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "XagI",
      sequence = "CCTNNNNNAGG",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "Bst98I",
      sequence = "CTTAAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BasI",
      sequence = "CCANNNNNTGG",
      display = Map(),
      cutsBefore = 7),
    RestrictionEnzyme(
      name = "Pun14627I",
      sequence = "TGCGCA",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "MltI",
      sequence = "AGCT",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "MspNII",
      sequence = "RGATCY",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Cfr9I",
      sequence = "CCCGGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "CeqI",
      sequence = "GATATC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "BecAII",
      sequence = "GGCC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "PspN4I",
      sequence = "GGNNCC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "Bsp2095I",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "CscI",
      sequence = "CCGCGG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "Hsp92I",
      sequence = "GRCGYC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BspWI",
      sequence = "GCNNNNNNNGC",
      display = Map(),
      cutsBefore = 7),
    RestrictionEnzyme(
      name = "Eco32I",
      sequence = "GATATC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "PflKI",
      sequence = "GGCC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BmiI",
      sequence = "GGNNCC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "PhoI",
      sequence = "GGCC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BspLAII",
      sequence = "TTCGAA",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "EagBI",
      sequence = "CGATCG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "AanI",
      sequence = "TTATAA",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "Asp10HII",
      sequence = "CCANNNNNTGG",
      display = Map(),
      cutsBefore = 7),
    RestrictionEnzyme(
      name = "Hpy51I",
      sequence = "GTSAC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "Bse17I",
      sequence = "CCWGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "EcoRII",
      sequence = "CCWGG",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "SmoI",
      sequence = "CTYRAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "XmnI",
      sequence = "GAANNNNTTC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "Eae46I",
      sequence = "CCGCGG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "Bsp4009I",
      sequence = "GGATCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "MflI",
      sequence = "RGATCY",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "AccB7I",
      sequence = "CCANNNNNTGG",
      display = Map(),
      cutsBefore = 7),
    RestrictionEnzyme(
      name = "AsiI",
      sequence = "GGATCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "SenPT14bI",
      sequence = "CCGCGG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "SslI",
      sequence = "CCWGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "SwaI",
      sequence = "ATTTAAAT",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "PceI",
      sequence = "AGGCCT",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "GceGLI",
      sequence = "CCGCGG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "BshTI",
      sequence = "ACCGGT",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "SaqAI",
      sequence = "TTAA",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Bse24I",
      sequence = "CCWGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BavBI",
      sequence = "CAGCTG",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "XciI",
      sequence = "GTCGAC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BciBI",
      sequence = "ATCGAT",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BlfI",
      sequence = "TCCGGA",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "AjoI",
      sequence = "CTGCAG",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "OxaNI",
      sequence = "CCTNAGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "MnoI",
      sequence = "CCGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "FspBI",
      sequence = "CTAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "EagI",
      sequence = "CGGCCG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "RsrI",
      sequence = "GAATTC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BspRI",
      sequence = "GGCC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Kpn49kII",
      sequence = "CCSGG",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "BspKT8I",
      sequence = "AAGCTT",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "EclXI",
      sequence = "CGGCCG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Bsp519I",
      sequence = "GRGCYC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "SfaI",
      sequence = "GGCC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Mgl14481I",
      sequence = "CCSGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Eco56I",
      sequence = "GCCGGC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "MstI",
      sequence = "TGCGCA",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "HpyF3I",
      sequence = "CTNAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BavAI",
      sequence = "CAGCTG",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "SseBI",
      sequence = "AGGCCT",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "PspCI",
      sequence = "CACGTG",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "BspM90I",
      sequence = "GTATAC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "BbeI",
      sequence = "GGCGCC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "BstXI",
      sequence = "CCANNNNNNTGG",
      display = Map(),
      cutsBefore = 8),
    RestrictionEnzyme(
      name = "BvuI",
      sequence = "GRGCYC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "BpuB5I",
      sequence = "CGTACG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BstDEI",
      sequence = "CTNAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "SplI",
      sequence = "CGTACG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BthCI",
      sequence = "GCNGC",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "FnuEI",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "Eco91I",
      sequence = "GGTNACC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "SarI",
      sequence = "AGGCCT",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "Bsh45I",
      sequence = "GWGCWC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "PlaII",
      sequence = "TTCGAA",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "SatI",
      sequence = "GCNGC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "MlsI",
      sequence = "TGGCCA",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "ApyI",
      sequence = "CCWGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "PcsI",
      sequence = "WCGNNNNNNNCGW",
      display = Map(),
      cutsBefore = 7),
    RestrictionEnzyme(
      name = "LlaCI",
      sequence = "AAGCTT",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Pfl27I",
      sequence = "RGGWCCY",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "XbaI",
      sequence = "TCTAGA",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Kpn2I",
      sequence = "TCCGGA",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BavI",
      sequence = "CAGCTG",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "BssAI",
      sequence = "RCCGGY",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "AflIII",
      sequence = "ACRYGT",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "SfcI",
      sequence = "CTRYAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BseDI",
      sequence = "CCNNGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "MspI",
      sequence = "CCGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BtuMI",
      sequence = "TCGCGA",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "BseQI",
      sequence = "GGCC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "DrdI",
      sequence = "GACNNNNNNGTC",
      display = Map(),
      cutsBefore = 7),
    RestrictionEnzyme(
      name = "TthHB8I",
      sequence = "TCGA",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "HaeII",
      sequence = "RGCGCY",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "SauMI",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "SalI",
      sequence = "GTCGAC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Pfl21I",
      sequence = "CTGCAG",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "XcmI",
      sequence = "CCANNNNNNNNNTGG",
      display = Map(),
      cutsBefore = 8),
    RestrictionEnzyme(
      name = "AfeI",
      sequence = "AGCGCT",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "EcoT14I",
      sequence = "CCWWGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "DsaII",
      sequence = "GGCC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "RshI",
      sequence = "CGATCG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "PtaI",
      sequence = "TCCGGA",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "CpoI",
      sequence = "CGGWCCG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "XmaJI",
      sequence = "CCTAGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BsaWI",
      sequence = "WCCGGW",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "TaqXI",
      sequence = "CCWGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Bca77I",
      sequence = "WCCGGW",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BciT130I",
      sequence = "CCWGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Tru1I",
      sequence = "TTAA",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Eco105I",
      sequence = "TACGTA",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "MspCI",
      sequence = "CTTAAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BstBSI",
      sequence = "GTATAC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "Pac25I",
      sequence = "CCCGGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "XmaI",
      sequence = "CCCGGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "FbaI",
      sequence = "TGATCA",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "SpeI",
      sequence = "ACTAGT",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "ClaI",
      sequence = "ATCGAT",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "SpmI",
      sequence = "ATCGAT",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Bco118I",
      sequence = "RCCGGY",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Tsp32II",
      sequence = "TCGA",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Eco47III",
      sequence = "AGCGCT",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "PamII",
      sequence = "GRCGYC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BdiI",
      sequence = "ATCGAT",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "SauSI",
      sequence = "GCCGGC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "Sth368I",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "CfuII",
      sequence = "CTGCAG",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "FnuCI",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "RaqI",
      sequence = "CCGCGG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "AeuI",
      sequence = "CCWGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Bme1580I",
      sequence = "GKGCMC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "ApaORI",
      sequence = "CCWGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "StuI",
      sequence = "AGGCCT",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "BssHII",
      sequence = "GCGCGC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "AsiSI",
      sequence = "GCGATCGC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "SthI",
      sequence = "GGTACC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "FauNDI",
      sequence = "CATATG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "AliAJI",
      sequence = "CTGCAG",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "XhoII",
      sequence = "RGATCY",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "AsiAI",
      sequence = "ACCGGT",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "EcoT22I",
      sequence = "ATGCAT",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "PlaAI",
      sequence = "CYCGRG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "ZanI",
      sequence = "CCWGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "FspMSI",
      sequence = "GGWCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "NaeI",
      sequence = "GCCGGC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "ApaBI",
      sequence = "GCANNNNNTGC",
      display = Map(),
      cutsBefore = 8),
    RestrictionEnzyme(
      name = "CspBI",
      sequence = "GCGGCCGC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "MvnI",
      sequence = "CGCG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BshI",
      sequence = "GGCC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "KoxII",
      sequence = "GRGCYC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "AluBI",
      sequence = "AGCT",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "PflBI",
      sequence = "CCANNNNNTGG",
      display = Map(),
      cutsBefore = 7),
    RestrictionEnzyme(
      name = "BcuI",
      sequence = "ACTAGT",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Bst28I",
      sequence = "ATCGAT",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "ApeKI",
      sequence = "GCWGC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "XmaCI",
      sequence = "CCCGGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "NheI",
      sequence = "GCTAGC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BspAAIII",
      sequence = "GGATCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "EgeI",
      sequence = "GGCGCC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "BstB7SI",
      sequence = "RCCGGY",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "EheI",
      sequence = "GGCGCC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "Pvu84II",
      sequence = "CAGCTG",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "Mly113I",
      sequence = "GGCGCC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "SanDI",
      sequence = "GGGWCCC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BloHII",
      sequence = "CTGCAG",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "Rme21I",
      sequence = "ATCGAT",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BspXII",
      sequence = "TGATCA",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BbuI",
      sequence = "GCATGC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "PunAI",
      sequence = "CYCGRG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "PunAII",
      sequence = "RCATGY",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "BspMAI",
      sequence = "CTGCAG",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "Pde133I",
      sequence = "GGCC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Acc113I",
      sequence = "AGTACT",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "BslI",
      sequence = "CCNNNNNNNGG",
      display = Map(),
      cutsBefore = 7),
    RestrictionEnzyme(
      name = "AviII",
      sequence = "TGCGCA",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "PstI",
      sequence = "CTGCAG",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "MaeK81I",
      sequence = "CGTACG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "RflFI",
      sequence = "GTCGAC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Cfr6I",
      sequence = "CAGCTG",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "Nli3877I",
      sequence = "CYCGRG",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "DraI",
      sequence = "TTTAAA",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "AhaIII",
      sequence = "TTTAAA",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "FspII",
      sequence = "TTCGAA",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Csp68KI",
      sequence = "GGWCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "XhoI",
      sequence = "CTCGAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "CviRI",
      sequence = "TGCA",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "SciI",
      sequence = "CTCGAG",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "Bsp1286I",
      sequence = "GDGCHC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "LlaAI",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "Psp1406I",
      sequence = "AACGTT",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "AfaI",
      sequence = "GTAC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Acc65I",
      sequence = "GGTACC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BmyI",
      sequence = "GDGCHC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "AsnI",
      sequence = "ATTAAT",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "CsiAI",
      sequence = "ACCGGT",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "SunI",
      sequence = "CGTACG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "PflFI",
      sequence = "GACNNNGTC",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "BstMBI",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "BstMZ611I",
      sequence = "CCNGG",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "PlaI",
      sequence = "GGCC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BstBAI",
      sequence = "YACGTR",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "SsiBI",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "TspEI",
      sequence = "AATT",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "XapI",
      sequence = "RAATTY",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "XpaI",
      sequence = "CTCGAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "AatII",
      sequence = "GACGTC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "GluI",
      sequence = "GCNGC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BvuBI",
      sequence = "CGTACG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BspBRI",
      sequence = "GGCC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "KpnI",
      sequence = "GGTACC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "BstDSI",
      sequence = "CCRYGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "SciNI",
      sequence = "GCGC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "PacI",
      sequence = "TTAATTAA",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "BbvAII",
      sequence = "ATCGAT",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Bsp50I",
      sequence = "CGCG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "NspSAII",
      sequence = "GGTNACC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BpuI",
      sequence = "GRGCYC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "Pae2kI",
      sequence = "AGATCT",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "AorI",
      sequence = "CCWGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Bsp98I",
      sequence = "GGATCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "CacI",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "Csp6I",
      sequence = "GTAC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "SscL1I",
      sequence = "GANTC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "HgiS22I",
      sequence = "CCSGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "PlaAII",
      sequence = "GTAC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "PdiI",
      sequence = "GCCGGC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "HgiI",
      sequence = "GRCGYC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BavBII",
      sequence = "GGNCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "CauII",
      sequence = "CCSGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "AcpI",
      sequence = "TTCGAA",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "CviJI",
      sequence = "RGCY",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "MunI",
      sequence = "CAATTG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BstZI",
      sequence = "CGGCCG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BspR7I",
      sequence = "CCTNAGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "AagI",
      sequence = "ATCGAT",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "MchAII",
      sequence = "GGCC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "XcyI",
      sequence = "CCCGGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BsiBI",
      sequence = "GATNNNNATC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "HauII",
      sequence = "TGGCCANNNNNNNNNNN",
      display = Map(),
      cutsBefore = 17),
    RestrictionEnzyme(
      name = "Srl32DII",
      sequence = "GAATTC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BspUI",
      sequence = "GCSGC",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "Csp68KII",
      sequence = "TTCGAA",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "NdeII",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "Bse634I",
      sequence = "RCCGGY",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Tvu2HI",
      sequence = "GGCC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "CviKI-1",
      sequence = "RGCY",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BspOVII",
      sequence = "ATCGAT",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Ppu21I",
      sequence = "YACGTR",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "NmuCI",
      sequence = "GTSAC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "BseCI",
      sequence = "ATCGAT",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "MluB2I",
      sequence = "TCGCGA",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "SauLPII",
      sequence = "CTCGAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "SauBMKI",
      sequence = "GCCGGC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "BspKT6I",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "Bsh1365I",
      sequence = "GATNNNNATC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "HgiGI",
      sequence = "GRCGYC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "AliI",
      sequence = "GGATCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "LlaBI",
      sequence = "CTRYAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BspLAIII",
      sequence = "AAGCTT",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BfrBI",
      sequence = "ATGCAT",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "BsePI",
      sequence = "GCGCGC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "ApiI",
      sequence = "CTGCAG",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "ErhB9II",
      sequence = "CCWWGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "AhyI",
      sequence = "CCCGGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "SceIII",
      sequence = "GCCGGC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BstPZ740I",
      sequence = "CTTAAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Bme18I",
      sequence = "GGWCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BspHI",
      sequence = "TCATGA",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "CviBI",
      sequence = "GANTC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BstZ17I",
      sequence = "GTATAC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "GceI",
      sequence = "CCGCGG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "PshBI",
      sequence = "ATTAAT",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Tsp32I",
      sequence = "TCGA",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Bse8I",
      sequence = "GATNNNNATC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "EclI",
      sequence = "CAGCTG",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "MfeI",
      sequence = "CAATTG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Tth111I",
      sequence = "GACNNNGTC",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "AccB1I",
      sequence = "GGYRCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "XmiI",
      sequence = "GTMKAC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "FnuAI",
      sequence = "GANTC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BssKI",
      sequence = "CCNGG",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "PbrTI",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "AvcI",
      sequence = "GGNCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "CcrI",
      sequence = "CTCGAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "AsuC2I",
      sequence = "CCSGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BsiEI",
      sequence = "CGRYCG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "AspS9I",
      sequence = "GGNCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "NspI",
      sequence = "RCATGY",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "HpyCH4V",
      sequence = "TGCA",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "VpaK11AI",
      sequence = "GGWCC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "AxyI",
      sequence = "CCTNAGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BspT104I",
      sequence = "TTCGAA",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BstHPI",
      sequence = "GTTAAC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "AatI",
      sequence = "AGGCCT",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "SepMI",
      sequence = "GATATC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "BsmSI",
      sequence = "CCWWGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Pae17kI",
      sequence = "CAGCTG",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "BbiII",
      sequence = "GRCGYC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BamNxI",
      sequence = "GGWCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Pfl8I",
      sequence = "GGATCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "AhaII",
      sequence = "GRCGYC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "TscI",
      sequence = "ACGT",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "Kpn378I",
      sequence = "CCGCGG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "Bsp67I",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "SdaI",
      sequence = "CCTGCAGG",
      display = Map(),
      cutsBefore = 6),
    RestrictionEnzyme(
      name = "TliI",
      sequence = "CTCGAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "CcoI",
      sequence = "GCCGGC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "MspNI",
      sequence = "GGWCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "MlaAI",
      sequence = "CTCGAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BoxI",
      sequence = "GACNNNNGTC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "AclNI",
      sequence = "ACTAGT",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "SpaHI",
      sequence = "GCATGC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "MauBI",
      sequence = "CGCGCGCG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Hpy99I",
      sequence = "CGWCG",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "AdeI",
      sequence = "CACNNNGTG",
      display = Map(),
      cutsBefore = 6),
    RestrictionEnzyme(
      name = "MalI",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BmeRI",
      sequence = "GACNNNNNGTC",
      display = Map(),
      cutsBefore = 6),
    RestrictionEnzyme(
      name = "SinI",
      sequence = "GGWCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Bim19II",
      sequence = "GGCC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Psp5II",
      sequence = "RGGWCCY",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "ErhB9I",
      sequence = "CGATCG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "AtsI",
      sequence = "GACNNNGTC",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "OliI",
      sequence = "CACNNNNGTG",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "BluI",
      sequence = "CTCGAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Asp718I",
      sequence = "GGTACC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Bsp123I",
      sequence = "CGCG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Bsp106I",
      sequence = "ATCGAT",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BbrI",
      sequence = "AAGCTT",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BloHI",
      sequence = "RGATCY",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "AccB2I",
      sequence = "RGCGCY",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "MlaI",
      sequence = "TTCGAA",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "AspBHII",
      sequence = "RGGWCCY",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "HhaI",
      sequence = "GCGC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "BfuCI",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "Lmu60I",
      sequence = "CCTNAGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "NspBII",
      sequence = "CMGCKG",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "PflMI",
      sequence = "CCANNNNNTGG",
      display = Map(),
      cutsBefore = 7),
    RestrictionEnzyme(
      name = "Srl5DI",
      sequence = "CTGCAG",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "Bse21I",
      sequence = "CCTNAGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Hin1I",
      sequence = "GRCGYC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "SgrBI",
      sequence = "CCGCGG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "Sse9I",
      sequence = "AATT",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "EsaBC4I",
      sequence = "GGCC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BspT107I",
      sequence = "GGYRCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Eco1831I",
      sequence = "CCSGG",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "AquI",
      sequence = "CYCGRG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Bac36I",
      sequence = "GGNCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "AleI",
      sequence = "CACNNNNGTG",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "TseFI",
      sequence = "GTSAC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "BspCI",
      sequence = "CGATCG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "Sag23I",
      sequence = "CTGCAG",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "PdmI",
      sequence = "GAANNNNTTC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "BstYI",
      sequence = "RGATCY",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BcnI",
      sequence = "CCSGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "CltI",
      sequence = "GGCC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BstRZ246I",
      sequence = "ATTTAAAT",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "MluI",
      sequence = "ACGCGT",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Ecl37kI",
      sequence = "CTGCAG",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "BssECI",
      sequence = "CCNNGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "CthII",
      sequence = "CCWGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "LlaG2I",
      sequence = "GCTAGC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BseSI",
      sequence = "GKGCMC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "Bme361I",
      sequence = "GGCC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Hpy178III",
      sequence = "TCNNGA",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "MamI",
      sequence = "GATNNNNATC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "Eco13kI",
      sequence = "CCNGG",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "EcoT38I",
      sequence = "GRGCYC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "NlaIII",
      sequence = "CATG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "NmeRI",
      sequence = "CAGCTG",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "ChaI",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "BfrI",
      sequence = "CTTAAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Ssp5230I",
      sequence = "GACGTC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "MaeII",
      sequence = "ACGT",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "CfuI",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BspAAI",
      sequence = "CTCGAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "SduI",
      sequence = "GDGCHC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "TspRI",
      sequence = "CASTGNN",
      display = Map(),
      cutsBefore = 7),
    RestrictionEnzyme(
      name = "Bpu14I",
      sequence = "TTCGAA",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BstFI",
      sequence = "AAGCTT",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "FaeI",
      sequence = "CATG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "HgiJII",
      sequence = "GRGCYC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "AvrBII",
      sequence = "CCTAGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Bsp143II",
      sequence = "RGCGCY",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "PgaI",
      sequence = "ATCGAT",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "FpsJI",
      sequence = "CCGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BlsI",
      sequence = "GCNGC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "Msp20I",
      sequence = "TGGCCA",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "MaeIII",
      sequence = "GTNAC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "HalI",
      sequence = "GAATTC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "PpuXI",
      sequence = "RGGWCCY",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "SspDI",
      sequence = "GGCGCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "NdaI",
      sequence = "GGCGCC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "NopI",
      sequence = "GTCGAC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "PanI",
      sequence = "CTCGAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BceBI",
      sequence = "CGCG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "DsaIV",
      sequence = "GGWCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BmrFI",
      sequence = "CCNGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "DsaIII",
      sequence = "RGATCY",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BstPAI",
      sequence = "GACNNNNGTC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "Bli41I",
      sequence = "ATCGAT",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Sth117I",
      sequence = "CCWGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Ecl18kI",
      sequence = "CCNGG",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "AccI",
      sequence = "GTMKAC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Tsp45I",
      sequence = "GTSAC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "SseAI",
      sequence = "GGCGCC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "ZrmI",
      sequence = "AGTACT",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "YenI",
      sequence = "CTGCAG",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "BbvAIII",
      sequence = "TCCGGA",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "RtrI",
      sequence = "GTCGAC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "CbrI",
      sequence = "CCWGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Asi372I",
      sequence = "ATGCAT",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "TspBI",
      sequence = "CCRYGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "AscI",
      sequence = "GGCGCGCC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "SflI",
      sequence = "CTGCAG",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "BthAI",
      sequence = "GGWCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "LspI",
      sequence = "TTCGAA",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "SspI",
      sequence = "AATATT",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "BsuRI",
      sequence = "GGCC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "MspR9I",
      sequence = "CCNGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "SstII",
      sequence = "CCGCGG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "CvnI",
      sequence = "CCTNAGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "AhdI",
      sequence = "GACNNNNNGTC",
      display = Map(),
      cutsBefore = 6),
    RestrictionEnzyme(
      name = "BspAAII",
      sequence = "TCTAGA",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BtkII",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "BbvAI",
      sequence = "GAANNNNTTC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "BspLU11I",
      sequence = "ACATGT",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "SfiI",
      sequence = "GGCCNNNNNGGCC",
      display = Map(),
      cutsBefore = 8),
    RestrictionEnzyme(
      name = "MroI",
      sequence = "TCCGGA",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BstIZ316I",
      sequence = "CACNNNGTG",
      display = Map(),
      cutsBefore = 6),
    RestrictionEnzyme(
      name = "BspM39I",
      sequence = "CAGCTG",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "Bsp63I",
      sequence = "CTGCAG",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "BspOI",
      sequence = "GCTAGC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "ZhoI",
      sequence = "ATCGAT",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Pde12I",
      sequence = "GGNCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Msp17I",
      sequence = "GRCGYC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "AspEI",
      sequence = "GACNNNNNGTC",
      display = Map(),
      cutsBefore = 6),
    RestrictionEnzyme(
      name = "SdiI",
      sequence = "GGCCNNNNNGGCC",
      display = Map(),
      cutsBefore = 8),
    RestrictionEnzyme(
      name = "McrI",
      sequence = "CGRYCG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "BstI",
      sequence = "GGATCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Sbo13I",
      sequence = "TCGCGA",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "AbsI",
      sequence = "CCTCGAGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Cfr13I",
      sequence = "GGNCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "FmuI",
      sequence = "GGNCC",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "SrfI",
      sequence = "GCCCGGGC",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "BseLI",
      sequence = "CCNNNNNNNGG",
      display = Map(),
      cutsBefore = 7),
    RestrictionEnzyme(
      name = "BnaI",
      sequence = "GGATCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Pae14kI",
      sequence = "CCGCGG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "HpyCH4IV",
      sequence = "ACGT",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BstAUI",
      sequence = "TGTACA",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Hpy8I",
      sequence = "GTNNAC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "BfuAII",
      sequence = "GCATGC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "FgoI",
      sequence = "CTAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Bsu15I",
      sequence = "ATCGAT",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "AfiI",
      sequence = "CCNNNNNNNGG",
      display = Map(),
      cutsBefore = 7),
    RestrictionEnzyme(
      name = "PaeQI",
      sequence = "CCGCGG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "MluCI",
      sequence = "AATT",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "DseDI",
      sequence = "GACNNNNNNGTC",
      display = Map(),
      cutsBefore = 7),
    RestrictionEnzyme(
      name = "Bsu1854I",
      sequence = "GRGCYC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "Tru9I",
      sequence = "TTAA",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "NcoI",
      sequence = "CCATGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "SmaI",
      sequence = "CCCGGG",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "BptI",
      sequence = "CCWGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "SsoII",
      sequence = "CCNGG",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "AsuNHI",
      sequence = "GCTAGC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "LcaI",
      sequence = "ATCGAT",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "NsiCI",
      sequence = "GATATC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "SmlI",
      sequence = "CTYRAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Bst2I",
      sequence = "CCWGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BstHZ55I",
      sequence = "CCANNNNNNTGG",
      display = Map(),
      cutsBefore = 8),
    RestrictionEnzyme(
      name = "CviAII",
      sequence = "CATG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BceCI",
      sequence = "GCNNNNNNNGC",
      display = Map(),
      cutsBefore = 7),
    RestrictionEnzyme(
      name = "BsiXI",
      sequence = "ATCGAT",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BstX2I",
      sequence = "RGATCY",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "MgoI",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "Sau3239I",
      sequence = "CTCGAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "FatI",
      sequence = "CATG",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "ScrFI",
      sequence = "CCNGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "HgiBI",
      sequence = "GGWCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Nsp7121I",
      sequence = "GGNCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Bpu95I",
      sequence = "CGCG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "HsuI",
      sequence = "AAGCTT",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "NotI",
      sequence = "GCGGCCGC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "CviRII",
      sequence = "GTAC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "PpeI",
      sequence = "GGGCCC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "Taq52I",
      sequence = "GCWGC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "FnuDII",
      sequence = "CGCG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Psp03I",
      sequence = "GGWCC",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "CelII",
      sequence = "GCTNAGC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "EspI",
      sequence = "GCTNAGC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "AlwNI",
      sequence = "CAGNNNCTG",
      display = Map(),
      cutsBefore = 6),
    RestrictionEnzyme(
      name = "BsrGI",
      sequence = "TGTACA",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BsiMI",
      sequence = "TCCGGA",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "AhaI",
      sequence = "CCSGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "CauB3I",
      sequence = "TCCGGA",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "SlaI",
      sequence = "CTCGAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BgiI",
      sequence = "GACNNNGTC",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "SmiI",
      sequence = "ATTTAAAT",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "NspLKI",
      sequence = "GGCC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "AvaII",
      sequence = "GGWCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "SepI",
      sequence = "ATGCAT",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "Zsp2I",
      sequence = "ATGCAT",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "SpuI",
      sequence = "CCGCGG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "PaeI",
      sequence = "GCATGC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "SelI",
      sequence = "CGCG",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "BstMCI",
      sequence = "CGRYCG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "Tsp8EI",
      sequence = "GCCNNNNNGGC",
      display = Map(),
      cutsBefore = 7),
    RestrictionEnzyme(
      name = "NspV",
      sequence = "TTCGAA",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "HhaII",
      sequence = "GANTC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "TfiI",
      sequence = "GAWTC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BspTI",
      sequence = "CTTAAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Bme12I",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "Bme1390I",
      sequence = "CCNGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "HgiCIII",
      sequence = "GTCGAC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "FaiI",
      sequence = "YATR",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BspLAI",
      sequence = "GCGC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "SstI",
      sequence = "GAGCTC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "RgaI",
      sequence = "GCGATCGC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "BamHI",
      sequence = "GGATCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "DriI",
      sequence = "GACNNNNNGTC",
      display = Map(),
      cutsBefore = 6),
    RestrictionEnzyme(
      name = "FnuDIII",
      sequence = "GCGC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "BscFI",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "BsrBRI",
      sequence = "GATNNNNATC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "Afa22MI",
      sequence = "CGATCG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "SauNI",
      sequence = "GCCGGC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "BmgT120I",
      sequence = "GGNCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Eco88I",
      sequence = "CYCGRG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BsaHI",
      sequence = "GRCGYC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "AoxI",
      sequence = "GGCC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "CfrBI",
      sequence = "CCWWGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Mae7806III",
      sequence = "GGWCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "AccIII",
      sequence = "TCCGGA",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "SsoI",
      sequence = "GAATTC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "AspA2I",
      sequence = "CCTAGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Pfl23II",
      sequence = "CGTACG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BlpI",
      sequence = "GCTNAGC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BamGI",
      sequence = "CAGCTG",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "SacI",
      sequence = "GAGCTC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "Bce751I",
      sequence = "GGATCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "KflI",
      sequence = "GGGWCCC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Pde137I",
      sequence = "CCGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BcuAI",
      sequence = "GGWCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "XcaI",
      sequence = "GTATAC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "BsiWI",
      sequence = "CGTACG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "SspRFI",
      sequence = "TTCGAA",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BstENI",
      sequence = "CCTNNNNNAGG",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "AocI",
      sequence = "CCTNAGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BstEII",
      sequence = "GGTNACC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "StrI",
      sequence = "CTCGAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BspA2I",
      sequence = "CCTAGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "SmoLI",
      sequence = "CCGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "SphI",
      sequence = "GCATGC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "Ple19I",
      sequence = "CGATCG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "EcaI",
      sequence = "GGTNACC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "SacII",
      sequence = "CCGCGG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "MchAI",
      sequence = "GCGGCCGC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "ErhI",
      sequence = "CCWWGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "KspI",
      sequence = "CCGCGG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "Fsp4HI",
      sequence = "GCNGC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "AccII",
      sequence = "CGCG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "HgiCII",
      sequence = "GGWCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "HsoI",
      sequence = "GCGC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Pun14627II",
      sequence = "CAGCTG",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "MhaAI",
      sequence = "CTGCAG",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "BstJZ301I",
      sequence = "CTNAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Kaz48kI",
      sequence = "RGGNCCY",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "MaeK81II",
      sequence = "GGNCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Ecl136II",
      sequence = "GAGCTC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "BanII",
      sequence = "GRGCYC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "CbeI",
      sequence = "GGCC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "KroI",
      sequence = "GCCGGC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BstSFI",
      sequence = "CTRYAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "FriOI",
      sequence = "GRGCYC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "MchI",
      sequence = "GGCGCC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "SgfI",
      sequence = "GCGATCGC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "BstAFI",
      sequence = "CTTAAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BseAI",
      sequence = "TCCGGA",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "PalAI",
      sequence = "GGCGCGCC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "SshAI",
      sequence = "CCTNAGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "TaaI",
      sequence = "ACNGT",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "ItaI",
      sequence = "GCNGC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BstSCI",
      sequence = "CCNGG",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "Eco53kI",
      sequence = "GAGCTC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "Eco1524I",
      sequence = "AGGCCT",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "BstT7I",
      sequence = "TGATCA",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "ZraI",
      sequence = "GACGTC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "SsiAI",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "Psp23I",
      sequence = "CTGCAG",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "NsiI",
      sequence = "ATGCAT",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "PciI",
      sequence = "ACATGT",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "SgeI",
      sequence = "CNNGNNNNNNNNN",
      display = Map(),
      cutsBefore = 13),
    RestrictionEnzyme(
      name = "PfaAII",
      sequence = "CATATG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "HjaI",
      sequence = "GATATC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "RcaI",
      sequence = "TCATGA",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BfmI",
      sequence = "CTRYAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "MspA1I",
      sequence = "CMGCKG",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "NblI",
      sequence = "CGATCG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "NciI",
      sequence = "CCSGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Pae18kI",
      sequence = "AGATCT",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BshGI",
      sequence = "CCWGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BavCI",
      sequence = "ATCGAT",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Bpu1102I",
      sequence = "GCTNAGC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BepI",
      sequence = "CGCG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "AceI",
      sequence = "GCWGC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Bsp13I",
      sequence = "TCCGGA",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Pru2I",
      sequence = "GGCC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "PspALI",
      sequence = "CCCGGG",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "HpyF10VI",
      sequence = "GCNNNNNNNGC",
      display = Map(),
      cutsBefore = 7),
    RestrictionEnzyme(
      name = "SuiI",
      sequence = "GCWGC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Bsp153AI",
      sequence = "CAGCTG",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "BscI",
      sequence = "ATCGAT",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BsoBI",
      sequence = "CYCGRG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BcoI",
      sequence = "CYCGRG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "DraII",
      sequence = "RGGNCCY",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "SgrAI",
      sequence = "CRCCGGYG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Pme55I",
      sequence = "AGGCCT",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "MroNI",
      sequence = "GCCGGC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "AosI",
      sequence = "TGCGCA",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "HgiHIII",
      sequence = "GGWCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "SteI",
      sequence = "AGGCCT",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "VspI",
      sequence = "ATTAAT",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "SleI",
      sequence = "CCWGG",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "CfrJ4I",
      sequence = "CCCGGG",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "PscI",
      sequence = "ACATGT",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "ErpI",
      sequence = "GGWCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Bsc107I",
      sequence = "CCNNNNNNNGG",
      display = Map(),
      cutsBefore = 7),
    RestrictionEnzyme(
      name = "MhlI",
      sequence = "GDGCHC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "CflI",
      sequence = "CTGCAG",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "Eco147I",
      sequence = "AGGCCT",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "Bsp211I",
      sequence = "GGCC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "ScaI",
      sequence = "AGTACT",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "Bsp1894I",
      sequence = "GGNCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "HinfI",
      sequence = "GANTC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "HpyCI",
      sequence = "GATATC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "UnbI",
      sequence = "GGNCC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "SfuI",
      sequence = "TTCGAA",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "CviAI",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "AbrI",
      sequence = "CTCGAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BsuTUI",
      sequence = "ATCGAT",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Bst19II",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "Bsp119I",
      sequence = "TTCGAA",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "TaiI",
      sequence = "ACGT",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "Ppu111I",
      sequence = "GAATTC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "CcuI",
      sequence = "GGNCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BspFI",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "Bsm6I",
      sequence = "GWGCWC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "BspJI",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "BsiLI",
      sequence = "CCWGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BspBI",
      sequence = "CTGCAG",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "Srl55DI",
      sequence = "GAATTC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "RrhJ1I",
      sequence = "GCCGGC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "ThaI",
      sequence = "CGCG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "PspPPI",
      sequence = "RGGWCCY",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "NspIII",
      sequence = "CYCGRG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "PfoI",
      sequence = "TCCNGGA",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "CcyI",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "Bsp120I",
      sequence = "GGGCCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Psu161I",
      sequence = "CGATCG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "XmaIII",
      sequence = "CGGCCG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "PaePI",
      sequence = "CTGCAG",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "BspBII",
      sequence = "GGNCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Bsh1285I",
      sequence = "CGRYCG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "MscI",
      sequence = "TGGCCA",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "BcmI",
      sequence = "ATCGAT",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Bst100I",
      sequence = "CCWGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BstEZ359I",
      sequence = "GTTAAC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "BshNI",
      sequence = "GGYRCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "PpaAII",
      sequence = "TCGA",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BsnI",
      sequence = "GGCC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BssMI",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "NarI",
      sequence = "GGCGCC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "HinP1I",
      sequence = "GCGC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Bsp19I",
      sequence = "CCATGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BbtI",
      sequence = "GCGC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "RalF40I",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "SauLPI",
      sequence = "GCCGGC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "Uba153AI",
      sequence = "CAGCTG",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "SauHPI",
      sequence = "GCCGGC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "SbfI",
      sequence = "CCTGCAGG",
      display = Map(),
      cutsBefore = 6),
    RestrictionEnzyme(
      name = "FblI",
      sequence = "GTMKAC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "RmaI",
      sequence = "CTAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "MxaI",
      sequence = "GAGCTC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "PspXI",
      sequence = "VCTCGAGB",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "AglI",
      sequence = "CCWGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "SniI",
      sequence = "CCWGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "PaeAI",
      sequence = "CCGCGG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "Asp10HI",
      sequence = "TTCGAA",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "MspSWI",
      sequence = "ATTTAAAT",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "AauI",
      sequence = "TGTACA",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "EcoO65I",
      sequence = "GGTNACC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Aor13HI",
      sequence = "TCCGGA",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Bse15I",
      sequence = "CYCGRG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BtkI",
      sequence = "CGCG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BspLS2I",
      sequence = "GDGCHC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "BstNZ169I",
      sequence = "ATCGAT",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "AspAI",
      sequence = "GGTNACC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "CciNI",
      sequence = "GCGGCCGC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Bsu23I",
      sequence = "TCCGGA",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BsaAI",
      sequence = "YACGTR",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "HacI",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "Bse118I",
      sequence = "RCCGGY",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BetI",
      sequence = "WCCGGW",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BsuBI",
      sequence = "CTGCAG",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "BseX3I",
      sequence = "CGGCCG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BsmRI",
      sequence = "TGTACA",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Eco47I",
      sequence = "GGWCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "HgiDII",
      sequence = "GTCGAC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Rtr63I",
      sequence = "GTCGAC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Psp124BI",
      sequence = "GAGCTC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "MstII",
      sequence = "CCTNAGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "FspI",
      sequence = "TGCGCA",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "MthZI",
      sequence = "CTAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BsiKI",
      sequence = "GGTNACC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Psp6I",
      sequence = "CCWGG",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "AsiGI",
      sequence = "ACCGGT",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "AvoI",
      sequence = "RCATGY",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "EcoHK31I",
      sequence = "YGGCCR",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Bsa29I",
      sequence = "ATCGAT",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "SpoI",
      sequence = "TCGCGA",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "Rsr2I",
      sequence = "CGGWCCG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "PsuI",
      sequence = "RGATCY",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "SolI",
      sequence = "GGATCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "CstI",
      sequence = "CTGCAG",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "Eam1105I",
      sequence = "GACNNNNNGTC",
      display = Map(),
      cutsBefore = 6),
    RestrictionEnzyme(
      name = "BsaJI",
      sequence = "CCNNGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "AluI",
      sequence = "AGCT",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "NdeI",
      sequence = "CATATG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Bco27I",
      sequence = "CCGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "LpnI",
      sequence = "RGCGCY",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "BstT10I",
      sequence = "GGTNACC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "PinBI",
      sequence = "ATGCAT",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "SexAI",
      sequence = "ACCWGGT",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "NcrI",
      sequence = "AGATCT",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BspXI",
      sequence = "ATCGAT",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "NgoMIV",
      sequence = "GCCGGC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "MreI",
      sequence = "CGCCGGCG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Acc16I",
      sequence = "TGCGCA",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "GstI",
      sequence = "GGATCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "PaeBI",
      sequence = "CCCGGG",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "NruGI",
      sequence = "GACNNNNNGTC",
      display = Map(),
      cutsBefore = 6),
    RestrictionEnzyme(
      name = "Fsp1604I",
      sequence = "CCWGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BstSWI",
      sequence = "ATTTAAAT",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "CauI",
      sequence = "GGWCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "PabI",
      sequence = "GTAC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "MssI",
      sequence = "GTTTAAAC",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "CfrI",
      sequence = "YGGCCR",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Sru30DI",
      sequence = "AGGCCT",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "NmeCI",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "PvuI",
      sequence = "CGATCG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "AspI",
      sequence = "GACNNNGTC",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "PmeI",
      sequence = "GTTTAAAC",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "FbrI",
      sequence = "GCNGC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Bsu1532I",
      sequence = "CGCG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Ssp4800I",
      sequence = "TGTACA",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Asp700I",
      sequence = "GAANNNNTTC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "PpaAI",
      sequence = "TTCGAA",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "PluTI",
      sequence = "GGCGCC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "EscI",
      sequence = "CTCGAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BspFNI",
      sequence = "CGCG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "GlaI",
      sequence = "GCGC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "PspAI",
      sequence = "CCCGGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "SrlI",
      sequence = "GCCGGC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "HgiEI",
      sequence = "GGWCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "EagMI",
      sequence = "GGWCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "LplI",
      sequence = "ATCGAT",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Hpy188I",
      sequence = "TCNGA",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "HindII",
      sequence = "GTYRAC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "Eco64I",
      sequence = "GGYRCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "PalI",
      sequence = "GGCC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BstSLI",
      sequence = "GKGCMC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "SfeI",
      sequence = "CTRYAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Aor51HI",
      sequence = "AGCGCT",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "VpaK11BI",
      sequence = "GGWCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Bli86I",
      sequence = "ATCGAT",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "XceI",
      sequence = "RCATGY",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "Alw44I",
      sequence = "GTGCAC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "DinI",
      sequence = "GGCGCC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "Sol10179I",
      sequence = "CTCGAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BspKMI",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "CfoI",
      sequence = "GCGC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "XorII",
      sequence = "CGATCG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "Bst40I",
      sequence = "CCGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Hin1II",
      sequence = "CATG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "BfoI",
      sequence = "RGCGCY",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "MspB4I",
      sequence = "GGYRCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "EaeI",
      sequence = "YGGCCR",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "HpyCH4III",
      sequence = "ACNGT",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "AocII",
      sequence = "GDGCHC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "AaaI",
      sequence = "CGGCCG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Eci125I",
      sequence = "GGTNACC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "McaTI",
      sequence = "GCGCGC",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "BmtI",
      sequence = "GCTAGC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "MfoAI",
      sequence = "GGCC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Cac8I",
      sequence = "GCNNGC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "Ecl2zI",
      sequence = "CTGCAG",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "BfaI",
      sequence = "CTAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BstBZ153I",
      sequence = "GCGCGC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "PfeI",
      sequence = "GAWTC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "TasI",
      sequence = "AATT",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "PsiI",
      sequence = "TTATAA",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "TaqI",
      sequence = "TCGA",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "SenPT16I",
      sequence = "CGGCCG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "DsaI",
      sequence = "CCRYGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "NphI",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "Uba4009I",
      sequence = "GGATCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "MvaI",
      sequence = "CCWGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Sse1825I",
      sequence = "GGGWCCC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "EaeAI",
      sequence = "CCCGGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "HincII",
      sequence = "GTYRAC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "Esp4I",
      sequence = "CTTAAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Kpn49kI",
      sequence = "GAATTC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "FseI",
      sequence = "GGCCGGCC",
      display = Map(),
      cutsBefore = 6),
    RestrictionEnzyme(
      name = "FsiI",
      sequence = "RAATTY",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Ppu10I",
      sequence = "ATGCAT",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "TneDI",
      sequence = "CGCG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BpuAmI",
      sequence = "GAGCTC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "FssI",
      sequence = "GGWCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "PaeR7I",
      sequence = "CTCGAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "AvaI",
      sequence = "CYCGRG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "ApaI",
      sequence = "GGGCCC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "SauI",
      sequence = "CCTNAGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "MavI",
      sequence = "CTCGAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Hpy188III",
      sequence = "TCNNGA",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "HpaII",
      sequence = "CCGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "CchI",
      sequence = "CTAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "PshAI",
      sequence = "GACNNNNGTC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "BstPI",
      sequence = "GGTNACC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BstAPI",
      sequence = "GCANNNNNTGC",
      display = Map(),
      cutsBefore = 7),
    RestrictionEnzyme(
      name = "PspOMI",
      sequence = "GGGCCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "AseII",
      sequence = "CCSGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "SmuEI",
      sequence = "GGWCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "SexBI",
      sequence = "CCGCGG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "ParI",
      sequence = "TGATCA",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Sse8647I",
      sequence = "AGGWCCT",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BsuFI",
      sequence = "CCGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Rrh4273I",
      sequence = "GTCGAC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "SacNI",
      sequence = "GRGCYC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "BsiHKCI",
      sequence = "CYCGRG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "PfaAI",
      sequence = "GGYRCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Bal228I",
      sequence = "GGNCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "EclRI",
      sequence = "CCCGGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Cfr42I",
      sequence = "CCGCGG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "Bst22I",
      sequence = "CCNNNNNNNGG",
      display = Map(),
      cutsBefore = 7),
    RestrictionEnzyme(
      name = "CfrA4I",
      sequence = "CTGCAG",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "Hpy166II",
      sequence = "GTNNAC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "Sth134I",
      sequence = "CCGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BsiQI",
      sequence = "TGATCA",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Bst38I",
      sequence = "CCWGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "RsrII",
      sequence = "CGGWCCG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BsiZI",
      sequence = "GGNCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BglI",
      sequence = "GCCNNNNNGGC",
      display = Map(),
      cutsBefore = 7),
    RestrictionEnzyme(
      name = "BspZEI",
      sequence = "ATCGAT",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "FdiI",
      sequence = "GGWCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "PspEI",
      sequence = "GGTNACC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Bsp105I",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "BssT1I",
      sequence = "CCWWGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BspMKI",
      sequence = "GTCGAC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BssNAI",
      sequence = "GTATAC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "RflFII",
      sequence = "AGTACT",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "Sth302I",
      sequence = "CCWGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "RspLKII",
      sequence = "GGATCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "NgoAIV",
      sequence = "GCCGGC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "HpyBI",
      sequence = "GTAC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "AspLEI",
      sequence = "GCGC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "PpuMI",
      sequence = "RGGWCCY",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BalI",
      sequence = "TGGCCA",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "BtgI",
      sequence = "CCRYGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "MaeI",
      sequence = "CTAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "MseI",
      sequence = "TTAA",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "TelI",
      sequence = "GACNNNGTC",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "NlaIV",
      sequence = "GGNNCC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "Msp67I",
      sequence = "CCNGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "SurI",
      sequence = "GGATCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BglII",
      sequence = "AGATCT",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BstFNI",
      sequence = "CGCG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BstH2I",
      sequence = "RGCGCY",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "BstVI",
      sequence = "CTCGAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BstHHI",
      sequence = "GCGC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "BclI",
      sequence = "TGATCA",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Kzo49I",
      sequence = "GGWCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Bsp1407I",
      sequence = "TGTACA",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "NspSAIV",
      sequence = "GGATCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Bse16I",
      sequence = "CCWGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "StyI",
      sequence = "CCWWGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "AcrII",
      sequence = "GGTNACC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Bme216I",
      sequence = "GGWCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "TauI",
      sequence = "GCSGC",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "PsuAI",
      sequence = "YACGTR",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "Sau96I",
      sequence = "GGNCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BstSI",
      sequence = "CYCGRG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BinSII",
      sequence = "GGCGCC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "BseT10I",
      sequence = "GGTNACC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BpuMI",
      sequence = "CCSGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "MroXI",
      sequence = "GAANNNNTTC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "AasI",
      sequence = "GACNNNNNNGTC",
      display = Map(),
      cutsBefore = 7),
    RestrictionEnzyme(
      name = "AcsI",
      sequence = "RAATTY",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "KspAI",
      sequence = "GTTAAC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "VneI",
      sequence = "GTGCAC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BsaOI",
      sequence = "CGRYCG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "HinJCI",
      sequence = "GTYRAC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "BimI",
      sequence = "TTCGAA",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Ksp22I",
      sequence = "TGATCA",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BanAI",
      sequence = "GGCC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "PpuAI",
      sequence = "CGTACG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "RseI",
      sequence = "CAYNNNNRTG",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "BseBI",
      sequence = "CCWGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "CbiI",
      sequence = "TTCGAA",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "CsiBI",
      sequence = "GCGGCCGC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "TseI",
      sequence = "GCWGC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BsiYI",
      sequence = "CCNNNNNNNGG",
      display = Map(),
      cutsBefore = 7),
    RestrictionEnzyme(
      name = "Bsp1720I",
      sequence = "GCTNAGC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BseT9I",
      sequence = "GGTNACC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "SspBI",
      sequence = "TGTACA",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "SruI",
      sequence = "TTTAAA",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "BpvUI",
      sequence = "CGATCG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "BsrFI",
      sequence = "RCCGGY",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "EcoNI",
      sequence = "CCTNNNNNAGG",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "ApaCI",
      sequence = "GGATCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "SuaI",
      sequence = "GGCC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Bbi24I",
      sequence = "ACGCGT",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BbvBI",
      sequence = "GGYRCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BthEI",
      sequence = "CCWGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "HapII",
      sequence = "CCGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "ApaLI",
      sequence = "GTGCAC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "PspPI",
      sequence = "GGNCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Csp68KIII",
      sequence = "ATGCAT",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "HspAI",
      sequence = "GCGC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Kpn2kI",
      sequence = "CCNGG",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "RruI",
      sequence = "TCGCGA",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "HgiAI",
      sequence = "GWGCWC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "EsaBC3I",
      sequence = "TCGA",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "SgrDI",
      sequence = "CGTCGACG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BsoFI",
      sequence = "GCNGC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "GdiI",
      sequence = "AGGCCT",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "BthP35I",
      sequence = "CTRYAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Srl56DI",
      sequence = "CTRYAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "MvrI",
      sequence = "CGATCG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "AgsI",
      sequence = "TTSAA",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "SmiMI",
      sequence = "CAYNNNNRTG",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "BisI",
      sequence = "GCNGC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BshKI",
      sequence = "GGNCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Bst4CI",
      sequence = "ACNGT",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "XcpI",
      sequence = "CTGCAG",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "PagI",
      sequence = "TCATGA",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Bbv12I",
      sequence = "GWGCWC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "Csp68KVI",
      sequence = "CGCG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "PaeHI",
      sequence = "GRGCYC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "DmaI",
      sequence = "CAGCTG",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "BbrPI",
      sequence = "CACGTG",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "AjnI",
      sequence = "CCWGG",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "BstC8I",
      sequence = "GCNNGC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "Kzo9I",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "Mph1103I",
      sequence = "ATGCAT",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "Eco52I",
      sequence = "CGGCCG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Eco27kI",
      sequence = "CYCGRG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BliHKI",
      sequence = "CCTNAGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BspJII",
      sequence = "ATCGAT",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BthDI",
      sequence = "CCWGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BspDI",
      sequence = "ATCGAT",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "SspAI",
      sequence = "CCWGG",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "TatI",
      sequence = "WGTACW",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "PssI",
      sequence = "RGGNCCY",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "Tru201I",
      sequence = "RGATCY",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Sth302II",
      sequence = "CCGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Eco130I",
      sequence = "CCWWGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "SfaAI",
      sequence = "GCGATCGC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "Nsp29132II",
      sequence = "GGATCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Mlu23I",
      sequence = "GGATCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Van91I",
      sequence = "CCANNNNNTGG",
      display = Map(),
      cutsBefore = 7),
    RestrictionEnzyme(
      name = "BspANI",
      sequence = "GGCC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "CelI",
      sequence = "GGATCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "MboI",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "BstSNI",
      sequence = "TACGTA",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "CviKI",
      sequence = "RGCY",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BseJI",
      sequence = "GATNNNNATC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "NsbI",
      sequence = "TGCGCA",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "RspXI",
      sequence = "TCATGA",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Bsh1236I",
      sequence = "CGCG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Tsp509I",
      sequence = "AATT",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "Mlu31I",
      sequence = "TGGCCA",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "StyD4I",
      sequence = "CCNGG",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "CspAI",
      sequence = "ACCGGT",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "NspSAI",
      sequence = "CYCGRG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "SdeAII",
      sequence = "CCNGG",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "TspMI",
      sequence = "CCCGGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Bsp143I",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "DdeI",
      sequence = "CTNAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Pae5kI",
      sequence = "CCGCGG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "BstNI",
      sequence = "CCWGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Bpa34I",
      sequence = "AGTACT",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "XspI",
      sequence = "CTAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Bst2UI",
      sequence = "CCWGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "PmaCI",
      sequence = "CACGTG",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "SecI",
      sequence = "CCNNGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "NspHI",
      sequence = "RCATGY",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "BsiCI",
      sequence = "TTCGAA",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "PamDI",
      sequence = "CCATGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "CpfI",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "MwoI",
      sequence = "GCNNNNNNNGC",
      display = Map(),
      cutsBefore = 7),
    RestrictionEnzyme(
      name = "Bsu36I",
      sequence = "CCTNAGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "CciI",
      sequence = "TCATGA",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "NlaII",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "PamI",
      sequence = "TGCGCA",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "BaeGI",
      sequence = "GKGCMC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "NgoPII",
      sequence = "GGCC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "CviQI",
      sequence = "GTAC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Bci29I",
      sequence = "ATCGAT",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Sag16I",
      sequence = "CTGCAG",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "Hin2I",
      sequence = "CCGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BssHI",
      sequence = "CTCGAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BmpI",
      sequence = "GGWCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "SgsI",
      sequence = "GGCGCGCC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "NunII",
      sequence = "GGCGCC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Fnu4HI",
      sequence = "GCNGC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Hsp92II",
      sequence = "CATG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "BstKTI",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "BsaBI",
      sequence = "GATNNNNATC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "Bce22I",
      sequence = "GGNCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Eco24I",
      sequence = "GRGCYC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "RspLKI",
      sequence = "GCATGC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "CaiI",
      sequence = "CAGNNNCTG",
      display = Map(),
      cutsBefore = 6),
    RestrictionEnzyme(
      name = "PasI",
      sequence = "CCCWGGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Bfi89I",
      sequence = "YGGCCR",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "HaeIII",
      sequence = "GGCC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "PvuII",
      sequence = "CAGCTG",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "Vha464I",
      sequence = "CTTAAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "NruI",
      sequence = "TCGCGA",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "Sbi68I",
      sequence = "CTCGAG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BstMWI",
      sequence = "GCNNNNNNNGC",
      display = Map(),
      cutsBefore = 7),
    RestrictionEnzyme(
      name = "HgiCI",
      sequence = "GGYRCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Slu1777I",
      sequence = "GCCGGC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "Ama87I",
      sequence = "CYCGRG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BstM6I",
      sequence = "CCWGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "PteI",
      sequence = "GCGCGC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "FdiII",
      sequence = "TGCGCA",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "Eco255I",
      sequence = "AGTACT",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "EcoHI",
      sequence = "CCSGG",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "BstUI",
      sequence = "CGCG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "AsuIII",
      sequence = "GRCGYC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "CsiI",
      sequence = "ACCWGGT",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "PspLI",
      sequence = "CGTACG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "HgiHI",
      sequence = "GGYRCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BlnI",
      sequence = "CCTAGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "AceII",
      sequence = "GCTAGC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "SnaBI",
      sequence = "TACGTA",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "EcoO128I",
      sequence = "GGTNACC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "AsuI",
      sequence = "GGNCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Sst12I",
      sequence = "CTGCAG",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "HindIII",
      sequence = "AAGCTT",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BstENII",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "Sse232I",
      sequence = "CGCCGGCG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "BsrAI",
      sequence = "GGWCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Tsp4CI",
      sequence = "ACNGT",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "SexCI",
      sequence = "CCGCGG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "NgoPIII",
      sequence = "CCGCGG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "HalII",
      sequence = "CTGCAG",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "RigI",
      sequence = "GGCCGGCC",
      display = Map(),
      cutsBefore = 6),
    RestrictionEnzyme(
      name = "BspOVI",
      sequence = "GACNNNNNGTC",
      display = Map(),
      cutsBefore = 6),
    RestrictionEnzyme(
      name = "UbaM39I",
      sequence = "CAGCTG",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "MspV281I",
      sequence = "GWGCWC",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "PstNI",
      sequence = "CAGNNNCTG",
      display = Map(),
      cutsBefore = 6),
    RestrictionEnzyme(
      name = "HgiDI",
      sequence = "GRCGYC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "CboI",
      sequence = "CCGG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "AseI",
      sequence = "ATTAAT",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "AbaI",
      sequence = "TGATCA",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "SsbI",
      sequence = "AAGCTT",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Csp45I",
      sequence = "TTCGAA",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Eco81I",
      sequence = "CCTNAGG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Sse8387I",
      sequence = "CCTGCAGG",
      display = Map(),
      cutsBefore = 6),
    RestrictionEnzyme(
      name = "BmeT110I",
      sequence = "CYCGRG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "MslI",
      sequence = "CAYNNNNRTG",
      display = Map(),
      cutsBefore = 5),
    RestrictionEnzyme(
      name = "SnoI",
      sequence = "GTGCAC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BteI",
      sequence = "GGCC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "AspNI",
      sequence = "GGNNCC",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "AspMDI",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "AspMI",
      sequence = "AGGCCT",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "PmlI",
      sequence = "CACGTG",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "DpnII",
      sequence = "GATC",
      display = Map(),
      cutsBefore = 0),
    RestrictionEnzyme(
      name = "BcoAI",
      sequence = "CACGTG",
      display = Map(),
      cutsBefore = 3),
    RestrictionEnzyme(
      name = "Psp1009I",
      sequence = "GCCNNNNNGGC",
      display = Map(),
      cutsBefore = 7),
    RestrictionEnzyme(
      name = "MabI",
      sequence = "ACCWGGT",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "AcpII",
      sequence = "CCANNNNNTGG",
      display = Map(),
      cutsBefore = 7),
    RestrictionEnzyme(
      name = "Cfr10I",
      sequence = "RCCGGY",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "FauBII",
      sequence = "CGCG",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Uur960I",
      sequence = "GCNGC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "PstNHI",
      sequence = "GCTAGC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "PsyI",
      sequence = "GACNNNGTC",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "GalI",
      sequence = "CCGCGG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "Afa16RI",
      sequence = "CGATCG",
      display = Map(),
      cutsBefore = 4),
    RestrictionEnzyme(
      name = "BanIII",
      sequence = "ATCGAT",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "Bse64I",
      sequence = "GGTNACC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Bsp6I",
      sequence = "GCNGC",
      display = Map(),
      cutsBefore = 2),
    RestrictionEnzyme(
      name = "OfoI",
      sequence = "CYCGRG",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "Bsu54I",
      sequence = "GGNCC",
      display = Map(),
      cutsBefore = 1),
    RestrictionEnzyme(
      name = "BstACI",
      sequence = "GRCGYC",
      display = Map(),
      cutsBefore = 2)
    // TODO: eventually it would be nice not to filter this down, so that the plasmid map can show everything,
    // but it's too slow. Optimize the mapping and then remove the filter.
  ).filter(normalEnzymeSet contains _.name)
  val knownEnzymesByName = knownEnzymes.map {
    case enz @ RestrictionEnzyme(name, _, _, _) => (name, enz)
  }.toMap
}