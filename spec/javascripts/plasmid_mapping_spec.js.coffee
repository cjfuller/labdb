#= require jquery
#= require d3
#= require plasmid_mapping

describe "RestrictionEnzymeData", ->

  it "has no data until loaded explicitly", ->
    expect(plmp.RestrictionEnzymeData.data).not.to.exist

  enzyme_string = '{"AscI":{"cut_bef":2,"rec_seq":"GGCGCGCC"}}'

  it "should be able to load enzyme data from the appropriate element in the page", ->
    $("<div/>", {id: "enzyme-data", enz_data: enzyme_string}).appendTo("html")
    plmp.RestrictionEnzymeData.read_from_page()
    expect(plmp.RestrictionEnzymeData.data).to.exist


  describe "Enzymes", ->

    beforeEach ->
      plmp.RestrictionEnzymeData.read_data(enzyme_string)

    it "should be able to load a data object from JSON", ->
      expect(plmp.RestrictionEnzymeData.data).to.exist

    it "should be able to name the loaded enzyme", ->
      expect(plmp.RestrictionEnzymeData.get("AscI")).not.to.be.null

    it "should retrieve the correct enzyme properties", ->
      plmp.RestrictionEnzymeData.cuts_before("AscI").should.eql(2)
      plmp.RestrictionEnzymeData.seq("AscI").should.eql("GGCGCGCC")


