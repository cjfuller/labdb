require "spec_helper"

describe QuickSearchController do
  fixtures :users

  before :each do
    request.env["HTTPS"] = "on"
    log_in(request.session)
  end

  it "should redirect to the appropriate search page when searching numbers" do
    get :do_quick_search, database: "Plasmids", number: "5"
    response.should redirect_to(controller: "plasmids", action: "index", plasmid: { number: 5 })
  end

  it "should redirect to the appropriate search page when searching aliases" do
    get :do_quick_search, database: "Oligonucleotides", alias: "an oligo"
    response.should redirect_to(controller: "oligos", action: "index", oligo: { oligoalias: "an oligo" })
  end

  it "should go back to the quick start page on invalid search" do
    get :do_quick_search, database: "", number: ""
    response.should redirect_to("/")
  end
end
