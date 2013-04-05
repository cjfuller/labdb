require "spec_helper"

describe SearchesController do
  describe "routing" do

    it "routes to #index" do
      get("/searches").should raise_exception
    end

    it "routes to #new" do
      get("/searches/new").should raise_exception
    end

    it "routes to #show" do
      get("/searches/1").should raise_exception
    end

    it "routes to #edit" do
      get("/searches/1/edit").should raise_exception
    end

    it "routes to #create" do
      post("/searches").should raise_exception
    end

    it "routes to #update" do
      put("/searches/1").should raise_exception
    end

    it "routes to #destroy" do
      delete("/searches/1").should raise_exception
    end

  end
end
