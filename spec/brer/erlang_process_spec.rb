require 'spec_helper'

describe Brer::ErlangProcess do
  class Thing
    def test
    end
  end

  before do
    @sock = mock(:sock, :gets => "{\"ok\":\"result\"}").as_null_object
    TCPSocket.stub(:new) { @sock }
  end

  describe "3 types of messages to erlang:" do
    it "should start_link on init" do
      @sock.stub(:write) { |msg| msg.should =~ /^\[\"start_link\",/ }
      Brer::ErlangProcess.new(:p1) { Thing.new }
    end
    it "should call on defined method" do
      p1 = Brer::ErlangProcess.new(:p1) { Thing.new }
      @sock.stub(:write) { |msg| msg.should =~ /^\[\"call\",/ }
      p1.test
    end
    it "should cast on undefined method" do
      p1 = Brer::ErlangProcess.new(:p1) { Thing.new }
      @sock.stub(:write) { |msg| msg.should =~ /^\[\"cast\",/ }
      p1.undefined_test
    end
  end

  describe "argument checking" do
    it "prevents both target class and target object" do
      lambda {
        Brer::ErlangProcess.new(:p1, Thing) { Thing.new }
      }.should raise_error(ArgumentError)
    end
    it "prevents neither" do
      lambda {
        Brer::ErlangProcess.new(:p1)
      }.should raise_error(ArgumentError)
    end
  end

  describe "lazy target object initialization" do
    class Thing2
      def initialize
        raise "Hello"
      end
      def test; end
    end

    it "works" do
      p1 = Brer::ErlangProcess.new(:p1, Thing2)
      p1.test
      @sock.stub(:gets) { "{\"error\":\"timeout\"}" }
      lambda { p1.test }.should raise_error(RuntimeError, "Hello")
    end
  end
end
