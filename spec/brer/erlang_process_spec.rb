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
    it "prevents passing both target class and target object" do
      lambda {
        Brer::ErlangProcess.new(:p1, Thing) { Thing.new }
      }.should raise_error(ArgumentError)
    end
    it "prevents passing neither" do
      lambda {
        Brer::ErlangProcess.new(:p1)
      }.should raise_error(ArgumentError)
    end
    it "requires a symbol as name" do
      lambda {
        Brer::ErlangProcess.new("p1") { Thing.new }
      }.should raise_error(ArgumentError)
    end
  end

  describe "target object initialization" do
    class Thing2
      def initialize
        raise "Hello"
      end
      def test; end
    end

    it "can be done right away" do
      lambda {
        Brer::ErlangProcess.new(:p1) { Thing2.new }
      }.should raise_error(RuntimeError, "Hello")
    end

    it "can be done lazily (only on timeout)" do
      p1 = Brer::ErlangProcess.new(:p1, Thing2)
      p1.test
      p1.test
      @sock.stub(:gets) { "{\"error\":\"timeout\"}" }
      lambda { p1.test }.should raise_error(RuntimeError, "Hello")
    end
  end

  describe "initialize arguments" do
    class Thing3 < Brer::TargetObject; end
    class Thing4; def initialize(*args) end end

    it "are passed to target after start_link if it subclasses TargetObject" do
      msgs = []
      @sock.stub(:write) { |msg| msgs << msg }
      Brer::ErlangProcess.new(:p1) { Thing3.new("1", "2", "3") }
      msgs.should == [
        ["start_link","thing","p1",[]].to_json,
        ["cast","p1","initialize","1"].to_json,
        ["cast","p1","initialize","2"].to_json,
        ["cast","p1","initialize","3"].to_json
      ]
    end

    it "aren't passed to target if it doesn't subclass TargetObject" do
      msgs = []
      @sock.stub(:write) { |msg| msgs << msg }
      Brer::ErlangProcess.new(:p1) { Thing4.new("1", "2", "3") }
      msgs.should == [
        ["start_link","thing","p1",[]].to_json
      ]
    end
  end

  describe "timeout" do
    it "should stop all calls to the socket" do 
      @sock.should_receive(:write).exactly(3).times
      p1 = Brer::ErlangProcess.new(:p1, Thing) # (1) start_link
      p1.test # (2) call
      @sock.stub(:gets) { "{\"error\":\"timeout\"}" }
      p1.test # (3) call
      @sock.should_receive(:write).exactly(0).times
      p1.test
      p1.test
      p1.undefined_test
    end

    it "should redirect past & future messages to the target" do 
      msgs = []
      thing = mock(:thing).as_null_object
      thing.stub(:test)  { |arg| msgs << [:test,  arg] }
      thing.stub(:test2) { |arg| msgs << [:test2, arg] }
      p1 = Brer::ErlangProcess.new(:p1) { thing }

      p1.test(1) # [:test,  1]
      @sock.stub(:gets) { "{\"error\":\"timeout\"}" }
      p1.test2(2) # [:test2, 2]
      
      msgs.should == [
        [:test,  1],
        [:test2, 2]
      ]
    end

    it "should not redirect future async (cast) messages to the target" do 
      msgs = []
      thing = mock(:thing).as_null_object
      thing.stub(:test)  { |arg| msgs << [:test,  arg] }
      p1 = Brer::ErlangProcess.new(:p1) { thing }

      p1.test(1) # [:test,  1]
      @sock.stub(:gets) { "{\"error\":\"timeout\"}" }
      p1.undefined_test(2) # async
      
      msgs.should == [[:test,  1]]
    end

    it "should raise an UnretraceableStateError after an async message" do 
      p1 = Brer::ErlangProcess.new(:p1, Thing)
      p1.undefined_test # cast
      @sock.stub(:gets) { "{\"error\":\"timeout\"}" }
      lambda { p1.test }.should raise_error(Brer::UnretraceableStateError)
    end
  end

  describe "return value" do
    describe "of call" do
      it "the socket's result['ok']" do
        @sock.stub(:gets) { {"ok" => "123"}.to_json }
        p1 = Brer::ErlangProcess.new(:p1, Thing)
        p1.test.should == "123"
      end

      it "the target's return value after a timeout" do
        @sock.stub(:gets) { {"ok" => "123"}.to_json }
        p1 = Brer::ErlangProcess.new(:p1, Thing)
        p1.test.should == "123"
      end
    end
  end
end
