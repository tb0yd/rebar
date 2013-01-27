require 'rubygems'
require 'socket'
require 'json'

module Brer
  class ErlangTimeoutError < StandardError; end
  class ErlangProcess
    def initialize(name, target_class=nil)
      raise ArgumentError.new if target_class.nil? && !block_given?
      raise ArgumentError.new if !target_class.nil? && block_given?

      @name = name
      if target_class
        @target = target_class.new
      else
        @target = yield
        #if @target.is_a?(Brer::TargetObject)
        #  cast(:initialize_args, @target.initialize_args.to_json)
        #end
      end

      start_link
    end
  
    def method_missing(method, *params)
      call(method, params)
    end

    private
  
    def demarshal(str)
      json = JSON.parse(str)
      if json.has_key?("ok")
        json["ok"]
      elsif json["error"] == "timeout"
        @timeout = true
        raise ErlangTimeoutError.new
      end
    end
  
    def call(fun, args)
      if @timeout
        @target.send(fun, *args)
      else
        sock = TCPSocket.new('127.0.0.1', 5500)
        sock.write([:call, @name, fun, args].to_json)
        demarshal(sock.gets)
      end
    rescue ErlangTimeoutError => e
      @target.send(fun, *args)
    end
  
    def cast(fun, args)
      sock = TCPSocket.new('127.0.0.1', 5500)
      sock.write([:cast, @name, fun, args].to_json)
      nil
    end

    def start_link
      # TODO: don't hardcode the erlang module
      sock = TCPSocket.new('127.0.0.1', 5500)
      sock.write([:start_link, :thing, @name, []].to_json)
      demarshal(sock.gets)
    rescue ErlangTimeoutError => e
    end
  end
end
