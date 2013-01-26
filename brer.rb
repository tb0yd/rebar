require 'rubygems'
require 'socket'
require 'json'

module Brer
  class ErlangProcess
    def initialize(name, target_class=nil)
      raise ArgumentError.new if target_class.nil? && !block_given?
      raise ArgumentError.new if !target_class.nil? && block_given?

      @name = name
      if target_class
        @target = target_class.new
      else
        @target = yield
      end

      start_link
    end
  
    def method_missing(*args)
      method, *params = args
      #if @timed_out
      #  @target.send(method, *params)
      #else
        result = call(method, params)

        #if result == "timeout" # TODO: make into a symbol
        #  @timed_out = true
        #  @target.send(method, *params)
        #else
          result
        #end
      #end
    end

    private
  
    def demarshal(str)
      JSON.parse(str)["result"]
    end
  
    def call(fun, args)
      sock = TCPSocket.new('127.0.0.1', 5500)
      sock.write([:call, @name, fun, args].to_json)
      demarshal(sock.gets)
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
    end
  end
end
