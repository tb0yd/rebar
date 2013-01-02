require 'rubygems'
require 'socket'
require 'json'

#Usage: erlang = Rebar::Erlang.new(:process_1, '127.0.0.1', 5500)
module Rebar
  class Erlang
    def initialize(name, address='127.0.0.1', port=5500)
      @name = name.to_s
      @address = address
      @port = port

      @sock = TCPSocket.new(@address, @port)
      @sock.write({:start_process => @name}.to_json)
      demarshal(@sock.gets)
    end
  
    def method_missing(*args)
      method, *params = args
      rpc("#{@name}:#{method}", params)
    end
  
    def marshal(fun, args)
      {:method => fun, :params => args, :id => 0}.to_json
    end
  
    def demarshal(str)
      s = JSON.parse(str)
      p s
      s["result"]
    end
  
    def rpc(fun, args)
      json_request_string = marshal(fun, args)
      json_response_string = nil
      begin
        @sock = TCPSocket.new(@address, @port)
        @sock.write(json_request_string)
        json_response_string = @sock.gets
      rescue
        raise
      end
      demarshal(json_response_string)
    end
  end
end
