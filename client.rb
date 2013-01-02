require 'rubygems'
require 'socket'
require 'json'

#Usage:
#  pr1 = Brother.process(:pr1) # starts a gen_server with module "process" and register at "pr1"
#  pr2 = Brother.thing(:pr2) # starts a gen_server with module "thing" and register at "pr2"
module Brother
  class ErlangProcess
    def initialize(module_name, process_name, address='127.0.0.1', port=5500)
      @module = module_name.to_s
      @name = process_name.to_s
      @address = address
      @port = port

      @sock = TCPSocket.new(@address, @port)
      @sock.write({:start_process => @name, :module => @module}.to_json)
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

  def self.method_missing(module_name, process_name)
    Brother::ErlangProcess.new(module_name, process_name)
  end
end
