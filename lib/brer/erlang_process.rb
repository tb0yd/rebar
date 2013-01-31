require 'rubygems'
require 'socket'
require 'json'

module Brer
  class ErlangTimeoutError < StandardError; end
  class UnretraceableStateError < StandardError; end
  class ErlangProcess
    def initialize(name, target_class=nil)
      raise ArgumentError.new if target_class.nil? && !block_given?
      raise ArgumentError.new if !target_class.nil? && block_given?
      raise ArgumentError.new unless name.is_a?(Symbol)

      @name = name # the process moniker

      if target_class 
        # user wants to bypass target object initialization
        @target_class = target_class
      else
        # user is targeting an existing object, so we can reflect on the object
        @target = yield
      end

      start_link

      if @target && @target.is_a?(Brer::TargetObject)
        # lose ability to roll back (it's a cast)
        @target.initialize_args.each do |arg|
          cast(:initialize, arg)
        end
      end
    end
  
    # all non-defined methods become casts
    def method_missing(method, *params)
      if target_object_respond_to?(method)
        call(method, params)
      else
        cast(method, params)
      end
    end

    private

    # lazy eval target object, allow initialization only to happen when needed
    def target_object
      @target ||= @target_class.new
    end

    # lazy check if method was defined in target class
    def target_object_respond_to?(method)
      if @target
        @target.respond_to?(method)
      else
        @target_class.instance_methods(false).include?(method)
      end
    end
  
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
        target_object.send(fun, *args)
      else
        sock = TCPSocket.new('127.0.0.1', 5500)
        sock.write([:call, @name, fun, args].to_json)
        @history << [fun, args] unless @history == :lost
        begin
          result = demarshal(sock.gets)
        rescue ErlangTimeoutError => e
          if @history == :lost
            # erlang timed out and you've done async operations. there's no way to 
            # retrace your state to continue with your target object.
            raise UnretraceableStateError.new
          else
            # try and repeat all messages passed to erlang from the beginning to now
            # in target object
            @history.each do |hist|
              fun, args = hist
              result = target_object.send(fun, *args)
            end
          end
        end
        result
      end
    end
  
    def cast(fun, args)
      unless @timeout # ignore if process has timed out (let user re-init)
        sock = TCPSocket.new('127.0.0.1', 5500)
        sock.write([:cast, @name, fun, args].to_json)
        @history = :lost # can't keep track anymore
      end
      nil
    end

    def start_link
      # TODO: don't hardcode the erlang module
      sock = TCPSocket.new('127.0.0.1', 5500)
      sock.write([:start_link, :thing, @name, []].to_json)
      @history = []
      demarshal(sock.gets)
    rescue ErlangTimeoutError => e
      # don't care about return value, no history to roll back
    end
  end
  class TargetObject
    attr_reader :initialize_args # recall args passed to new()
    def initialize(*args)
      @initialize_args = args
    end
  end
end

