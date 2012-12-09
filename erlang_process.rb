require 'client'
class ErlangProcess
  ERLANG = Rebar::Erlang.new(:process_access, '127.0.0.1', 5500)

  def initialize(process_name, data)
    ERLANG.start_process(process_name, data)
  end
end
