class ErlangProcess
  ERLANG = Rebar::Erlang.new(:process_access, '127.0.0.1', 5500)

  def initialize(process_name, data)
    @process_name = process_name
    ERLANG.start_process(@process_name, data)
  end

  def method_missing(cmd, *args)
    ERLANG.send_to_process(@process_name, cmd)
  end
end
