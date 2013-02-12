#Brer

####About the name

My goal for writing Brer is to produce a framework for the Ruby and Erlang languages to work together, while providing the simplest possible API. Along the theme of working together, I chose the name "Brer," or Br'er which is short for Brother. It's a title given to animals in old folk tales.

You can consider it an acronym too, as BrER (Bridge for Erlang and Ruby), or BREr (Bridge for Ruby and Erlang).

####API

The Ruby API works like this. Let's start by defining a new Ruby class which incrementally builds a plaintext document:

`
class Thing
  def puts(str)
    @body = (@body || "") + str + "/n"
  end

  def to_s
    @body
  end
end
`

Now, here is an example of the Brer API using this new class:


`
thing = Brer::ErlangProcess[:thing] { Thing.new }
`

ErlangProcess#[] is a factory method for Erlang processes. It will either start a new Erlang process with the moniker you pass in or return it if one already exists.

`
thing.puts "Hello world!"
thing.puts "This is me!"
thing.puts "Life should be!"
puts thing.to_s
`

These 4 lines are supposed to send messages to the "thing" process in the Erlang virtual machine and await a response from #to_s. But wait! This is all Ruby. Where does Erlang get involved?

Let's create an Erlang module which implements the "Thing" interface:

`
-module(thing).
-exports([puts/2, to_s/1]).

start() ->
  {ok, <<"">>}.

puts(State, String) ->
  {ok, State ++ String}.
  
to_s(State) ->
  {ok, State, State ++ <<"Fun for everyone!\n">>}.
`

If you're wondering where the "start" method and oks come from, check the docs for the OTP gen_server behavior which every Brer Erlang process needs to have.

Now the fun part: testing.

`
> thing.puts "Hello world!"
=> true
> thing.puts "This is me!"
=> true
> thing.puts "Life should be!"
=> true
> puts thing.to_s
Hello world!
This is me!
Life should be!
Fun for everyone!
=> true
>
` 

As you can see, the Ruby interpreter delivers all messages to the Erlang process and receives a response from the thing:to_s function.

And this is the happy path. So now all the interesting problems come. 

**Problem 1**: What happens if the Erlang process never actually starts up? Will the Ruby interpreter hang forever?

**Answer**: No. In the event of a timeout, the ErlangProcess object fails gracefully by redirecting all pending messages to the target object. 

For example, say we make this change:

`
start() ->
  erlang:wait(5000), %% wait for 5 seconds
  {ok, <<"">>}.
`

So we have caused the Erlang program to hang on startup. Let's run the same test:

`
> thing.puts "Hello world!"
=> "Hello world!"
> thing.puts "This is me!"
=> "Hello world!/nThis is me!"
> thing.puts "Life should be!"
=> "Hello world!/nThis is me!/nLife should be!/n"
> puts thing.to_s
(…2 seconds elapse…)
Hello world!
This is me!
Life should be!
…
...
=> nil
>` 

Fellow Ruby aficionados will probably notice by the output that the messages have all been delivered to the target Thing object. If an ErlangProcess experiences a timeout, no further attempts will be made to contact Erlang until a new ErlangProcess object is instantiated.

The timeout is a configuration option which defaults at an arbitrary value of 2 seconds.

So any programmer using this framework must prepare for the possibility of all messages being sent to the target object.

**Problem 2**: What if I don't necessarily want a reply from Erlang, but prefer to program asynchronously?

**Answer**: If you do not expect a reply from the message you are sending, you can simply leave it out of the target object definition. Calls to methods not defined by the target object will be sent to the Erlang process as a handle_cast. They will not be sent to the target object in the event of a timeout.

For example, try:

`
class Thing
  def to_s
    @body
  end
end
`

And do the same test:

`
> thing.puts "Hello world!"
=> true
> thing.puts "This is me!"
=> true
> thing.puts "Life should be!"
=> true
> puts thing.to_s
Hello world!
This is me!
Life should be!
Fun for everyone!
=> nil
>` 

**Problem 3**: What if initializing the target Ruby object is the very operation I was trying to speed up using Erlang? Is it possible to leave that out?

**Answer**: No -- choosing a target object is a requirement. However, there is a workaround for this:

`
\# instead of:
\# thing = Brer::ErlangProcess[:thing] { ReallyCostlyToInitializeThing.new }
\# try:
thing = Brer::ErlangProcess[:thing, ReallyCostlyToInitializeThing]
`

This will allow you to postpone the instantiation of the ReallyCostlyToInitializeThing object until needed. The flipside is that you will not be able to pass in arguments.

**Problem 4**: Uh-oh. You didn't tell me I could pass arguments when instantiating my target object.

**Answer**: The Erlang process doesn't care or know anything about the target object -- neither the class name, the arguments to #new, nor anything else. Swim at your own risk.

But the good news is that when defining your target object, you can include the Brer::TargetObject module, which automatically serializes  arguments from #new into JSON and passes them to an "init_arg" method on your Erlang module.

For example, you could rewrite our example code like this, for fun:

`
class Thing < String
  include Brer::TargetObject
  alias puts <<
end
`

`
-module(thing).
-exports([puts/2, to_s/1]).

start() ->
  {ok, <<"">>}.
  
init_arg(State, String) ->
  {ok, String}.

puts(State, String) ->
  {ok, State ++ String}.
  
to_s(State) ->
  {ok, State, State ++ <<"Fun for everyone!\n">>}.
`

But it's not really a good idea.

**Problem 5**: But what if I make my connection with Erlang, start passing messages, and then Erlang times out? Am I going to lose all the state I saved in Erlang?

**Answer**: Short answer: no -- your changes can be saved and rolled into your target object.

However, you can't use this feature if you've done any asynchronous operations with Erlang using _cast_ (including _initialize_args_). However the code will not leave your target object in an unreliable state -- if a timeout occurs after an asynchronous operation, the code will raise a Brer::UntraceableStateError.
