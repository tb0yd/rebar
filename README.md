#Brer

####About the name

My goal for writing Brer is to produce a framework for the Ruby and Erlang languages to work together, while providing the simplest possible API. Along the theme of working together, I chose the name "Brer," or Br'er which is short for Brother. It's a title given to animals in old folk tales.

You can consider it an acronym too, as BrER (Bridge for Erlang and Ruby), or BREr (Bridge for Ruby and Erlang).

####API

The Ruby API works like this. Let's start by defining a new Ruby class which incrementally builds a plaintext document:

```ruby
class Thing
  def puts(str)
    @body = (@body || "") + str + "\n"
  end

  def result
    @body
  end
end
```

Now, here is an example of the Brer API using this new class:


```ruby
thing = Brer::ErlangProcess.new(thing) { Thing.new }
```

ErlangProcess.new will either start a new Erlang process with the moniker you pass in or return it if one already exists.

```ruby
thing.puts "Hello world!"
thing.puts "This is me!"
thing.puts "Life should be!"
puts thing.result
```

These 4 lines are supposed to send messages to the "thing" process in the Erlang virtual machine and await a response from #to_s. But wait! This is all Ruby. Where does Erlang get involved?

Let's create an Erlang module which implements the "Thing" interface:

```erlang
-module(thing).
-behaviour(gen_server).
-export([init/1, handle_call/3, terminate/2, handle_cast/2, code_change/3, handle_info/2]).

init(_) ->
  {ok, []}.

handle_call([puts, Bin], _From, State) ->
  {reply, true, [State,Bin,<<"\n">>]};
  
handle_call([result], _From, State) ->
  Result = list_to_binary([State, <<"Fun for everyone!\n">>]),
  {reply, Result, State}.

handle_cast([puts, Bin], State) ->
  {noreply, [State,Bin,<<"\n">>]};

handle_cast(_, State) ->
  {noreply, State}.

handle_info(_, State) ->
  {noreply,State}.

terminate(_, _) ->
  ok.

code_change(_, State, _) ->
  {ok, State}.
```

Now the fun part: testing.

    1.9.3p194 :114 > thing = Brer::ErlangProcess.new(:thing) { Thing.new }
     => #<Brer::ErlangProcess:0x007ffef905a348 @name=:thing, @target=#<Thing:0x007ffef9059f60>, @history=[]> 
    1.9.3p194 :035 > thing.puts "Hello world!"
     => true 
    1.9.3p194 :036 > thing.puts "This is me!"
     => true 
    1.9.3p194 :037 > thing.puts "Life should be!"
     => true 
    1.9.3p194 :038 > puts thing.result
    Hello world!
    This is me!
    Life should be!
    Fun for everyone!
     => nil 

As you can see, the Ruby interpreter delivers all messages to the Erlang process and receives a response from the thing:to_s function.

And this is the happy path. So now all the interesting problems come. 

**Problem 1**: What happens if the Erlang process never actually starts up? Will the Ruby interpreter hang forever?

**Answer**: No. In the event of a timeout, the ErlangProcess object fails gracefully by redirecting all pending messages to the target object. 

For example, say we make this change:

```erlang
handle_call([puts, Bin], _From, State) ->
  timer:sleep(2000), %% wait for 2 seconds.
  {reply, true, [State,Bin,<<"\n">>]};
```

So we have caused the Erlang program to hang during the "puts" method. Let's recompile our Erlang program and then run the same test:

    1.9.3p194 :114 > thing = Brer::ErlangProcess.new(:thing2) { Thing.new }
     => #<Brer::ErlangProcess:0x007ffef905a348 @name=:thing2, @target=#<Thing:0x007ffef9059f60>, @history=[]> 
    1.9.3p194 :088 > thing.puts "Hello world!"
     => "Hello world!\n" 
    1.9.3p194 :089 > thing.puts "This is me!"
     => "Hello world!\nThis is me!\n" 
    1.9.3p194 :090 > thing.puts "Life should be!"
     => "Hello world!\nThis is me!\nLife should be!\n" 
    1.9.3p194 :091 > puts thing.result
    Hello world!
    This is me!
    Life should be!
     => nil 

Because our Ruby implementation doesn't add the "Fun for everyone" line, it doesn't appear in the output. So if an ErlangProcess experiences a timeout, no further attempts will be made to contact Erlang until a new ErlangProcess object is instantiated.

So any programmer using this framework must prepare for the possibility of all messages being sent to the target object.

**Problem 2**: What if I don't necessarily want a reply from Erlang, but prefer to program asynchronously?

**Answer**: If you do not expect a reply from the message you are sending, you can simply leave it out of the target object definition. Calls to methods not defined by the target object will be sent to the Erlang process as a handle_cast. They will not be sent to the target object in the event of a timeout.

For example, try removing the "puts" method:

```ruby
class AsyncThing
  def result
    @body
  end
end
```

And do the same test:

    1.9.3p194 :114 > thing = Brer::ErlangProcess.new(:thing3) { AsyncThing.new }
     => #<Brer::ErlangProcess:0x007ffef905a348 @name=:thing3, @target=#<AsyncThing:0x007ffef9059f60>, @history=[]> 
    1.9.3p194 :115 > thing.puts "Hello world!"
     => nil 
    1.9.3p194 :116 > thing.puts "This is me!"
     => nil 
    1.9.3p194 :117 > thing.puts "Life should be!"
     => nil 
    1.9.3p194 :118 > puts thing.result
    Hello world!
    This is me!
    Life should be!
    Fun for everyone!
     => nil 

The only visible difference is that the calls to "puts" return _nil_
instead of _true_, and that's because async calls to Erlang through Brer
always return _nil_.

**Problem 3**: What if initializing the target Ruby object is the very operation I was trying to speed up using Erlang? Is it possible to leave that out?

**Answer**: No -- choosing a target object is a requirement. However, there is a workaround for this:

```ruby
# instead of:
# thing = Brer::ErlangProcess[:thing] { ReallyCostlyToInitializeThing.new }
# try:
thing = Brer::ErlangProcess[:thing, ReallyCostlyToInitializeThing]
```

This will allow you to postpone the instantiation of the ReallyCostlyToInitializeThing object until needed. The flipside is that you will not be able to pass in arguments.

**Problem 4**: Uh-oh. You didn't tell me I could pass arguments when instantiating my target object.

**Answer**: The Erlang process doesn't care or know anything about the target object -- neither the class name, the arguments to #new, nor anything else. Swim at your own risk.

But the good news is that when defining your target object, you can define an initialize_args() method that gives Brer a list of arguments to be serialized and immediately passed into the object via an "initialize" method on your Erlang module. This will only happen if the process isn't already running.

For example, you could make these changes to Thing:

```ruby
class Thing
  def initialize(str="")
    @initialize_args = [str]
    @body = str + "\n"
  end

  attr_reader :initialize_args
end
```

And then make the Erlang module respond to "initialize" casts...

```erlang
handle_cast([initialize|Arg], State) ->
  {noreply, [State,Arg]};
```

And then you can do:

    1.9.3p194 :023 > thing = Brer::ErlangProcess.new(:thing4) { Thing.new("Hello world!") }
     => #<Brer::ErlangProcess:0x007fc3f4154180 @name=:thing4, @target=#<Thing:0x007fc3f4154108 @initialize_args=["Hello world!"], @body="Hello world!\n">, @history=:lost> 
    1.9.3p194 :024 > thing.puts "This is me!"
     => true 
    1.9.3p194 :025 > thing.puts "Life should be!"
     => true 
    1.9.3p194 :026 > puts thing.result
    Hello world!
    This is me!
    Life should be!
    Fun for everyone!
     => nil 

**Problem 5**: But what if I make my connection with Erlang, start passing messages, and then Erlang times out? Am I going to lose all the state I saved in Erlang?

**Answer**: Short answer: no -- your changes can be saved and rolled into your target object.

However, you can't use this feature if you've done any asynchronous operations with Erlang using _cast_ (including _initialize_). However Brer will not leave your target object in an unreliable state -- if a timeout occurs after an asynchronous operation, it will raise a Brer::UntraceableStateError.
