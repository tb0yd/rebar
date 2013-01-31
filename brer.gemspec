# -*- encoding: utf-8 -*-
$:.push File.expand_path("../lib", __FILE__)

Gem::Specification.new do |s|
  s.name        = "brer"
  s.version     = 0.01
  s.authors     = ["Tyler Boyd"]
  s.email       = ["tboyd47@gmail.com"]
  s.homepage    = ""
  s.summary     = %q{ Bridge for Ruby and ERlang }
  s.description = %q{ Simple framework tying Ruby objects to Erlang processes }

  s.rubyforge_project = "brer"

  s.files         = %w(
    lib/brer/erlang_process.rb
    brer.gemspec
  )

  #s.test_files    = `git ls-files -- {test,spec,features}/*`.split("\n")
  #s.executables   = `git ls-files -- bin/*`.split("\n").map{ |f| File.basename(f) }
  s.require_paths = ["lib"]
  s.add_development_dependency 'activerecord'
  s.add_runtime_dependency "activerecord"
end
