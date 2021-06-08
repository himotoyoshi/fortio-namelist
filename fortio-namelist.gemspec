Gem::Specification::new do |s|
  version = "1.3.0"
  files = Dir.glob("**/*") - [ 
                               Dir.glob("fortio-namelist-*.gem"), 
                               Dir.glob("test/**/*"),
                               Dir.glob("work/**/*"),
                             ].flatten

  s.platform    = Gem::Platform::RUBY
  s.name        = "fortio-namelist"
  s.summary     = "A library for reading/writing fortran namelist file"
  s.description = <<-HERE
  This is a Ruby library for reading and writing Fortran's namelist. 
  This library allows you to read a namelist string as a Hash object, 
  or dump a Hash object to a namelist string.
  HERE
  s.version     = version
  s.license     = 'MIT'
  s.author      = "Hiroki Motoyoshi"
  s.email       = ""
  s.homepage    = 'https://github.com/himotoyoshi/fortio-namelist'
  s.files       = files
  s.required_ruby_version = ">= 2.4.0"
  s.add_development_dependency "racc", "~> 1.5"
end

