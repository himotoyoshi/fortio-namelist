Gem::Specification::new do |s|
  version = "1.1.0"
  files = Dir.glob("**/*") - [ 
                               Dir.glob("fortio-namelist-*.gem"), 
                               Dir.glob("test/**/*"),
                               Dir.glob("work/**/*"),
                             ].flatten

  s.platform    = Gem::Platform::RUBY
  s.name        = "fortio-namelist"
  s.summary     = "A library for reading/writing fortran namelist file"
  s.description = <<-HERE
    A library for reading/writing fortran namelist file
  HERE
  s.version     = version
  s.licenses    = ['MIT']
  s.author      = "Hiroki Motoyoshi"
  s.email       = ""
  s.homepage    = 'https://github.com/himotoyoshi/fortio-namelist'
  s.files       = files
  s.required_ruby_version = ">= 2.4.0"
end

