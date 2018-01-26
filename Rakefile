desc 'build'
task :build => ['compiler', 'interp']
  
desc 'compiler'
task :compiler do
  puts `ocamlbuild -use-ocamlfind src/main.byte`
  `mv main.byte min-caml`
end

desc 'interp'
task :interp do
  puts `ocamlbuild -use-ocamlfind src/jit/min-camli.byte`
  `mv min-camli.byte min-caml`
end

desc 'test'
task :test => ['test:all']

desc 'clean'
task :clean do
  puts `ocamlbuild -clean`
end

namespace 'test' do
  desc 'simple'
  task :simple do
    Dir.glob('test/simple*.ml').each do |test|
      name = test.split('/')[-1]
      bytecode = name.sub('.ml', '.byte')
      puts `ocamlbuild -use-ocamlfind test/#{bytecode}`
      puts `./#{bytecode}`
    end
  end

  desc 'all' 
  task :all do
    Dir.glob('test/*.ml').each do |test|
      name = test.split('/')[-1]
      bytecode = name.sub('.ml', '.byte')
      puts `ocamlbuild -use-ocamlfind test/#{bytecode}`
      puts `./#{bytecode}`
    end
  end
end
