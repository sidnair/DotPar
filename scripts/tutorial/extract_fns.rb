# encoding: UTF-8

Exceptions = ["dotparc helloWorld.par", "dotparc *.par", "out.parc",
  "./out.parc",  "Hello, World", "func function-name : return-type(optional parameter declarations)\n{\ndeclarations and statements;\nreturn return-val;\n}"]

def extract_verbatim(filename)
  str = File.open(filename).read
  regex = /\\begin{verbatim}(.*?)\\end{verbatim}/m
  str.scan(regex).each do |f|
    cleaned = clean(f[0])
    p_wrapped(cleaned) unless Exceptions.include?(cleaned)
  end
end

def clean(s)
  s.strip.gsub('“', '"').gsub('”', '"')
end

def p_wrapped(f)
  puts "func wrapper:void() {"
  puts f
  puts "}"
end


def usage
  "ruby extract_fns.rb filename.text"
end

if ARGV.length == 0
  usage
  return
end

extract_verbatim(ARGV[0])
