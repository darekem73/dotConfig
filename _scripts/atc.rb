#!/usr/bin/ruby
require 'optparse'
require 'yaml'

options = {}
OptionParser.new do |opts|
  opts.banner = "Usage: #{opts.program_name} { --config CONF [ --colors COLORS ] [ --font-size FS ] [ --font-name NAME] }"
  $BANNER = opts.banner
  opts.on("--config FILE_NAME", "alacritty config") do |f|
    options[:config] = f
  end
  opts.on("--colors FILE_NAME", "colors definition file") do |f|
    options[:colors] = f
  end
  opts.on("--font-size FS", "font size") do |f|
    options[:fs] = f
  end
  opts.on("--font-name NAME", "font name") do |f|
    options[:fn] = f
  end
  opts.on("-h","--help","Help") do
    puts opts
    exit
  end
end.parse!

if not options[:config]
  puts "Use -h"
  exit
end

config=YAML.load(File.read(options[:config]))
colors=YAML.load(File.read(options[:colors])) if options[:colors]
fs = options[:fs]
fn = options[:fn]

if colors
  puts "Applying: #{options[:colors]}"
  config["colors"]=colors["colors"]
end

if fs
  puts "Applying font size: #{fs}"
  config["font"]["size"] = fs.to_i
end

if fn
  puts "Applying font name: #{fn}"
  ["normal","bold","italic"].each {|f| config["font"][f]["family"]="#{fn}"};
end

File.write(options[:config],config.to_yaml)
