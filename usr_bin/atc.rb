#!/usr/bin/ruby
require 'optparse'
require 'yaml'

options = {}
OptionParser.new do |opts|
  opts.banner = "Usage: #{opts.program_name} { --config CONF --colors COLORS }"
  $BANNER = opts.banner
  opts.on("--config FILE_NAME", "alacritty config") do |f|
    options[:config] = f
  end
  opts.on("--colors FILE_NAME", "colors definition file") do |f|
    options[:colors] = f
  end
  opts.on("-h","--help","Help") do
    puts opts
    exit
  end
end.parse!

if not options[:config] or not options[:colors]
  puts "Use -h"
  exit
end

config=YAML.load(File.read(options[:config]))
colors=YAML.load(File.read(options[:colors]))

if config and colors
  puts "Applying: #{options[:colors]}"
  config["colors"]=colors["colors"]
  File.write(options[:config],config.to_yaml)
end
