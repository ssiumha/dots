#!/usr/bin/env ruby

require 'thor'
require '~/dotfiles/ruby/all.rb'

LIB_PATHS = Dir["#{__dir__}/lib/**.rb"]
LIB_PATHS.each { require _1 }
Dir["#{__dir__}/lib/**rb"].map { File.basename(_1).gsub(/.rb$/, '') }


class Chiz < Thor
  LIB_PATHS.each do |path|
    name = File.basename(path).gsub(/.rb$/, '')
    next if name == 'base'

    lib_klass = const_get("Lib::#{name.gsub(/(^.|_.)/) { _1.tr('_', '').upcase }}Chiz")
    desc name, "#{name} cheatsheet"
    subcommand name, lib_klass
  end
end

Chiz.start(ARGV)
