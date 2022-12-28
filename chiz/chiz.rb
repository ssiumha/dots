#!/usr/bin/env ruby

require 'bundler/inline'

gemfile do
  source 'https://rubygems.org'
  gem 'thor'
end


require 'thor'
require '~/dotfiles/ruby/all.rb'

require "#{__dir__}/lib/base.rb"

LIB_PATHS = Dir["#{__dir__}/lib/**.rb"]
LIB_PATHS.each { require _1 }
Dir["#{__dir__}/lib/**rb"].map { File.basename(_1).gsub(/.rb$/, '') }


class Chiz < Thor
  default_task :default

  desc 'default', ''
  def default
    # ./lib/rails/factorybot.rb:4:      md :ignore_uniq, 'how to duplicate field factory model', <<~MD, lang: :ruby
    res = `rg --color never -n -i '\smd :' ./lib | sort`
    md_list = res.split("\n").map do |line|
      filepath, number, _md, *body = line.split(':')
      body = body.join(' ')

      title, desc = /^(.+?), '(.*?)', /.match(body).captures
      filepath.gsub!(%r{./lib/(.+).rb}, '\1')

      [filepath, number, title, desc]
    end

    width_filepath = md_list.map { _1[0].size + _1[1].size + 1 }.max
    width_title = md_list.map { _1[2].size + 1 }.max
    width_desc = md_list.map { _1[3].size + 1 }.max

    res = md_list.map do |filepath, number, title, desc|
      "%-#{width_filepath}s | %-#{width_title}s | %s" % ["#{filepath}:#{number}", title, desc]
    end

    selected = fzf(res)
    filepath, title, desc = selected.first.split('|').map(&:strip)

    file = filepath.split(':').first
    puts selected
    invoke file, [title]
  end

  # TODO: ctrl-c trap

  # TODO: argument not working
  desc 'e', 'chiz edit lib/cmd'
  def e(cmd: nil)
    puts cmd
    # if args.nil?
    #   # TODO: open fzf
    # else
    # end

    # cmd = args.first
    # puts cmd
    # exec %{ cd #{__dir__} && v lib/#{cmd}.rb }
  end

  LIB_PATHS.each do |path|
    name = File.basename(path).gsub(/.rb$/, '')
    next if name == 'base'

    lib_klass = const_get("Lib::#{name.gsub(/(^.|_.)/) { _1.tr('_', '').upcase }}Chiz")
    desc name, "#{name} cheatsheet"
    subcommand name, lib_klass
  end
end

Chiz.start(ARGV)
