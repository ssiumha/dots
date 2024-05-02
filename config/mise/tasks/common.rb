# frozen_string_literal: true

require 'io/console'
require 'fileutils'

module Cmd
  module_function

  def fzf_fd(path=nil, args=nil) = fzf `fd '#{path}' #{args}`.split

  def fzf(items)
    open("|fzf", "w+") do
      _1.puts items
      return _1.read.chomp
    end
  end
end

class Time
  def self.how_ago(time)
    delta = (Time.now - Time.new(time)).to_i
    case delta
    in ..0
      "#{-delta}s after"
    in 0..300
      "#{delta}s ago"
    in 0..3599
      "#{delta / 60}m ago"
    in 0..86400
      "#{delta / 3600}h ago"
    else
      "#{delta / 86400} days ago"
    end
  end
end

class String
  %i[black red green yellow blue magenta cyan gray].each_with_index do |k, i|
    define_method k, -> { "\e[#{30 + i}m#{self}\e[0m" }
    define_method "bg_#{k}", -> { "\e[#{40 + i}m#{self}\e[0m" }
  end

  {
    bold: -> { "\e[1m#{self}\e[22m" },
    italic: -> { "\e[1m#{self}\e[22m" },
    underline: -> { "\e[1m#{self}\e[22m" },
    blink: -> { "\e[1m#{self}\e[22m" },
    reverse_color: -> { "\e[1m#{self}\e[22m" }
  }.each do
    define_method _1, _2
  end

  def noop
    gsub(/\e\[\d+m/, '')
  end
  alias esc noop

  def underscore = self.gsub(/([a-z0-9])([A-Z])/, '\1_\2').downcase
end

class Symbol
  def underscore = self.to_s.underscore.to_sym
end

class Hash
  def deep_transform_keys(&block)
    result = {}
    each do |key, value|
      result[yield(key)] = (value in Hash) ? value.deep_transform_keys(&block) : value
    end
    result
  end
end

module Token
  def self.get(name)
    token_path = File.join(Dir.home, ".cache/tokens/#{name}")

    unless File.exist?(token_path)
      print help(name)

      token = STDIN.noecho(&:gets).chomp

      if token.empty?
        puts 'Invalid API Token'
        exit 1
      end

      FileUtils.mkdir_p(File.dirname(token_path))
      File.write(token_path, token)
    end

    File.read(token_path)
  end

  def self.help(name)
    case name.to_sym
    when :todoist
      <<~EOF.chomp
        Get API Token from https://app.todoist.com/app/settings/integrations/developer
        >
      EOF
    end
  end
end
