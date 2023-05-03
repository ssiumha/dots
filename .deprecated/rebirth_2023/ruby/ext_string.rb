# frozen_string_literal: true

# ext color string
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
end
