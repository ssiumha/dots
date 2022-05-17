module Lib
  class Base < Thor
    default_task :default

    desc 'default', ''
    def default(args: nil)
      # name, description, usage, ancestor_name, options
      commands = self.class.commands.filter_map do |name, command|
        next if name.to_sym == :help

        '%-10s %s' % [name, command.description]
      end

      fzf(commands).map do
        puts _1
        invoke _1.split.first
      end
    end

    no_tasks do
      def doc(text)
        text.split("\n").map { |line| line.gsub(/(#.+)/) { _1.green } }
      end
    end
  end
end
