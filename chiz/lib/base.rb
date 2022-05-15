module Lib
  class Base < Thor
    default_task :default

    desc 'default', ''
    def default(args: nil)
      #puts self.class.methods
      puts self.class.instance_methods(false)
    end

    no_tasks do
      def doc(str)
        str.split("\n").map { _1.start_with?('#') ? _1.green : _1 }
      end
    end
  end
end
