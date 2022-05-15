module Lib
  class RubyChiz < Base
    desc 'bundle', 'initialize bundle project'
    def bundle
      puts <<~END
        bundle init
        bundle config set --local path vendor/bundle
      END
    end

    desc 'var', ''
    def var
      puts doc(<<~END)
        # extract variable from hash
        hash = { key1: 1, key2: 2 }
        key1, key2 = hash.values_at(:key1, :key2)
      END
    end

    desc 'class_var', ''
    def class_var
      puts doc(<<~END)
        # class_variable
        @@class_var = 1
        class_variable_get(name)
        class_variable_set(name, val)
      END
    end

    desc 'meta_class', ''
    def meta_class
      puts doc(<<~END)
        # Module#method_added
        #   - 다음 경우에 호출된다
        #     - def foo; end
        #     - define_method :bar, instance_method(:foo)
        #   - singleton method는 BasicObject#singleton_method_added
        #   - Thor에서 desc -> def 하는 식으로 작성할 때 사용할 수 있다
        def self.method_added(method_name_symbol)
        end

        # missing
        #   - 동적 method, const를 만들 때 사용된다
        #   - BasicObjct#method_missing(name, *args, &block)
        #     - Object#respond_to_missing?(name, include_private = false)
        #       - respond_to?만 정의하면 method 함수로 가져올 때 실패한다
        #   - Module#const_missing(sym)
        #     - Module#const_set(sym, obj)
      END
    end
  end
end
