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

    desc 'doc', ''
    def doc
      puts doc(<<~END)
        # rubydoc tag
        #   https://www.rubydoc.info/gems/yard/file/docs/Tags.md

        # basic ->
        # 함수 설졍은 주석으로 작성
        # @return [String, Array<String>, nil] 반환 값에 대한 설명
        #   줄바꿈도 사용할 수 있다
        # @param name [String] 인자에 대한 설명 작성
        # @param io [#read] 함수 이름은 #xxx 형태로 작성할 수 있다
        def awesome(name, io); io.read end

        # more ->
        # @param (see #awesome) # reference도 가능
        # @deprecated Use {#awesome} instead.
        # @example 예시 코드를 표현할 수도 있다
        #   foo.awesome2 #=> nil
        # @note 설명을 적거나
        # @todo TODO도 관리할 수 있다
        def awesome2(name, io) end

        # hash ->
        # @param [Hash] opts 해시 값을 옵션으로 받을 경우
        # @option opts [String] :subject The subject
        # @option opts [String] :body 이런 식으로 표시할 수 있다
        def send_email(opts = {}) end
      END
    end
  end
end
