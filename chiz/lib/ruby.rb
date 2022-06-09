module Lib
  class RubyChiz < Base
    desc 'bundle', 'initialize bundle project'
    def bundle
      puts <<~END
        bundle init
        bundle config set --local path vendor/bundle
      END
    end

    desc 'proc', 'proc, lambda, call, method'
    def proc
      puts_doc <<~END
        # proc
        #   익명 함수의 기본 단위
        #   block 또한 proc 타입이다
        #   클로저가 적용되며, return 하면 proc이 실행되는 위치에서 반환된다
        #     next, break로 반환 처리를 해야한다
        p = Proc.new { |x| ... } | proc { |x| ... }
        p.call(3) | p.(3) | p[3]

        # labmda
        #   Proc을 좀 더 strict하게 만듬 (class가 Proc (lambda) 형태)
        #   인자 갯수가 정확히 일치하지 않으면 에러 (`->{}` 는 인자 1개)
        #   내부에서 return으로 반환할 수 있다
        l = lambda { |x| ... } | ->(x) { ... }
        l.lambda?
      END
    end

    desc 'svar', 'specific variable'
    def svar
      puts_doc <<~END
        $0, $PROGRAM_NAME
          스크립트 실행 시작 파일 이름

        __FILE__
          현재 파일 이름
            my_file_path = File.expand_path(File.dirname(__FILE__))

          다음 형태로 직접 실행된건지 체크 가능
            `puts 'main!' if __FILE__ == $0`

        DATA, __END__
          Object#DATA, 스크립트 파일마다 접근 가능한 파일 객체
          파일 맨끝에 __END__ 밑에 작성된 텍스트를 DATA.read로 읽어올 수 있다

          __FILE__이 아니라 $0을 참조한다 (require, load로 다른 파일 DATA에 접근할 수 없다)

          sinatra에서는 이런 형태로 직접 파일에서 읽어들이고 있다
            https://github.com/sinatra/sinatra/blob/master/lib/sinatra/base.rb
            `app, data = io.gsub("\\r\\n", "\\n").split(/^__END__$/, 2)`
      END
    end

    desc 'var', 'variable usecase'
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

    desc 'rubydoc', ''
    def rubydoc
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

    desc 'nokogiri', 'html parse library nokogiri'
    def nokogiri
      puts doc(<<~END)
        require 'nokogiri'

        doc = Nokogiri::HTML(html)
        doc.attr('data-title') # Nokogiri::XML::Attr
      END
    end
  end
end
