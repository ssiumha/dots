module Lib
  class RubyChiz < Base
    md :gem, 'ruby library package manager', <<~MD
      ruby를 설치하면 기본으로 포함되어있다.
      보통 system ruby library를 관리할 때 사용하고, 프로젝트 별 라이브러리는 bundle로 관리하면 된다.

      Gemfile: bundler에서 사용되는 버전 관리 포맷

      ```ruby
      gem '<package>', '~> 0.1.0' # <= 0.1.0, < 0.2 와 동일하다
      ```
    MD

    md :bundle, 'cheatsheet bundler', <<~MD, lang: :bash
      # 업데이트를 보수적으로: 버전업 최소화, 불필요한 업데이트 배제
      bundle update --conservative <package>
    MD

    md :init_bundle, 'initialize bundle project', <<~MD
      bundle init
      bundle config set --local path vendor/bundle

      bundle config set --local bin vendor/bundle/bin
      bundle config set --local cache_path vendor/bundle/cache

      bundle config set --local jobs 4
      bundle config set --local retry 3
      bundle config set --local clean true
    MD

    md :inline_bundle, 'single-file script gems', <<~MD, lang: :ruby
      # Gemfile 없이 단일 스크립트 파일에서 bundle을 통한 라이브러리를 받아 쓸 수 있다
      # 라이브러리는 스크립트가 실행 될 때 설치된다
      #   - global gem 환경에 설치되며 `gem list` 로 확인할 수 있다

      require 'bundler/inline' do

      gemfile do
        source 'https://rubygems.org'
        gem 'httparty'
      end

      response = HTTParty.get('...')
    MD

    md :proc, 'proc, lambda, call, method', <<~MD
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
    MD

    md :block, 'block, yield, method', <<~MD, lang: :ruby
      # 기본적인 운용. yield를 호출 할 때마다 block이 실행된다
      def basic_block_method
        if block_given?
          yield 1
          yield 2
        end
      end

      # instance_eval을 사용해서 block 안의 self를 instance 본체로 만든다
      # scope이 self라 private method에도 접근할 수 있다
      def instance_eval_block(&block)
        instance_eval(&block) if block
      end

      # 다른 함수에 block을 다시 넘길 때는 앞에 &을 붙여서 사용한다
      def apply_block(&block)
        instance_eval_block(&block)
      end
    MD

    md :svar, 'specific variable: FILE, $0', <<~MD
      $0, $PROGRAM_NAME
        스크립트 실행 시작 파일 이름

      __FILE__
        현재 파일 이름 (상대경로)
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
    MD

    md :var, 'variable usecase', <<~MD, lang: :ruby
      # extract variable from hash
      hash = { key1: 1, key2: 2 }
      key1, key2 = hash.values_at(:key1, :key2)
    MD

    md :class_var, '', <<~MD, lang: :ruby
      # class_variable
      @@class_var = 1
      class_variable_get(name)
      class_variable_set(name, val)
    MD

    md :meta_class, '', <<~MD, lang: :ruby
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
    MD

    md :singleton_class, '', <<~MD, lang: :ruby
      # singleton class는 항상 instance를 경유해서 사용하는데,
      # class.method_missing을 사용해서 instance를 가진 Module처럼 사용하는 방법
      # 대신 호출 구간에 method_missing 콜스택이 좀 포함되게 되는 문제가 좀 있다
      class SingletonBase
        include Singleton

        def self.method_missing(message, *args, &block)
          if public_instance_methods(false).include?(message)
            instance.send(message, *args, &block)
          else
            super
          end
        end

        def self.respond_to_missing?(method_name, include_private = false)
          public_instance_methods(false).include?(method_name)
        end
      end
    MD

    md :yard, 'rubydoc', <<~MD, lang: :ruby
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
    MD

    md :nokogiri, 'html parse library nokogiri', <<~MD, lang: :ruby
      require 'nokogiri'

      doc = Nokogiri::HTML(html)
      doc.attr('data-title') # Nokogiri::XML::Attr

      doc.css('.search-path')
    MD

    md :set_class, 'simple cheatsheet set class', <<~MD, lang: :ruby
      # create
      s = Set[1,2]
      s = [1, 2].to_set

      # check
      s.inlude? 3 or s.member? 3 or s === 3
    MD

    md :system_call, 'system method', <<~MD, lang: :ruby
      # 스크립트 텍스트를 system으로 편하게 넘기기
      system *%w[kubectl exec -it deploy/name -n bard -- bin/bundle exec rails runner] << <<~'CMD'
        puts Rails.env
      CMD
    MD

    md :awesome_array, 'array features', <<~MD
      # 모든 조합의 수를 만들어주는 함수

      ```ruby
      [1,2,3].combination(1).to_a # => [[1], [2], [3]]
      [1,2,3].combination(2).to_a # => [[1,2], [1,3], [2,3]]
      [1,2,3].combination(3).to_a # => [[1, 2, 3]]
      ```
    MD

    md :source_location, 'where is class, method definition file', <<~MD, lang: :ruby
      # Method#source_location
      User.method(:import).source_location

      # Object#const_source_location
      Object.const_source_location 'User'

      # loaded features grep
      $LOADED_FEATURES.grep /user/
    MD

    md :read_source, 'how to read ruby sources', <<~MD, lang: :ruby
      # 코드 위치 찾기
      User.method(:import).source_location

      # bundle library 폴더 위치 열기
      bundle info activesupport # Path 확인 가능
      bundle open activesupport # 직접 이동

      # irb를 후킹해서 분석
      binding.irb # ruby standard
      binding.pry # pry gem
      byebug # byebug gem

      # 호출 위치 찾기
      puts caller # ruby standard
      backtrace # byebug gem 디버그 도중
      # - byebug 기능을 쓸 수 있다
      #   - up/down (스택 이동)
      #   - frame n (지정 스택으로 이동)
      #   - list (코드 표시)
      #   - set listsize 20 (코드 표시 라인 수 지정)
      #   - editor (편집기 열기)

      # gem 내부에도 수정하며 읽은 다음..
      bundle pristine # Gemfile gem 전부 복원
      bundle exec gem pristine [GEMNAME] --no-extension # 특정 gem만 복원
    MD

    md :snippet_flatten, '#snippet flatten collection struct', <<~MD, lang: :ruby
      def flatten_collection(query, prefix = nil)
        query = query.map.with_index { |v, idx| [idx, v] } if query.is_a? Array

        query.to_a.reduce({}) do |a, (k, v)|
          res = case v
                when Hash, Array
                  flatten_collection(v, "\#{prefix}\#{k}#".to_sym)
                else
                  { "\#{prefix}\#{k}": v }
                end

          a.merge(res)
        end
      end
    MD

    md :run_script, '#cli run script using heredoc', <<~MD, lang: :sh
      ruby <<EOF
        puts 'test'
      EOF

      cat <<EOF | ruby
        puts 'test'
      EOF

      ruby - "$@" << EOF
        puts ARGV
      EOF

      ruby -e 'code' -- argument

      # 라이브러리 안쓸때 시작속도 최대한 올리기
      ruby -e '' --disable=gems
    MD

    md :oneline, '#cli oneliner, like awk', <<~MD, lang: :sh
      # awk와 비교하면 변수할당이나 연산에서 글자수가 늘어나는 단점이 있다
      # 대신 str 유틸, process 실행을 좀 더 쉽게할 수 있다. perl의 온전한 대체로 사용 가능
      ruby -ne 'print if /test/'  # grep
      ruby -pe 'gsub(/:/, "-")'   # sed
      ruby -ane 'print $F[-1]'    # awk
      ruby -ne 'BEGIN{puts "begin"}; END{puts "end"}'    # awk
      ruby -ne 'a = system("ls")' -e 'b = `ls`' -e 'c = %x(ls)'

      # magic variable
      $. = line number

      # 정규식 정렬
      ## case: blablabla blablabla Name=xxxxx blablabla blablabla
      cat input.txt | ruby -e 'puts $stdin.sort_by { _1[/(?<= Name=)\S+/] || "" }'

      # options
      #   -C<dir>  : 해당 디렉토리에서 실행
      #   -l       : 줄바꿈 문자를 자동으로 제거
      #   -n       : $_ 변수에 한줄씩 넣으며 루프
      #   -p       : -n과 동일. 대신 자동으로 $_이 출력된다
      #   -a       : 라인을 split하여 $F에 저장
      #   -F<char> : -a 옵션의 split 문자를 지정
    MD
  end
end
