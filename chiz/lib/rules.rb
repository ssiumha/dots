module Lib
  class RulesChiz < Base
    def self.ascii
      require 'cgi'
      table = 32.upto(127).map do
        [
          "#{_1}",
          "#{_1.to_s(16)}".upcase,
          "#{CGI.escape _1.chr.to_s}",
          "#{_1.chr.to_s}"
        ]
      end

      format = '%-4s %-4s %-4s %-4s'
      header = ['Dec', 'Hex', 'Uri', 'Chr']

      table.map
           .with_index { [(_2 % 16 == 0) && ['', format % header], format % _1] }
           .flatten
           .reject { _1 == false }
           .slice(1..-1)
           .join("\n")
    end
    md :ascii, 'ascii table', ascii, lang: :txt

    md :semver, 'semantic version', <<~MD
      https://semver.org/
      https://0ver.org/

      Major.Minor.Patch-pre.release+build 형태의 버전 표기

      patch -> 버그 수정
      minor -> 이전 버전과 호환 되는 새 기능, 개선
      major -> 이전 버전과 호환될 수 없는 변화

      1.0.0-alpha+sha.5114f85 등의 형태로 쓸 수 있다

      build는 메타정보로 버전에 아무 영향을 끼치지 않는다

      prerelease는 영문자 -> 숫자 순으로 우선도를 갖는다
        1.0.0-alpha <
        1.0.0-alpha.beta <
        1.0.0-beta.2 <
        1.0.0-beta.11 <
        1.0.0

      npm에서는..
        - 캐럿은 MAJOR version이 바뀌는게 아니면 호환이 된다 가정하고 최대한 올린다
          - ^1.2.0 : >=1.2.0 <2.0
          - ^1.0 : >=1.0.0 <2.0
        - 틸드는 마지막 파트만 올리도록 동작한다
          - ~0.0.1 : >=0.0.1 <0.1.0
          - ~0.1.1 : >=0.1.1 <0.2.0
    MD
  end
end
