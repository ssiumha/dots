module Lib
  class RailsChiz
    class FactorybotChiz < Base
      md :ignore_uniq, 'how to duplicate field factory model', <<~MD, lang: :ruby
        # - initialize_with를 사용할 경우, build로 데이터를 만들어도 model이 생성되는 문제가 있다
        # - create로 넘어올 필드에 대해 일일히 적용해야만 한다
        #   - ex) create(:user, age: 3) # 밑의 코드에서 age는 적용 안되는 상태
        factory :user do
          to_create do |instance|
            instance.id = User.find_or_create_by(name: instance.name).id
            instance.reload
          end
        end
      MD
    end
  end
end
