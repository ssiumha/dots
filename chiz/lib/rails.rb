module Lib
  class RailsChiz < Base
    desc 'render', 'render snippets'
    def render
      puts <<~END
        render 'products/show'
        render template: 'products/show'
        render :edit
        render action: :edit
        render inline: "<% ... %>"
        render plain: "OK"
        render html: helpers.tag.strong('Not Found')
        render json: @product
        render xml: @product
        render js: "alert('Hello Rails');"
        render body: "raw"
        render file: "\#{Rails.root}/public/404.html", layout: false
      END
    end

    desc 'model', ''
    def model
      puts doc(<<~END)
        # ActiveRecord::Store
        #   - https://api.rubyonrails.org/classes/ActiveRecord/Store.html
        #   - 중요하지 않은 데이터를 JSON serialize 해서 저장하는 기능
      END
    end

    desc 'in?', 'in?, check include'
    def check_include
      puts doc(<<~END)
        1.in? 1..10
        # => true
      END
    end

    md 'concern', 'getting started concern', <<~MD
      model, controller에서 사용되는 기능을 관심사 단위로 나눈 것
        - app/models/concerns/, controllers/concerns/
        - app/models/some_model/some_concern.rb -> 이런 형태로 작성하여 모델에 한정된 관심사를 격리할 수도 있다

      Kent beck, Lots of little pieces—Good code invariably has small methods and small objects
        작은 객체들이 서로 메시지를 주고 받으며 복잡한 일을 할 수 있게 하는게 좋다

      ```ruby
        class Post < AR::Base
          include Mentions
          ...
        end

        ## /app/models/post/mentions.rb
        module Post::Mentions
          extends AS::Concern
          ...
        end
      ```
    MD

    desc 'factorybot', 'factorybot subcommand'
    subcommand 'factorybot', Class.new(Base, &proc {
      desc 'ignore uniq', 'how to duplicate field factory model'
      def ignore_uniq
        puts doc(<<~END)
          # - initialize_with를 사용할 경우, build로 데이터를 만들어도 model이 생성되는 문제가 있다
          # - create로 넘어올 필드에 대해 일일히 적용해야만 한다
          #   - ex) create(:user, age: 3) # 밑의 코드에서 age는 적용 안되는 상태
          factory :user do
            to_create do |instance|
              instance.id = User.find_or_create_by(name: instance.name).id
              instance.reload
            end
          end
        END
      end
    })
  end
end
