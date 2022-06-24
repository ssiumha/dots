module Lib
  class RailsChiz < Base
    md :render, 'render snippets', <<~MD
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
    MD

    md :model, '', <<~MD
      ActiveRecord::Store
        - https://api.rubyonrails.org/classes/ActiveRecord/Store.html
        - 중요하지 않은 데이터를 JSON serialize 해서 저장하는 기능

      ActiveSupport::CurrentAttributes
        - app/models/current.rb에 위치시켜 쓰는 편
        - global한 변수를 정의하고 Model 접근하듯이 Current.user로 사용할 수 있다
    MD

    md :in?, 'in?, check include', <<~MD
      1.in? 1..10 # => true
    MD

    md :concern, 'getting started concern', <<~MD
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

    md :tag_helper, 'tag.xxx, ActionView::Helpers::TagHelper', <<~MD, lang: :ruby
      # erb 안쓰고 코드에서 바로 리턴시키거나, class 조작이 복잡할 때 사용
      tag.div tag.p('Hello World') # => <div><p>Hello world!</p></div>

      # children으로 추가하기
      #   safe_join == join.safe_html
      tag.div do
        safe_join [
          tag.a('링크', href: 'example.com'),
          tag.label('설명')
        ]
      end
    MD

    md :form_helper, 'form_with, form_for, ActionView::Helpers::FormHelper', <<~MD
      # 대부분의 브라우저 form은 get, post만 지원하기 때문에,
      #   put, delete 등은 _method를 추가해서 별도로 처리된다
      form_with url: edit_user_path(user), method: :put do |f|
        safe_join [
          f.hidden_field(:name, value: ''),
          f.text_area(:description, value: '', rows: 10, style: 'margin: 16px 0'),
          f.submit('저장')
        ]
      end
    MD

    md :caching, '', <<~MD
      russian doll caching
        https://blog.appsignal.com/2018/04/03/russian-doll-caching-in-rails.html
    MD

    desc 'factorybot', 'factorybot subcommand'
    subcommand 'factorybot', Class.new(Base, &proc {
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
    })
  end
end
