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

    md :render_overview, 'render overview', <<~MD
      rails 7 기준

      https://guides.rubyonrails.org/action_view_overview.html

      template으로 erb, builder, jbuilder를 사용 가능

      ActionView::Renderer#render가 주요 진입 지점

        LookupContext -> PathSet -> Resolver -> FileSystemResolver 순으로 처리되며 경로를 탐색한다


      인자에 따라서 각기 다른 renderer를 호출한다

        ActionView::Renderer::CollectionRenderer -> partial:, collection: 인자 사용시
        ActionView::Renderer::ObjectRenderer -> partial:, object: 인자 사용시
        ActionView::Renderer::PartialRenderer -> partial: 사용시,
        ActionView::Renderer::TemplateRenderer -> 그 외 모든 경우
    MD

    md :model, 'active record', <<~MD
      # ActiveRecord::Store
        - https://api.rubyonrails.org/classes/ActiveRecord/Store.html
        - 중요하지 않은 데이터를 JSON serialize 해서 저장하는 기능

      # ActiveSupport::CurrentAttributes
        - app/models/current.rb에 위치시켜 쓰는 편
        - global한 변수를 정의하고 Model 접근하듯이 Current.user로 사용할 수 있다

      # Context Validate
        - 특정 context 범위에서만 적용될 validate를 지정할 수 있다
      ```ruby
        class Account < ApplicationRecord
          validates :username, length: { minimum: 6 }, on: :create
          validates :username, length: { minimum: 3 }, on: :admin
        end

        Account.new(username: 'mds').valid?(:admin)
      ```

      # rails 7 추가 쿼리
      - load_async
      - sole, find_sole_by
      - associated(:association)
        ```ruby
        # Before:
        account.users.joins(:contact).where.not(contact_id: nil)

        # After:
        account.users.where.associated(:contact)
        ```

      # ActiveStorage + variant
      class User < ApplicationRecord
        has_one_attached :avatar do |attachable|
          attachable.variant :thumb, resize: "100x100"
        end
      end

      #Call avatar.variant(:thumb) to get a thumb variant of an avatar:
      <%= image_tag user.avatar.variant(:thumb) %>
    MD

    md :action_controller, 'controller', <<~MD
      # rails 7에서 추가. 파일스트리밍이 가능하다
      send_stream(filename: "subscribers.csv") do |stream|
        stream.write "email_address,updated_at\\n"

        @subscribers.find_each do |subscriber|
          stream.write "\#{subscriber.email_address},\#{subscriber.updated_at}\\n"
        end
      end
    MD

    md :turbo, 'hotwired turbo', <<~MD, lang: :ruby
      # html erb에서의 사용
      # id는 대신 dom_id(@article) 형태로도 쓸 수 있다
      <%= turbo_frame_tag :articles, src: articles_path do %>
        <div>로딩중</div>
      <% end %>

      # format이 turbo_stream 이면 자동으로 show.turbo_stream을 사용한다
      # show.turbo_stream.erb

      # render에서 바로 사용
      render turbo_stream: turbo_stream.remove(xxx)

      # 직접 지정해서 render 할 경우:
      respond_to |format|
        format.turbo_stream do
          render turbo_stream: turbo_stream.update(:bookmark_list, partial: 'shared/news/bookmark_list', locals: locals)
        end
      end

      # js: fetch로 turbo 요청
      fetch(url, {
        headers: {
          Accept: 'tet/vnd.turbo-stream.html'
          'X-CSRF-Token': document.querySelector('meta[name="csrf-token"]').content,
          'Content-Type': 'application/json'
        },
        body: JSON.stringify(params)
      })
      .then(r => r.text())
      .then(html => Turbo.renderStreamMessage(html))
    MD

    md :stimulus, 'hotwired stimulus', <<~MD, lang: :js
      // values
      //  data 값을 변수할당 & 타입변환까지 해준다. Array, Boolean, Number, Object, String 지원
      //  Boolean은 0 이거나 false 일 때 false에 해당하고 Array, Object는 JSON으로 해석된다
      // <div data-controller="loader" data-loader-url-value="/messages"></div>
      class extends Controller {
        static values = {
          url: String ,
          interval: { type: Number, default: 5 }
        }

        connect() {
          this.urlValue
          this.hasUrlValue
        }

        urlValueChanged(current, old) { /* 값 변경시 작동 */ }
      }

      // element를 갖고 다른 컨트롤러 가져오기
      const bookmarkList = this.application.getControllerForElementAndIdentifier(document.querySelector('#bookmark-list'), 'bookmark-list')
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


      # rails 7부터는 attributes로 데이터를 정의할 수 있다
      <input <%= tag.attributes(type: :text, aria: { label: "Search" }) %>>

      # => <input type="text" aria-label="Search" />
    MD

    md :form_helper, 'form_with, form_for, ActionView::Helpers::FormHelper', <<~MD, lang: :ruby
      # 대부분의 브라우저 form은 get, post만 지원하기 때문에,
      #   put, delete 등은 _method를 추가해서 별도로 처리된다
      form_with url: edit_user_path(user), method: :put do |f|
        safe_join [
          f.hidden_field(:name, value: ''),
          f.text_area(:description, value: '', rows: 10, style: 'margin: 16px 0'),
          f.submit('저장')
        ]
      end

      # local: 기본 값은 true. form_with_generates_remote_forms 옵션으로 제어할 수 있다
      #   local: false 가 설정되면 XHR 요청을 보낸다
    MD

    md :caching, '', <<~MD
      russian doll caching
        https://blog.appsignal.com/2018/04/03/russian-doll-caching-in-rails.html
    MD

    md :db_migrate, '', <<~MD, lang: :sh
      # DB의 schema_migrations 테이블에서 버전이 관리된다
      #   select * from schema_migrations

      # 특정 버전 하나만 rollback 시키기
      rails db:migrate:down VERSION=20220624083352
    MD

    md :regex, 'regex, gsub, string', <<~MD, lang: :ruby
      # escape special characters
      str.gsub /[[:punct:]]/, '_'
    MD

    md :sprockets, 'rails assets pipeline', <<~MD
      https://github.com/rails/sprockets
      js, scss 의존성 관리 + bundling 라이브러리

      주석으로 'reuiqre xxx' 표시를 해서 필요한 의존성 파일을 로드한다

      require           -> 단일 파일 추가
      require_self      -> 현재 스크립트 body 순서 정렬용
      require_directory -> 해당 폴더 밑의 모든 파일을 require 처리
      require_tree      -> 서브디렉토리를 재귀적으로 포함시킨다
      link              ->
      stub              -> 특정 require를 무시시킬 때 사용

      rails 7 부터는 bundling 자체를 배제하는 방향으로 가고 있어서 대신 propshaft를 써볼 수도 있다
        https://github.com/rails/propshaft
    MD

    md :params, 'request parameters', <<~MD, lang: :ruby
      # 직접 생성
      ActionController::Parameters.new(key: 'value')

      # 객체 param(array, object)의 name, post_id 가져오기
      params.require(:author).permit(:name, :post_id)

      # permit 되지 않은 params이 존재하면 raise
      # params -> { a: 123, b: 234 }
      params.permit(:c) #-> found unpermitted parameters: :a, :b

      # permit 없이 hash로 고치고 싶다면..
      url_for(params.to_unsafe_h)

      # params hash 가져오기
      request.query_parameters
    MD

    md :debug, 'debugger, pry, erb, binding', <<~MD, lang: :ruby
      # 코드 중간에 debugger를 실행하면 console이 열린다
      debugger

      # binding을 사용하여 pry로 붙을 수 있다 (https://github.com/pry/pry)
      binding.pry
    MD

    md :routes, 'routes cheatsheet', <<~MD, lang: :ruby
      # https://guides.rubyonrails.org/routing.html
      # config/routes.rb

      # GET       /photos           photos#index    display a list of all photos
      # GET       /photos/new       photos#new      return an HTML form for creating a new photo
      # GET       /photos/:id       photos#show     display a specific photo
      # GET       /photos/:id/edit  photos#edit     return an HTML form for editing a photo

      # POST      /photos           photos#create   create a new photo
      # PATCH/PUT /photos/:id       photos#update   update a specific photo
      # DELETE    /photos/:id       photos#destroy  delete a specific photo
    MD

    require_relative './rails/factorybot'
    desc 'factorybot', 'factorybot subcommand'
    subcommand 'factorybot', FactorybotChiz
  end
end
