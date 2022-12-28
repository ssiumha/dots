module Lib
  class RailsChiz < Base
    md :status_code, 'rails status code', <<~MD
      Informational
      100	:continue	Continue
      101	:switching_protocols	Switching Protocols
      102	:processing	Processing
      103	:early_hints	Early Hints

      Success
      200	:ok	Ok
      201	:created	Created
      202	:accepted	Accepted
      203	:non_authoritative_information	Non Authoritative Information
      204	:no_content	No Content
      205	:reset_content	Reset Content
      206	:partial_content	Partial Content
      207	:multi_status	Multi Status
      208	:already_reported	Already Reported
      226	:im_used	IM Used

      Redirection
      300	:multiple_choices	Multiple Choices
      301	:moved_permanently	Moved Permanently
      302	:found	Found
      303	:see_other	See Other
      304	:not_modified	Not Modified
      305	:use_proxy	Use Proxy
      306	:reserved	Reserved
      307	:temporary_redirect	Temporary Redirect
      308	:permanent_redirect	Permanent Redirect

      Client Error
      400	:bad_request	Bad Request
      401	:unauthorized	Unauthorized
      402	:payment_required	Payment Required
      403	:forbidden	Forbidden
      404	:not_found	Not Found
      405	:method_not_allowed	Method Not Allowed
      406	:not_acceptable	Not Acceptable
      407	:proxy_authentication_required	Proxy Authentication Required
      408	:request_timeout	Request Timeout
      409	:conflict	Conflict
      410	:gone	Gone
      411	:length_required	Length Required
      412	:precondition_failed	Precondition Failed
      413	:request_entity_too_large	Request Entity Too Large
      414	:request_uri_too_long	Request Uri Too Long
      415	:unsupported_media_type	Unsupported Media Type
      416	:requested_range_not_satisfiable	Requested Range Not Satisfiable
      417	:expectation_failed	Expectation Failed
      421	:misdirected_request	Misdirected Request
      422	:unprocessable_entity	Unprocessable Entity
      423	:locked	Locked
      424	:failed_dependency	Failed Dependency
      425	:too_early	Too Early
      426	:upgrade_required	Upgrade Required
      428	:precondition_required	Precondition Required
      429	:too_many_requests	Too Many Requests
      431	:request_header_fields_too_large	Request Header Fields Too Large
      451	:unavailable_for_legal_reasons	Unavailable for Legal Reasons

      Server Error
      500	:internal_server_error	Internal Server Error
      501	:not_implemented	Not Implemented
      502	:bad_gateway	Bad Gateway
      503	:service_unavailable	Service Unavailable
      504	:gateway_timeout	Gateway Timeout
      505	:http_version_not_supported	Http Version Not Supported
      506	:variant_also_negotiates	Variant Also Negotiates
      507	:insufficient_storage	Insufficient Storage
      508	:loop_detected	Loop Detected
      509	:bandwidth_limit_exceeded	Bandwidth Limit Exceeded
      510	:not_extended	Not Extended
      511	:network_authentication_required	Network Authentication Required
    MD
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

      # ref: actionpack/action_dispatch/routing/redirection.rb
      render status: :permanent_redirect,
             layout: false,
             body: '...'
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

    md :model, 'active record, tip & tricks', <<~MD
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

      # Table Name
      ```
      class Countries < ActiveRecord::Base
        self.table_name = 'cc'
      end
      ```
      - Model 이름으로 실제 DB Table 이름을 유추하지 않고 직접 지정

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

      # polymorphic

      ```ruby
      class Picture < ApplicationRecord
        belongs_to :imageable, polymorphic :true
      end

      class Employee < ApplicationRecord
        has_many :pictures, as: :imageable
      end

      class Product < ApplicationRecord
        has_many :pictures, as: :imageable
      end
      ```

      - https://guides.rubyonrails.org/association_basics.html#polymorphic-associations
      - Picture에 imageable_id, imageable_type 2개의 참조용 필드를 사용하여 여러 타입의 모델을 참조하는 다형성 관계를 맺을 수 있다


      # saved_change_to
      ```
        after_save_commit :notify_to_slack, if: :saved_change_to_address?
      ```
      - https://api.rubyonrails.org/classes/ActiveRecord/AttributeMethods/Dirty.html
      - attribute가 변경되었는지 여부를 판단할 수 있다
    MD

    md :model_enum, 'active record enum', <<~MD, lang: :ruby
      class DeployModel < ApplicationRecord
        enum deploy_status: %i[init in_progress error done], _prefix: :deploy
      end

      # prefix를 붙임으로 아래와 같이 쓸 수 있다
      deploy_model.deploy_init!
      deploy_model.deploy_init?
    MD

    md :model_validate, 'invalid rescue active model exception', <<~MD, lang: :ruby
      begin SomeTable.first.update! name: 'invalid';
      rescue ActiveRecord::RecordInvalid => i; puts i.record.errors.each{|e|puts e.message}; end
    MD

    md :active_support_tips, 'hidden features', <<~MD
      # ActiveSupport::Configurable
      - 설정 관리용 concern
      - config_accessor 를 통해 변수를 Class 변수로써 관리한다
      - 모든 instance에서 공유된 변수를 가질 수 있다
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

      # application.turbo_stream 사용해보기
      #   # ref: https://github.com/hotwired/turbo-rails/blob/main/app/controllers/turbo/frames/frame_request.rb
      layout -> { 'application' if turbo_frame_request? }
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

    md :helpers, 'xxxHelper', <<~MD, lang: :ruby
      # 주로 view에 관련된 코드를 정리하기 위해 사용한다

      # 기본적으로 한번 정의되면 모든 controller에서 사용할 수 있다
      # 이를 원하지 않는다면 include_all_helpers를 false로 설정해야한다. (default: true)

      # config/application.rb
      config.action_controller.include_all_helpers = false

      # include_all_helpers가 false라면 각 controller마다 사용할 helper를 선언해줘야한다
      # controllers/some_controller.rb
      helper :all

      helper :formatted_time
      helper FormattedTimeHelper
      helper 'formats/time'  # Formats::TimeHelper

      # template 내에서는 바로 호출할 수 있고
      # 각 컨트롤러 내부에서는 helpers.xxx 형태로 함수에 접근할 수 있다
    MD


    md :tag_helper, 'tag.xxx, ActionView::Helpers::TagHelper. erb features', <<~MD, lang: :ruby
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

      # class_names
      <div class="<% if user.admin? %>admin<% end %><% if user.active? %> active <% end %>">
      # 대신 class_names를 쓸 수 있다
      <div class="<%= class_names admin: user.admin?, active: user.active? %>">

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

      프로젝트가 커질수록 느려지는 특성이 좀 있는듯

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
      # https://edgeapi.rubyonrails.org/classes/ActionController/Parameters.html

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

      # uri로 변환
      params.to_query
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

    md :core_ext, 'custom core_ext, lib, autoload', <<~MD, lang: :ruby
      # lib 밑의 경로는 기본 autoload에 포함되지 않는다
      # 사용하려면 `require 'core_ext/string'` 형태로 가져와야한다 (경로에서 lib 부분은 생략)

      # 기본 autoload에 포함시키면 이름 규칙에 따라 사용 시점에 require 시킬 수 있다
      # config/application.rb
      config.autoload_paths << Rails.root.join('lib/core_ext')
      config.eager_load_paths << Rails.root.join('lib/core_ext')


      # 다만 core_ext는 이미 정의된 class를 확장시키는 케이스라
      # initializers에 등록해놓고 사용하는게 맞다

      # config/initializers/extensions.rb | config/initializers/core_exts.rb
      Dir[Rails.root.join('lib/core_ext/**/*.rb')].each { |file| require file }
      Dir[Rails.root.join('lib/ruby_ext/**/*.rb')].each { |file| require file }
      Dir[Rails.root.join('lib/rails_ext/**/*.rb')].each { |file| require file }

      # lib/core_ext/string.rb
      class String
        ...
      end
    MD

    md :path_trick, 'wow', <<~MD, lang: :ruby
      Rails.root.join('app/view') == Rails.root/'app'/'view'
    MD

    md :migrate, 'db migrate, column, alter; create model', <<~MD, lang: :bash
      # table 생성
      rails g migration CreateCountries

      # column 추가
      # string -> varchar(255)에 해당
      rails g migration AddPhoneNumberToUsers phone_number:string:index:unique

      # 대충 이런 느낌으로 정의
      # create_table :posts do |t|
      #   t.references :user, null: false, foreign_key: true
      #   t.string     :name, null: false
      # end
      # add_index :posts, [:user_id, :name], unique: true

      # 적용
      rails db:migrate

      # 취소
      rails db:rollback STEP=1
    MD

    md :datetime, 'rails datetime ext', <<~MD, lang: :ruby
      # ref: activesupport/lib/active_support/core_ext/date_time

      # 현재시각
      DateTime.current #=> Time.zone.now.to_datetime

      # 날짜연산
      DateTime.current - 2.days + 8.hours
      DateTime.current.advance(days: -2, hours: +8)
    MD

    md :lib_n_plus_1, '#bookmark ar_lazy_preload', <<~MD
      - https://github.com/DmitryTsepelev/ar_lazy_preload
      - https://blog.appsignal.com/2018/04/03/russian-doll-caching-in-rails.html

      - 상황에 맞춰 손으로 일일히 includes 하지 말고 ar_lazy_preload 시켜서 N+1 문제를 거의 해결 가능
      - russian doll caching 이랑도 잘 어울린다는듯
    MD

    md :responder, 'custom respond_with', <<~MD
      - https://jtway.co/5-steps-to-add-remote-modals-to-your-rails-app-8c21213b4d0c
      - ActionController::Responder를 사용하여 respond_with 처리를 커스터마이징할 수 있다
    MD

    md :serializer, 'ActiveModel::Serializer', <<~MD
      ```rb
      # app/serializers/chat_serializer.rb
      class ChatSerializer < ActiveModel::Serializer
        attributes :id, :title, :messages

        def messages
          object.messages
        end
      end

      # usage:
        render json: user, serializer: SenderSerializer
        render json: users, each_serializer: SenderSerializer
      ```

      - json 등 model을 다른 포맷으로 변경할 때 사용
      - belongs_to로 관계도 지정할 수 있다
    MD

    md :active_job, 'active job', <<~MD
      rails에서 비동기처리에 사용되는 라이브러리

      특정
      - Model의 method 호출을 비동기로 만들기에 특화
      - Queuing 관리가 은폐. 기본적으로 취소 기구가 없다
      - 스케쥴러가 간략하다
        - 재귀적인 스케쥴링 설정 불가
      - 여러 gem에서 통용될 수 있는 인터페이스 형태
        - backend 고유 기능은 고려되지 않는다

      -> 유저의 web 요청을 받아, 시간이 걸리는 처리(초~분)를 비동기로 처리하고 응답을 반환하는게 주용도

      한계
      - 1대에서 처리 가능한 job을 넘어가면 불안정
        - 장시간 실행하는 처리가 있으면 retry로 조정하기 힘듬
        - redis가 스케일 병목이 된다
      - 여러 단계로 job이 나눠지면 의존 관계 파악이 어렵다
      - sidekiq + redis는 최악의 경우 queue 손실이 일어날 수 있다
    MD

    md :active_admin, 'cheatsheet', <<~MD
      # Update Action 만들기
      ```ruby
      show do
        div id: :div_anchor, style: 'position: relative; top: -20vh;'
        form action: set_value_admin_xxx_path(anchor: :div_anchor),
          style: 'display: inline-block',
          method: :post do |f|
            f.input name: :authenticity_token, type: :hidden, value: form_authenticity_token
            f.input name: :arg1, type: :hidden, value: arg1

            f.button '버튼'
          end
        end
      end

      member_action :set_value, method: :post do
        resource.update! v: params[:arg1]
        redirect_back fallback_location: admin_xxxx_path(resource)
      end
      ```

      - anchor 써서 갱신 후 스크롤 조정해버리기
      - form은 원래 get, post만 지원해줘서 post로 구현. method input field 추가하면 될 것 같긴한데..
      - csrf는 직접 추가..
      - controller에 추가해도 되는데, 기본 기능으로 해결하고 싶었다
    MD

    md :sidekiq_flush_all, 'flush all redis queue', <<~MD, lang: :ruby
      Sidekiq.redis(&:flushdb)
    MD

    md :awesome_libs, 'library, util, list', <<~MD, lang: :ruby
      # model id를 hashid로 표시
      gem 'hashid-rails'
    MD

    md :annotate, 'annotate infos model, routes..', <<~MD, lang: :bash
      # routes.rb나 mobel/xxx.rb에 annotate comment를 추가해놓을 수 있다
    	bundle exec annotate --route --sort --ignore-routes "admin/" --position-in-routes bottom --trace --force
      bundle exec annotate --models --show-migration --show-foreign-keys --complete-foreign-keys --show-indexes -simple-indexes --with-comment --trace --force
    MD

    require_relative './rails/factorybot'
    desc 'factorybot', 'factorybot subcommand'
    subcommand 'factorybot', FactorybotChiz
  end
end
