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
  end
end
