require 'net/http'
require 'uri'

module MyLib
  class <<self
    # @param service [String] ex) service/elasticsearch-coordinating-only
    # @param port [String] ex) 9100:9200
    # @param namespace [String] ex) default
    # @param context [String] ex) arn:aws:eks:ap-northeast-2:xxxxx:cluster/xxxx
    def with_kube_pf(port:, service:, namespace:, context: nil)
      args = "--context #{context}" unless context

      r, w = IO.pipe
      pid = spawn "kubectl #{args} port-forward -n#{namespace} #{service} #{port}",
                  out: w, err: [:child, :out]

      sleep 0.1 until r.readline.tap { STDERR.puts _1 } =~ /Forwarding/
        w.close

      yield
    ensure
      Process.kill 'KILL', pid
    end

    def req_http(method, path, params, port: 80, url: 'localhost')
      uri = URI(path).tap { _1.query = URI.encode_www_form(params) unless params.empty?; puts _1.to_s }
      req = case method
            when :get then Net::HTTP::Get.new(uri.to_s)
            when :post then Net::HTTP::Post.new(uri.to_s)
            end
      req.body = yield if block_given?
      req['Content-Type'] = 'application/json'
      Net::HTTP.new(url, port).request(req).body
    end
  end
end
