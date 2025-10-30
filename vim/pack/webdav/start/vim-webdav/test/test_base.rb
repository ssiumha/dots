#!/usr/bin/env ruby
# Base test class with common setup/teardown for WebDAV plugin tests

require 'minitest/autorun'
require 'securerandom'
require_relative 'tmux_helper'
require_relative 'screen_helper'

class TestWebDAVBase < Minitest::Test
  include TmuxHelper
  include ScreenHelper

  def setup
    @container = "vim-webdav-test-#{SecureRandom.hex(4)}"

    # Start container
    system("docker run --rm -d --name #{@container} vim-webdav-test", out: '/dev/null')
    wait_for_container

    # Start mock server
    docker_exec("cd /root/.vim/pack/webdav/start/vim-webdav/test && ruby mock-server.rb > /dev/null 2>&1 &")
    wait_for_server

    # Create tmux session
    docker_exec("tmux new-session -d -s test")
  end

  def wait_for_container(timeout = 2)
    start = Time.now
    while Time.now - start < timeout
      output = docker_exec("echo 'ready'")
      return if output.strip == "ready"
      sleep 0.05
    end
    raise "Container not ready"
  end

  def wait_for_server(timeout = 2)
    start = Time.now
    while Time.now - start < timeout
      output = docker_exec("curl -s -o /dev/null -w '%{http_code}' http://localhost:9999 || echo '000'")
      return if output.strip != "000"
      sleep 0.05
    end
    raise "Mock server not ready"
  end

  def teardown
    system("docker rm -f #{@container}", out: '/dev/null')
  end
end
