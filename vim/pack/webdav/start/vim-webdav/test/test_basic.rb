#!/usr/bin/env ruby
# Basic plugin functionality tests

require_relative 'test_base'

class TestWebDAVBasic < TestWebDAVBase
  # Test plugin loads
  def test_plugin_loads
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")
    vim_cmd("if exists('g:loaded_webdav') | echo 'LOADED' | endif")
    assert_includes capture, "LOADED"
  end

  # Test commands exist
  def test_commands_exist
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")
    vim_cmd("if exists(':WebDAVList') == 2 | echo 'LIST_EXISTS' | endif")
    assert_includes capture, "LIST_EXISTS"

    vim_cmd("if exists(':WebDAVGet') == 2 | echo 'GET_EXISTS' | endif")
    assert_includes capture, "GET_EXISTS"
  end
end
