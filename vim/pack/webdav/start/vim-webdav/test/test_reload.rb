#!/usr/bin/env ruby
# WebDAV :e! (force reload) tests

require_relative 'test_base'

class TestWebDAVReload < TestWebDAVBase

  # Test :e! discarding local unsaved changes
  def test_reload_discard_local_changes
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")
    vim_cmd("WebDAVGet /test/file1.txt")
    wait_for { capture.include?("This is test file content") }

    # Make local modifications (don't save)
    vim_cmd("normal! ggdG")
    vim_cmd("call setline(1, 'Local unsaved changes')")
    sleep 0.1

    # Verify buffer is modified
    vim_cmd("if &modified | echo 'MODIFIED' | else | echo 'NOT_MODIFIED' | endif")
    wait_for_text("MODIFIED", 1)
    assert_includes capture, "MODIFIED", "Buffer should be modified before :e!"

    # Reload with :e! (force discard changes)
    vim_cmd("edit!")
    wait_for_text("This is test file content", 1.5)

    # Verify local changes discarded, server content restored
    vim_cmd("echo getline(1)")
    sleep 0.1
    output = capture
    assert_includes output, "This is test file content", "Should restore original server content"
    refute_includes output, "Local unsaved changes", "Local changes should be discarded"

    # Verify buffer is not modified after reload
    vim_cmd("if &modified | echo 'MODIFIED' | else | echo 'NOT_MODIFIED' | endif")
    wait_for { capture.include?("NOT_MODIFIED") }
  end

  # Test :e! fetching external server modifications
  def test_reload_external_server_modification
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    # Create unique test file
    random_key = rand(100000..999999)
    test_file = "/test/reload_test_#{random_key}.md"

    # Create file on server
    docker_exec("curl -s -X PUT http://localhost:9999#{test_file} -d InitialContent > /dev/null")
    sleep 0.1

    # Open file in vim
    vim_cmd("WebDAVGet #{test_file}")
    wait_for_text("InitialContent", 1.5)
    vim_cmd("echo getline(1)")
    sleep 0.1
    assert_includes capture, "InitialContent", "Should load initial content"

    # Modify file on server (external change)
    docker_exec("curl -s -X PUT http://localhost:9999#{test_file} -d ServerModifiedContent > /dev/null")
    sleep 0.1

    # Reload with :e!
    vim_cmd("edit!")
    wait_for_text("ServerModifiedContent", 1.5)

    # Verify content updated to server version
    vim_cmd("echo getline(1)")
    sleep 0.1
    output = capture
    assert_includes output, "ServerModifiedContent", "Should fetch updated server content"
    refute_includes output, "InitialContent", "Should not have old content"

    # Cleanup
    docker_exec("curl -s -X DELETE http://localhost:9999#{test_file} > /dev/null 2>&1")
  end

end
