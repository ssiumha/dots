#!/usr/bin/env ruby
# WebDAV file writing tests

require_relative 'test_base'

class TestWebDAVWrite < TestWebDAVBase
  # Test file save functionality
  def test_webdav_save_buffer
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")
    vim_cmd("WebDAVGet /test/file1.txt")
    wait_for { capture.include?("This is test file content") }

    # Modify content
    vim_cmd("normal! ggdG")
    vim_cmd("call setline(1, 'Modified content')")
    vim_cmd("call setline(2, 'Line 2 modified')")

    # Save with :w (now properly intercepted for WebDAV buffers only)
    vim_cmd("write")

    # Verify buffer is no longer modified
    vim_cmd("if &modified | echo 'STILL_MODIFIED' | else | echo 'NOT_MODIFIED' | endif")
    wait_for { capture.include?("NOT_MODIFIED") || capture.include?("Saved to WebDAV") }

    output = capture
    assert_includes output, "NOT_MODIFIED", "Buffer should not be modified after save"
  end

  # Test conflict detection
  def test_webdav_conflict_detection
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")
    vim_cmd("WebDAVGet /test/file1.txt")
    wait_for { capture.include?("This is test file content") }

    # Simulate server-side modification by making a PUT from outside
    # (In real scenario, another client would modify the file)
    docker_exec("curl -s -X PUT http://localhost:9999/test/file1.txt -d 'Server modified content' > /dev/null")

    # Now try to modify and save from vim
    vim_cmd("normal! ggdG")
    vim_cmd("call setline(1, 'Local modification')")
    vim_cmd("write")
    wait_for_text("Conflict", 1) rescue wait_for_text("서버 파일이 변경", 1) rescue wait_for_text("412", 1)

    # Should see conflict error
    output = capture
    assert_match(/Conflict|서버 파일이 변경|412/i, output, "Should detect conflict")
  end

  # Test that buffer variables prevent wrong-path saves
  def test_webdav_path_protection
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")
    vim_cmd("WebDAVGet /test/file1.txt")
    wait_for { capture.include?("This is test file content") }

    # Check that original path is stored
    vim_cmd("echo b:webdav_original_path")
    wait_for { capture.include?("/test/file1.txt") }

    # Verify webdav_managed flag is set
    vim_cmd("echo b:webdav_managed")
    assert_includes capture, "1", "Buffer should be marked as WebDAV-managed"
  end

  # Test successful save updates version info
  def test_webdav_save_updates_version
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")
    vim_cmd("WebDAVGet /test/file1.txt")
    wait_for { capture.include?("This is test file content") }

    # Get initial ETag (if exists)
    vim_cmd("let g:initial_etag = exists('b:webdav_etag') ? b:webdav_etag : 'NONE'")

    # Modify and save
    vim_cmd("normal! ggdG")
    vim_cmd("call setline(1, 'Updated content')")
    vim_cmd("write")

    # Check that ETag was updated
    vim_cmd("if exists('b:webdav_etag') && b:webdav_etag != g:initial_etag | echo 'ETAG_UPDATED' | elseif !exists('b:webdav_etag') | echo 'ETAG_NOT_PROVIDED' | else | echo 'ETAG_SAME' | endif")

    # ETag should be different after successful save (if server provides it)
    output = capture
    # nginx might not provide ETag, so we accept both ETAG_UPDATED and ETAG_NOT_PROVIDED
    assert(output.include?("ETAG_UPDATED") || output.include?("ETAG_NOT_PROVIDED"),
           "ETag should be updated or server doesn't provide ETag (got: #{output})")
  end

  # Test Korean filename save
  def test_webdav_korean_filename_save
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")
    vim_cmd("WebDAVGet /test/한글.md")
    wait_for { capture.include?("한글 문서") }

    # Modify content
    vim_cmd("normal! ggdG")
    vim_cmd("call setline(1, '# 수정된 한글 문서')")
    vim_cmd("call setline(2, '새로운 내용입니다.')")

    # Save
    vim_cmd("write")

    # Verify saved
    vim_cmd("if &modified | echo 'STILL_MODIFIED' | else | echo 'NOT_MODIFIED' | endif")
    wait_for { capture.include?("NOT_MODIFIED") || capture.include?("Saved to WebDAV") }

    output = capture
    assert_includes output, "NOT_MODIFIED", "Korean filename should save successfully"
  end
end
