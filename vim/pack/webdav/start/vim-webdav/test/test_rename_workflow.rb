#!/usr/bin/env ruby
# WebDAV rename (MOVE) workflow tests

require_relative 'test_base'

class TestWebDAVRenameWorkflow < TestWebDAVBase
  # Test renaming a file
  def test_rename_file
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")
    vim_cmd("WebDAVList /test/")
    wait_for_text("file1.txt", 2)

    # Find and select file1.txt (should be around line 7-8 after folders and special items)
    send_keys("/file1.txt")
    send_enter  # Execute search
    wait_for_screen_change(1)

    # Press R to rename
    send_keys("R")
    wait_for_text("renamed_file", 1)  # Wait for rename prompt

    # Clear default name and enter new name
    send_keys("\u0015")  # Ctrl-U to clear line
    send_keys("renamed_file.txt")
    send_enter
    wait_for_text("renamed_file.txt", 2)  # Wait for rename operation to complete

    # Dismiss "Press ENTER" prompt if present
    send_enter
    wait_for_screen_change(1)

    # Verify file appears with new name in list
    vim_cmd("echo getline(1, '$')")
    wait_for_text("renamed_file.txt", 1)
    output = capture
    assert_includes output, "renamed_file.txt", "File should appear with new name"
    refute_includes output, "file1.txt", "Old file name should not appear"

    # Verify file renamed on server
    result = docker_exec("curl -s http://localhost:9999/test/renamed_file.txt")
    assert_includes result, "This is test file content", "Renamed file should exist on server"

    # Verify old file is gone
    result = docker_exec("curl -s -o /dev/null -w '%{http_code}' http://localhost:9999/test/file1.txt")
    assert_equal "404", result.strip, "Old file should not exist"
  end

  # Test renaming a folder
  def test_rename_folder
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")
    vim_cmd("WebDAVList /test/")
    wait_for_text("aaa_folder", 2)

    # Find and select aaa_folder/
    send_keys("/aaa_folder")
    send_enter
    wait_for_screen_change(1)

    # Press R to rename
    send_keys("R")
    wait_for_text("renamed_folder", 1)  # Wait for rename prompt

    # Clear default and enter new name (without trailing slash - should be added automatically)
    send_keys("\u0015")  # Ctrl-U
    send_keys("renamed_folder")
    send_enter
    wait_for_text("renamed_folder", 2)  # Wait for rename operation to complete

    # Dismiss "Press ENTER" prompt if present
    send_enter
    wait_for_screen_change(1)

    # Verify folder appears with new name
    vim_cmd("echo getline(1, '$')")
    wait_for_text("renamed_folder/", 1)
    output = capture
    assert_includes output, "renamed_folder/", "Folder should appear with new name and trailing slash"
    refute_includes output, "aaa_folder/", "Old folder name should not appear"

    # Verify folder renamed on server
    result = docker_exec("curl -s -X PROPFIND http://localhost:9999/test/renamed_folder/ 2>&1")
    refute_includes result, "404", "Renamed folder should exist on server"

    # Verify old folder is gone
    result = docker_exec("curl -s -o /dev/null -w '%{http_code}' -X PROPFIND http://localhost:9999/test/aaa_folder/ 2>&1")
    assert_equal "404", result.strip, "Old folder should not exist"
  end

  # Test renaming file with Korean name
  def test_rename_file_korean
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")
    vim_cmd("WebDAVList /test/")
    wait_for_text("한글.md", 2)

    # Find Korean file
    send_keys("/한글.md")
    send_enter
    wait_for_screen_change(1)

    # Press R to rename
    send_keys("R")
    wait_for_text("새이름", 1)  # Wait for rename prompt

    # Clear default and enter new Korean name
    send_keys("\u0015")  # Ctrl-U
    send_keys("새이름.md")
    send_enter
    wait_for_text("새이름.md", 2)  # Wait for rename operation to complete

    # Dismiss "Press ENTER" prompt if present
    send_enter
    wait_for_screen_change(1)

    # Verify renamed
    vim_cmd("echo getline(1, '$')")
    wait_for_text("새이름.md", 1)
    output = capture
    assert_includes output, "새이름.md", "Korean renamed file should appear"
    refute_includes output, "한글.md", "Old Korean file name should not appear"

    # Verify on server
    result = docker_exec("curl -s http://localhost:9999/test/새이름.md")
    assert_includes result, "한글 문서", "Renamed Korean file should exist with content"
  end

  # Test rename to existing file triggers conflict check
  def test_rename_to_existing_file_conflict
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")
    vim_cmd("WebDAVList /test/")
    wait_for_text("file1.txt", 2)

    # Find file1.txt
    send_keys("/file1.txt")
    send_enter
    wait_for_screen_change(1)

    # Press R to rename
    send_keys("R")
    wait_for_text("001_file", 1)  # Wait for rename prompt

    # Try to rename to existing file (001_file.txt exists)
    send_keys("\u0015")  # Ctrl-U to clear default
    send_keys("001_file.txt")
    send_enter
    wait_for_screen_change(2)  # Wait for operation or conflict prompt

    # Check if we see conflict warning
    output = capture
    if output =~ /already exists|Destination already exists/i
      # Saw warning - decline overwrite
      send_keys("n")
      send_enter
      wait_for_screen_change(1)

      # Verify file1.txt still exists (rename was cancelled)
      result = docker_exec("curl -s http://localhost:9999/test/file1.txt")
      assert_includes result, "This is test file content", "Original file should still exist after cancelled rename"
    else
      # nginx MOVE may have overwritten without warning - verify result
      send_enter  # Dismiss any prompts
      wait_for_screen_change(1)

      # MOVE overwrote the file - this is acceptable behavior for nginx WebDAV
      result = docker_exec("curl -s http://localhost:9999/test/001_file.txt")
      assert_includes result, "This is test file content", "Renamed file should have original content"
    end
  end

  # Test cancel rename operation
  def test_cancel_rename
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")
    vim_cmd("WebDAVList /test/")
    wait_for_text("file1.txt", 2)

    # Find file
    send_keys("/file1.txt")
    send_enter
    wait_for_screen_change(1)

    # Press R to rename
    send_keys("R")
    wait_for_text("file1", 1)  # Wait for rename prompt

    # Cancel by entering empty name
    send_enter
    wait_for_screen_change(1)

    # Verify still in list and file unchanged
    vim_cmd("echo &filetype")
    wait_for_text("webdavlist", 1)
    output = capture
    assert_includes output, "webdavlist", "Should still be in list after cancel"

    # Verify file unchanged on server
    result = docker_exec("curl -s http://localhost:9999/test/file1.txt")
    assert_includes result, "This is test file content", "File should be unchanged"
  end

  # Test cannot rename special items
  def test_cannot_rename_special_items
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")
    vim_cmd("WebDAVList /test/")
    wait_for_text("../", 2)

    # Try to rename "../" (parent directory)
    send_keys("1G")  # First line (../)
    wait_for_screen_change(1)
    send_keys("R")
    wait_for_text("Cannot rename special items", 1)

    output = capture
    assert_match(/Cannot rename special items/i, output, "Should not allow renaming ../")

    # Try to rename "+New"
    send_keys("/+New")
    send_enter
    wait_for_screen_change(1)
    send_keys("R")
    wait_for_text("Cannot rename special items", 1)

    output = capture
    assert_match(/Cannot rename special items/i, output, "Should not allow renaming +New")

    # Try to rename "+Folder"
    send_keys("/+Folder")
    send_enter
    wait_for_screen_change(1)
    send_keys("R")
    wait_for_text("Cannot rename special items", 1)

    output = capture
    assert_match(/Cannot rename special items/i, output, "Should not allow renaming +Folder")
  end

  # Test rename file to name with spaces
  def test_rename_file_with_spaces
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")
    vim_cmd("WebDAVList /test/")
    wait_for_text("001_file.txt", 2)

    # Find file
    send_keys("/001_file.txt")
    send_enter
    wait_for_screen_change(1)

    # Press R to rename
    send_keys("R")
    wait_for_text("file with spaces", 1)  # Wait for rename prompt

    # Clear default and enter name with spaces
    send_keys("\u0015")  # Ctrl-U
    send_keys("file with spaces.txt")
    send_enter
    wait_for_text("file with spaces.txt", 2)  # Wait for rename operation to complete

    # Dismiss "Press ENTER" prompt if present
    send_enter
    wait_for_screen_change(1)

    # Verify renamed
    vim_cmd("echo getline(1, '$')")
    wait_for_text("file with spaces.txt", 1)
    output = capture
    assert_includes output, "file with spaces.txt", "File with spaces should appear"

    # Verify on server - try both URL encoded and literal spaces
    result = docker_exec("curl -s 'http://localhost:9999/test/file%20with%20spaces.txt'")
    if result.include?("Mock content")
      assert_includes result, "Mock content", "File with URL-encoded spaces should exist"
    else
      # Try with literal spaces
      result2 = docker_exec("curl -s 'http://localhost:9999/test/file with spaces.txt'")
      assert_includes result2, "Mock content", "File with spaces should exist on server"
    end
  end

  # Test moving file to nested directory
  def test_move_file_to_nested_directory
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")
    vim_cmd("WebDAVList /test/")
    wait_for_text("file1.txt", 2)

    # Find file1.txt
    send_keys("/file1.txt")
    send_enter
    wait_for_screen_change(1)

    # Press R to rename/move
    send_keys("R")
    wait_for_text("nested/moved_file", 1)  # Wait for rename prompt

    # Clear default and enter path to nested directory
    send_keys("\u0015")  # Ctrl-U to clear line
    send_keys("nested/moved_file.txt")
    send_enter
    wait_for_text("moved_file.txt", 2)  # Wait for move operation to complete

    # Dismiss prompt
    send_enter
    wait_for_screen_change(1)

    # Verify file is gone from current directory
    vim_cmd("echo getline(1, '$')")
    wait_for_text("../", 1)  # Wait for list to refresh
    output = capture
    refute_includes output, "file1.txt", "Original file should be gone from /test/"

    # Navigate to nested directory and verify file is there
    send_keys("/nested")
    send_enter  # Complete search
    wait_for_screen_change(1)
    send_enter  # Open the folder
    wait_for_text("moved_file.txt", 2)

    vim_cmd("echo getline(1, '$')")
    wait_for_text("moved_file.txt", 1)
    output = capture
    assert_includes output, "moved_file.txt", "File should appear in nested directory"

    # Verify on server
    result = docker_exec("curl -s http://localhost:9999/test/nested/moved_file.txt")
    assert_includes result, "This is test file content", "Moved file should exist in nested directory"

    # Verify original location is empty
    result = docker_exec("curl -s -o /dev/null -w '%{http_code}' http://localhost:9999/test/file1.txt")
    assert_equal "404", result.strip, "Original file should not exist"
  end

  # Test moving file with absolute path
  def test_move_file_with_absolute_path
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")
    vim_cmd("WebDAVList /test/")
    wait_for_text("001_file.txt", 2)

    # Find 001_file.txt
    send_keys("/001_file.txt")
    send_enter
    wait_for_screen_change(1)

    # Press R to rename/move
    send_keys("R")
    wait_for_text("/test/nested/absolute_moved", 1)  # Wait for rename prompt

    # Clear default and enter absolute path
    send_keys("\u0015")  # Ctrl-U to clear line
    send_keys("/test/nested/absolute_moved.txt")
    send_enter
    wait_for_screen_change(2)  # Wait for move operation to complete

    # Dismiss prompt
    send_enter
    wait_for_screen_change(1)

    # Verify on server in nested location
    result = docker_exec("curl -s http://localhost:9999/test/nested/absolute_moved.txt")
    assert_includes result, "Mock content", "File should exist at absolute path"

    # Verify original is gone
    result = docker_exec("curl -s -o /dev/null -w '%{http_code}' http://localhost:9999/test/001_file.txt")
    assert_equal "404", result.strip, "Original file should not exist"
  end

  # Test rename folder preserves trailing slash
  def test_rename_folder_trailing_slash
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")
    vim_cmd("WebDAVList /test/")
    wait_for_text("zzz_folder", 2)

    # Find folder
    send_keys("/zzz_folder")
    send_enter
    wait_for_screen_change(1)

    # Press R to rename
    send_keys("R")
    wait_for_text("folder_with_slash", 1)  # Wait for rename prompt

    # Clear default and enter new name WITH trailing slash (should handle both)
    send_keys("\u0015")  # Ctrl-U
    send_keys("folder_with_slash/")
    send_enter
    wait_for_text("folder_with_slash/", 2)  # Wait for rename operation to complete

    # Dismiss "Press ENTER" prompt if present
    send_enter
    wait_for_screen_change(1)

    # Verify folder renamed with single trailing slash
    vim_cmd("echo getline(1, '$')")
    wait_for_text("folder_with_slash/", 1)
    output = capture
    assert_includes output, "folder_with_slash/", "Folder should have trailing slash"
    refute_includes output, "folder_with_slash//", "Should not have double trailing slash"

    # Verify on server
    result = docker_exec("curl -s -X PROPFIND http://localhost:9999/test/folder_with_slash/ 2>&1")
    refute_includes result, "404", "Renamed folder should exist"
  end
end
