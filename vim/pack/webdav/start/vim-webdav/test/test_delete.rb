#!/usr/bin/env ruby
# WebDAV DELETE tests

require_relative 'test_base'

class TestWebDAVDelete < TestWebDAVBase
  # Test deleting a file
  def test_delete_file
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999", "WEBDAV_TEST_MODE" => "1")
    vim_cmd("WebDAVList /test/")
    wait_for_text("file1.txt")  # Wait for list to load

    # Create a test file to delete
    random_key = rand(100000..999999)
    test_file = "delete_test_#{random_key}.txt"
    docker_exec("curl -s -X PUT http://localhost:9999/test/#{test_file} -d TestContent > /dev/null")

    # Refresh list to see new file
    vim_cmd("WebDAVList /test/")
    wait_for_text(test_file)  # Wait for new file to appear

    # Find the test file
    send_keys("/#{test_file}")
    send_enter
    wait_for_text(test_file)  # Wait for cursor to move

    # Ensure we're in normal mode and press D to delete
    send_keys("\e")  # Escape to normal mode
    send_keys("D")

    # Wait for deletion and list refresh
    send_enter
    wait_until_gone(test_file, 2)  # Wait for file to disappear from list

    # Verify file is gone from list
    vim_cmd("echo getline(1, '$')")
    output = capture
    refute_includes output, test_file, "File should be removed from list"

    # Verify file is deleted on server
    result = docker_exec("curl -s -o /dev/null -w '%{http_code}' http://localhost:9999/test/#{test_file}")
    assert_equal "404", result.strip, "File should be deleted on server"
  end

  # Test deleting an empty folder
  def test_delete_empty_folder
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999", "WEBDAV_TEST_MODE" => "1")
    vim_cmd("WebDAVList /test/")
    wait_for_text("file1.txt")  # Wait for list to load

    # Create an empty folder
    random_key = rand(100000..999999)
    test_folder = "empty_folder_#{random_key}/"
    docker_exec("curl -s -X MKCOL http://localhost:9999/test/#{test_folder} > /dev/null")

    # Refresh list
    vim_cmd("WebDAVList /test/")
    wait_for_text(test_folder)  # Wait for new folder to appear

    # Find the folder
    send_keys("/#{test_folder}")
    send_enter
    wait_for_text(test_folder)  # Wait for cursor to move

    # Ensure we're in normal mode and press D to delete
    send_keys("\e")  # Escape to normal mode
    send_keys("D")
    wait_for_text("Checking if folder", 1.5)  # Wait for empty check

    # Wait for deletion and list refresh
    send_enter
    wait_until_gone(test_folder, 2)  # Wait for folder to disappear

    # Verify folder is gone
    vim_cmd("echo getline(1, '$')")
    output = capture
    refute_includes output, test_folder, "Empty folder should be deleted"

    # Verify on server
    result = docker_exec("curl -s -o /dev/null -w '%{http_code}' -X PROPFIND http://localhost:9999/test/#{test_folder} 2>&1")
    assert_equal "404", result.strip, "Folder should be deleted on server"
  end

  # Test that deleting non-empty folder fails
  def test_cannot_delete_nonempty_folder
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999", "WEBDAV_TEST_MODE" => "1")
    vim_cmd("WebDAVList /test/")
    wait_for_text("file1.txt")  # Wait for list to load

    # Create folder with content
    random_key = rand(100000..999999)
    test_folder = "nonempty_folder_#{random_key}/"
    docker_exec("curl -s -X MKCOL http://localhost:9999/test/#{test_folder} > /dev/null")
    docker_exec("curl -s -X PUT http://localhost:9999/test/#{test_folder}file.txt -d Content > /dev/null")

    # Refresh list
    vim_cmd("WebDAVList /test/")
    wait_for_text(test_folder)  # Wait for new folder to appear

    # Find the folder
    send_keys("/#{test_folder}")
    send_enter
    wait_for_text(test_folder)  # Wait for cursor to move

    # Ensure we're in normal mode and press D to delete
    send_keys("\e")  # Escape to normal mode
    send_keys("D")

    # Wait for error message about non-empty folder
    wait_for_text("not empty", 2)
    output = capture
    assert_match(/not empty|Delete contents first/i, output, "Should reject deletion of non-empty folder")

    # Verify folder still exists
    result = docker_exec("curl -s -X PROPFIND http://localhost:9999/test/#{test_folder} 2>&1")
    refute_includes result, "404", "Folder should still exist"

    # Cleanup
    docker_exec("curl -s -X DELETE http://localhost:9999/test/#{test_folder}file.txt > /dev/null 2>&1")
    docker_exec("curl -s -X DELETE http://localhost:9999/test/#{test_folder} > /dev/null 2>&1")
  end

  # Test cannot delete special items
  def test_cannot_delete_special_items
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999", "WEBDAV_TEST_MODE" => "1")
    vim_cmd("WebDAVList /test/")
    wait_for_text("../")  # Wait for list to load

    # Try to delete "../"
    send_keys("1G")  # First line (../)
    wait_for_text("../")
    send_keys("\e")  # Ensure normal mode
    send_keys("D")
    wait_for_text("Cannot delete", 1)

    output = capture
    assert_match(/Cannot delete special items/i, output, "Should not allow deleting ../")

    # Try to delete "+New"
    send_keys("/+New")
    send_enter
    wait_for_text("+New")
    send_keys("\e")  # Ensure normal mode
    send_keys("D")
    wait_for_text("Cannot delete", 1)

    output = capture
    assert_match(/Cannot delete special items/i, output, "Should not allow deleting +New")

    # Try to delete "+Folder"
    send_keys("/+Folder")
    send_enter
    wait_for_text("+Folder")
    send_keys("\e")  # Ensure normal mode
    send_keys("D")
    wait_for_text("Cannot delete", 1)

    output = capture
    assert_match(/Cannot delete special items/i, output, "Should not allow deleting +Folder")
  end

  # Test cancel deletion
  def test_cancel_delete
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")
    vim_cmd("WebDAVList /test/")
    wait_for_text("file1.txt")  # Wait for list to load

    # Find file1.txt
    send_keys("/file1.txt")
    send_enter
    wait_for_text("file1.txt")  # Wait for cursor to move

    # Ensure we're in normal mode and press D to delete
    send_keys("\e")  # Escape to normal mode
    send_keys("D")
    wait_for_text("Delete", 1)  # Wait for confirmation dialog

    # Cancel (n = No)
    send_keys("n")
    send_enter
    wait_for_text("file1.txt")  # Wait for list to reappear

    # Verify file still exists
    result = docker_exec("curl -s http://localhost:9999/test/file1.txt")
    assert_includes result, "This is test file content", "File should still exist after cancel"
  end

  # Test deleting Korean filename
  def test_delete_korean_file
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999", "WEBDAV_TEST_MODE" => "1")
    vim_cmd("WebDAVList /test/")
    wait_for_text("한글.md")  # Wait for list to load

    # Create test file with Korean name
    random_key = rand(100000..999999)
    test_file = "테스트파일#{random_key}.txt"
    # URL encode Korean characters
    encoded_file = test_file.encode('UTF-8')
    docker_exec("curl -s -X PUT 'http://localhost:9999/test/#{encoded_file}' -d KoreanContent > /dev/null")

    # Refresh list
    vim_cmd("WebDAVList /test/")
    wait_for_text(test_file)  # Wait for new file to appear

    # Find the Korean file
    send_keys("/#{test_file}")
    send_enter
    wait_for_text(test_file)  # Wait for cursor to move

    # Ensure we're in normal mode and press D to delete
    send_keys("\e")  # Escape to normal mode
    send_keys("D")

    # Wait for deletion and list refresh
    send_enter
    wait_until_gone(test_file, 2)  # Wait for file to disappear

    # Verify deleted
    vim_cmd("echo getline(1, '$')")
    output = capture
    refute_includes output, test_file, "Korean file should be deleted"
  end
end
