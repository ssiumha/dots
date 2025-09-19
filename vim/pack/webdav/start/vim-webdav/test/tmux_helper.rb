# Helper methods for tmux + Docker testing
module TmuxHelper
  def docker_exec(cmd)
    `docker exec #{@container} sh -c '#{cmd}'`
  end

  def send_keys(keys)
    docker_exec("tmux send-keys -t test \"#{keys}\"")
  end

  def send_enter
    docker_exec("tmux send-keys -t test C-m")
  end

  def vim_cmd(cmd)
    send_keys(":")
    send_keys(cmd)
    send_enter
  end

  def clear_screen
    docker_exec("tmux send-keys -t test C-l")  # Ctrl+L to clear
    sleep 0.1
  end

  def capture
    docker_exec("tmux capture-pane -t test -p").strip
  end

  def start_vim(env = {})
    env_str = env.map { |k, v| "#{k}=#{v}" }.join(" ")
    send_keys("#{env_str} vim")
    send_enter
    wait_for { capture.include?("VIM") || capture.include?("~") }
  end

  def wait_for(timeout = 1)
    start = Time.now
    while Time.now - start < timeout
      return if yield
      sleep 0.02
    end
    raise "Timeout waiting for condition"
  end
end