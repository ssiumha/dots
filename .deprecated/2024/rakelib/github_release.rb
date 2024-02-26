# frozen_string_literal: true

require 'net/http'
require 'uri'
require 'fileutils'

# format examples:
#   - https://github.com/neovim/neovim/tags
#   - https://github.com/neovim/neovim/releases/latest
#   - https://github.com/neovim/neovim/releases/tag/stable
#   - https://github.com/neovim/neovim/releases/expanded_assets/v0.8.2
class GithubRelease
  def initialize(path, tag)
    @path, @tag = [path, tag]
  end

  def install
    download; unpack; symlink;
  rescue => e
    puts e
  end

  private

  def cache_dir
    "#{Dir.home}/.cache/dotfiles/gh/downloads/#{@path.gsub('/', '__')}"
  end

  # TODO linux case
  def download_url_path
    @download_url_path ||= begin
                             download_paths = assets_urls

                             download_paths = case `uname -s`.chop
                                              when /darwin/i then download_paths.grep(/darwin|apple|mac/)
                                              else download_paths.grep(/linux/)
                                              end

                             download_paths = assets_urls if download_paths.empty?

                             if download_paths.count > 1
                               download_paths = case `uname -m`.chop
                                                when /x86_64/ then download_paths.grep(/amd64|x86_64/)
                                                when /arm64/ then download_paths.grep(/arm64|aarch64/)
                                                else
                                                end
                             end

                             download_paths.first
                           end
  end

  def expanded_assets
    @expanded_assets ||= Net::HTTP.get_response URI("https://github.com/#{@path}/releases/expanded_assets/#{@tag}")
  end

  def assets_urls
    expanded_assets.body.split("\n").grep(/href/).grep_v(/sha256sum|deb|msi/).map { |url| /href="(?<href>.+?)"/ =~ url; href }
  end


  def download_filename
    @download_filename ||= begin
                             filename = download_url_path.split('/').last
                             "#{@tag}_#{filename}"
                           end
  end

  def download
    %x{ mkdir -p #{cache_dir} }

    if download_url_path.nil?
      binding.irb
      raise "not found download_url_path: #{@path}"
    end

    if File.exist?("#{cache_dir}/#{download_filename}")
      puts 'skip. file already exist'
    else
      %x{ curl -L -o "#{cache_dir}/#{download_filename}" "https://github.com/#{download_url_path}" }
    end
  end

  def unpack
    Dir.chdir(cache_dir) do
      %x{ rm -rf #{@tag}; mkdir #{@tag}; }

      case download_filename
      when /tar\.gz/ then %x{ tar -xf #{download_filename} -C ./#{@tag} }
      when /.zip/    then %x{ unzip #{download_filename} -d ./#{@tag} }
      else
      end
    end
  end

  def symlink
    bins = Dir.glob("#{cache_dir}/#{@tag}/**/*").select { |path| File.stat(path).mode.to_s(8).match('100755') }
    bins = bins.grep /bin/ if bins.count > 1

    bins.each do |binpath|
      binname = File.basename(binpath)
      FileUtils.rm "#{Dir.home}/.local/bin/#{binname}", force: true
      FileUtils.ln_s(binpath, "#{Dir.home}/.local/bin/#{binname}")
    end
  end
end
