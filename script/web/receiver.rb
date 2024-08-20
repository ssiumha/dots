require 'sinatra'
require 'time'

set :bind, '0.0.0.0'

data_dir = File.join(Dir.home, "logs/receiver")
Dir.mkdir data_dir unless File.exist? data_dir

get '/' do
  erb :index
end

post '/save' do
  text = params[:text]
  timestamp = Time.now.strftime("%Y%m%d-%H%M%S")
  path = File.join(data_dir, "txt-#{timestamp}.txt")

  File.write(path, text)

  redirect '/'
end

__END__

@@index
<!DOCTYPE html>
<html>
<head>
  <title>Receiver</title>
</head>
<body>
  <h1>Input Text</h1>
  <form action="/save" method="post">
    <textarea name="text" rows="4" cols="50"></textarea><br>
    <input type="submit" value="Submit">
  </form>
</body>
</html>
